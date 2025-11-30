#' Read lintr settings
#'
#' Lintr searches for settings for a given source file in the following order:
#'  1. options defined as `linter.setting`.
#'  2. `linter_file` in the same directory
#'  3. `linter_file` in the project directory
#'  4. `linter_file` in the user home directory
#'  5. [default_settings()]
#'
#' The default linter_file name is `.lintr` but it can be changed with option `lintr.linter_file`
#'   or the environment variable `R_LINTR_LINTER_FILE`
#' This file is a DCF file, see [base::read.dcf()] for details.
#' Here is an example of a `.lintr` file:
#'
#'  ```
#'  linters: linters_with_defaults(
#'      any_duplicated_linter(),
#'      any_is_na_linter(),
#'      backport_linter("oldrel-4", except = c("R_user_dir", "str2lang")),
#'      line_length_linter(120L),
#'      missing_argument_linter(),
#'      unnecessary_concatenation_linter(allow_single_expression = FALSE),
#'      yoda_test_linter()
#'    )
#'  exclusions: list(
#'      "inst/doc/creating_linters.R" = 1,
#'      "inst/example/bad.R",
#'      "tests/testthat/default_linter_testcode.R",
#'      "tests/testthat/dummy_packages"
#'    )
#'  ```
#'
#' Experimentally, we also support keeping the config in a plain R file. By default we look for
#'   a file named `.lintr.R` (in the same directories where we search for `.lintr`).
#' We are still deciding the future of config support in lintr, so user feedback is welcome.
#'   The advantage of R is that it maps more closely to how the configs are actually stored,
#'   whereas the DCF approach requires somewhat awkward formatting of parseable R code within
#'   valid DCF key-value pairs. The main disadvantage of the R file is it might be _too_ flexible,
#'   with users tempted to write configs with side effects causing hard-to-detect bugs or
# "   otherwise "abusing" the ability to evaluate generic R code. Other recursive key-value stores
#'   like YAML could work, but require new dependencies and are harder to parse
#'   both programmatically and visually.
#' Here is an example of a `.lintr.R` file:
#'
#'  ```r
#'  linters <- linters_with_defaults(
#'      any_duplicated_linter(),
#'      any_is_na_linter(),
#'      backport_linter("oldrel-4", except = c("R_user_dir", "str2lang")),
#'      line_length_linter(120L),
#'      missing_argument_linter(),
#'      unnecessary_concatenation_linter(allow_single_expression = FALSE),
#'      yoda_test_linter()
#'    )
#'  exclusions <- list(
#'      "inst/doc/creating_linters.R" = 1,
#'      "inst/example/bad.R",
#'      "tests/testthat/default_linter_testcode.R",
#'      "tests/testthat/dummy_packages"
#'    )
#'  ```
#'
#' @param filename Source file to be linted.
#' @param call Passed to malformed to ensure linear trace.
read_settings <- function(filename, call = parent.frame()) {
  reset_settings()

  # doing lint(text=) should read settings from the current directory, #2847
  location <- if (missing(filename)) "." else filename
  config_file <- find_config(location)
  default_encoding <- find_default_encoding(location)
  if (!is.null(default_encoding)) {
    # Locally override the default for encoding if we found a smart default
    default_settings[["encoding"]] <- default_encoding
  }

  config <- read_config_file(config_file, call = call)
  validate_config_file(config, config_file, default_settings)

  for (setting in names(default_settings)) {
    value <- get_setting(setting, config, default_settings)
    if (setting == "exclusions") {
      if (!is.null(config_file)) {
        root <- dirname(config_file)
      } else {
        root <- getwd()
      }
      value <- normalize_exclusions(value, root = root)
    }

    settings[[setting]] <- value
  }
}

#' @param call Passed to malformed to ensure linear trace.
#' @noRd
read_config_file <- function(config_file, call = parent.frame()) {
  if (is.null(config_file)) {
    return(NULL)
  }

  # clickable link for eventual error messages.
  malformed_file <- link_config_file(config_file) # nolint: object_usage_linter. TODO(#2252).
  config <- new.env()
  if (endsWith(config_file, ".R")) {
    load_config <- function(file) sys.source(file, config, keep.source = FALSE, keep.parse.data = FALSE)
    malformed <- function(e) {
      cli_abort(
        "Malformed config file ({malformed_file}), ensure it is valid R syntax.",
        parent = e,
        call = call
      )
    }
  } else {
    load_config <- function(file) {
      dcf_values <- read.dcf(file, all = TRUE)
      for (setting in names(dcf_values)) {
        parsed_setting <- withCallingHandlers(
          str2lang(dcf_values[[setting]]),
          error = function(e) {
            cli_abort(
              "Malformed config setting {.field {setting}}:",
              parent = e
            )
          }
        )
        # https://adv-r.hadley.nz/conditions.html
        setting_value <- withCallingHandlers(
          tryCatch(
            eval(parsed_setting),
            error = function(e) {
              cli_abort(
                "Error from config setting {.code {setting}}.",
                parent = e
              )
            }
          ),
          warning = function(w) {
            cli_warn(
              "Warning from config setting {.code {setting}}.",
              parent = w
            )
            invokeRestart("muffleWarning")
          }
        )
        assign(setting, setting_value, envir = config)
      }
    }
    malformed <- function(e) {
      cli_abort(
        "Malformed config file ({malformed_file}):",
        parent = e,
        call = call
      )
    }
  }
  # https://adv-r.hadley.nz/conditions.html
  withCallingHandlers(
    tryCatch(
      load_config(config_file),
      error = malformed
    ),
    warning = function(w) {
      cli_warn(
        "Warning encountered while loading config:",
        parent = w
      )
      invokeRestart("muffleWarning")
    }
  )
  config
}

validate_config_file <- function(config, config_file, defaults) {
  matched <- names(config) %in% names(defaults)
  if (!all(matched)) {
    unused_settings <- names(config)[!matched] # nolint: object_usage_linter. TODO(#2252).
    config_link <- link_config_file(config_file) # nolint: object_usage_linter. TODO(#2252).
    cli_warn("Found unused settings in config file ({config_link}): {.field unused_settings}")
  }

  validate_regex(config,
    c("exclude", "exclude_next", "exclude_start", "exclude_end", "exclude_linter", "exclude_linter_sep")
  )
  validate_character_string(config, c("encoding", "cache_directory"))
  validate_true_false(config, "error_on_lint")
  validate_linters(config$linters)
  validate_exclusions(config$exclusions)
}

is_character_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
# perl=TRUE matches rex::re_matches()
is_valid_regex <- function(str) !inherits(tryCatch(grepl(str, "", perl = TRUE), condition = identity), "condition")
is_single_regex <- function(x) is_character_string(x) && is_valid_regex(x)
is_true_false <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)

validate_keys <- function(config, keys, test, what) {
  for (key in keys) {
    val <- config[[key]]
    if (is.null(val)) {
      next
    }
    if (!test(val)) {
      cli_abort(c(
        i = "Setting {.code {key}} should be {.strong {what}}.",
        x = "Instead, it is {.field {val}}."
      ))
    }
  }
}

validate_regex <- function(config, keys) {
  validate_keys(config, keys, is_single_regex, "a single regular expression")
}

validate_character_string <- function(config, keys) {
  validate_keys(config, keys, is_character_string, "a character string")
}

validate_true_false <- function(config, keys) {
  validate_keys(config, keys, is_true_false, "TRUE or FALSE")
}

validate_linters <- function(linters) {
  if (is.null(linters)) {
    return(invisible())
  }

  is_linters <- vapply(linters, is_linter, logical(1L))
  if (!all(is_linters)) {
    non_linters <- which(!is_linters) # nolint: object_usage_linter. TODO(#2252).
    cli_abort(c(
      i = "Setting {.arg linters} should be a list of linters.",
      x = "Found non-linters at elements: {.str {non_linters}}."
    ))
  }
}

validate_exclusions <- function(exclusions) {
  if (is.null(exclusions)) {
    return(invisible())
  }

  exclusion_names <- names2(exclusions)
  has_names <- nzchar(exclusion_names)
  unnamed_is_string <-
    vapply(exclusions[!has_names], \(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1L))
  if (!all(unnamed_is_string)) {
    problematic_entries <- which(!has_names)[!unnamed_is_string] # nolint: object_usage_linter. TODO(#2252).
    cli_abort(c(
      i = "Unnamed entries of setting {.arg exclusions} should be strings naming files or directories.",
      x = "Check exclusions: {.str {problematic_entries}}."
    ))
  }
  for (ii in which(has_names)) validate_named_exclusion(exclusions, ii)
}

validate_named_exclusion <- function(exclusions, idx) {
  entry <- exclusions[[idx]]
  if (is.list(entry)) {
    valid_entry <- vapply(entry, \(x) is.numeric(x) && !anyNA(x), logical(1L))
  } else {
    valid_entry <- is.numeric(entry) && !anyNA(entry)
  }
  if (!all(valid_entry)) {
    cli_abort(c(
      i = "Named entries of setting {.arg exclusions} should designate line numbers for exclusion.",
      x = "Check exclusions: {idx}."
    ))
  }
}

lintr_option <- function(setting, default = NULL) getOption(paste0("lintr.", setting), default)

get_setting <- function(setting, config, defaults) {
  lintr_option(setting) %||% config[[setting]] %||% defaults[[setting]]
}

reset_settings <- function() list2env(default_settings, envir = settings)

find_default_encoding <- function(filename) {
  if (is.null(filename)) {
    return(NULL)
  }

  filename |>
    find_package(allow_rproj = TRUE) |>
    find_rproj_at() |>
    get_encoding_from_dcf()
}

get_encoding_from_dcf <- function(file) {
  if (is.null(file)) {
    return(NULL)
  }

  encodings <- tryCatch(
    unname(drop(read.dcf(file, "Encoding"))),
    error = \(e) NULL,
    warning = \(e) NULL
  )

  encodings <- encodings[!is.na(encodings)]
  if (length(encodings) > 0L) {
    return(encodings[1L])
  }

  NULL
}

link_config_file <- function(path) {
  cli::style_hyperlink(
    cli::col_blue(basename(path)),
    paste0("file://", path)
  )
}
