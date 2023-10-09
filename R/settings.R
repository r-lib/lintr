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
#' Experimentally, we also support keeping the config in a plain R file. By default we look for
#'   a file named '.lintr.R' (in the same directories where we search for '.lintr').
#' We are still deciding the future of config support in lintr, so user feedback is welcome.
#'   The advantage of R is that it maps more closely to how the configs are actually stored,
#'   whereas the DCF approach requires somewhat awkward formatting of parseable R code within
#'   valid DCF key-value pairs. The main disadvantage of the R file is it might be _too_ flexible,
#'   with users tempted to write configs with side effects causing hard-to-detect bugs or
#"   otherwise "abusing" the ability to evaluate generic R code. Other recursive key-value stores
#'   like YAML could work, but require new dependencies and are harder to parse
#'   both programmatically and visually.
#' @param filename source file to be linted
read_settings <- function(filename) {
  reset_settings()

  config_file <- find_config(filename)
  default_encoding <- find_default_encoding(filename)
  if (!is.null(default_encoding)) {
    # Locally override the default for encoding if we found a smart default
    default_settings[["encoding"]] <- default_encoding
  }

  config <- read_config_file(config_file)
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

read_config_file <- function(config_file) {
  if (is.null(config_file)) {
    return(NULL)
  }

  config <- new.env()
  if (endsWith(config_file, ".R")) {
    load_config <- function(file) sys_source(file, config)
    malformed <- function(e) {
      stop("Malformed config file, ensure it is valid R syntax\n  ", conditionMessage(e), call. = FALSE)
    }
  } else {
    load_config <- function(file) {
      dcf_values <- read.dcf(file, all = TRUE)
      for (setting in names(dcf_values)) {
        tryCatch(
          assign(setting, eval(str2lang(dcf_values[[setting]])), envir = config),
          error = function(e) stop("Malformed config setting '", setting, "'\n  ", conditionMessage(e), call. = FALSE)
        )
      }
    }
    malformed <- function(e) {
      stop("Malformed config file, ensure it ends in a newline\n  ", conditionMessage(e), call. = FALSE)
    }
  }
  tryCatch(load_config(config_file), warning = malformed, error = malformed)
  config
}

validate_config_file <- function(config, config_file, defaults) {
  matched <- names(config) %in% names(defaults)
  if (!all(matched)) {
    warning("Found unused settings in config '", config_file, "': ", toString(names(config)[!matched]))
  }

  validate_regex(config,
    c("exclude", "exclude_next", "exclude_start", "exclude_end", "exclude_linter", "exclude_linter_sep")
  )
  validate_character_string(config, c("encoding", "cache_directory", "comment_token"))
  validate_true_false(config, c("comment_bot", "error_on_lint"))
  validate_linters(config$linters)
  validate_exclusions(config$exclusions)
}

is_character_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
is_valid_regex <- function(str) !inherits(tryCatch(grepl(str, ""), condition = identity), "condition")
is_single_regex <- function(x) is_character_string(x) && is_valid_regex(x)
is_true_false <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)

validate_keys <- function(config, keys, test, what) {
  for (key in keys) {
    val <- config[[key]]
    if (is.null(val)) {
      next
    }
    if (!test(val)) {
      stop("Setting '", key, "' should be ", what, ", not '", toString(val), "'.")
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
    stop(
      "Setting 'linters' should be a list of linters, but found non-linters at elements ",
      toString(which(!is_linters)), "."
    )
  }
}

validate_exclusions <- function(exclusions) {
  if (is.null(exclusions)) {
    return(invisible())
  }

  exclusion_names <- names2(exclusions)
  has_names <- exclusion_names != ""
  unnamed_is_string <-
    vapply(exclusions[!has_names], function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1L))
  if (!all(unnamed_is_string)) {
    stop(
      "Unnamed entries of setting 'exclusions' should be strings naming files or directories, check entries: ",
      toString(which(!has_names)[!unnamed_is_string]), "."
    )
  }
  for (ii in which(has_names)) validate_named_exclusion(exclusions, ii)
}

validate_named_exclusion <- function(exclusions, idx) {
  entry <- exclusions[[idx]]
  if (is.list(entry)) {
    valid_entry <- vapply(entry, function(x) is.numeric(x) && !anyNA(x), logical(1L))
  } else {
    valid_entry <- is.numeric(entry) && !anyNA(entry)
  }
  if (!all(valid_entry)) {
    stop(
      "Named entries of setting 'exclusions' should designate line numbers for exclusion, ",
      "check exclusion: ", idx, "."
    )
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

  root_path <- find_package(filename, allow_rproj = TRUE)
  rproj_enc <- get_encoding_from_dcf(find_rproj_at(root_path))
  if (!is.null(rproj_enc)) {
    return(rproj_enc)
  }
  rproj_enc
}

get_encoding_from_dcf <- function(file) {
  if (is.null(file)) {
    return(NULL)
  }

  encodings <- tryCatch(
    unname(drop(read.dcf(file, "Encoding"))),
    error = function(e) NULL,
    warning = function(e) NULL
  )

  if (!is.null(encodings)) {
    # Produces a warning in R <= 3.5 if encoding is NULL
    encodings <- encodings[!is.na(encodings)]
  }
  if (length(encodings) > 0L) {
    return(encodings[1L])
  }

  NULL
}
