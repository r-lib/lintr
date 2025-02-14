#' Lint a file, directory, or package
#'
#' * `lint()` lints a single file.
#' * `lint_dir()` lints all files in a directory.
#' * `lint_package()` lints all likely locations for R files in a package, i.e.
#'   `R/`, `tests/`, `inst/`, `vignettes/`, `data-raw/`, `demo/`, and `exec/`.
#'
#' Read `vignette("lintr")` to learn how to configure which linters are run
#' by default.
#' Note that if files contain unparseable encoding problems, only the encoding problem will be linted to avoid
#' unintelligible error messages from other linters.
#'
#' @param filename Either the filename for a file to lint, or a character string of inline R code for linting.
#'   The latter (inline data) applies whenever `filename` has a newline character (\\n).
#' @param linters A named list of linter functions to apply. See [linters] for a full list of default and available
#'   linters.
#' @param ... Provide additional arguments to be passed to:
#'   - [exclude()] (in case of `lint()`; e.g. `lints` or `exclusions`)
#'   - [lint()] (in case of `lint_dir()` and `lint_package()`; e.g. `linters` or `cache`)
#' @param cache When logical, toggle caching of lint results. If passed a character string, store the cache in this
#'   directory.
#' @param parse_settings Logical, default `TRUE`. Whether to try and parse the [settings][read_settings]. Otherwise,
#'   the [default_settings()] are used.
#' @param text Optional argument for supplying a string or lines directly, e.g. if the file is already in memory or
#'   linting is being done ad hoc.
#'
#' @return An object of class `c("lints", "list")`, each element of which is a `"list"` object.
#'
#' @examples
#' # linting inline-code
#' lint("a = 123\n")
#' lint(text = "a = 123")
#'
#' # linting a file
#' f <- tempfile()
#' writeLines("a=1", f)
#' lint(f)
#' unlink(f)
#'
#' @export
lint <- function(filename, linters = NULL, ..., cache = FALSE, parse_settings = TRUE, text = NULL) {
  # TODO(#2502): Remove this workaround.
  dot_names <- if (getRversion() %in% c("4.1.1", "4.1.2")) names(list(...)) else ...names()
  check_dots(dot_names, c("exclude", "parse_exclusions"))

  needs_tempfile <- missing(filename) || re_matches(filename, rex(newline))
  inline_data <- !is.null(text) || needs_tempfile
  lines <- get_lines(filename, text)

  if (needs_tempfile) {
    filename <- tempfile()
    con <- file(filename, open = "w", encoding = settings$encoding)
    on.exit(unlink(filename), add = TRUE)
    writeLines(text = lines, con = con, sep = "\n")
    close(con)
  }

  filename <- normalize_path(filename, mustWork = !inline_data) # to ensure a unique file in cache
  source_expressions <- get_source_expressions(filename, lines)

  if (isTRUE(parse_settings)) {
    read_settings(filename)
    on.exit(reset_settings(), add = TRUE)
  }

  linters <- define_linters(linters)
  linters <- Map(validate_linter_object, linters, names(linters))

  cache_path <- define_cache_path(cache)

  lint_cache <- load_cache(filename, cache_path)
  lint_obj <- define_cache_key(filename, inline_data, lines)
  lints <- retrieve_file(lint_cache, lint_obj, linters)
  if (!is.null(lints)) {
    return(exclude(lints, lines = lines, linter_names = names(linters), ...))
  }

  file_linter_names <- names(linters)[vapply(linters, is_linter_level, logical(1L), "file")]
  expression_linter_names <- names(linters)[vapply(linters, is_linter_level, logical(1L), "expression")]

  lints <- list()
  if (!is_tainted(source_expressions$lines)) {
    for (expr in source_expressions$expressions) {
      if (is_lint_level(expr, "expression")) {
        necessary_linters <- expression_linter_names
      } else {
        necessary_linters <- file_linter_names
      }
      for (linter in necessary_linters) {
        # use withCallingHandlers for friendlier failures on unexpected linter errors
        lints[[length(lints) + 1L]] <- withCallingHandlers(
          get_lints(expr, linter, linters[[linter]], lint_cache, source_expressions$lines),
          error = function(cond) {
            cli_abort(
              "Linter {.fn linter} failed in {.file {filename}}:",
              parent = cond
            )
          }
        )
      }
    }
  }

  lints <- maybe_append_error_lint(lints, source_expressions$error, lint_cache, filename)
  lints <- reorder_lints(flatten_lints(lints))
  class(lints) <- c("lints", "list")

  cache_file(lint_cache, filename, linters, lints)
  save_cache(lint_cache, filename, cache_path)

  res <- exclude(lints, lines = lines, linter_names = names(linters), ...)

  # simplify filename if inline
  zap_temp_filename(res, needs_tempfile)
}

#' @param path For the base directory of the project (for `lint_dir()`) or
#'   package (for `lint_package()`).
#' @param relative_path if `TRUE`, file paths are printed using their path relative to the base directory.
#'   If `FALSE`, use the full absolute path.
#' @param exclusions exclusions for [exclude()], relative to the package path.
#' @param pattern pattern for files, by default it will take files with any of the extensions
#' .R, .Rmd, .qmd, .Rnw, .Rhtml, .Rrst, .Rtex, .Rtxt allowing for lowercase r (.r, ...).
#' @param show_progress Logical controlling whether to show linting progress with a simple text
#'   progress bar _via_ [utils::txtProgressBar()]. The default behavior is to show progress in
#'   [interactive()] sessions not running a testthat suite.
#'
#' @examples
#' if (FALSE) {
#'   lint_dir()
#'
#'   lint_dir(
#'     linters = list(semicolon_linter()),
#'     exclusions = list(
#'       "inst/doc/creating_linters.R" = 1,
#'       "inst/example/bad.R",
#'       "renv"
#'     )
#'   )
#' }
#' @export
#' @rdname lint
lint_dir <- function(path = ".", ...,
                     relative_path = TRUE,
                     exclusions = list("renv", "packrat"),
                     # TODO(r-lib/rex#85): Re-write in case-sensitive rex()
                     pattern = "(?i)[.](r|rmd|qmd|rnw|rhtml|rrst|rtex|rtxt)$",
                     parse_settings = TRUE,
                     show_progress = NULL) {
  # TODO(#2502): Remove this workaround.
  dot_names <- if (getRversion() %in% c("4.1.1", "4.1.2")) names(list(...)) else ...names()
  check_dots(dot_names, c("lint", "exclude", "parse_exclusions"))

  if (isTRUE(parse_settings)) {
    read_settings(path)
    on.exit(reset_settings(), add = TRUE)

    exclusions <- c(exclusions, settings$exclusions)
  }

  if (is.null(show_progress)) show_progress <- interactive() && !identical(Sys.getenv("TESTTHAT"), "true")

  exclusions <- normalize_exclusions(
    exclusions,
    root = path,
    pattern = pattern
  )

  # normalize_path ensures names(exclusions) and files have the same names for the same files.
  # It also ensures all paths have forward slash
  # Otherwise on windows, files might incorrectly not be excluded in to_exclude
  files <- normalize_path(dir(
    path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  ))

  # Remove fully ignored files to avoid reading & parsing
  files <- drop_excluded(files, exclusions)

  if (length(files) == 0L) {
    lints <- list()
    class(lints) <- "lints"
    return(lints)
  }

  if (isTRUE(show_progress)) {
    lints <- lapply(
      # NB: This cli API is experimental (https://github.com/r-lib/cli/issues/709)
      cli::cli_progress_along(files, name = "Running linters"),
      function(idx) {
        lint(files[idx], ..., parse_settings = FALSE, exclusions = exclusions)
      }
    )
  } else {
    lints <- lapply(
      files,
      function(file) { # nolint: unnecessary_lambda_linter.
        lint(file, ..., parse_settings = FALSE, exclusions = exclusions)
      }
    )
  }

  lints <- flatten_lints(lints)
  lints <- reorder_lints(lints)

  if (relative_path) {
    path <- normalize_path(path, mustWork = FALSE)
    lints[] <- lapply(
      lints,
      function(x) {
        x$filename <- re_substitutes(x$filename, rex(path, one_of("/", "\\")), "")
        x
      }
    )
    attr(lints, "path") <- path
  }

  class(lints) <- "lints"

  lints
}

drop_excluded <- function(files, exclusions) {
  to_exclude <- vapply(
    files,
    function(file) file %in% names(exclusions) && is_excluded_file(exclusions[[file]]),
    logical(1L)
  )
  files[!to_exclude]
}

#' @examples
#' if (FALSE) {
#'   lint_package()
#'
#'   lint_package(
#'     linters = linters_with_defaults(semicolon_linter = semicolon_linter()),
#'     exclusions = list("inst/doc/creating_linters.R" = 1, "inst/example/bad.R")
#'   )
#' }
#' @export
#' @rdname lint
lint_package <- function(path = ".", ...,
                         relative_path = TRUE,
                         exclusions = list("R/RcppExports.R"),
                         parse_settings = TRUE,
                         show_progress = NULL) {
  if (length(path) > 1L) {
    cli_abort(c(
      x = "Only linting one package at a time is supported.",
      i = "Instead, {.val {length(path)}} package paths were provided."
    ))
  }
  pkg_path <- find_package(path)

  if (is.null(pkg_path)) {
    cli_warn(c(
      i = "Didn't find any R package searching upwards from {.file {normalize_path(path)}}"
    ))
    return(NULL)
  }

  if (parse_settings) {
    read_settings(pkg_path)
    on.exit(reset_settings(), add = TRUE)
  }

  exclusions <- normalize_exclusions(
    c(exclusions, settings$exclusions),
    root = pkg_path
  )

  r_directories <- file.path(pkg_path, c("R", "tests", "inst", "vignettes", "data-raw", "demo", "exec"))
  lints <- lint_dir(r_directories,
    relative_path = FALSE,
    exclusions = exclusions,
    parse_settings = FALSE,
    show_progress = show_progress,
    ...
  )

  if (isTRUE(relative_path)) {
    path <- normalize_path(pkg_path, mustWork = FALSE)
    lints[] <- lapply(
      lints,
      function(x) {
        x$filename <- re_substitutes(x$filename, rex(path, one_of("/", "\\")), "")
        x
      }
    )
    attr(lints, "path") <- path
  }

  lints
}

#' Run a linter on a source expression, optionally using a cache
#'
#' @param expr A source expression.
#' @param linter Name of the linter.
#' @param linter_fun Closure of the linter.
#' @param lint_cache Cache environment, or `NULL` if caching is disabled.
#'
#' @return A list of lints generated by the linter on `expr`.
#'
#' @noRd
get_lints <- function(expr, linter, linter_fun, lint_cache, lines) {
  expr_lints <- NULL
  if (has_lint(lint_cache, expr, linter)) {
    # retrieve_lint() might return NULL if missing line number is encountered.
    # It could be caused by nolint comments.
    expr_lints <- retrieve_lint(lint_cache, expr, linter, lines)
  }

  if (is.null(expr_lints)) {
    expr_lints <- flatten_lints(linter_fun(expr))

    for (i in seq_along(expr_lints)) {
      expr_lints[[i]]$linter <- linter
    }

    cache_lint(lint_cache, expr, linter, expr_lints)
  }
  expr_lints
}

define_linters <- function(linters = NULL) {
  if (is.null(linters)) {
    linters <- settings$linters
    names(linters) <- auto_names(linters)
  } else if (is_linter(linters)) {
    linters <- list(linters)
    names(linters) <- attr(linters[[1L]], "name", exact = TRUE)
  } else if (is.list(linters)) {
    names(linters) <- auto_names(linters)
  } else {
    name <- deparse(substitute(linters))
    linters <- list(linters)
    names(linters) <- name
  }
  linters
}

validate_linter_object <- function(linter, name) {
  if (is_linter(linter)) {
    return(linter)
  }
  cli_abort(c(
    i = "Expected {.fn {name}} to be a function of class {.cls linter}.",
    x = "Instead, it is {.obj_type_friendly {linter}}."
  ))
}

is_linter_factory <- function(fun) {
  # A linter factory is a function whose last call is to Linter()
  bdexpr <- body(fun)
  # covr internally transforms each call into if (TRUE) { covr::count(...); call }
  while (is.call(bdexpr) && (bdexpr[[1L]] == "{" || (bdexpr[[1L]] == "if" && bdexpr[[2L]] == "TRUE"))) {
    bdexpr <- bdexpr[[length(bdexpr)]]
  }
  is.call(bdexpr) && identical(bdexpr[[1L]], as.name("Linter"))
}

reorder_lints <- function(lints) {
  files <- vapply(lints, `[[`, character(1L), "filename")
  lines <- vapply(lints, `[[`, integer(1L), "line_number")
  columns <- vapply(lints, `[[`, integer(1L), "column_number")
  lints[order(
    files,
    lines,
    columns
  )]
}


#' Create a `lint` object
#' @param filename path to the source file that was linted.
#' @param line_number line number where the lint occurred.
#' @param column_number column number where the lint occurred.
#' @param type type of lint.
#' @param message message used to describe the lint error
#' @param line code source where the lint occurred
#' @param ranges a list of ranges on the line that should be emphasized.
#' @return an object of class `c("lint", "list")`.
#' @name lint-s3
#' @export
Lint <- function(filename, line_number = 1L, column_number = 1L, # nolint: object_name.
                 type = c("style", "warning", "error"),
                 message = "", line = "", ranges = NULL) {
  validate_lint_object(message, line, line_number, column_number, ranges)

  type <- match.arg(type)

  obj <- list(
    filename = filename,
    line_number = as.integer(line_number),
    column_number = as.integer(column_number),
    type = type,
    message = message,
    line = line,
    ranges = ranges,
    linter = NA_character_
  )
  class(obj) <- c("lint", "list")
  obj
}

validate_lint_object <- function(message, line, line_number, column_number, ranges) {
  if (length(message) != 1L || !is.character(message)) {
    cli_abort("{.arg message} must be a character string")
  }
  if (is.object(message)) {
    cli_abort("{.arg message} must be a simple string, but has class {.cls {class(message)}}")
  }
  if (length(line) != 1L || !is.character(line)) {
    cli_abort("{.arg line} must be a character string.")
  }
  max_col <- max(nchar(line) + 1L, 1L, na.rm = TRUE)
  if (!is_number(column_number) || column_number < 0L || column_number > max_col) {
    cli_abort("
      {.arg column_number} must be an integer between {.val {0}} and {.val {max_col}} ({.code nchar(line) + 1}),
      not {.obj_type_friendly {column_number}}.
    ")
  }
  if (!is_number(line_number) || line_number < 1L) {
    cli_abort("{.arg line_number} must be a positive integer, not {.obj_type_friendly {line_number}}.")
  }
  check_ranges(ranges, max_col)
  invisible()
}

is_number <- function(number, n = 1L) {
  length(number) == n && is.numeric(number) && !anyNA(number)
}

is_valid_range <- function(range, max_col) {
  0L <= range[[1L]] &&
    range[[1L]] <= range[[2L]] &&
    range[[2L]] <= max_col
}

check_ranges <- function(ranges, max_col, call = parent.frame()) {
  if (is.null(ranges)) {
    return()
  }
  if (!is.list(ranges)) {
    cli_abort(
      "{.arg ranges} must be {.code NULL} or a list, not {.obj_type_friendly {ranges}}.",
      call = call
    )
  }

  for (range in ranges) {
    if (!is_number(range, 2L)) {
      cli_abort(
        "{.arg ranges} must only contain integer vectors of length 2 without {.code NA}s.",
        call = call
      )
    } else if (!is_valid_range(range, max_col)) {
      cli_abort(
        "{.arg ranges} must satisfy {.val {0}} <= range[1L] <= range[2L] <= {.val {max_col}} (nchar(line) + 1).",
        call = call
      )
    }
  }
}

rstudio_source_markers <- function(lints) {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    cli_abort("{.pkg rstudioapi} is required for {.fn rstudio_source_markers}.") # nocov
  }

  # package path will be NULL unless it is a relative path
  package_path <- attr(lints, "path")

  # generate the markers
  markers <- lapply(lints, function(x) {
    filename <- if (!is.null(package_path)) {
      file.path(package_path, x$filename)
    } else {
      x$filename
    }

    marker <- list()
    marker$type <- x$type
    marker$file <- filename
    marker$line <- x$line_number
    marker$column <- x$column_number
    marker$message <- paste0("[", x$linter, "] ", x$message)
    marker
  })

  # request source markers
  out <- rstudioapi::callFun(
    "sourceMarkers",
    name = "lintr",
    markers = markers,
    basePath = package_path,
    autoSelect = "first"
  )

  # workaround to avoid focusing an empty Markers pane
  # when possible, better solution is to delete the "lintr" source marker list
  # https://github.com/rstudio/rstudioapi/issues/209
  if (length(lints) == 0L) {
    Sys.sleep(0.1)
    rstudioapi::executeCommand("activateConsole")
  }

  out
}

#' Checkstyle Report for lint results
#'
#' Generate a report of the linting results using the [Checkstyle](https://checkstyle.sourceforge.io) XML format.
#'
#' @param lints the linting results.
#' @param filename the name of the output report
#' @export
checkstyle_output <- function(lints, filename = "lintr_results.xml") {
  # package path will be NULL unless it is a relative path
  package_path <- attr(lints, "path")

  # setup file
  d <- xml2::xml_new_document()
  n <- xml2::xml_add_child(d, "checkstyle", version = paste0("lintr-", utils::packageVersion("lintr")))

  # output the style markers to the file
  lapply(split(lints, names(lints)), function(lints_per_file) {
    filename <- if (!is.null(package_path)) {
      file.path(package_path, lints_per_file[[1L]]$filename)
    } else {
      lints_per_file[[1L]]$filename
    }
    f <- xml2::xml_add_child(n, "file", name = filename)

    lapply(lints_per_file, function(x) {
      xml2::xml_add_child(
        f, "error",
        line = as.character(x$line_number),
        column = as.character(x$column_number),
        severity = switch(x$type,
          style = "info",
          x$type
        ),
        message = x$message
      )
    })
  })

  xml2::write_xml(d, filename)
}

#' SARIF Report for lint results
#'
#' Generate a report of the linting results using the [SARIF](https://sarifweb.azurewebsites.net/) format.
#'
#' @param lints the linting results.
#' @param filename the name of the output report
#' @export
sarif_output <- function(lints, filename = "lintr_results.sarif") {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cli_abort("{.pkg jsonlite} is required to produce SARIF reports. Please install to continue.") # nocov
  }

  # package path will be `NULL` unless it is a relative path
  package_path <- attr(lints, "path")

  if (is.null(package_path)) {
    cli_abort("Package path needs to be a relative path.")
  }

  # setup template
  sarif <- jsonlite::fromJSON(
    system.file("extdata", "sarif-template.json", package = "lintr"),
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )

  # assign values
  sarif$runs[[1L]]$results <- NULL
  sarif$runs[[1L]]$tool$driver$rules <- NULL
  sarif$runs[[1L]]$tool$driver$version <- as.character(utils::packageVersion("lintr"))
  sarif$runs[[1L]]$originalUriBaseIds$ROOTPATH$uri <- ""
  rule_index_exists <- FALSE
  root_path_uri <- gsub("\\", "/", package_path, fixed = TRUE)

  if (startsWith(root_path_uri, "/")) {
    root_path_uri <- paste0("file://", root_path_uri)
  } else {
    root_path_uri <- paste0("file:///", root_path_uri) # nocov
  }

  if (!endsWith(root_path_uri, "/")) {
    root_path_uri <- paste0(root_path_uri, "/")
  }

  sarif$runs[[1L]]$originalUriBaseIds$ROOTPATH$uri <- root_path_uri

  # loop and assign result values
  for (lint in lints) {
    one_result <- list()

    if (is.null(sarif$runs[[1L]]$tool$driver$rules)) {
      rule_index_exists <- 0L
    } else {
      rule_index_exists <-
        which(vapply(
          sarif$runs[[1L]]$tool$driver$rules,
          function(x) x$id == lint$linter,
          logical(1L)
        ))
      if (length(rule_index_exists) == 0L || is.na(rule_index_exists[1L])) {
        rule_index_exists <- 0L
      }
    }

    if (rule_index_exists == 0L) {
      new_rule <- list(
        id = lint$linter,
        fullDescription = list(text = lint$message),
        defaultConfiguration = list(
          level = switch(lint$type, style = "note", lint$type)
        )
      )
      sarif$runs[[1L]]$tool$driver$rules <- append(sarif$runs[[1L]]$tool$driver$rules, list(new_rule))
      rule_index <- length(sarif$runs[[1L]]$tool$driver$rules) - 1L
    } else {
      rule_index <- rule_index_exists - 1L
    }

    one_result <- append(one_result, c(ruleId = lint$linter))
    one_result <- append(one_result, c(ruleIndex = rule_index))
    one_result <- append(one_result, list(message = list(text = lint$message)))
    one_location <- list(physicalLocation = list(
      artifactLocation = list(
        uri = gsub("\\", "/", lint$filename, fixed = TRUE),
        uriBaseId = "ROOTPATH"
      ),
      region = list(
        startLine = lint$line_number,
        startColumn = lint$column_number,
        snippet = list(text = lint$line)
      )
    ))
    one_result <- append(one_result, c(locations = list(list(one_location))))
    sarif$runs[[1L]]$results <- append(sarif$runs[[1L]]$results, list(one_result))
  }

  # if lints is empty, add empty results list
  if (length(lints) == 0L) {
    sarif$runs[[1L]]$results <- list()
  }

  write(jsonlite::toJSON(sarif, pretty = TRUE, auto_unbox = TRUE), filename)
}

highlight_string <- function(message, column_number = NULL, ranges = NULL) {
  maximum <- max(column_number, unlist(ranges))

  line <- fill_with(" ", maximum)

  for (range in ranges) {
    substr(line, range[1L], range[2L]) <- fill_with("~", range[2L] - range[1L] + 1L)
  }

  substr(line, column_number, column_number + 1L) <- "^"

  line
}

fill_with <- function(character = " ", length = 1L) {
  paste(collapse = "", rep.int(character, length))
}

has_positional_logical <- function(dots) {
  length(dots) > 0L &&
    is.logical(dots[[1L]]) &&
    !nzchar(names2(dots)[1L])
}

maybe_append_error_lint <- function(lints, error, lint_cache, filename) {
  if (is_lint(error)) {
    error$linter <- "error"
    lints[[length(lints) + 1L]] <- error

    if (!is.null(lint_cache)) {
      cache_lint(lint_cache, list(filename = filename, content = ""), "error", error)
    }
  }
  lints
}

get_lines <- function(filename, text) {
  if (!is.null(text)) {
    strsplit(paste(text, collapse = "\n"), "\n", fixed = TRUE)[[1L]]
  } else if (re_matches(filename, rex(newline))) {
    strsplit(gsub("\n$", "", filename), "\n", fixed = TRUE)[[1L]]
  } else {
    read_lines(filename)
  }
}

zap_temp_filename <- function(res, needs_tempfile) {
  if (needs_tempfile) {
    for (i in seq_along(res)) {
      res[[i]][["filename"]] <- "<text>"
    }
  }
  res
}
