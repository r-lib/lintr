#' Lintr
#'
#' Checks adherence to a given style, syntax errors and possible semantic issues.
#' Supports on the fly checking of R code edited with Emacs, Vim and Sublime Text.
#' @seealso [lint()], [lint_package()], [lint_dir()], [linters]
#' @importFrom stats na.omit
#' @importFrom utils capture.output getParseData relist
#' @keywords internal
"_PACKAGE"

#' Lint a file, directory, or package
#'
#' * `lint()` lints a single file.
#' * `lint_dir()` lints all files in a directory.
#' * `lint_package()` lints all likely locations for R files in a package, i.e.
#'   `R/`, `tests/`, `inst/`, `vignettes/`, `data-raw/`, and `demo/`.
#'
#' Read `vigentte("lintr")` to learn how to configure which linters are run
#' by default.
#' Note that if files contain unparseable encoding problems, only the encoding problem will be linted to avoid
#' unintelligible error messages from other linters.
#'
#' @param filename either the filename for a file to lint, or a character string of inline R code for linting.
#' The latter (inline data) applies whenever `filename` has a newline character (\\n).
#' @param linters a named list of linter functions to apply. See [linters] for a full list of default and available
#' linters.
#' @param ... additional arguments passed to [exclude()].
#' @param cache given a logical, toggle caching of lint results. If passed a character string, store the cache in this
#' directory.
#' @param parse_settings whether to try and parse the settings.
#' @param text Optional argument for supplying a string or lines directly, e.g. if the file is already in memory or
#' linting is being done ad hoc.
#'
#' @aliases lint_file
# TODO(next release after 3.0.0): remove the alias
#' @return A list of lint objects.
#'
#' @examples
#' \dontrun{
#'   lint("some/file-name.R") # linting a file
#'   lint("a = 123\n")        # linting inline-code
#'   lint(text = "a = 123")   # linting inline-code
#' }
#'
#' @export
lint <- function(filename, linters = NULL, ..., cache = FALSE, parse_settings = TRUE, text = NULL) {
  # TODO(next release after 3.0.0): remove this deprecated workaround
  dots <- list(...)
  if (has_positional_logical(dots)) {
    warning(
      "'cache' is no longer available as a positional argument; please supply 'cache' as a named argument instead. ",
      "This warning will be upgraded to an error in the next release."
    )
    cache <- dots[[1L]]
    dots <- dots[-1L]
  }

  needs_tempfile <- missing(filename) || rex::re_matches(filename, rex::rex(newline))
  inline_data <- !is.null(text) || needs_tempfile
  lines <- get_lines(filename, text)

  if (needs_tempfile) {
    filename <- tempfile()
    con <- file(filename, open = "w", encoding = settings$encoding)
    on.exit(unlink(filename), add = TRUE)
    writeLines(text = lines, con = con, sep = "\n")
    close(con)
  }

  filename <- normalizePath(filename, mustWork = !inline_data)  # to ensure a unique file in cache
  source_expressions <- get_source_expressions(filename, lines)

  if (isTRUE(parse_settings)) {
    read_settings(filename)
    on.exit(clear_settings, add = TRUE)
  }

  linters <- define_linters(linters)
  linters <- Map(validate_linter_object, linters, names(linters))

  cache_path <- define_cache_path(cache)

  lint_cache <- load_cache(filename, cache_path)
  lint_obj <- define_cache_key(filename, inline_data, lines)
  lints <- retrieve_file(lint_cache, lint_obj, linters)
  if (!is.null(lints)) {
    # TODO: once cache= is fully deprecated as 3rd positional argument (see top of body), we can restore the cleaner:
    # > exclude(lints, lines = lines, ...)
    return(do.call(exclude, c(list(lints, lines = lines, linter_names = names(linters)), dots)))
  }

  lints <- list()
  if (!is_tainted(source_expressions$lines)) {
    for (expr in source_expressions$expressions) {
      for (linter in names(linters)) {
        lints[[length(lints) + 1L]] <- get_lints(expr, linter, linters[[linter]], lint_cache, source_expressions$lines)
      }
    }
  }

  lints <- maybe_append_error_lint(lints, source_expressions$error, lint_cache, filename)
  lints <- structure(reorder_lints(flatten_lints(lints)), class = "lints")

  cache_file(lint_cache, filename, linters, lints)
  save_cache(lint_cache, filename, cache_path)

  # TODO: once cache= is fully deprecated as 3rd positional argument (see top of body), we can restore the cleaner:
  # > exclude(lints, lines = lines, ...)
  res <- do.call(exclude, c(list(lints, lines = lines, linter_names = names(linters)), dots))

  # simplify filename if inline
  zap_temp_filename(res, needs_tempfile)
}

#' @param path For the base directory of the project (for `lint_dir()`) or
#'   package (for `lint_package()`).
#' @param ... additional arguments passed to [lint()], e.g. `linters` or `cache`.
#' @param relative_path if `TRUE`, file paths are printed using their path relative to the base directory.
#'   If `FALSE`, use the full absolute path.
#' @param exclusions exclusions for [exclude()], relative to the package path.
#' @param pattern pattern for files, by default it will take files with any of the extensions .R, .Rmd, .Rnw, .Rhtml,
#' .Rrst, .Rtex, .Rtxt allowing for lowercase r (.r, ...)
#' @examples
#' \dontrun{
#'   lint_dir()
#'   lint_dir(
#'     linters = list(semicolon_linter())
#'     cache = TRUE,
#'     exclusions = list("inst/doc/creating_linters.R" = 1, "inst/example/bad.R", "renv")
#'   )
#' }
#' @export
#' @rdname lint
lint_dir <- function(path = ".", ...,
                     relative_path = TRUE,
                     exclusions = list("renv", "packrat"),
                     pattern = rex::rex(".", one_of("Rr"), or("", "html", "md", "nw", "rst", "tex", "txt"), end),
                     parse_settings = TRUE) {
  # TODO(next release after 3.0.0): remove this deprecated workaround
  dots <- list(...)
  if (has_positional_logical(dots)) {
    warning(
      "'relative_path' is no longer available as a positional argument; ",
      "please supply 'relative_path' as a named argument instead. ",
      "This warning will be upgraded to an error in the next release."
    )
    relative_path <- dots[[1L]]
    dots <- dots[-1L]
  }

  if (isTRUE(parse_settings)) {
    read_settings(path)
    on.exit(clear_settings, add = TRUE)

    exclusions <- c(exclusions, settings$exclusions)
  }

  exclusions <- normalize_exclusions(
    exclusions,
    root = path,
    pattern = pattern
  )

  # normalizePath ensures names(exclusions) and files have the same names for the same files.
  # Otherwise on windows, files might incorrectly not be excluded in to_exclude
  files <- normalizePath(dir(
    path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  ))

  # Remove fully ignored files to avoid reading & parsing
  files <- drop_excluded(files, exclusions)

  lints <- flatten_lints(lapply(
    files,
    function(file) {
      maybe_report_progress()
      # TODO: once relative_path= is fully deprecated as 2nd positional argument (see top of body), restore the cleaner:
      # > lint(file, ..., parse_settings = FALSE, exclusions = exclusions)
      do.call(lint, c(list(file, parse_settings = FALSE, exclusions = exclusions), dots))
    }
  ))

  maybe_report_progress(done = TRUE)

  lints <- reorder_lints(lints)

  if (relative_path == TRUE) {
    path <- normalizePath(path, mustWork = FALSE)
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
#' \dontrun{
#'   lint_package()
#'
#'   lint_package(
#'     linters = linters_with_defaults(semicolon_linter = semicolon_linter())
#'     cache = TRUE,
#'     exclusions = list("inst/doc/creating_linters.R" = 1, "inst/example/bad.R")
#'   )
#' }
#' @export
#' @rdname lint
lint_package <- function(path = ".", ...,
                         relative_path = TRUE,
                         exclusions = list("R/RcppExports.R"),
                         parse_settings = TRUE) {
  # TODO(next release after 3.0.0): remove this deprecated workaround
  dots <- list(...)
  if (has_positional_logical(dots)) {
    warning(
      "'relative_path' is no longer available as a positional argument; ",
      "please supply 'relative_path' as a named argument instead. ",
      "This warning will be upgraded to an error in the next release."
    )
    relative_path <- dots[[1L]]
    dots <- dots[-1L]
  }

  pkg_path <- find_package(path)

  if (is.null(pkg_path)) {
    warning("Didn't find any R package searching upwards from '", path, "'.")
    return(NULL)
  }

  if (parse_settings) {
    read_settings(pkg_path)
    on.exit(clear_settings, add = TRUE)
  }

  exclusions <- normalize_exclusions(
    c(exclusions, settings$exclusions),
    root = pkg_path
  )

  r_directories <- file.path(pkg_path, c("R", "tests", "inst", "vignettes", "data-raw", "demo"))
  # TODO: once relative_path= is fully deprecated as 2nd positional argument (see top of body), restore the cleaner:
  # > lints <- lint_dir(r_directories, relative_path = FALSE, exclusions = exclusions, parse_settings = FALSE, ...)
  lints <- do.call(
    lint_dir,
    c(list(r_directories, relative_path = FALSE, exclusions = exclusions, parse_settings = FALSE), dots)
  )

  if (isTRUE(relative_path)) {
    path <- normalizePath(pkg_path, mustWork = FALSE)
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
  } else if (inherits(linters, "linter")) {
    linters <- list(linters)
    names(linters) <- attr(linters[[1L]], "name", exact = TRUE)
  } else if (!is.list(linters)) {
    name <- deparse(substitute(linters))
    linters <- list(linters)
    names(linters) <- name
  } else {
    names(linters) <- auto_names(linters)
  }
  linters
}

validate_linter_object <- function(linter, name) {
  if (!inherits(linter, "linter") && is.function(linter)) {
    if (is_linter_factory(linter)) {
      old <- "Passing linters as variables"
      new <- "a call to the linters (see ?linters)"
      lintr_deprecated(old = old, new = new, version = "3.0.0",
                       type = "")
      linter <- linter()
    } else {
      old <- "The use of linters of class 'function'"
      new <- "linters classed as 'linter' (see ?Linter)"
      lintr_deprecated(old = old, new = new, version = "3.0.0",
                       type = "")
      linter <- Linter(linter, name = name)
    }
  } else if (!is.function(linter)) {
    stop(gettextf("Expected '%s' to be a function of class 'linter', not a %s of class '%s'",
                  name, typeof(linter), class(linter)[[1L]]))
  }
  linter
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

has_description <- function(path) {
  desc_info <- file.info(file.path(path, "DESCRIPTION"))
  !is.na(desc_info$size) && desc_info$size > 0.0 && !desc_info$isdir
}

find_package <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)

  while (!has_description(path)) {
    path <- dirname(path)
    if (is_root(path)) {
      return(NULL)
    }
  }

  path
}

find_rproj_at <- function(path) {
  head(list.files(path = path, pattern = "\\.Rproj$", full.names = TRUE), 1L)
}

find_rproj <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)

  while (length(res <- find_rproj_at(path)) == 0L) {
    path <- dirname(path)
    if (is_root(path)) {
      return(NULL)
    }
  }

  res
}

is_root <- function(path) {
  identical(path, dirname(path))
}

has_config <- function(path, config) {
  file.exists(file.path(path, config))
}

find_config2 <- function(path) {
  config <- basename(getOption("lintr.linter_file"))
  path <- normalizePath(path, mustWork = FALSE)

  while (!has_config(path, config)) {
    path <- dirname(path)
    if (is_root(path)) {
      return(character())
    }
  }
  return(file.path(path, config))
}

pkg_name <- function(path = find_package()) {
  if (is.null(path)) {
    return(NULL)
  } else {
    read.dcf(file.path(path, "DESCRIPTION"), fields = "Package")[1L]
  }
}

#' Create a `lint` object
#' @param filename path to the source file that was linted.
#' @param line_number line number where the lint occurred.
#' @param column_number column number where the lint occurred.
#' @param type type of lint.
#' @param message message used to describe the lint error
#' @param line code source where the lint occurred
#' @param ranges a list of ranges on the line that should be emphasized.
#' @param linter deprecated. No longer used.
#' @return an object of class 'lint'.
#' @name lint-s3
#' @export
Lint <- function(filename, line_number = 1L, column_number = 1L, # nolint: object_name.
                 type = c("style", "warning", "error"),
                 message = "", line = "", ranges = NULL, linter = "") {
  if (!missing(linter)) {
    lintr_deprecated(
      old = "Using the `linter` argument of `Lint()`",
      version = "3.0.0",
      type = ""
    )
  }

  type <- match.arg(type)

  structure(
    list(
      filename = filename,
      line_number = as.integer(line_number),
      column_number = as.integer(column_number),
      type = type,
      message = message,
      line = line,
      ranges = ranges,
      linter = NA_character_
    ),
    class = "lint")
}

rstudio_source_markers <- function(lints) {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("'rstudioapi' is required for rstudio_source_markers().") # nocov
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
        severity = switch(
          x$type,
          style = "info",
          x$type
        ),
        message = x$message)
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

  # package path will be NULL unless it is a relative path
  package_path <- attr(lints, "path")

  # setup template
  sarif <- jsonlite::fromJSON(
    '{
      "$schema": "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0-rtm.5.json",
      "version": "2.1.0",
      "runs": [
        {
          "tool": {
            "driver": {
              "name": "lintr",
              "informationUri": "https://lintr.r-lib.org/",
              "version": "2.0.1",
              "rules": [
                {
                  "id": "trailing_whitespace_linter",
                  "fullDescription": {
                    "text": "Trailing whitespace is superfluous."
                  },
                  "defaultConfiguration": {
                    "level": "note"
                  }
                }
              ]
            }
          },
          "results": [
            {
              "ruleId": "trailing_whitespace_linter",
              "ruleIndex": 0,
              "message": {
                "text": "Trailing blank lines are superfluous."
              },
              "locations": [
                {
                  "physicalLocation": {
                    "artifactLocation": {
                      "uri": "TestFileFolder/hello.r",
                      "uriBaseId": "ROOTPATH"
                    },
                    "region": {
                      "startLine": 2,
                      "startColumn": 22,
                      "snippet": {
                        "text": "print(Hello World!) "
                      }
                    }
                  }
                }
              ]
            }
          ],
          "columnKind": "utf16CodeUnits",
          "originalUriBaseIds": {
            "ROOTPATH": {
              "uri": "file:///C:/repos/repototest/"
            }
          }
        }
      ]
    }',
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )

  # assign values
  sarif$runs[[1L]]$results <- NULL
  sarif$runs[[1L]]$tool$driver$rules <- NULL
  sarif$runs[[1L]]$tool$driver$version <-
    as.character(utils::packageVersion("lintr"))
  sarif$runs[[1L]]$originalUriBaseIds$ROOTPATH$uri <- ""
  rule_index_exists <- FALSE
  root_path_uri <- gsub("\\\\", "/", package_path)

  if (startsWith(root_path_uri, "/")) {
    root_path_uri <- paste("file://", root_path_uri, sep = "")
  } else {
    root_path_uri <- paste("file:///", root_path_uri, sep = "")
  }

  if (!endsWith(root_path_uri, "/")) {
    root_path_uri <- paste(root_path_uri, "/", sep = "")
  }

  sarif$runs[[1L]]$originalUriBaseIds$ROOTPATH$uri <- root_path_uri

  # loop and assign result values
  for (lint in lints) {
    one_result <- list()

    if (is.null(sarif$runs[[1L]]$tool$driver$rules)) {
      rule_index_exists <- 0L
    } else {
      rule_index_exists <-
        which(sapply(sarif$runs[[1L]]$tool$driver$rules,
                     function(x) x$id == lint$linter))
      if (length(rule_index_exists) == 0L ||
          is.na(rule_index_exists[1L])) {
        rule_index_exists <- 0L
      }
    }

    if (rule_index_exists == 0L) {
      new_rule <- list(
        id = lint$linter,
        fullDescription = list(text = lint$message),
        defaultConfiguration = list(level = switch(lint$type,
                                                   style = "note",
                                                   lint$type))
      )
      sarif$runs[[1L]]$tool$driver$rules <-
        append(sarif$runs[[1L]]$tool$driver$rules, list(new_rule))
      rule_index <- length(sarif$runs[[1L]]$tool$driver$rules) - 1L
    } else {
      rule_index <- rule_index_exists - 1L
    }

    one_result <- append(one_result, c(ruleId = lint$linter))
    one_result <- append(one_result, c(ruleIndex = rule_index))
    one_result <-
      append(one_result, list(message = list(text = lint$message)))
    one_location <- list(physicalLocation = list(
      artifactLocation = list(
        uri = gsub("\\\\", "/", lint$filename),
        uriBaseId = "ROOTPATH"
      ),
      region = list(
        startLine = lint$line_number,
        startColumn = lint$column_number,
        snippet = list(text = lint$line)
      )
    ))
    one_result <-
      append(one_result, c(locations = list(list(one_location))))

    sarif$runs[[1L]]$results <-
      append(sarif$runs[[1L]]$results, list(one_result))
  }

  write(jsonlite::toJSON(sarif, pretty = TRUE, auto_unbox = TRUE),
        filename)
}

highlight_string <- function(message, column_number = NULL, ranges = NULL) {

  maximum <- max(column_number, unlist(ranges))

  line <- fill_with(" ", maximum)

  lapply(ranges, function(range) {
    substr(line, range[1L], range[2L]) <<-
      fill_with("~", range[2L] - range[1L] + 1L)
  })

  substr(line, column_number, column_number + 1L) <- "^"

  line
}

fill_with <- function(character = " ", length = 1L) {
  paste0(collapse = "", rep.int(character, length))
}

has_positional_logical <- function(dots) {
  length(dots) > 0L &&
    is.logical(dots[[1L]]) &&
    !nzchar(names2(dots)[1L])
}

maybe_report_progress <- function(done = FALSE) {
  if (interactive() && !identical(Sys.getenv("TESTTHAT"), "true")) {
    # nocov start
    if (done) {
      message()
    } else {
      message(".", appendLF = FALSE)
    }
    # nocov end
  }
}

maybe_append_error_lint <- function(lints, error, lint_cache, filename) {
  if (inherits(error, "lint")) {
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
  } else if (rex::re_matches(filename, rex::rex(newline))) {
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
