#' Lintr
#'
#' Checks adherence to a given style, syntax errors and possible semantic
#' issues.  Supports on the fly checking of R code edited with Emacs, Vim and
#' Sublime Text.
#' @name lintr
#' @seealso \code{\link{lint}}, \code{\link{lint_package}}, \code{\link{linters}}
#' @importFrom stats na.omit
#' @importFrom utils capture.output getParseData relist
NULL

#' Lint a given file
#'
#' Apply one or more linters to a file and return a list of lints found.
#' @param filename the given filename to lint.
#' @param linters a list of linter functions to apply see \code{\link{linters}}
#' for a full list of default and available linters.
#' @param cache given a logical, toggle caching of lint results. If passed a
#' character string, store the cache in this directory.
#' @param ... additional arguments passed to \code{\link{exclude}}.
#' @param parse_settings whether to try and parse the settings
#' @export
#' @name lint_file
lint <- function(filename, linters = NULL, cache = FALSE, ..., parse_settings = TRUE) {

  inline_data <- rex::re_matches(filename, rex::rex(newline))
  if (inline_data) {
    content <- gsub("\n$", "", filename)
    filename <- tempfile()
    on.exit(unlink(filename))
    cat(file = filename, content, sep = "\n")
  }
  source_expressions <- get_source_expressions(filename)

  if (isTRUE(parse_settings)) {
    read_settings(filename)
    on.exit(clear_settings, add = TRUE)
  }

  if (is.null(linters)) {
    linters <- settings$linters
  } else if (!is.list(linters)) {
    name <- deparse(substitute(linters))
   linters <- list(linters)
    names(linters) <- name
  } else {
    names(linters) <- auto_names(linters)
  }

  lints <- list()
  itr <- 0

  cache_path <- if (isTRUE(cache)) {
    settings$cache_directory
  } else if (is.character(cache)) {
    cache
  } else {
    character(0)
  }

  if (length(cache_path)) {
    lint_cache <- load_cache(filename, cache_path)
    lints <- retrieve_file(lint_cache, filename, linters)
    if (!is.null(lints)) {
      return(exclude(lints, ...))
    }
    cache = TRUE
  } else {
    cache = FALSE
  }

  for (expr in source_expressions$expressions) {
    for (linter in names(linters)) {
      if (isTRUE(cache) && has_lint(lint_cache, expr, linter)) {
        lints[[itr <- itr + 1L]] <- retrieve_lint(lint_cache, expr, linter, source_expressions$lines)
      }
      else {
        expr_lints <- flatten_lints(linters[[linter]](expr)) # nolint

        lints[[itr <- itr + 1L]] <- expr_lints
        if (isTRUE(cache)) {
          cache_lint(lint_cache, expr, linter, expr_lints)
        }
      }
    }
  }

  if (inherits(source_expressions$error, "lint")) {
    lints[[itr <- itr + 1L]] <- source_expressions$error

    if (isTRUE(cache)) {
      cache_lint(lint_cache, list(filename = filename, content = ""), "error", source_expressions$error)
    }
  }

  lints <- structure(reorder_lints(flatten_lints(lints)), class = "lints")


  if (isTRUE(cache)) {
    cache_file(lint_cache, filename, linters, lints)
    save_cache(lint_cache, filename, cache_path)
  }

  res <- exclude(lints, ...)

  # simplify filename if inline
  if (inline_data) {
    for (i in seq_along(res)) {
      res[[i]][["filename"]] <- "<text>"
    }
  }
  res
}

reorder_lints <- function(lints) {
  files <- vapply(lints, `[[`, character(1), "filename")
  lines <- vapply(lints, `[[`, integer(1), "line_number")
  columns <- vapply(lints, `[[`, integer(1), "column_number")
  lints[
    order(files,
      lines,
      columns)
    ]
}

#' Lint a package
#'
#' Apply one or more linters to all of the R files in a package.
#' @param path the path to the base directory of the package, if \code{NULL},
#' the base directory will be searched for by looking in the parent directories
#' of the current directory.
#' @param relative_path if \code{TRUE}, file paths are printed using their path
#' relative to the package base directory.  If \code{FALSE}, use the full
#' absolute path.
#' @param ... additional arguments passed to \code{\link{lint}}
#' @export
lint_package <- function(path = ".", relative_path = TRUE, ...) {
  path <- find_package(path)

  read_settings(path)
  on.exit(clear_settings, add = TRUE)

  names(settings$exclusions) <- normalizePath(file.path(path, names(settings$exclusions)))
  exclusions <- force(settings$exclusions)

  files <- dir(
    path = file.path(path,
                     c("R",
                       "tests",
                       "inst")
                     ),
    pattern = rex::rex(".", one_of("Rr"), end),
    recursive = TRUE,
    full.names = TRUE
  )

  files <- normalizePath(files)

  lints <- flatten_lints(lapply(files,
      function(file) {
        if (interactive()) {
          message(".", appendLF = FALSE)
        }
        lint(file, ..., parse_settings = FALSE, exclusions = exclusions)
      }))

  if (interactive()) {
    message() # for a newline
  }

  lints <- reorder_lints(lints)

  if (relative_path == TRUE) {
    lints[] <- lapply(lints,
      function(x) {
        x$filename <- relative_path(x$filename, path)
        x
      })
    attr(lints, "path") <- path
  }

  class(lints) <- "lints"

  lints
}


# 'path' and 'relative_to' are both character vectors
relative_path <- function(path, relative_to = find_package()) {
  lp <- length(path)
  lr <- length(relative_to)
  if (lr != lp) {
    if (lr == 0L) {
      stop("'relative_to' must have length >= 1")
    } else if (lr == 1L) {
      relative_to <- rep.int(relative_to, lp)
    } else if (lp == 1L) {
      path <- rep.int(path, lr)
    } else {
      stop("'path' and 'relative_to' must have length 1 or the same length")
    }
  }

  r <- character(lp)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  relative_to <- normalizePath(relative_to, winslash = "/", mustWork = TRUE)

  # identical paths - the "." case
  which <- path == relative_to
  n_same <- sum(which)
  if (n_same) {
    r[which] <- rep.int(".", n_same)
  }

  # relative_to path is shorter - the "./" case
  np <- nchar(path)
  nr <- nchar(relative_to)
  which <- np > nr
  tmp <- strsplit(path[which], split = relative_to[which], fixed = TRUE)
  tmp <- vapply(tmp, `[[`, character(1L), 2L, USE.NAMES = FALSE)
  r[which] <- sub("^/", "", tmp)

  # relative_to path is longer - the "../" case
  which <- np < nr
  tmp <- strsplit(relative_to[which], split = path[which], fixed = TRUE)
  tmp <- vapply(tmp, `[[`, character(1L), 2L, USE.NAMES = FALSE)
  tmp <- strsplit(tmp, split = "/", fixed = TRUE)
  tmp <- vapply(tmp, length, integer(1L), USE.NAMES = FALSE) - 1L
  r[which] <- as.character(Map(
    function(n) {do.call(file.path, c(rep.int(list(".."), n), fsep = "/"))},
    tmp
  ))

  r
}


find_package <- function(path = getwd()) {
  start_path <- getwd()
  on.exit(setwd(start_path))
  setwd(path)

  prev_path <- ""
  while (!file.exists(file.path(prev_path, "DESCRIPTION"))) {
    # this condition means we are at the root directory, so give up
    if (prev_path %==% getwd()) {
      return(NULL)
    }
    prev_path <- getwd()
    setwd("..")
  }
  prev_path
}

pkg_name <- function(path = find_package()) {
  if (is.null(path)) {
    return(NULL)
  } else {
    read.dcf(file.path(path, "DESCRIPTION"), fields = "Package")[1]
  }
}

#' Create a \code{Lint} object
#' @param filename path to the source file that was linted.
#' @param line_number line number where the lint occurred.
#' @param column_number column the lint occurred.
#' @param type type of lint.
#' @param message message used to describe the lint error
#' @param line code source where the lint occured
#' @param ranges ranges on the line that should be emphasized.
#' @param linter name of linter that created the Lint object.
#' @export
Lint <- function(filename, line_number = 1L, column_number = 1L,
  type = c("style", "warning", "error"),
  message = "", line = "", ranges = NULL, linter = "") {

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
      linter = linter
      ),
    class = "lint")
}

rstudio_source_markers <- function(lints) {

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
    marker$message <- x$message
    marker
  })

  # request source markers
  rstudioapi::callFun("sourceMarkers",
                      name = "lintr",
                      markers = markers,
                      basePath = package_path,
                      autoSelect = "first")
}

#' Checkstyle Report for lint results
#'
#' Generate a report of the linting results using the
#' \href{http://checkstyle.sourceforge.net/}{Checkstyle} XML format.
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
      file.path(package_path, lints_per_file[[1]]$filename)
    } else {
      lints_per_file[[1]]$filename
    }
    f <- xml2::xml_add_child(n, "file", name = filename)

    lapply(lints_per_file, function(x) {
      xml2::xml_add_child(f, "error",
        line = as.character(x$line_number),
        column = as.character(x$column_number),
        severity = switch(x$type,
          style = "info",
          x$type),
        message = x$message)
    })
  })

  xml2::write_xml(d, filename)
}

highlight_string <- function(message, column_number = NULL, ranges = NULL) {

  maximum <- max(column_number, unlist(ranges))

  line <- fill_with(" ", maximum)

  lapply(ranges, function(range) {
    substr(line, range[1], range[2]) <<-
      fill_with("~", range[2] - range[1] + 1L)
    })

  substr(line, column_number, column_number + 1L) <- "^"

  line
}

fill_with <- function(character = " ", length = 1L) {
  paste0(collapse = "", rep.int(character, length))
}
