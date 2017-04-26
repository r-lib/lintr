#' Clear the lintr cache
#'
#' @param file filename whose cache to clear.  If you pass \code{NULL}, it will
#' delete all of the caches.
#' @param dir directory to store caches.  Reads option 'lintr.cache_directory'
#' as the default.
#' @param path deprecated argument.
#' @return 0 for success, 1 for failure, invisibly.
#' @export
clear_cache <- function(file = NULL, dir = NULL, path = NULL) {
  if (!missing(path)) {
    lintr_deprecated("path", "dir", "1.0.0.9001", type="Argument")
    dir <- path
  }

  read_settings(file)

  if (is.null(dir)) {
    dir <- settings$cache_directory
  }

  path <- if (is.null(file)) {
    dir
  } else {
    get_cache_file_path(file, dir)
  }

  unlink(path, recursive = TRUE)
}


get_cache_file_path <- function(file, dir) {
  file.path(
    dir,
    tryCatch(
      relative_path(file),
      error=function(e){basename(file)}
      # fallback if file not relative to package, e.g. temporary file created by expect_lint() when
      # called with a string instead of a file
    )
  )
}


load_cache <- function(file, dir = NULL) {
  read_settings(file)

  if (is.null(dir)) {
    dir <- settings$cache_directory
  }

  env <- new.env(parent = emptyenv())

  file <- get_cache_file_path(file, dir)
  if (file.exists(file)) {
    load(file = file, envir = env)
  } # else nothing to do for source file that has no cache

  env
}

save_cache <- function(cache, file, dir = NULL) {
  read_settings(file)

  if (is.null(dir)) {
    dir <- settings$cache_directory
  }

  file <- get_cache_file_path(file, dir)
  dir <- dirname(file)
  if (!file.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  save(file = file, envir = cache, list = ls(envir = cache))
}

cache_file <- function(cache, filename, linters, lints) {
  assign(envir = cache,
    x = digest::digest(list(linters, readLines(filename)), algo = "sha1"),
    value = lints
  )
}

retrieve_file <- function(cache, filename, linters) {
  key <- digest::digest(list(linters, readLines(filename)), algo = "sha1")
  if (exists(key, envir = cache)) {
    get(
      envir = cache,
      x = digest::digest(list(linters, readLines(filename)), algo = "sha1")
      )
  } else {
    NULL
  }
}

cache_lint <- function(cache, expr, linter, lints) {
  assign(
    envir = cache,
    x = digest::digest(list(linter, expr$content), algo = "sha1"),
    value = lints)
}

retrieve_lint <- function(cache, expr, linter, lines) {
  lints <- get(
    envir = cache,
    x = digest::digest(list(linter, expr$content), algo = "sha1")
  )
  lints[] <- lapply(lints, function(lint) {
    lint$line_number <- find_new_line(lint$line_number, unname(lint$line), lines)
    lint
  })
  cache_lint(cache, expr, linter, lints)
  lints
}

has_lint <- function(cache, expr, linter) {
  exists(envir = cache,
    x = digest::digest(list(linter, expr$content), algo = "sha1"),
    )
}

find_new_line <- function(line_number, line, lines) {

  if (lines[line_number] %==% line) {
    return(line_number)
  }

  width <- 1L

  while (width <= length(lines)) {
    low <- line_number - width
    if (low > 0L) {
      if (lines[low] %==% line) {
        return(low)
      }
    }
    high <- line_number + width
    if (high <= length(lines)) {
      if (lines[high] %==% line) {
        return(high)
      }
    }
    width <- width + 1L
  }
  NA
}
