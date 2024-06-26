#' Clear the lintr cache
#'
#' @param file filename whose cache to clear. If you pass `NULL`, it will delete all of the caches.
#' @param path directory to store caches. Reads option 'lintr.cache_directory' as the default.
#' @return 0 for success, 1 for failure, invisibly.
#' @export
clear_cache <- function(file = NULL, path = NULL) {
  if (is.null(path)) {
    # Only retrieve settings if `path` isn't specified.
    # Otherwise, other settings may inadvertently be loaded, such as exclusions.
    read_settings(file)
    path <- settings$cache_directory
  }

  if (!is.null(file)) {
    path <- get_cache_file_path(file, path)
  }

  unlink(path, recursive = TRUE)
}

define_cache_path <- function(cache) {
  if (isTRUE(cache)) {
    settings$cache_directory
  } else if (is.character(cache)) {
    cache
  } else {
    character()
  }
}

define_cache_key <- function(filename, inline_data, lines) {
  if (inline_data) list(content = get_content(lines), TRUE) else filename
}


get_cache_file_path <- function(file, path) {
  # this assumes that a normalized absolute file path was given
  file.path(path, digest::digest(file, algo = "sha1"))
}

load_cache <- function(file, path = NULL) {
  if (is.null(path) || length(path) == 0L) {
    return()
  }
  env <- new.env(parent = emptyenv())

  file <- get_cache_file_path(file, path)
  if (file.exists(file)) {
    tryCatch(
      load(file = file, envir = env),
      warning = function(w) {
        invokeRestart("muffleWarning")
      },
      error = function(e) {
        cli_warn(
          "Could not load cache file {.file {file}}:",
          parent = e
        )
      }
    )
  } # else nothing to do for source file that has no cache

  env
}

save_cache <- function(cache, file, path = NULL) {
  if (is.null(cache)) {
    return()
  }
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  save(file = get_cache_file_path(file, path), envir = cache, list = ls(envir = cache))
}

cache_file <- function(cache, filename, linters, lints) {
  if (is.null(cache)) {
    return()
  }
  assign(
    envir = cache,
    x = digest_content(linters, filename),
    value = lints,
    inherits = FALSE
  )
}

retrieve_file <- function(cache, filename, linters) {
  if (is.null(cache)) {
    return(NULL)
  }
  mget(
    envir = cache,
    x = digest_content(linters, filename),
    mode = "list",
    inherits = FALSE,
    ifnotfound = list(NULL)
  )[[1L]]
}

cache_lint <- function(cache, expr, linter, lints) {
  if (is.null(cache)) {
    return()
  }
  assign(
    envir = cache,
    x = digest_content(linter, expr),
    value = lints,
    inherits = FALSE
  )
}

retrieve_lint <- function(cache, expr, linter, lines) {
  lints <- get(
    envir = cache,
    x = digest_content(linter, expr),
    mode = "list",
    inherits = FALSE
  )
  for (i in seq_along(lints)) {
    new_line_number <- find_new_line(
      lints[[i]]$line_number,
      unname(lints[[i]]$line),
      lines
    )
    if (is.na(new_line_number)) {
      return(NULL)
    }
    lints[[i]]$line_number <- new_line_number
  }
  cache_lint(cache, expr, linter, lints)
  lints
}

has_lint <- function(cache, expr, linter) {
  if (is.null(cache)) {
    return(FALSE)
  }
  exists(
    envir = cache,
    x = digest_content(linter, expr),
    mode = "list",
    inherits = FALSE
  )
}

digest_content <- function(linters, obj) {
  content <- if (is.list(obj)) {
    # assume an expression (global expression if obj$parsed_content is lacking)
    list(linters, obj$content, is.null(obj$parsed_content))
  } else {
    # assume a filename
    list(linters, readLines(obj))
  }
  digest::digest(content, algo = "sha1")
}

find_new_line <- function(line_number, line, lines) {
  if (lines[line_number] %==% line) {
    return(line_number)
  }

  width <- 1L

  while (width <= length(lines)) {
    low <- line_number - width
    if (low > 0L && lines[low] %==% line) {
      return(low)
    }
    high <- line_number + width
    if (high <= length(lines) && lines[high] %==% line) {
      return(high)
    }
    width <- width + 1L
  }
  NA
}
