# cache-wide stuff

#' @include cache.R

save_cache <- function(cache, ...) {
  UseMethod("save_cache", cache)
}

save_cache.default <- function(cache, file, path = NULL) {
  if (is.null(path)) {
    # Only retrieve settings if `path` isn't specified.
    # Otherwise, other settings may inadvertently be loaded, such as exclusions.
    read_settings(file)
    path <- settings$cache_directory
  }

  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  save(
    file = get_cache_file_path(file, path),
    envir = cache,
    list = ls(envir = cache)
  )
}

save_cache.Cache <- function(cache) {
  if (isTRUE(cache$should_cache)) {
    save_cache(cache$lint_cache, cache$filename, cache$cache_path)
  }
}

# file-specific stuff

cache_file <- function(cache, ...) {
  UseMethod("cache_file", cache)
}

cache_file.default <- function(cache, filename, linters, lints) {
  assign(
    envir = cache,
    x = digest_content(linters, filename),
    value = lints,
    inherits = FALSE
  )
}

cache_file.Cache <- function(cache, lints) {
  if (isTRUE(cache$should_cache)) {
    cache_file(cache$lint_cache, cache$filename, cache$linters, lints)
  }
}

# lint-specific stuff

retrieve_lint <- function(cache, ...) {
  UseMethod("retrieve_lint", cache)
}

retrieve_lint.default <- function(cache, expr, linter, lines) {
  lints <- get(
    envir = cache,
    x = digest_content(linter, expr),
    mode = "list",
    inherits = FALSE
  )
  lints[] <- lapply(lints, function(lint) {
    lint$line_number <- find_new_line(
      lint$line_number, unname(lint$line), lines
    )
    lint
  })
  cache_lint(cache, expr, linter, lints)
  lints
}

retrieve_lint.Cache <- function(cache, expr, linter, lines) {
  if (!"lint_cache" %in% names(cache)) {
    stop("Attempting to read from a non-existing cache")
  }
  retrieve_lint(
    cache$lint_cache, expr, linter, lines
  )
}

cache_lint <- function(cache, ...) {
  UseMethod("cache_lint", cache)
}

cache_lint.default <- function(cache, expr, linter, lints) {
  assign(
    envir = cache,
    x = digest_content(linter, expr),
    value = lints,
    inherits = FALSE)
}

cache_lint.Cache <- function(cache, expr, linter, lints) {
  if (isTRUE(cache$should_cache)) {
    cache_lint(cache$lint_cache, expr, linter, lints)
  }
}

has_lint <- function(cache, ...) {
  UseMethod("has_lint", cache)
}

has_lint.default <- function(cache, expr, linter) {
  exists(
    envir = cache,
    x = digest_content(linter, expr),
    mode = "list",
    inherits = FALSE
  )
}

has_lint.Cache <- function(cache, expr, linter) {
  isTRUE(cache$should_cache) && has_lint(cache$lint_cache, expr, linter)
}
