has_description <- function(path) {
  desc_info <- file.info(file.path(path, "DESCRIPTION"))
  !is.na(desc_info$size) && desc_info$size > 0.0 && !desc_info$isdir
}

find_package <- function(path) {
  depth <- 2L
  while (!has_description(path)) {
    path <- dirname(path)
    if (is_root(path) || depth <= 0L) {
      return(NULL)
    }
    depth <- depth - 1L
  }
  path
}

find_rproj_or_package <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)

  depth <- 2L
  while (!(has_description(path) || has_rproj(path))) {
    path <- dirname(path)
    if (is_root(path) || depth <= 0L) {
      return(NULL)
    }
    depth <- depth - 1L
  }
  path
}

has_rproj <- function(path) {
  length(head(Sys.glob(file.path(path, "*.Rproj")), n = 1L)) == 1L
}

find_rproj_at <- function(path) {
  head(Sys.glob(file.path(path, "*.Rproj")), n = 1L)
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
