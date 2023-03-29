has_description <- function(path) {
  desc_info <- file.info(file.path(path, "DESCRIPTION"))
  !is.na(desc_info$size) && desc_info$size > 0.0 && !desc_info$isdir
}

has_rproj <- function(path) {
  length(Sys.glob(file.path(path, "*.Rproj"))) > 0L
}

find_package <- function(path, allow_rproj = FALSE, max_depth = 2L) {
  path <- normalizePath(path, mustWork = !allow_rproj)
  if (allow_rproj) {
    found <- function(path) has_description(path) || has_rproj(path)
  } else {
    found <- function(path) has_description(path)
  }

  depth <- max_depth
  while (!found(path)) {
    path <- dirname(path)
    if (is_root(path) || depth <= 0L) {
      return(NULL)
    }
    depth <- depth - 1L
  }
  path
}

find_rproj_at <- function(path) {
  head(Sys.glob(file.path(path, "*.Rproj")), n = 1L)
}

is_root <- function(path) {
  identical(path, dirname(path))
}

is_directory <- function(filename) {
  is_dir <- file.info(filename)$isdir

  !is.na(is_dir) && is_dir
}

has_config <- function(path, config) {
  file.exists(file.path(path, config))
}

find_config <- function(filename) {
  if (is.null(filename)) {
    return(NULL)
  }
  linter_file <- getOption("lintr.linter_file")

  ## if users changed lintr.linter_file, return immediately.
  if (is_absolute_path(linter_file) && file.exists(linter_file)) {
    return(linter_file)
  }

  path <- if (is_directory(filename)) {
    filename
  } else {
    dirname(filename)
  }

  # List possible .lintr file locations
  file_locations <- c(
    # Check for a file in the current directory
    file.path(path, linter_file),
    # Next check for a file in the .github/linters directory
    file.path(path, ".github", "linters", linter_file),
    # Next check for a file in higher directories
    find_config2(path),
    # Next check for a file in the user directory
    # cf: rstudio@bc9b6a5 SessionRSConnect.R#L32
    file.path(Sys.getenv("HOME", unset = "~"), linter_file)
  )

  # Search through locations, return first valid result
  for (loc in file_locations) {
    if (isTRUE(file.exists(loc))) {
      return(loc)
    }
  }

  NULL
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
