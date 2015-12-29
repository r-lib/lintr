
#' Read lintr settings
#'
#' Lintr searches for settings for a given file in the following order.
#' \enumerate{
#'   \item options defined as \code{linter.setting}.
#'   \item \code{linter_file} in the same directory
#'   \item \code{linter_file} in the project directory
#'   \item \code{\link{default_settings}}
#' }
#'
#' The default linter_file name is \code{.lintr} but it can be changed with option
#' \code{lintr.linter_file}.  This file is a dcf file, see \code{\link{read.dcf}} for details.
#' @param filename source file to be linted
read_settings <- function(filename) {
  clear_settings()

  config_file <- find_config(filename)

  if (!is.null(config_file)) {
    config <- read.dcf(config_file, all = TRUE)
  } else {
    config <- NULL
  }

  for (setting in names(default_settings)) {
    value <- get_setting(setting, config, default_settings)
    if (setting == "exclusions") {
      value <- normalize_exclusions(value)
    }

    settings[[setting]] <- value
  }
}

get_setting <- function(setting, config, defaults) {
  option <- getOption(paste(sep = ".", "lintr", setting))
  if (!is.null(option)) {
    option
  } else if (!is.null(config[[setting]])) {
    eval(parse(text = config[[setting]]))
  } else {
    defaults[[setting]]
  }
}

clear_settings <- function() {
  rm(list = ls(settings), envir = settings)
}

find_config <- function(filename) {

  if (is.null(filename)) {
    return(NULL)
  }
  linter_file <- getOption("lintr.linter_file")

  if (!is_directory(filename)) {
    path <- dirname(filename)
  } else {
    path <- filename
  }
  ## check for a file in the current directory
  linter_config <- file.path(path, linter_file)
  if (isTRUE(file.exists(linter_config))) {
    return(linter_config)
  }

  ## next check for a file in the project directory
  project <- find_package(path)
  linter_config <- file.path(project, linter_file)
  if (isTRUE(file.exists(linter_config))) {
    return(linter_config)
  }

  return(NULL)
}

is_directory <- function(filename) {
  is_dir <- file.info(filename)$isdir

  !is.na(is_dir) && is_dir
}
