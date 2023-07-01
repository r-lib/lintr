
#' Read lintr settings
#'
#' Lintr searches for settings for a given source file in the following order.
#'  1. options defined as `linter.setting`.
#'  2. `linter_file` in the same directory
#'  3. `linter_file` in the project directory
#'  4. `linter_file` in the user home directory
#'  5. [default_settings()]
#'
#' The default linter_file name is `.lintr` but it can be changed with option `lintr.linter_file`
#' or the environment variable `R_LINTR_LINTER_FILE`
#' This file is a dcf file, see [base::read.dcf()] for details.
#' @param filename source file to be linted
read_settings <- function(filename) {
  clear_settings()

  config_file <- find_config(filename)
  default_encoding <- find_default_encoding(filename)
  if (!is.null(default_encoding)) {
    # Locally override the default for encoding if we found a smart default
    default_settings[["encoding"]] <- default_encoding
  }

  if (!is.null(config_file)) {
    malformed <- function(e) {
      stop("Malformed config file, ensure it ends in a newline\n  ", conditionMessage(e), call. = FALSE)
    }
    config <- tryCatch(
      read.dcf(config_file, all = TRUE),
      warning = malformed,
      error = malformed
    )
  } else {
    config <- NULL
  }

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

get_setting <- function(setting, config, defaults) {
  option <- getOption(paste(sep = ".", "lintr", setting))
  if (!is.null(option)) {
    option
  } else if (!is.null(config[[setting]])) {
    malformed <- function(e) {
      stop("Malformed config setting '", setting, "'\n  ", conditionMessage(e), call. = FALSE)
    }
    tryCatch(
      eval(parse(text = config[[setting]])),
      error = malformed
    )
  } else {
    defaults[[setting]]
  }
}

clear_settings <- function() {
  rm(list = ls(settings), envir = settings)
}

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
