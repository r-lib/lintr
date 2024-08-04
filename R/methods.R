#' @export
format.lint <- function(x, ..., width = getOption("lintr.format_width")) {
  if (requireNamespace("cli", quietly = TRUE)) {
    color <- switch(x$type,
      warning = cli::col_magenta,
      error = cli::col_red,
      style = cli::col_blue,
      cli::style_bold
    )
    emph <- cli::style_bold
  } else {
    # nocov start
    color <- identity
    emph <- identity
    # nocov end
  }

  annotated_msg <- paste0(
    emph(
      x$filename, ":",
      as.character(x$line_number), ":",
      as.character(x$column_number), ": ",
      sep = ""
    ),
    color(x$type, ": ", sep = ""),
    "[", x$linter, "] ",
    emph(x$message)
  )

  if (!is.null(width)) {
    annotated_msg <- paste(strwrap(annotated_msg, exdent = 4L, width = width), collapse = "\n")
  }

  paste0(
    annotated_msg, "\n",
    # swap tabs for spaces for #528 (sorry Richard Hendricks)
    chartr("\t", " ", x$line), "\n",
    highlight_string(x$message, x$column_number, x$ranges),
    "\n"
  )
}

#' @export
print.lint <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

markdown <- function(x, info, ...) {
  cat(
    sep = "",
    "[", x$filename, ":",
    as.character(x$line_number), ":",
    as.character(x$column_number), ":", "]",
    "(",
    file.path(
      "https://github.com",
      info$user,
      info$repo,
      "blob",
      info$commit,
      x$filename
    ), "#L", x$line_number,
    ")",
    " ",
    "*", x$type, ":", "* ",
    "[", x$linter, "] ",
    "**", x$message, "**\n",
    "```r\n\U200B", # we use a zero width unicode character here so that Github
    # does not strip the leading whitespace
    x$line, "\n",
    highlight_string(x$message, x$column_number, x$ranges),
    "\n```\n"
  )
  invisible(x)
}

#' @export
format.lints <- function(x, ..., width = getOption("lintr.format_width")) {
  paste(vapply(x, format, character(1L), width = width), collapse = "\n")
}

#' @export
print.lints <- function(x, ...) {
  use_rstudio_source_markers <- lintr_option("rstudio_source_markers", TRUE) &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::hasFun("sourceMarkers")

  github_annotation_project_dir <- lintr_option("github_annotation_project_dir", "")

  if (length(x) > 0L) {
    inline_data <- x[[1L]][["filename"]] == "<text>"
    if (!inline_data && use_rstudio_source_markers) {
      rstudio_source_markers(x)
    } else if (in_github_actions() && !in_pkgdown()) {
      github_actions_log_lints(x, project_dir = github_annotation_project_dir)
    } else {
      lapply(x, print, ...)
    }

    if (isTRUE(settings$error_on_lint)) quit("no", 31L, FALSE) # nocov
  }

  if (length(x) == 0L) {
    cli_inform(c("i" = "No lints found."))
    if (use_rstudio_source_markers) rstudio_source_markers(x) # clear RStudio source markers
  }

  invisible(x)
}

trim_output <- function(x, max = 65535L) {
  # if x is less than the max, just return it
  if (length(x) <= 0L || nchar(x) <= max) {
    return(x)
  }

  # otherwise trim x to the max, then search for the lint starts
  x <- substr(x, 1L, max)

  re <- rex(
    "[", except_some_of(":"), ":", numbers, ":", numbers, ":", "]",
    "(", except_some_of(")"), ")",
    space,
    "*", or("style", "warning", "error"), ":", "*",
    except_some_of("\r\n"), newline,
    except_some_of("\r\n"), newline,
    except_some_of("\r\n"), newline,
    except_some_of("\r\n"), newline,
    except_some_of("\r\n"), newline
  )

  lint_starts <- re_matches(x, re, global = TRUE, locations = TRUE)[[1L]]

  # if at least one lint ends before the cutoff, cutoff there, else just use
  # the cutoff
  last_end <- lint_starts[NROW(lint_starts), "end"]
  if (!is.na(last_end)) {
    substr(x, 1L, last_end)
  } else {
    x
  }
}

#' @export
names.lints <- function(x, ...) {
  vapply(x, `[[`, character(1L), "filename")
}

#' @export
split.lints <- function(x, f = NULL, ...) {
  if (is.null(f)) f <- names(x)
  splt <- split.default(x, f)
  for (i in names(splt)) class(splt[[i]]) <- "lints"
  return(splt)
}

#' @export
as.data.frame.lints <- function(x, row.names = NULL, optional = FALSE, ...) { # nolint: object_name. (row.names, #764)
  data.frame(
    filename = vapply(x, `[[`, character(1L), "filename"),
    line_number = vapply(x, `[[`, integer(1L), "line_number"),
    column_number = vapply(x, `[[`, integer(1L), "column_number"),
    type = vapply(x, `[[`, character(1L), "type"),
    message = vapply(x, `[[`, character(1L), "message"),
    line = vapply(x, `[[`, character(1L), "line"),
    linter = vapply(x, `[[`, character(1L), "linter")
  )
}

as_tibble.lints <- function(x, ..., # nolint: object_name_linter.
                            .rows = NULL,
                            .name_repair = c("check_unique", "unique", "universal", "minimal"),
                            rownames = NULL) {
  stopifnot(requireNamespace("tibble", quietly = TRUE))
  tibble::as_tibble(as.data.frame(x), ..., .rows = .rows, .name_repair = .name_repair, rownames = rownames)
}

as.data.table.lints <- function(x, keep.rownames = FALSE, ...) { # nolint: object_name_linter.
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  data.table::setDT(as.data.frame(x), keep.rownames = keep.rownames, ...)
}

#' @export
`[.lints` <- function(x, ...) {
  attrs <- attributes(x)
  x <- unclass(x)
  x <- x[...]
  attributes(x) <- attrs
  x
}

#' @export
summary.lints <- function(object, ...) {
  filenames <- vapply(object, `[[`, character(1L), "filename")
  types <- factor(vapply(object, `[[`, character(1L), "type"),
    levels = c("style", "warning", "error")
  )
  tbl <- table(filenames, types)
  filenames <- rownames(tbl)
  res <- as.data.frame.matrix(tbl, row.names = NULL)
  res$filenames <- filenames %||% character()
  nms <- colnames(res)
  res[order(res$filenames), c("filenames", nms[nms != "filenames"])]
}
