#' @export
print.lint <- function(x, ...) {

  color <- switch(x$type,
    "warning" = crayon::magenta,
    "error" = crayon::red,
    "style" = crayon::blue,
    crayon::bold
  )

  cat(sep = "",
    crayon::bold(x$filename, ":",
    as.character(x$line_number), ":",
    as.character(x$column_number), ": ", sep = ""),
    color(x$type, ": ", sep = ""),
    crayon::bold(x$message), "\n",
    x$line, "\n",
    highlight_string(x$message, x$column_number, x$ranges),
    "\n"
    )
  invisible(x)
}

markdown <- function(x, info, ...) {

  cat(sep = "",
      "[", x$filename, ":",
      as.character(x$line_number), ":",
      as.character(x$column_number), ":", "]",
      "(",
        paste(sep = "/",
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
print.lints <- function(x, ...) {
  has_lints <- length(x) > 0

  if (getOption("lintr.rstudio_source_markers", TRUE) &&
      rstudioapi::hasFun("sourceMarkers")) {
    rstudio_source_markers(x)
  } else {
    if (has_lints && in_ci() && settings$comment_bot) {

      info <- ci_build_info()

      lint_output <-
        trim_output(paste0(collapse = "\n",
                           capture.output(invisible(lapply(x, markdown, info, ...)))
                           )
      )

      github_comment(lint_output, info, ...)
    }
    lapply(x, print, ...)
  }

  if (has_lints && isTRUE(settings$error_on_lint)) {
    quit("no", 31, FALSE)
  }
  invisible(x)
}

trim_output <- function(x, max = 65535) {

  # if x is less than the max, just return it
  if (length(x) <= 0 || nchar(x) <= max) {
    return(x)
  }

  # otherwise trim x to the max, then search for the lint starts
  x <- substr(x, 1, max)

  re <- rex::rex("[", except_some_of(":"), ":", numbers, ":", numbers, ":", "]",
                 "(", except_some_of(")"), ")",
                 space,
                 "*", or("style", "warning", "error"), ":", "*",
                 except_some_of(newline), newline, except_some_of(newline),
                 newline, except_some_of(newline), newline)

  lint_starts <- rex::re_matches(x, re, global = TRUE, locations = TRUE)[[1]]

  # if at least one lint ends before the cutoff, cutoff there, else just use
  # the cutoff
  last_end <- lint_starts[NROW(lint_starts), "end"]
  if (!is.na(last_end)) {
    substr(x, 1, last_end)
  } else {
    x
  }
}

#' @export
names.lints <- function(x, ...) {
  vapply(x, `[[`, character(1), "filename")
}

#' @export
split.lints <- function(x, f=NULL, ...) {
  if (is.null(f)) f <- names(x)
  splt <- split.default(x, f)
  for (i in names(splt)) class(splt[[i]]) <- "lints"
  return(splt)
}

#' @export
as.data.frame.lints <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(filename = vapply(x, `[[`, character(1), "filename"),
             line_number = vapply(x, `[[`, numeric(1), "line_number"),
             column_number = vapply(x, `[[`, numeric(1), "column_number"),
             type = vapply(x, `[[`, character(1), "type"),
             message = vapply(x, `[[`, character(1), "message"),
             line = vapply(x, `[[`, character(1), "line"),
             linter = vapply(x, `[[`, character(1), "linter"),
             stringsAsFactors = FALSE
  )
}

#' @export
`[.lints` <- function(x, ...) {
  attrs <- attributes(x)
  x <- unclass(x)
  x <- x[...]
  attributes(x) <- attrs
  x
}
