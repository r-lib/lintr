#' @export
print.lint <- function(x, ...) {

  color <- switch(x$type,
    "warning" = crayon::magenta,
    "error" = crayon::red,
    "style" = crayon::blue,
    crayon::bold
  )

  message(
    crayon::bold(x$filename, ":",
    as.character(x$line_number), ":",
    as.character(x$column_number), ": ", sep = ""),
    color(x$type, ": ", sep = ""),
    crayon::bold(x$message), "\n",
    x$line, "\n",
    highlight_string(x$message, x$column_number, x$ranges)
    )
  invisible(x)
}

#' @export
print.lints <- function(x, ...) {
  if (getOption("lintr.rstudio_source_markers", TRUE) &&
      rstudioapi::hasFun("sourceMarkers")) {
    rstudio_source_markers(x)
  }
  else {
    lapply(x, print, ...)
  }
  invisible(x)
}

#' @export
names.lints <- function(x, ...) {
  vapply(x, `[[`, character(1), "filename")
}

#' @export
split.lints <- function(x, f=NULL, ...) {
  if (is.null(f)) f <- names(x)
  splt <- split.default(x, f)
  for(i in names(splt)) class(splt[[i]]) <- "lints"
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
