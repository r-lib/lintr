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
