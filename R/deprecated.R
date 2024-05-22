#' Deprecated functions
#'
#' Functions that have been deprecated and replaced by newer ones. They will be removed in an
#' upcoming version of \pkg{lintr} and should thus not be used anymore.
#' @noRd
NULL

lintr_deprecated <- function(what, alternative = NULL, version = NULL,
                             type = "Function", signal = c("warning", "stop")) {
  signal <- match.arg(signal)
  signal <- match.fun(signal)
  msg <- c(
    c(type, " ", what, " was deprecated"),
    if (length(version) > 0L) c(" in lintr version ", version),
    ". ",
    if (length(alternative) > 0L) c("Use ", alternative, " instead.")
  )
  msg <- paste(msg, collapse = "")
  signal(msg, call. = FALSE, domain = NA)
}
