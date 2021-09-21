# Deprecated functions
#
# Functions that have been deprecated and replaced by newer ones. They will be removed in an
# upcoming version of \pkg{lintr} and should thus not be used anymore.
NULL

lintr_deprecated <- function(old, new = NULL, version = NULL,
                             type = "Function") {
  msg <- c(
    c(type, " ", old, " was deprecated"),
    if (length(version)) {
      c(" in lintr version ", version)
    },
    ". ",
    if (length(new)) {
      c("Use ", new, " instead.")
    }
  )
  msg <- paste0(msg, collapse = "")
  warning(msg, call. = FALSE, domain = NA)
}
