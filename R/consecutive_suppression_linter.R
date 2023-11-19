#' Force consecutive calls suppress* calls into just one when possible
#'
#' Suppressing the output of consecutive calls to [library()] with
#'   [suppressMessages()] or [suppressPackageStartupMessages()] need only be
#'   done once, by wrapping the sequence of library calls into
#'   one `{}` expression, as opposed to repeatedly
#'
