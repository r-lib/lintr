# Regression test for #737 via expecting clean to be lint-free

#' drink_me
#' @description empty
#'
#' @export
drink_me <- function(x, ...) {
  UseMethod("drink_me")
}

#' drink_me for most things
#' @export
drink_me.default <- function(x, ...) {
  1
}

#' drink_me for lists
#' @export
drink_me.list <- function(x, ...) {
  NULL
}

#' drink_me for data.frames
#' @export
drink_me.data.frame <- function(x, ...) {
  NULL
}
