# Regression test for R6 class lints

#' drink_the_new
#' @description empty
#'
#' @export
drink_the_new <- R6::R6Class("drink_the_new")

#' drink_the_new for most things
#' @export
drink_the_new.default <- function(x, ...) {
  1
}

#' drink_the_new for lists
#' @export
drink_the_new.list <- function(x, ...) {
  NULL
}

#' drink_the_new for data.frames
#' @export
drink_the_new.data.frame <- function(x, ...) {
  NULL
}
