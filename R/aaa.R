#' @import rex
#' @importFrom utils tail
#' @include utils.R
NULL

#' Available linters
#'
#' @rdname linters
#' @name linters
#' @title linters
#' @param source_file returned by \code{\link{get_source_expressions}}
#' @param length the length cutoff to use for the given linter.
NULL

# need to register rex shortcuts as globals to avoid CRAN check errors
rex::register_shortcuts("lintr")

utils::globalVariables("from", "lintr")
