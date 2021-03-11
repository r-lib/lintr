#' @import rex
#' @importFrom utils tail
#' @include utils.R
NULL

#' Available linters
#'
#' @rdname linters
#' @name linters
#' @title linters
#'
#' @description  A variety of linters is available in \pkg{lintr}. The most popular ones are readily
#' accessible through \code{\link{default_linters}}, though there are additional ones you may want
#' to use.
#'
#' All the functions listed below are \bold{getters} that return a closure of class 'linter'.
#' Within a \code{\link{lint}} function call, the linters in use are initialized with the provided
#' arguments and fed with the source file (provided by \code{\link{get_source_expressions}}).
#'
#' @param length the length cutoff to use for the given linter.
#' @return  A closure of class 'linter'.
NULL

# need to register rex shortcuts as globals to avoid CRAN check errors
rex::register_shortcuts("lintr")

utils::globalVariables(
  c("line1", "col1", "line2", "col2", # columns of parsed_content
    "id", "parent", "token", "terminal", "text"), # dito
  "lintr"
)
