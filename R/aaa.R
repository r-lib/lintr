#' @import rex
#' @importFrom utils tail
#' @include utils.R
NULL

# need to register rex shortcuts as globals to avoid CRAN check errors
rex::register_shortcuts("lintr")

utils::globalVariables("from", "lintr")
