#' @import rex
#' @importFrom utils tail
NULL

# need to register rex shortcuts as globals to avoid CRAN check errors
rex::register_shortcuts("lintr")

utils::globalVariables("from", "lintr")
