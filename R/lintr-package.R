#' Lintr
#'
#' Checks adherence to a given style, syntax errors, and possible semantic issues.
#' Supports on the fly checking of R code edited with Emacs, Vim, and Sublime Text.
#'
#' @seealso [lint()], [lint_package()], [lint_dir()], [linters]
#' @keywords internal
"_PACKAGE"

## lintr namespace: start
#' @importFrom cli cli_inform cli_abort cli_warn
#' @importFrom glue glue glue_collapse
#' @importFrom rex rex regex re_matches re_substitutes character_class
#' @importFrom stats complete.cases na.omit
#' @importFrom tools R_user_dir
#' @importFrom utils capture.output getParseData  globalVariables head relist tail
#' @importFrom xml2 as_list
#'   xml_attr xml_children xml_find_all xml_find_chr xml_find_lgl xml_find_num xml_find_first xml_name xml_text
## lintr namespace: end
NULL

# make binding available for mock testing
# ref: https://testthat.r-lib.org/dev/reference/local_mocked_bindings.html#base-functions
requireNamespace <- NULL
unlink <- NULL
quit <- NULL
