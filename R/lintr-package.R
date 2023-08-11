#' Lintr
#'
#' Checks adherence to a given style, syntax errors, and possible semantic issues.
#' Supports on the fly checking of R code edited with Emacs, Vim, and Sublime Text.
#'
#' @seealso [lint()], [lint_package()], [lint_dir()], [linters]
#' @keywords internal
"_PACKAGE"

## lintr namespace: start
#' @importFrom glue glue glue_collapse
#' @importFrom rex rex regex re_matches re_substitutes character_class
#' @importFrom stats na.omit
#' @importFrom utils capture.output head getParseData relist
#' @importFrom xml2 xml_attr xml_find_all xml_find_chr xml_find_num xml_find_first xml_name xml_text as_list
#' @importFrom cyclocomp cyclocomp
#' @importFrom utils data globalVariables packageVersion read.csv tail
#' @rawNamespace
#' if (getRversion() >= "4.0.0") {
#'   importFrom(tools, R_user_dir)
#' } else {
#'   importFrom(backports, R_user_dir)
#' }
## lintr namespace: end
NULL
