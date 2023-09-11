#' Lintr
#'
#' Checks adherence to a given style, syntax errors, and possible semantic issues.
#' Supports on the fly checking of R code edited with Emacs, Vim, and Sublime Text.
#'
#' @seealso [lint()], [lint_package()], [lint_dir()], [linters]
#' @keywords internal
"_PACKAGE"

## lintr namespace: start
#' @importFrom cyclocomp cyclocomp
#' @importFrom glue glue glue_collapse
#' @importFrom rex rex regex re_matches re_substitutes character_class
#' @importFrom stats na.omit
#' @importFrom utils capture.output getParseData getTxtProgressBar globalVariables head relist
#'   setTxtProgressBar tail txtProgressBar
#' @importFrom xml2 as_list
#'   xml_attr xml_find_all xml_find_chr xml_find_lgl xml_find_num xml_find_first xml_name xml_text
#' @rawNamespace
#' if (getRversion() >= "4.0.0") {
#'   importFrom(tools, R_user_dir)
#' } else {
#'   importFrom(backports, R_user_dir)
#' }
## lintr namespace: end
NULL
