#' Avoid unnecessary nested `if` conditional statements
#'
#' @examples
#' # will produce lints
#' writeLines("if (x) { \n  if (y) { \n   return(1L) \n  } \n}")
#' lint(
#'   text = "if (x) { \n  if (y) { \n   return(1L) \n  } \n}",
#'   linters = unnecessary_nested_if_linter()
#' )
#'
#' # okay
#' writeLines("if (x && y) { \n  return(1L) \n}")
#' lint(
#'   text = "if (x && y) { \n  return(1L) \n}",
#'   linters = unnecessary_nested_if_linter()
#' )
#'
#' writeLines("if (x) { \n  y <- x + 1L\n  if (y) { \n   return(1L) \n  } \n}")
#' lint(
#'   text = "if (x) { \n  y <- x + 1L\n  if (y) { \n   return(1L) \n  } \n}",
#'   linters = unnecessary_nested_if_linter()
#' )
#'
#' @evalRd rd_tags("unnecessary_nested_if_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unnecessary_nested_if_linter <- function() {
  xpath <- paste0(
    "//IF/parent::expr[not(ELSE)]/OP-RIGHT-PAREN/",
    c(
      "following-sibling::expr[IF and not(ELSE)]", # catch if (cond) if (other_cond) { ... }
      "following-sibling::expr[OP-LEFT-BRACE and count(expr) = 1]
         /expr[IF and not(ELSE)]" # catch if (cond) { if (other_cond) { ... } }
    ),
    collapse = " | "
  )

  Linter(function(source_expression) {
    # need the full file to also catch usages at the top level
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    lint_message <- paste(
      "Don't use nested `if` statements,",
      "where a single `if` with the combined conditional expression will do.",
      "For example, instead of `if (x) { if (y) { ... }}`, use `if (x && y) { ... }`."
    )

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}
