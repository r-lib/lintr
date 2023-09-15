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
unnecessary_nested_if_linter <- make_linter_from_xpath(
  xpath = paste0(
    "//IF/parent::expr[not(ELSE)]/OP-RIGHT-PAREN/",
    c(
      "following-sibling::expr[IF and not(ELSE)]", # catch if (cond) if (other_cond) { ... }
      "following-sibling::expr[OP-LEFT-BRACE and count(expr) = 1]
         /expr[IF and not(ELSE)]" # catch if (cond) { if (other_cond) { ... } }
    ),
    collapse = " | "
  ),
  lint_message = paste(
    "Don't use nested `if` statements,",
    "where a single `if` with the combined conditional expression will do.",
    "For example, instead of `if (x) { if (y) { ... }}`, use `if (x && y) { ... }`."
  ),
  # need the full file to also catch usages at the top level
  level = "file"
)
