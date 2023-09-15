#' Extraction operator linter
#'
#' Check that the `[[` operator is used when extracting a single element from an object,
#' not `[` (subsetting) nor `$` (interactive use).
#'
#' @details
#'
#' There are three subsetting operators in R (`[[`, `[`, and `$`) and they interact differently
#' with different data structures (atomic vector, list, data frame, etc.).
#'
#' Here are a few reasons to prefer the `[[` operator over `[` or `$` when you want to extract
#' an element from a data frame or a list:
#'
#'   - Subsetting a list with `[` always returns a smaller list, while `[[` returns
#'     the list element.
#'
#'   - Subsetting a named atomic vector with `[` returns a named vector, while `[[` returns
#'     the vector element.
#'
#'   - Subsetting a data frame (but not tibble) with `[` is type unstable; it can return
#'     a vector or a data frame. `[[`, on the other hand, always returns a vector.
#'
#'   - For a data frame (but not tibble), `$` does partial matching (e.g. `df$a` will subset
#'     `df$abc`), which can be a source of bugs. `[[` doesn't do partial matching.
#'
#' For data frames (and tibbles), irrespective of the size, the `[[` operator is slower than `$`.
#' For lists, however, the reverse is true.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'iris["Species"]',
#'   linters = extraction_operator_linter()
#' )
#'
#' lint(
#'   text = "iris$Species",
#'   linters = extraction_operator_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'iris[["Species"]]',
#'   linters = extraction_operator_linter()
#' )
#'
#' @references
#' - Subsetting [chapter](https://adv-r.hadley.nz/subsetting.html) from _Advanced R_ (Wickham, 2019).
#'
#' @evalRd rd_tags("extraction_operator_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
extraction_operator_linter <- function() {
  constant_nodes_in_brackets <- paste0("self::", c("expr", "OP-PLUS", "NUM_CONST", "STR_CONST"))
  xpath <- glue("
  //OP-DOLLAR[not(preceding-sibling::expr[1]/SYMBOL[text() = 'self' or text() = '.self'])]
  |
  //OP-LEFT-BRACKET[
    not(following-sibling::expr[1]/descendant::*[not({xp_or(constant_nodes_in_brackets)})]) and
    not(following-sibling::OP-COMMA)
  ]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content
    bad_exprs <- xml_find_all(xml, xpath)
    msgs <- sprintf("Use `[[` instead of `%s` to extract an element.", xml_text(bad_exprs))

    xml_nodes_to_lints(
      bad_exprs,
      source_expression = source_expression,
      lint_message = msgs,
      type = "warning"
    )
  })
}
