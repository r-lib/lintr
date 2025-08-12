#' Block usage of nested `ifelse()` calls
#'
#' Calling [ifelse()] in nested calls is problematic for two main reasons:
#'   1. It can be hard to read -- mapping the code to the expected output
#'      for such code can be a messy task/require a lot of mental bandwidth,
#'      especially for code that nests more than once
#'   2. It is inefficient -- `ifelse()` can evaluate _all_ of its arguments at
#'      both yes and no (see <https://stackoverflow.com/q/16275149>); this issue
#'      is exacerbated for nested calls
#'
#' Users can instead rely on a more readable alternative modeled after SQL
#'   CASE WHEN statements.
#'
#' Let's say this is our original code:
#'
#' ```r
#' ifelse(
#'   x == "a",
#'   2L,
#'   ifelse(x == "b", 3L, 1L)
#' )
#' ```
#'
#' Here are a few ways to avoid nesting and make the code more readable:
#'
#'   - Use `data.table::fcase()`
#'
#'     ```r
#'     data.table::fcase(
#'       x == "a", 2L,
#'       x == "b", 3L,
#'       default = 1L
#'     )
#'     ```
#'
#'   - Use `dplyr::case_match()`
#'
#'     ```r
#'     dplyr::case_match(
#'       x,
#'       "a" ~ 2L,
#'       "b" ~ 3L,
#'       .default = 1L
#'     )
#'     ```
#'
#'   - Use a look-up-and-merge approach (build a mapping table between values
#'     and outputs and merge this to the input)
#'
#'     ```r
#'     default <- 1L
#'     values <- data.frame(
#'       a = 2L,
#'       b = 3L
#'     )
#'     found_value <- values[[x]]
#'     ifelse(is.null(found_value), default, found_value)
#'     ```
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'ifelse(x == "a", 1L, ifelse(x == "b", 2L, 3L))',
#'   linters = nested_ifelse_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'dplyr::case_when(x == "a" ~ 1L, x == "b" ~ 2L, TRUE ~ 3L)',
#'   linters = nested_ifelse_linter()
#' )
#'
#' lint(
#'   text = 'data.table::fcase(x == "a", 1L, x == "b", 2L, default = 3L)',
#'   linters = nested_ifelse_linter()
#' )
#'
#' @evalRd rd_tags("nested_ifelse_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
nested_ifelse_linter <- function() {
  # NB: land on the nested (inner) call, not the outer call, and throw a lint with the inner call's name
  xpath <- glue("
  following-sibling::expr[
    expr[1]/SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]
  ]")

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(ifelse_funs)
    bad_expr <- xml_find_all(xml_calls, xpath)

    matched_call <- xp_call_name(bad_expr)
    lint_message <- paste(
      sprintf("Don't use nested %s() calls;", matched_call),
      "instead, try (1) data.table::fcase; (2) dplyr::case_when; or (3) using a lookup table."
    )
    xml_nodes_to_lints(bad_expr, source_expression, lint_message, type = "warning")
  })
}
