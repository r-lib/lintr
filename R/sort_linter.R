#' Check for common mistakes around sorting vectors
#'
#' This linter checks for some common mistakes when using [order()] or [sort()].
#'
#' First, it requires usage of `sort()` over `.[order(.)]`.
#'
#' [sort()] is the dedicated option to sort a list or vector. It is more legible
#' and around twice as fast as `.[order(.)]`, with the gap in performance
#' growing with the vector size.
#'
#' Second, it requires usage of [is.unsorted()] over equivalents using `sort()`.
#'
#' The base function `is.unsorted()` exists to test the sortedness of a vector.
#'   Prefer it to inefficient and less-readable equivalents like
#'   `x != sort(x)`. The same goes for checking `x == sort(x)` -- use
#'   `!is.unsorted(x)` instead.
#'
#' Moreover, use of `x == sort(x)` can be risky because [sort()] drops missing
#'   elements by default, meaning `==` might end up trying to compare vectors
#'   of differing lengths.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x[order(x)]",
#'   linters = sort_linter()
#' )
#'
#' lint(
#'   text = "x[order(x, decreasing = TRUE)]",
#'   linters = sort_linter()
#' )
#'
#' lint(
#'   text = "sort(x) == x",
#'   linters = sort_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x[sample(order(x))]",
#'   linters = sort_linter()
#' )
#'
#' lint(
#'   text = "y[order(x)]",
#'   linters = sort_linter()
#' )
#'
#' lint(
#'   text = "sort(x, decreasing = TRUE) == x",
#'   linters = sort_linter()
#' )
#'
#' # If you are sorting several objects based on the order of one of them, such
#' # as:
#' x <- sample(1:26)
#' y <- letters
#' newx <- x[order(x)]
#' newy <- y[order(x)]
#' # This will be flagged by the linter. However, in this very specific case,
#' # it would be clearer and more efficient to run order() once and assign it
#' # to an object, rather than mix and match order() and sort()
#' index <- order(x)
#' newx <- x[index]
#' newy <- y[index]
#'
#' @evalRd rd_tags("sort_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
sort_linter <- function() {
  non_keyword_arg <- "expr[not(preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB])]"
  order_xpath <- glue("
  //OP-LEFT-BRACKET
    /following-sibling::expr[1][
      expr[1][
        SYMBOL_FUNCTION_CALL[text() = 'order']
        and count(following-sibling::{non_keyword_arg}) = 1
        and following-sibling::{non_keyword_arg} =
          parent::expr[1]/parent::expr[1]/expr[1]
      ]
    ]
  ")

  sorted_xpath <- "
  self::*[
    (EQ or NE)
    and expr/expr = expr
    and not(expr/EQ_SUB)
  ]"


  arguments_xpath <-
    ".//SYMBOL_SUB[text() = 'method' or text() = 'decreasing' or text() = 'na.last']"

  arg_values_xpath <- glue("{arguments_xpath}/following-sibling::expr[1]")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    order_expr <- xml_find_all(xml, order_xpath)

    variable <- xml_text(xml_find_first(
      order_expr,
      ".//SYMBOL_FUNCTION_CALL[text() = 'order']/parent::expr[1]/following-sibling::expr[1]"
    ))

    order_expr <- strip_comments_from_subtree(order_expr)

    orig_call <- sprintf("%s[%s]", variable, get_r_string(order_expr))

    # Reconstruct new argument call for each expression separately
    arguments <- vapply(order_expr, function(e) {
      arg_names <- xml_text(xml_find_all(e, arguments_xpath))
      arg_values <- xml_text(xml_find_all(e, arg_values_xpath))
      if (!"na.last" %in% arg_names) {
        arg_names <- c(arg_names, "na.last")
        arg_values <- c(arg_values, "TRUE")
      }
      paste(arg_names, "=", arg_values, collapse = ", ")
    }, character(1L))

    new_call <- sprintf("sort(%s, %s)", variable, arguments)

    order_lints <- xml_nodes_to_lints(
      order_expr,
      source_expression = source_expression,
      lint_message = paste0(
        new_call, " is better than ", orig_call, ". ",
        "Note that it's always preferable to save the output of order() for the same variable ",
        "as a local variable than to re-compute it."
      ),
      type = "warning"
    )

    sort_calls <- xml_parent(xml_parent(source_expression$xml_find_function_calls("sort")))
    sort_calls <- strip_comments_from_subtree(sort_calls)
    sorted_expr <- xml_find_all(sort_calls, sorted_xpath)

    sorted_op <- xml_text(xml_find_first(sorted_expr, "*[2]"))
    lint_message <- ifelse(
      sorted_op == "==",
      "Use !is.unsorted(x) to test the sortedness of a vector.",
      "Use is.unsorted(x) to test the unsortedness of a vector."
    )

    sorted_lints <- xml_nodes_to_lints(
      sorted_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )

    c(order_lints, sorted_lints)
  })
}
