#' Unneeded concatenation linter
#'
#' Check that the [c()] function is not used without arguments nor with a single constant.
#'
#' @param allow_single_expression Logical, default `TRUE`. If `FALSE`, one-expression
#'   usages of `c()` are always linted, e.g. `c(x)` and `c(matrix(...))`. In some such
#'   cases, `c()` is being used for its side-effect of stripping non-name attributes;
#'   it is usually preferable to use the more readable [as.vector()] instead.
#'   [as.vector()] is not always preferable, for example with environments
#'   (especially, `R6` objects), in which case `list()` is the better alternative.
#'
#' @evalRd rd_tags("unneeded_concatenation_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unneeded_concatenation_linter <- function(allow_single_expression = TRUE) {
  msg_empty <- paste(
    "Unneeded concatenation without arguments.",
    'Replace the "c" call by NULL or, whenever possible,',
    "vector() seeded with the correct type and/or length."
  )

  msg_const <- 'Unneeded concatenation of a constant. Remove the "c" call.'

  non_constant_cond <- "SYMBOL or (expr and not(OP-COLON and count(expr[SYMBOL or expr]) != 2))"
  if (!allow_single_expression) {
    path_to_non_constant <- glue::glue("./expr[2][ {non_constant_cond} ]")
    non_constant_cond <- "1 = 0"

    msg_const_expr <- paste(
      'Unneeded concatenation of a simple expression. Remove the "c" call,',
      'replacing with "as.vector" if using "c" to string attributes, e.g. in converting an array to a vector.'
    )
  }

  to_pipe_xpath <- "
    ./preceding-sibling::*[1][
      self::PIPE or
      self::SPECIAL[text() = '%>%']
    ]
  "
  xpath_call <- glue::glue("
    //expr[
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'c']]
      and not(EQ_SUB)
      and (
        (
          count(expr) = 2
          and not(expr[2][ {non_constant_cond} ])
        ) or (
          count(expr) = 1
          and not( {to_pipe_xpath} / preceding-sibling::expr[ {non_constant_cond} ] )
        )
      )
    ]
  ")
  num_args_xpath <- "count(./expr) - 1"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content
    c_calls <- xml2::xml_find_all(xml, xpath_call)

    # bump count(args) by 1 if inside a pipeline
    num_args <- as.integer(xml2::xml_find_num(c_calls, num_args_xpath)) +
      as.integer(!is.na(xml2::xml_find_first(c_calls, to_pipe_xpath)))
    # NB: the xpath guarantees num_args is 0, 1, or 2. 2 comes
    #   in "a" %>% c("b").
    # TODO(michaelchirico): can we handle this all inside the XPath with reasonable concision?
    is_unneeded <- num_args <= 1L
    c_calls <- c_calls[is_unneeded]
    num_args <- num_args[is_unneeded]
    msg <- ifelse(num_args == 0L, msg_empty, msg_const)
    if (!allow_single_expression) {
      is_single_expression <- !is.na(xml2::xml_find_first(c_calls, path_to_non_constant))
      msg[is_single_expression] <- msg_const_expr
    }

    xml_nodes_to_lints(
      c_calls,
      source_expression = source_expression,
      lint_message = msg
    )
  })
}
