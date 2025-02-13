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
#' @examples
#' # will produce lints
#' lint(
#'   text = "x <- c()",
#'   linters = unnecessary_concatenation_linter()
#' )
#'
#' lint(
#'   text = "x <- c(TRUE)",
#'   linters = unnecessary_concatenation_linter()
#' )
#'
#' lint(
#'   text = "x <- c(1.5 + 2.5)",
#'   linters = unnecessary_concatenation_linter(allow_single_expression = FALSE)
#' )
#'
#' # okay
#' lint(
#'   text = "x <- NULL",
#'   linters = unnecessary_concatenation_linter()
#' )
#'
#' # In case the intent here was to seed a vector of known size
#' lint(
#'   text = "x <- integer(4L)",
#'   linters = unnecessary_concatenation_linter()
#' )
#'
#' lint(
#'   text = "x <- TRUE",
#'   linters = unnecessary_concatenation_linter()
#' )
#'
#' lint(
#'   text = "x <- c(1.5 + 2.5)",
#'   linters = unnecessary_concatenation_linter(allow_single_expression = TRUE)
#' )
#'
#' @evalRd rd_tags("unnecessary_concatenation_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unnecessary_concatenation_linter <- function(allow_single_expression = TRUE) { # nolint: object_length_linter.
  stopifnot(
    is.logical(allow_single_expression),
    length(allow_single_expression) == 1L
  )

  msg_empty <-
    "Replace unnecessary c() by NULL or, whenever possible, vector() seeded with the correct type and/or length."

  msg_const <- "Remove unnecessary c() of a constant."

  non_constant_cond <- "SYMBOL or (expr and not(OP-COLON and count(expr[SYMBOL or expr]) != 2))"

  pipes <- setdiff(magrittr_pipes, "%$%")
  to_pipe_xpath <- glue("
    ./preceding-sibling::*[1][
      self::PIPE or
      self::SPECIAL[{ xp_text_in_table(pipes) }]
    ]
  ")
  if (allow_single_expression) {
    zero_arg_cond <-
      glue("count(expr) = 1 and not( {to_pipe_xpath} / preceding-sibling::expr[ {non_constant_cond} ])")
    one_arg_cond <-
      glue("count(expr) = 2 and not(expr[2][ {non_constant_cond} ])")
  } else {
    zero_arg_cond <- glue("count(expr) = 1 and not( {to_pipe_xpath} )")
    one_arg_cond <- "count(expr) = 2 and not(expr[2]/SYMBOL[text() = '...'])"
    path_to_non_constant <- glue("./expr[2][ {non_constant_cond} ]")

    msg_const_expr <- paste(
      "Remove unnecessary c() of a constant expression.",
      "Replace with as.vector() if c() is used to strip attributes, e.g. in converting an array to a vector."
    )
  }
  call_xpath <- glue("
  parent::expr[
    not(EQ_SUB)
    and ( {xp_or(zero_arg_cond, one_arg_cond)} )
  ]")
  num_args_xpath <- "count(./expr) - 1"

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls("c")
    c_calls <- xml_find_all(xml_calls, call_xpath)

    # bump count(args) by 1 if inside a pipeline
    num_args <- as.integer(xml_find_num(c_calls, num_args_xpath)) +
      as.integer(!is.na(xml_find_first(c_calls, to_pipe_xpath)))
    # NB: the xpath guarantees num_args is 0, 1, or 2. 2 comes
    #   in "a" %>% c("b").
    # TODO(#2476): Push this logic back into the XPath.
    is_unneeded <- num_args <= 1L
    c_calls <- c_calls[is_unneeded]
    num_args <- num_args[is_unneeded]
    msg <- ifelse(num_args == 0L, msg_empty, msg_const)
    if (!allow_single_expression) {
      is_single_expression <- !is.na(xml_find_first(c_calls, path_to_non_constant))
      msg[is_single_expression] <- msg_const_expr
    }

    xml_nodes_to_lints(
      c_calls,
      source_expression = source_expression,
      lint_message = msg
    )
  })
}
