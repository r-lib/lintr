#' Block usage of anonymous functions in iteration functions when unnecessary
#'
#' Using an anonymous function in, e.g., [lapply()] is not always necessary,
#'   e.g. `lapply(DF, sum)` is the same as `lapply(DF, function(x) sum(x))` and
#'   the former is more readable.
#'
#' Cases like `lapply(x, \(xi) grep("ptn", xi))` are excluded because, though
#'   the anonymous function _can_ be avoided, doing so is not always more
#'   readable.
#'
#' @param allow_comparison Logical, default `FALSE`. If `TRUE`, lambdas like
#'   `function(x) foo(x) == 2`, where `foo` can be extracted to the "mapping"
#'   function and `==` vectorized instead of called repeatedly, are linted.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "lapply(list(1:3, 2:4), function(xi) sum(xi))",
#'   linters = unnecessary_lambda_linter()
#' )
#'
#' lint(
#'   text = "sapply(x, function(xi) xi == 2)",
#'   linters = unnecessary_lambda_linter()
#' )
#'
#' lint(
#'   text = "sapply(x, function(xi) sum(xi) > 0)",
#'   linters = unnecessary_lambda_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "lapply(list(1:3, 2:4), sum)",
#'   linters = unnecessary_lambda_linter()
#' )
#'
#' lint(
#'   text = 'lapply(x, function(xi) grep("ptn", xi))',
#'   linters = unnecessary_lambda_linter()
#' )
#'
#' lint(
#'   text = "lapply(x, function(xi) data.frame(col = xi))",
#'   linters = unnecessary_lambda_linter()
#' )
#'
#' lint(
#'   text = "sapply(x, function(xi) xi == 2)",
#'   linters = unnecessary_lambda_linter(allow_comparison = TRUE)
#' )
#'
#' lint(
#'   text = "sapply(x, function(xi) sum(xi) > 0)",
#'   linters = unnecessary_lambda_linter(allow_comparison = TRUE)
#' )
#'
#' lint(
#'   text = "sapply(x, function(xi) sum(abs(xi)) > 10)",
#'   linters = unnecessary_lambda_linter()
#' )
#'
#' lint(
#'   text = "sapply(x, sum) > 0",
#'   linters = unnecessary_lambda_linter()
#' )
#'
#' @evalRd rd_tags("unnecessary_lambda_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unnecessary_lambda_linter <- function(allow_comparison = FALSE) {
  # include any base function like those where FUN is an argument
  #   and ... follows positionally directly afterwards (with ...
  #   being passed on to FUN). That excludes functions like
  #   Filter/Reduce (which don't accept ...), as well as functions
  #   like sweep() (where check.margin comes between FUN and ...,
  #   and thus would need to be supplied in order to replicate
  #   positional arguments). Technically, these latter could
  #   be part of the linter logic (e.g., detect if the anonymous
  #   call is using positional or keyword arguments -- we can
  #   throw a lint for sweep() lambdas where the following arguments
  #   are all named) but for now it seems like overkill.
  apply_funs <- c(
    "lapply", "sapply", "vapply", "apply",
    "tapply", "rapply", "eapply", "dendrapply",
    "mapply", "by", "outer",
    "mclapply", "mcmapply", "parApply", "parCapply", "parLapply",
    "parLapplyLB", "parRapply", "parSapply", "parSapplyLB", "pvec",
    purrr_mappers
  )

  # OP-PLUS: condition for complex literal, e.g. 0+2i.
  # NB: this includes 0+3 and TRUE+FALSE, which are also fine.
  inner_comparison_xpath <- glue("
  parent::expr
    /expr[FUNCTION or OP-LAMBDA]
    /expr[
      ({ xp_or(infix_metadata$xml_tag[infix_metadata$comparator]) })
      and expr[
        expr/SYMBOL_FUNCTION_CALL
        and expr/SYMBOL
      ]
      and expr[
        NUM_CONST
        or STR_CONST
        or (OP-PLUS and count(expr/NUM_CONST) = 2)
      ]
    ]
  ")

  # outline:
  #   1. match one of the identified mappers
  #   2. match an anonymous function that can be "symbol-ized"
  #     a. it's a one-variable function [TODO(#2477): relax this]
  #     b. the function is a single call
  #     c. that call's _first_ argument is just the function argument (a SYMBOL)
  #       - and it has to be passed positionally (not as a keyword)
  #     d. the function argument doesn't appear elsewhere in the call
  default_fun_xpath <- glue("
  following-sibling::expr[(FUNCTION or OP-LAMBDA) and count(SYMBOL_FORMALS) = 1]
    /expr[last()][
      count(.//SYMBOL[self::* = preceding::SYMBOL_FORMALS[1]]) = 1
      and count(.//SYMBOL_FUNCTION_CALL[text() != 'return']) = 1
      and preceding-sibling::SYMBOL_FORMALS =
        .//expr[
          position() = 2
          and preceding-sibling::expr/SYMBOL_FUNCTION_CALL
          and not(preceding-sibling::*[1][self::EQ_SUB])
          and not(parent::expr[
            preceding-sibling::expr[not(SYMBOL_FUNCTION_CALL)]
            or following-sibling::*[not(self::OP-RIGHT-PAREN or self::OP-RIGHT-BRACE)]
          ])
        ]/SYMBOL
    ]
    /parent::expr
  ")

  # purrr-style inline formulas-as-functions, e.g. ~foo(.x)
  # logic is basically the same as that above, except we need
  #   1. a formula (OP-TILDE)
  #   2. the lone argument marker `.x` or `.`
  purrr_symbol <- "SYMBOL[text() = '.x' or text() = '.']"
  purrr_fun_xpath <- glue("
  following-sibling::expr[
    OP-TILDE
    and expr[OP-LEFT-PAREN/following-sibling::expr[1][not(preceding-sibling::*[2][self::SYMBOL_SUB])]/{purrr_symbol}]
    and not(expr/OP-LEFT-PAREN/following-sibling::expr[position() > 1]//{purrr_symbol})
  ]")

  # path to calling function symbol from the matched expressions
  fun_xpath <- "./parent::expr/expr/SYMBOL_FUNCTION_CALL"
  # path to the symbol of the simpler function that avoids a lambda
  symbol_xpath <- "expr[last()]//expr[SYMBOL_FUNCTION_CALL[text() != 'return']]"

  Linter(linter_level = "expression", function(source_expression) {
    default_calls <- source_expression$xml_find_function_calls(apply_funs)
    default_fun_expr <- xml_find_all(default_calls, default_fun_xpath)

    # TODO(#2478): Give a specific recommendation in the message.
    default_call_fun <- xml_text(xml_find_first(default_fun_expr, fun_xpath))
    default_symbol <- xml_text(xml_find_first(default_fun_expr, symbol_xpath))
    default_fun_lints <- xml_nodes_to_lints(
      default_fun_expr,
      source_expression = source_expression,
      lint_message = paste0(
        "Pass ", default_symbol, " directly as a symbol to ", default_call_fun, "() ",
        "instead of wrapping it in an unnecessary anonymous function. ",
        "For example, prefer lapply(DF, sum) to lapply(DF, function(x) sum(x))."
      ),
      type = "warning"
    )

    inner_comparison_lints <- NULL
    if (!allow_comparison) {
      sapply_vapply_calls <- source_expression$xml_find_function_calls(c("sapply", "vapply"))
      inner_comparison_expr <- xml_find_all(sapply_vapply_calls, inner_comparison_xpath)

      mapper <- xp_call_name(xml_find_first(inner_comparison_expr, "parent::expr/parent::expr"))
      if (length(mapper) > 0L) fun_value <- if (mapper == "sapply") "" else ", FUN.VALUE = <intermediate>"

      inner_comparison_lints <- xml_nodes_to_lints(
        inner_comparison_expr,
        source_expression = source_expression,
        lint_message = sprintf(
          paste(
            "Compare to a constant after calling %1$s() to get the full benefits of vectorization.",
            "Prefer %1$s(x, foo%2$s) == 2 over %1$s(x, function(xi) foo(xi) == 2, logical(1L))."
          ),
          mapper, fun_value
        ),
        type = "warning"
      )
    }

    purrr_calls <- source_expression$xml_find_function_calls(purrr_mappers)
    purrr_fun_expr <- xml_find_all(purrr_calls, purrr_fun_xpath)

    purrr_call_fun <- xml_text(xml_find_first(purrr_fun_expr, fun_xpath))
    purrr_symbol <- xml_text(xml_find_first(purrr_fun_expr, symbol_xpath))
    purrr_fun_lints <- xml_nodes_to_lints(
      purrr_fun_expr,
      source_expression = source_expression,
      lint_message = paste0(
        "Pass ", purrr_symbol, " directly as a symbol to ", purrr_call_fun, "() ",
        "instead of wrapping it in an unnecessary anonymous function. ",
        "For example, prefer purrr::map(DF, sum) to purrr::map(DF, ~sum(.x))."
      ),
      type = "warning"
    )

    c(default_fun_lints, inner_comparison_lints, purrr_fun_lints)
  })
}
