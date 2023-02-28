#' Block usage of anonymous functions in iteration functions when unnecessary
#'
#' Using an anonymous function in, e.g., [lapply()] is not always necessary,
#'   e.g. `lapply(DF, sum)` is the same as `lapply(DF, function(x) sum(x))` and
#'   the former is more readable.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "lapply(list(1:3, 2:4), function(xi) sum(xi))",
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
#' @evalRd rd_tags("unnecessary_lambda_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unnecessary_lambda_linter <- function() {
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
  apply_funs <- xp_text_in_table(c( # nolint: object_usage_linter. Used in glue call below.
    "lapply", "sapply", "vapply", "apply",
    "tapply", "rapply", "eapply", "dendrapply",
    "mapply", "by", "outer",
    "mclapply", "mcmapply", "parApply", "parCapply", "parLapply",
    "parLapplyLB", "parRapply", "parSapply", "parSapplyLB", "pvec",
    purrr_mappers
  ))

  # outline:
  #   1. match one of the identified mappers
  #   2. match an anonymous function that can be "symbol-ized"
  #     a. it's a one-variable function [TODO(michaelchirico): is this necessary?]
  #     b. the function is a single call
  #     c. that call's _first_ argument is just the function argument (a SYMBOL)
  #       - and it has to be passed positionally (not as a keyword)
  #     d. the function argument doesn't appear elsewhere in the call
  # TODO(#1703): handle explicit returns too: function(x) return(x)
  default_fun_xpath_fmt <- "
  //SYMBOL_FUNCTION_CALL[ {apply_funs} ]
    /parent::expr
    /following-sibling::expr[
      FUNCTION
      and count(SYMBOL_FORMALS) = 1
      and {paren_path}/OP-LEFT-PAREN/following-sibling::expr[1][not(preceding-sibling::*[1][self::EQ_SUB])]/SYMBOL
      and SYMBOL_FORMALS = {paren_path}/OP-LEFT-PAREN/following-sibling::expr[1]/SYMBOL
      and not(SYMBOL_FORMALS = {paren_path}/OP-LEFT-PAREN/following-sibling::expr[position() > 1]//SYMBOL)
    ]
  "
  default_fun_xpath <- paste(
    sep = "|",
    glue::glue(default_fun_xpath_fmt, paren_path = "expr"),
    glue::glue(default_fun_xpath_fmt, paren_path = "expr[OP-LEFT-BRACE and count(expr) = 1]/expr[1]")
  )

  # purrr-style inline formulas-as-functions, e.g. ~foo(.x)
  # logic is basically the same as that above, except we need
  #   1. a formula (OP-TILDE)
  #   2. the lone argument marker `.x` or `.`
  purrr_symbol <- "SYMBOL[text() = '.x' or text() = '.']"
  purrr_fun_xpath <- glue::glue("
  //SYMBOL_FUNCTION_CALL[ {xp_text_in_table(purrr_mappers)} ]
    /parent::expr
    /following-sibling::expr[
      OP-TILDE
      and expr[OP-LEFT-PAREN/following-sibling::expr[1][not(preceding-sibling::*[2][self::SYMBOL_SUB])]/{purrr_symbol}]
      and not(expr/OP-LEFT-PAREN/following-sibling::expr[position() > 1]//{purrr_symbol})
    ]
  ")

  # path to calling function symbol from the matched expressions
  fun_xpath <- "./parent::expr/expr/SYMBOL_FUNCTION_CALL"
  # path to the symbol of the simpler function that avoids a lambda
  symbol_xpath <- glue::glue("(expr|expr[OP-LEFT-BRACE]/expr[1])/expr[SYMBOL_FUNCTION_CALL]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    default_fun_expr <- xml2::xml_find_all(xml, default_fun_xpath)

    # TODO(michaelchirico): further message customization is possible here,
    #   e.g. don't always refer to 'lapply()' in the example, and customize to
    #   whether arguments need to be subsumed in '...' or not. The trouble is in
    #   keeping track of which argument the anonymous function is supplied (2nd
    #   argument for many calls, but 3rd e.g. for apply())
    default_call_fun <- xml2::xml_text(xml2::xml_find_first(default_fun_expr, fun_xpath))
    default_symbol <- xml2::xml_text(xml2::xml_find_first(default_fun_expr, symbol_xpath))
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

    purrr_fun_expr <- xml2::xml_find_all(xml, purrr_fun_xpath)

    purrr_call_fun <- xml2::xml_text(xml2::xml_find_first(purrr_fun_expr, fun_xpath))
    purrr_symbol <- xml2::xml_text(xml2::xml_find_first(purrr_fun_expr, symbol_xpath))
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

    c(default_fun_lints, purrr_fun_lints)
  })
}

purrr_mappers <- c(
  "map", "walk",
  "map_raw", "map_lgl", "map_int", "map_dbl", "map_chr", "map_vec",
  "map_df", "map_dfr", "map_dfc"
)
