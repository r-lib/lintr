#' Avoid implicit coercion to logical in `if ()` and `while ()` conditions
#'
#' Relying on implicit numeric-to-logical coercion in conditional expressions
#'   such as `if (length(x))` or `if (!nchar(s))` can be harder to read and error-prone
#'   (for instance, negative numbers like `-1L` from [regexpr()] coerce to `TRUE`,
#'   and `0`-length outputs cause runtime errors). Prefer explicit logical
#'   comparisons (`length(x) > 0L`, `length(x) == 0L`) or dedicated predicate
#'   functions ([nzchar()], [grepl()], [any()], `x %in% table`).
#'
#' @param coercers Character vector of additional function names returning
#'   numeric or integer values whose uncompared use in `if ()` or `while ()`
#'   conditions should be linted. Defaults to `"anyDuplicated"`, which is
#'   combined with the built-in set of numeric-returning base functions
#'   (`length()`, `lengths()`, `nrow()`, `ncol()`, `NROW()`, `NCOL()`, `dim()`,
#'   `nchar()`, `sum()`, `prod()`, `bitwAnd()`, `bitwOr()`, `bitwXor()`,
#'   `bitwNot()`, `bitwShiftL()`, `bitwShiftR()`, `match()`, `grep()`,
#'   `regexpr()`, `gregexpr()`, and `diff()`).
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "if (length(x)) TRUE",
#'   linters = implicit_coercion_linter()
#' )
#'
#' lint(
#'   text = "if (!nchar(s)) TRUE",
#'   linters = implicit_coercion_linter()
#' )
#'
#' lint(
#'   text = "if (1L) TRUE",
#'   linters = implicit_coercion_linter()
#' )
#'
#' lint(
#'   text = "if (x %% 2L) TRUE",
#'   linters = implicit_coercion_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "if (length(x) > 0L) TRUE",
#'   linters = implicit_coercion_linter()
#' )
#'
#' lint(
#'   text = "if (!nzchar(s)) TRUE",
#'   linters = implicit_coercion_linter()
#' )
#'
#' lint(
#'   text = "if (TRUE) TRUE",
#'   linters = implicit_coercion_linter()
#' )
#'
#' lint(
#'   text = "if (x %% 2L != 0L) TRUE",
#'   linters = implicit_coercion_linter()
#' )
#'
#' @evalRd rd_tags("implicit_coercion_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - [nzchar_linter()]
#' - <https://style.tidyverse.org/syntax.html#if-statements>
#' @export
implicit_coercion_linter <- function(coercers = "anyDuplicated") {
  if (!is.null(coercers) && !is.character(coercers)) {
    cli_abort("{.arg coercers} must be a character vector or {.code NULL}.")
  }

  base_coercers <- c(
    "length", "lengths", "nrow", "ncol", "NROW", "NCOL", "dim",
    "nchar",
    "sum", "prod",
    "bitwAnd", "bitwOr", "bitwXor", "bitwNot", "bitwShiftL", "bitwShiftR",
    "match", "grep", "regexpr", "gregexpr",
    "diff"
  )
  all_coercers <- union(base_coercers, coercers)
  non_neg_funs <- c("length", "lengths", "nrow", "ncol", "NROW", "NCOL", "dim", "anyDuplicated")

  coercer_table <- xp_text_in_table(all_coercers)
  call_cond <- glue("expr[1][SYMBOL_FUNCTION_CALL[{coercer_table}]]")

  is_cond_root <- "preceding-sibling::*[self::IF or self::WHILE] and not(preceding-sibling::expr)"
  is_bool_wrapper <- "*[not(self::COMMENT)][1][self::OP-LEFT-PAREN or self::OP-EXCLAMATION] or AND or OR or AND2 or OR2"
  in_bool_context <- glue("
    (({is_cond_root}) or ancestor::expr[{is_cond_root}])
    and not(ancestor::expr[(({is_cond_root}) or ancestor::expr[{is_cond_root}]) and not({is_bool_wrapper})])
  ")

  num_const_cond <- "NUM_CONST[not(text() = 'TRUE' or text() = 'FALSE' or starts-with(text(), 'NA'))]"
  literal_cond <- glue("
    {num_const_cond}
    or STR_CONST
    or (*[not(self::COMMENT)][1][self::OP-MINUS or self::OP-PLUS] and count(expr) = 1 and expr[{num_const_cond}])
  ")

  arith_cond <- "
    (
      OP-PLUS
      or OP-MINUS
      or OP-STAR
      or OP-SLASH
      or OP-CARET
      or OP-COLON
      or SPECIAL[text() = '%%' or text() = '%/%' or text() = '%*%' or text() = '%o%' or text() = '%x%']
    )
    and not(*[not(self::COMMENT)][1][self::OP-MINUS or self::OP-PLUS] and count(expr) = 1 and expr[NUM_CONST])
  "

  coercer_expr_cond <- glue("
    (
      OP-EXCLAMATION
      and expr[1][{call_cond} or (OP-LEFT-PAREN and expr[{call_cond}])]
    ) or (
      {call_cond}
      and not(parent::expr[OP-EXCLAMATION] or parent::expr[OP-LEFT-PAREN]/parent::expr[OP-EXCLAMATION])
    )
  ")

  xpath <- glue("
  (//IF | //WHILE)/following-sibling::expr[1]
    /descendant-or-self::expr[
      ({in_bool_context})
      and (({coercer_expr_cond}) or ({literal_cond}) or ({arith_cond}))
    ]
  ")

  get_lint_message <- function(bad_expr) {
    if (length(bad_expr) == 0L) {
      return(character())
    }
    is_literal <- xml_find_lgl_(bad_expr, glue("boolean({literal_cond})"))
    is_arith <- xml_find_lgl_(bad_expr, glue("boolean({arith_cond})"))
    is_call <- !is_literal & !is_arith

    msg <- character(length(bad_expr))
    msg[is_literal] <-
      "Use explicit TRUE or FALSE instead of relying on implicit coercion of literals in conditional expressions."
    msg[is_arith] <- paste(
      "Use an explicit comparison (e.g., != 0) instead of relying on",
      "implicit coercion of arithmetic expressions in conditional expressions."
    )

    if (any(is_call)) {
      call_nodes <- bad_expr[is_call]
      is_negated <- xml_find_lgl_(call_nodes, "boolean(OP-EXCLAMATION)")
      fn_name <- get_r_string(call_nodes, "descendant::SYMBOL_FUNCTION_CALL[1]")
      bang <- ifelse(is_negated, "!", "")
      comparator <- ifelse(fn_name %in% non_neg_funs, "> 0L", "!= 0L")
      comparator[is_negated] <- "== 0L"

      call_msg <- sprintf(
        "Use explicit comparison (e.g., %s(...) %s) instead of relying on implicit coercion in conditional expressions.", # nolint: line_length_linter.
        fn_name, comparator
      )
      is_nchar <- fn_name == "nchar"
      call_msg[is_nchar] <- sprintf(
        "Use %1$snzchar(x) instead of relying on implicit coercion of %1$snchar(x) in conditional expressions.",
        bang[is_nchar]
      )
      is_grep <- fn_name == "grep"
      call_msg[is_grep] <- sprintf(
        "Use %1$sgrepl() instead of relying on implicit coercion of %1$sgrep() in conditional expressions.",
        bang[is_grep]
      )
      msg[is_call] <- call_msg
    }

    msg
  }

  # nolint next: cyclocomp_linter. Helpers in factory inflate complexity (gaborcsardi/cyclocomp#30).
  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    bad_expr <- xml_find_all_(xml, xpath)
    lint_message <- get_lint_message(bad_expr)

    xml_nodes_to_lints(bad_expr, source_expression, lint_message = lint_message, type = "warning")
  })
}
