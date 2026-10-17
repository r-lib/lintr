#' Require explicit `add` in subsequent `on.exit()` calls
#'
#' Calling [base::on.exit()] multiple times in the same block without
#'   specifying `add = TRUE` overwrites previously registered exit handlers
#'   because `add` defaults to `FALSE`. Either pass `add = TRUE` to append to
#'   existing exit handlers, or pass `add = FALSE` explicitly to make
#'   overwriting intentional. Clearing calls (`on.exit()` with no arguments or
#'   `on.exit(NULL)`) are exempt.
#'
#' @examples
#' # will produce lints
#' code <- "f <- function() {\n  on.exit(cleanup1())\n  on.exit(cleanup2())\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = on_exit_linter()
#' )
#'
#' # okay
#' code <- "f <- function() {\n  on.exit(cleanup1())\n  on.exit(cleanup2(), add = TRUE)\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = on_exit_linter()
#' )
#'
#' code <- "f <- function() {\n  on.exit(cleanup1())\n  on.exit(cleanup2(), add = FALSE)\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = on_exit_linter()
#' )
#'
#' code <- "f <- function() {\n  on.exit(cleanup1())\n  on.exit()\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = on_exit_linter()
#' )
#'
#' @evalRd rd_tags("on_exit_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
on_exit_linter <- local({
  not_kwarg_cond <- "not(preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB])"
  has_non_null_expr <- glue("
    SYMBOL_SUB[text() = 'expr']/following-sibling::expr[1][not(NULL_CONST)]
    or expr[
      not(NULL_CONST)
      and {not_kwarg_cond}
      and not(parent::expr/SYMBOL_SUB[text() = 'expr'])
      and count(preceding-sibling::expr[{not_kwarg_cond}]) = 1
    ]
  ")

  has_add_arg <- glue("
    SYMBOL_SUB[text() = 'add']
    or expr[
      {not_kwarg_cond}
      and count(preceding-sibling::expr[{not_kwarg_cond}]) >= 2 - count(parent::expr/SYMBOL_SUB[text() = 'expr'])
    ]
  ")

  is_on_exit <- "expr[1]/SYMBOL_FUNCTION_CALL[text() = 'on.exit']"

  prev_on_exit_is_active <- glue("
    (
      preceding-sibling::expr[{is_on_exit}]
      | preceding-sibling::exprlist/expr[{is_on_exit}]
      | self::*[parent::exprlist]/parent::exprlist/preceding-sibling::expr[{is_on_exit}]
      | self::*[parent::exprlist]/parent::exprlist/preceding-sibling::exprlist/expr[{is_on_exit}]
    )[last()][{has_non_null_expr}]
  ")

  xpath <- glue("
  parent::expr[
    (
      parent::expr[OP-LEFT-BRACE]
      or parent::exprlist[not(parent::*) or parent::expr[OP-LEFT-BRACE]]
    )
    and ({has_non_null_expr})
    and not({has_add_arg})
    and {prev_on_exit_is_active}
  ]
  ")

  make_linter_from_function_xpath(
    function_names = "on.exit",
    xpath = xpath,
    lint_message = "Provide 'add = TRUE' to add an exit handler, or 'add = FALSE' to overwrite the previous on.exit() call.", # nolint: line_length_linter.
    type = "warning",
    level = "file"
  )
})
