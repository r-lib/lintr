#' Block instances of excessive nesting
#'
#' Excessive nesting harms readability. Use helper functions or early returns
#'   to reduce nesting wherever possible.
#'
#' @examples
#' # will produce lints
#' code <- "if (A) {\n  stop('A is bad!')\n} else {\n  do_good()\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = unnecessary_nesting_linter()
#' )
#'
#' code <- "tryCatch(\n  {\n    foo()\n  },\n  error = identity\n)"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = unnecessary_nesting_linter()
#' )
#'
#' # okay
#' code <- "if (A) {\n  stop('A is bad because a.')\n} else {\n  stop('!A is bad too.')\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = unnecessary_nesting_linter()
#' )
#'
#' code <- "capture.output({\n  foo()\n})"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = unnecessary_nesting_linter()
#' )
#'
#' @evalRd rd_tags("unnecessary_nesting_linter")
#' @seealso
#'  - [cyclocomp_linter()] for another linter that penalizes overly complexcode.
#'  - [linters] for a complete list of linters available in lintr.
#' @export
unnecessary_nesting_linter <- function() {
  exit_calls <- c("stop", "return", "abort", "quit", "q")
  # These calls can be called in the sibling branch and not trigger a lint,
  #   allowing for cleanly parallel code, where breaking it would often harm readability:
  #   > if (A) {
  #   >   stop()
  #   > } else {
  #   >   warning()
  #   > }
  # NB: print() is intentionally excluded since its usage is usually a mistake (?print_linter)
  signal_calls <- c(
    exit_calls,
    "warning", "warn", "message", "cat", "LOG", "stopifnot"
  )
  exit_call_expr <- glue("
  expr[SYMBOL_FUNCTION_CALL[{xp_text_in_table(exit_calls)}]]
  ")
  # block IF here for cases where a nested if/else is entirely within
  #   one of the branches.
  # TODO(michaelchirico): we could try and make the parallel exits requirement
  #   more recursive, but it's a pain to do so.
  no_signal_call_expr <- glue("
  expr[
    OP-LEFT-BRACE
    and expr[
      position() = last()
      and not(IF)
      and not(expr[SYMBOL_FUNCTION_CALL[{xp_text_in_table(signal_calls)}]])
    ]
  ]
  ")
  # condition for ELSE should be redundant, but include for robustness
  # condition on parent::expr[IF] ensures we're at the first `if` of a sequence of if/else statements
  # condition on expr uses following-sibling or preceding-sibling to ensure
  #   that the other expr falls on a different branch (earlier used separate
  #   conditions on two expr[], but seems to allow any branch with >1 statement
  #   to lead to a lint.
  # use position() = last() to ignore any expr but the last one in any branch.
  if_else_exit_xpath <- glue("
  //expr[
    IF
    and ELSE
    and not(parent::expr[IF])
    and expr[
      OP-LEFT-BRACE
      and expr[position() = last() and {exit_call_expr}]
      and (
        following-sibling::{no_signal_call_expr}
        or preceding-sibling::{no_signal_call_expr}
      )
    ]
  ]
  ")

  # several carve-outs of common cases where single-expression braces are OK
  #   - control flow statements: if, for, while, repeat, switch()
  #       + switch() is unique in being a function, not a language element
  #       + include foreach() as a common package-based for loop extension
  #   - function definitions
  #       + includes purrr-like anonymous functions as ~ {...}
  #   - rlang's double-brace expressions like {{ var }}
  #       + NB: both braces would trigger here, so we must exclude both of them
  #   - any expression ending like `})` or `}]`
  #       + note that nesting is not improved by "fixing" such cases,
  #         and could also be worsened
  #       + motivated by the most common cases:
  #          * test_that("test", { expr })
  #          * with(x, { expr }) / within(x, { expr })
  #          * suppressWarnings({ expr })
  #          * DataTable[, { expr }]
  #          * DataTable[, col := { expr }] <- requires carve-out for `:=`
  unnecessary_brace_xpath <- "
  //OP-LEFT-BRACE
    /parent::expr[
      count(expr) = 1
      and not(preceding-sibling::*[
        self::FUNCTION
        or self::FOR
        or self::IF
        or self::WHILE
        or self::REPEAT
        or self::expr/SYMBOL_FUNCTION_CALL[text() = 'switch']
        or self::expr/expr/SYMBOL_FUNCTION_CALL[text() = 'foreach']
        or self::OP-TILDE
        or self::LEFT_ASSIGN[text() = ':=']
      ])
      and not(expr/OP-LEFT-BRACE)
      and not(preceding-sibling::OP-LEFT-BRACE)
      and not(
        OP-RIGHT-BRACE/@end + 1 = following-sibling::OP-RIGHT-PAREN/@end
        or OP-RIGHT-BRACE/@end + 1 = following-sibling::OP-RIGHT-BRACKET/@end
      )
    ]
  "

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    if_else_exit_expr <- xml_find_all(xml, if_else_exit_xpath)
    # TODO(michaelchirico): customize the error message to the exit clause used
    if_else_exit_lints <- xml_nodes_to_lints(
      if_else_exit_expr,
      source_expression = source_expression,
      lint_message = paste0(
        "Reduce the nesting of this if/else statement by unnesting the ",
        "portion without an exit clause (i.e., ",
        paste0(exit_calls, "()", collapse = ", "),
        ")."
      ),
      type = "warning"
    )

    unnecessary_brace_expr <- xml_find_all(xml, unnecessary_brace_xpath)
    unnecessary_brace_lints <- xml_nodes_to_lints(
      unnecessary_brace_expr,
      source_expression = source_expression,
      lint_message = "Reduce the nesting of this statement by removing the braces {}.",
      type = "warning"
    )

    c(if_else_exit_lints, unnecessary_brace_lints)
  })
}
