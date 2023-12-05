#' Block instances of unnecessary nesting
#'
#' Excessive nesting harms readability. Use helper functions or early returns
#'   to reduce nesting wherever possible.
#'
#' @param allow_assignment Logical, default `TRUE`, in which case
#'   braced expressions consisting only of a single assignment are skipped.
#'   if `FALSE`, all braced expressions with only one child expression are linted.
#'   The `TRUE` case facilitates interaction with [implicit_assignment_linter()]
#'   for certain cases where an implicit assignment is necessary, so a braced
#'   assignment is used to further distinguish the assignment. See examples.
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
#' code <- "expect_warning(\n  {\n    x <- foo()\n  },\n  'warned'\n)"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = unnecessary_nesting_linter(allow_assignment = FALSE)
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
#' code <- "expect_warning(\n  {\n    x <- foo()\n  },\n  'warned'\n)"
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
unnecessary_nesting_linter <- function(allow_assignment = TRUE) {
  exit_calls <- c("stop", "return", "abort", "quit", "q")
  exit_call_expr <- glue("
    expr[SYMBOL_FUNCTION_CALL[{xp_text_in_table(exit_calls)}]]
  ")
  # block IF here for cases where a nested if/else is entirely within
  #   one of the branches.
  no_exit_call_expr <- glue("
  expr[
    OP-LEFT-BRACE
    and expr[
      position() = last()
      and not(IF)
      and not(expr[SYMBOL_FUNCTION_CALL[{xp_text_in_table(exit_calls)}]])
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
        following-sibling::{no_exit_call_expr}
        or preceding-sibling::{no_exit_call_expr}
      )
    ]
  ]
  ")

  assignment_cond <- if (allow_assignment) "expr[LEFT_ASSIGN or RIGHT_ASSIGN]" else "false"

  # several carve-outs of common cases where single-expression braces are OK
  #   - control flow statements: if, for, while, repeat
  #       + include foreach() as a common package-based for loop extension
  #   - function definitions
  #       + includes purrr-like anonymous functions as ~ {...}
  #   - rlang's double-brace expressions like {{ var }}
  #       + NB: both braces would trigger here, so we must exclude both of them
  #   - any expression starting like `({` or `[{` or ending like `})` or `}]`
  #       + note that nesting is not improved by "fixing" such cases, and could also be worsened
  #       + motivated by the most common cases:
  #          * test_that("test", { expr })
  #          * with(x, { expr }) / within(x, { expr })
  #          * suppressWarnings({ expr })
  #          * DataTable[, { expr }]
  #          * DataTable[, col := { expr }] <- requires carve-out for `:=`
  unnecessary_brace_xpath <- glue("
  //OP-LEFT-BRACE
    /parent::expr[
      count(expr) = 1
      and not(preceding-sibling::*[
        self::FUNCTION
        or self::FOR
        or self::IF
        or self::WHILE
        or self::REPEAT
        or self::expr/expr/SYMBOL_FUNCTION_CALL[text() = 'foreach']
        or self::OP-TILDE
        or self::LEFT_ASSIGN[text() = ':=']
      ])
      and not(expr/OP-LEFT-BRACE)
      and not(preceding-sibling::OP-LEFT-BRACE)
      and not(
        OP-LEFT-BRACE/@end - 1 = preceding-sibling::*[1][self::OP-LEFT-PAREN or self::OP-LEFT-BRACKET]/@end
        or OP-RIGHT-BRACE/@end + 1 = following-sibling::*[1][self::OP-RIGHT-PAREN or self::OP-RIGHT-BRACKET]/@end
      )
      and not({assignment_cond})
    ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content


    if_else_exit_expr <- xml_find_all(xml, if_else_exit_xpath)
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
