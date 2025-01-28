#' Assignment linter
#'
#' Check that the specified operator is used for assignment.
#'
#' @param operator Character vector of valid assignment operators. Defaults to allowing `<-` and `<<-`; other valid
#'   options are `=`, `->`, `->>`, `%<>%`; use `"any"` to denote "allow all operators", in which case this linter only
#'   considers `allow_trailing` for generating lints.
#' @param allow_cascading_assign (Deprecated) Logical, default `TRUE`.
#'   If `FALSE`, [`<<-`][base::assignOps] and `->>` are not allowed.
#' @param allow_right_assign (Deprecated) Logical, default `FALSE`. If `TRUE`, `->` and `->>` are allowed.
#' @param allow_trailing Logical, default `TRUE`. If `FALSE` then assignments aren't allowed at end of lines.
#' @param allow_pipe_assign (Deprecated) Logical, default `FALSE`. If `TRUE`, magrittr's `%<>%` assignment is allowed.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x = mean(x)",
#'   linters = assignment_linter()
#' )
#'
#' code_lines <- "1 -> x\n2 ->> y"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = assignment_linter()
#' )
#'
#' lint(
#'   text = "x %<>% as.character()",
#'   linters = assignment_linter()
#' )
#'
#' lint(
#'   text = "x <- 1",
#'   linters = assignment_linter(operator = "=")
#' )
#'
#' # okay
#' lint(
#'   text = "x <- mean(x)",
#'   linters = assignment_linter()
#' )
#'
#' code_lines <- "x <- 1\ny <<- 2"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = assignment_linter()
#' )
#'
#' # customizing using arguments
#' code_lines <- "1 -> x\n2 ->> y"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = assignment_linter(allow_right_assign = TRUE)
#' )
#'
#' lint(
#'   text = "x <<- 1",
#'   linters = assignment_linter(allow_cascading_assign = FALSE)
#' )
#'
#' writeLines("foo(bar = \n 1)")
#' lint(
#'   text = "foo(bar = \n 1)",
#'   linters = assignment_linter(allow_trailing = FALSE)
#' )
#'
#' lint(
#'   text = "x %<>% as.character()",
#'   linters = assignment_linter(allow_pipe_assign = TRUE)
#' )
#'
#' lint(
#'   text = "x = 1",
#'   linters = assignment_linter(operator = "=")
#' )
#'
#' @evalRd rd_tags("assignment_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#assignment-1>
#' - <https://style.tidyverse.org/pipes.html#assignment-2>
#' @export
assignment_linter <- function(operator = c("<-", "<<-"),
                              allow_cascading_assign = TRUE,
                              allow_right_assign = FALSE,
                              allow_trailing = TRUE,
                              allow_pipe_assign = FALSE) {
  if (!missing(allow_cascading_assign)) {
    lintr_deprecated("allow_cascading_assign", '"<<-" and/or "->>" in operator', version = "3.2.0", type = "Argument")
    operator <- drop_or_add(operator, "<<-", allow_cascading_assign)
  }
  if (!missing(allow_right_assign)) {
    lintr_deprecated("allow_right_assign", '"->" in operator', version = "3.2.0", type = "Argument")
    operator <- drop_or_add(operator, c("->", if (allow_cascading_assign) "->>"), allow_right_assign)
  }
  if (!missing(allow_pipe_assign)) {
    lintr_deprecated("allow_pipe_assign", '"%<>%" in operator', version = "3.2.0", type = "Argument")
    operator <- drop_or_add(operator, "%<>%", allow_pipe_assign)
  }
  all_operators <- c("<-", "=", "->", "<<-", "->>", "%<>%")
  if ("any" %in% operator) {
    operator <- all_operators
  } else {
    operator <- match.arg(operator, all_operators, several.ok = TRUE)
  }
  trailing_assign_xpath <- paste0(
    collapse = " | ",
    c(
      "//LEFT_ASSIGN",
      "//RIGHT_ASSIGN",
      "//EQ_SUB",
      "//EQ_FORMALS",
      "//SPECIAL[text() = '%<>%']"
    ),
    "[@line1 < following-sibling::expr[1]/@line1]"
  )

  op_xpath_parts <- c(
    if (!"=" %in% operator) "//EQ_ASSIGN",
    # -> and ->> are both 'RIGHT_ASSIGN'
    glue("//RIGHT_ASSIGN[{ xp_text_in_table(setdiff(c('->', '->>'), operator)) }]"),
    # <-, :=, and <<- are all 'LEFT_ASSIGN'; check the text if blocking <<-.
    # NB: := is not linted because of (1) its common usage in rlang/data.table and
    #   (2) it's extremely uncommon as a normal assignment operator
    glue("//LEFT_ASSIGN[{ xp_text_in_table(setdiff(c('<-', '<<-'), operator)) }]"),
    if (!"%<>%" %in% operator) "//SPECIAL[text() = '%<>%']"
  )
  if (!is.null(op_xpath_parts)) {
    # NB: Also used, essentially, in implicit_assignment_linter. Keep in sync.
    implicit_assignment_xpath <- "
      [not(parent::expr[
        preceding-sibling::*[2][self::IF or self::WHILE]
        or parent::forcond
        or parent::expr/*[1][self::OP-LEFT-PAREN]
      ])]
    "
    op_xpath <- paste0(op_xpath_parts, implicit_assignment_xpath, collapse = " | ")
  } else {
    op_xpath <- NULL
  }

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    lints <- NULL
    if (!is.null(op_xpath)) {
      op_expr <- xml_find_all(xml, op_xpath)

      op_text <- xml_text(op_expr)
      op_lint_message_fmt <- rep(
        sprintf(
          "Use %s for assignment, not %%s.",
          if (length(operator) > 1L) paste("one of", toString(operator)) else operator
        ),
        length(op_text)
      )
      op_lint_message_fmt[op_text %in% c("<<-", "->>")] <-
        "Replace %s by assigning to a specific environment (with assign() or <-) to avoid hard-to-predict behavior."
      op_lint_message_fmt[op_text == "%<>%"] <-
        "Avoid the assignment pipe %s; prefer using <- and %%>%% separately."

      op_lint_message <- sprintf(op_lint_message_fmt, op_text)
      lints <- xml_nodes_to_lints(op_expr, source_expression, op_lint_message, type = "style")
    }

    if (!allow_trailing) {
      trailing_assign_expr <- xml_find_all(xml, trailing_assign_xpath)
      trailing_assign_text <- xml_text(trailing_assign_expr)
      trailing_assign_msg_fmt <- "Assignment %s should not be trailing at the end of a line."
      trailing_assign_msg <- sprintf(trailing_assign_msg_fmt, trailing_assign_text)
      lints <- c(lints,
        xml_nodes_to_lints(trailing_assign_expr, source_expression, trailing_assign_msg, type = "style")
      )
    }

    lints
  })
}

drop_or_add <- function(x, y, add) (if (add) union else setdiff)(x, y)
