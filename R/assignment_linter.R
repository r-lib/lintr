#' Assignment linter
#'
#' Check that `<-` is always used for assignment.
#'
#' @param allow_cascading_assign Logical, default `TRUE`.
#'   If `FALSE`, [`<<-`][base::assignOps] and `->>` are not allowed.
#' @param allow_right_assign Logical, default `FALSE`. If `TRUE`, `->` and `->>` are allowed.
#' @param allow_trailing Logical, default `TRUE`. If `FALSE` then assignments aren't allowed at end of lines.
#' @param allow_pipe_assign Logical, default `FALSE`. If `TRUE`, magrittr's `%<>%` assignment is allowed.
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
  all_operators <- c("<-", "=", "->", "<<-", "->>", ":=", "%<>%")
  if ("any" %in% operator) {
    operator <- all_operators
  } else {
    operator <- match.arg(operator, all_operators, several.ok = TRUE)
  }
  trailing_assign_xpath <- paste(
    collapse = " | ",
    c(
      paste0("//LEFT_ASSIGN", if (allow_cascading_assign) "" else "[text() = '<-']"),
      if (allow_right_assign) paste0("//RIGHT_ASSIGN", if (allow_cascading_assign) "" else "[text() = '->']"),
      "//EQ_SUB",
      "//EQ_FORMALS",
      if (!allow_pipe_assign) "//SPECIAL[text() = '%<>%']"
    ),
    "[@line1 < following-sibling::expr[1]/@line1]"
  )

  xpath <- paste(collapse = " | ", c(
    # always block = (NB: the parser differentiates EQ_ASSIGN, EQ_SUB, and EQ_FORMALS)
    "//EQ_ASSIGN",
    # -> and ->> are both 'RIGHT_ASSIGN'
    if (!allow_right_assign) "//RIGHT_ASSIGN" else if (!allow_cascading_assign) "//RIGHT_ASSIGN[text() = '->>']",
    # <-, :=, and <<- are all 'LEFT_ASSIGN'; check the text if blocking <<-.
    # NB: := is not linted because of (1) its common usage in rlang/data.table and
    #   (2) it's extremely uncommon as a normal assignment operator
    if (!allow_cascading_assign) "//LEFT_ASSIGN[text() = '<<-']",
    if (!allow_trailing) trailing_assign_xpath,
    if (!allow_pipe_assign) "//SPECIAL[text() = '%<>%']"
  ))

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)
    if (length(bad_expr) == 0L) {
      return(list())
    }

    operator <- xml_text(bad_expr)
    lint_message_fmt <- rep("Use <-, not %s, for assignment.", length(operator))
    lint_message_fmt[operator %in% c("<<-", "->>")] <-
      "Replace %s by assigning to a specific environment (with assign() or <-) to avoid hard-to-predict behavior."
    lint_message_fmt[operator == "%<>%"] <-
      "Avoid the assignment pipe %s; prefer using <- and %%>%% separately."

    if (!allow_trailing) {
      bad_trailing_expr <- xml_find_all(xml, trailing_assign_xpath)
      trailing_assignments <- xml2::xml_attrs(bad_expr) %in% xml2::xml_attrs(bad_trailing_expr)
      lint_message_fmt[trailing_assignments] <- "Assignment %s should not be trailing at the end of a line."
    }

    lint_message <- sprintf(lint_message_fmt, operator)
    xml_nodes_to_lints(bad_expr, source_expression, lint_message, type = "style")
  })
}

drop_or_add <- function(x, y, add) (if (add) union else setdiff)(x, y)
