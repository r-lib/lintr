#' Avoid implicit assignment in function calls
#'
#' Assigning inside function calls makes the code difficult to read, and should
#' be avoided, except for functions that capture side-effects (e.g. [capture.output()]).
#'
#' @param except A character vector of functions to be excluded from linting.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "if (x <- 1L) TRUE",
#'   linters = implicit_assignment_linter()
#' )
#'
#' lint(
#'   text = "mean(x <- 1:4)",
#'   linters = implicit_assignment_linter()
#' )
#'
#' # okay
#' writeLines("x <- 1L\nif (x) TRUE")
#' lint(
#'   text = "x <- 1L\nif (x) TRUE",
#'   linters = implicit_assignment_linter()
#' )
#'
#' writeLines("x <- 1:4\nmean(x)")
#' lint(
#'   text = "x <- 1:4\nmean(x)",
#'   linters = implicit_assignment_linter()
#' )
#'
#' @evalRd rd_tags("implicit_assignment_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#assignment>
#'
#' @export
implicit_assignment_linter <- function(except = c("bquote", "expression", "expr", "quo", "quos", "quote")) {
  stopifnot(is.null(except) || is.character(except))

  if (length(except) > 0L) {
    exceptions <- xp_text_in_table(except)
    xpath_exceptions <- glue::glue("
    //SYMBOL_FUNCTION_CALL[ not({exceptions}) ]")
  } else {
    xpath_exceptions <- "
    //SYMBOL_FUNCTION_CALL"
  }

  # The walrus operator `:=` is also `LEFT_ASSIGN`, but is not a relevant operator
  # to be considered for the present linter.
  assignments <- c(
    "LEFT_ASSIGN[text() != ':=']", # e.g. mean(x <- 1:4)
    "RIGHT_ASSIGN" # e.g. mean(1:4 -> x)
  )

  xpath_fun_call <- paste0(
    xpath_exceptions,
    "
    /parent::expr
    /following-sibling::expr[1]
    /"
  )
  xpath_fun_assigment <- paste0(
    xpath_fun_call,
    assignments,
    collapse = " | "
  )

  controls <- c(
    # e.g. if (x <- 1L) { ... }
    "
    //IF
    /following-sibling::expr[1]
    /",
    # e.g. while (x <- 0L) { ... }
    "
    //WHILE
    /following-sibling::expr[1]
    /",
    # e.g. for (x in y <- 1:10) { ... }
    "
    //forcond
    /expr[1]
    /"
  )
  xpath_controls_assignment <- paste0(
    rep(controls, each = length(assignments)),
    assignments,
    collapse = " | "
  )

  xpath <- paste0(c(xpath_controls_assignment, xpath_fun_assigment), collapse = " | ")

  Linter(function(source_expression) {
    # need the full file to also catch usages at the top level
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    lint_message <- paste(
      "Avoid implicit assignments in function calls.",
      "For example, instead of `if (x <- 1L) { ... }`, write `x <- 1L; if (x) { ... }`."
    )

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}
