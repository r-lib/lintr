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
    xpath_exceptions <- glue("SYMBOL_FUNCTION_CALL[ not({exceptions}) ]")
  } else {
    xpath_exceptions <- "SYMBOL_FUNCTION_CALL"
  }

  # The walrus operator `:=` is also `LEFT_ASSIGN`, but is not a relevant operator
  # to be considered for the present linter.
  assignments <- paste(
    "//LEFT_ASSIGN[text() != ':=']",
    "//RIGHT_ASSIGN",
    sep = " | "
  )

  xpath <- glue("
    ({assignments})
      /parent::expr[
        preceding-sibling::*[2][self::IF or self::WHILE]
        or parent::forcond
        or preceding-sibling::expr/{xpath_exceptions}
        or parent::expr/*[1][self::OP-LEFT-PAREN]
      ]
  ")

  Linter(function(source_expression) {
    # need the full file to also catch usages at the top level
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

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
