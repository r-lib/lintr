#' Avoid implicit assignment in function calls
#'
#' Assigning inside function calls makes the code difficult to read, and should
#' be avoided, except for functions that capture side-effects (e.g. [capture.output()]).
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
implicit_assignment_linter <- function() {
  assignments <- c(
    "LEFT_ASSIGN", # e.g. mean(x <- 1:4)
    "RIGHT_ASSIGN" # e.g. mean(1:4 -> x)
  )
  xpath_fun_call <- paste0(
    "//SYMBOL_FUNCTION_CALL[not(text() = 'capture.output')]
    /parent::expr
    /following::expr[1]/",
    assignments,
    collapse = " | "
  )

  controls <- c(
    "//IF/following::expr[1]/", # e.g. if (x <- 1L) { ... }
    "//WHILE/following::expr[1]/" # e.g. while (x <- 0L) { ... }
  )
  xpath_controls <- paste0(rep(controls, each = length(assignments)), assignments, collapse = " | ")

  xpath <- paste0(c(xpath_controls, xpath_fun_call), collapse = " | ")

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
