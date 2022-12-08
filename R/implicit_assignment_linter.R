#' Avoid implicit assignment in function calls
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
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
implicit_assignment_linter <- function() {
  assignments <- c(
    "LEFT_ASSIGN", # mean(x <- 1:4)
    "RIGHT_ASSIGN" # mean(1:4 -> x)
  )

  controls <- c(
    "//IF/following::expr/", # if (x <- 1L) { ... }
    "//WHILE/following::expr/" # while (x <- 0L) { ... }
  )
  xpath_controls <- paste0(rep(controls, each = length(assignments)), assignments, collapse = " | ")

  xpath_fun_call <- paste0(
    "//SYMBOL_FUNCTION_CALL/parent::expr/following::expr[1]/",
    assignments,
    collapse = " | "
  )

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
