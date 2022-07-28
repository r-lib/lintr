#' Cyclomatic complexity linter
#'
#' Check for overly complicated expressions. See [cyclocomp::cyclocomp()].
#'
#' @param complexity_limit expressions with a cyclomatic complexity higher than
#' this limit are linted, defaults to 15.
#' See [cyclocomp::cyclocomp()].
#' @evalRd rd_tags("cyclocomp_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @importFrom cyclocomp cyclocomp
#' @export
cyclocomp_linter <- function(complexity_limit = 15L) {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }
    complexity <- try_silently(
      cyclocomp::cyclocomp(parse(text = source_expression$content))
    )
    if (inherits(complexity, "try-error") || complexity <= complexity_limit) {
      return(list())
    }
    col1 <- source_expression[["column"]][1L]
    Lint(
      filename = source_expression[["filename"]],
      line_number = source_expression[["line"]][1L],
      column_number = source_expression[["column"]][1L],
      type = "style",
      message = sprintf(
        "Functions should have cyclomatic complexity of less than %d, this has %d.",
        complexity_limit, complexity
      ),
      ranges = list(rep(col1, 2L)),
      line = source_expression$lines[1L]
    )
  })
}
