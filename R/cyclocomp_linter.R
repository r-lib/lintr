#' Cyclomatic complexity linter
#'
#' Check for overly complicated expressions. See [cyclocomp::cyclocomp()].
#'
#' @param complexity_limit expressions with a cyclomatic complexity higher than this are linted, defaults to 15.
#' See [cyclocomp::cyclocomp()].
#' @evalRd rd_tags("cyclocomp_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @importFrom cyclocomp cyclocomp
#' @export
cyclocomp_linter <- function(complexity_limit = 15L) {
  Linter(function(source_expression) {
    if (!is.null(source_expression[["file_lines"]])) {
      # abort if source_expression is entire file, not a top level expression.
      return(NULL)
    }
    complexity <- try_silently(
      cyclocomp::cyclocomp(parse(text = source_expression$content))
    )
    if (inherits(complexity, "try-error")) return(NULL)
    if (complexity <= complexity_limit) return(NULL)
    Lint(
      filename = source_expression[["filename"]],
      line_number = source_expression[["line"]][1],
      column_number = source_expression[["column"]][1],
      type = "style",
      message = paste0(
        "functions should have cyclomatic complexity of less than ",
        complexity_limit, ", this has ", complexity, "."
      ),
      ranges = list(c(source_expression[["column"]][1], source_expression[["column"]][1])),
      line = source_expression$lines[1]
    )
  })
}
