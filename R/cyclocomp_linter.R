#' @describeIn linters Check for overly complicated expressions. See
#'   \code{\link[cyclocomp]{cyclocomp}}.
#' @param complexity_limit expressions with a cyclomatic complexity higher than
#'   this are linted, defaults to 25. See \code{\link[cyclocomp]{cyclocomp}}.
#' @importFrom cyclocomp cyclocomp
#' @export
cyclocomp_linter <- function(complexity_limit = 15L) {
  Linter(function(source_file) {
    if (!is.null(source_file[["file_lines"]])) {
      # abort if source_file is entire file, not a top level expression.
      return(NULL)
    }
    complexity <- try_silently(
      cyclocomp::cyclocomp(parse(text = source_file$content))
    )
    if (inherits(complexity, "try-error")) return(NULL)
    if (complexity <= complexity_limit) return(NULL)
    Lint(
      filename = source_file[["filename"]],
      line_number = source_file[["line"]][1],
      column_number = source_file[["column"]][1],
      type = "style",
      message = paste0(
        "functions should have cyclomatic complexity of less than ",
        complexity_limit, ", this has ", complexity, "."
      ),
      ranges = list(c(source_file[["column"]][1], source_file[["column"]][1])),
      line = source_file$lines[1]
    )
  })
}
