#' Exclude linters from execution on files
#'
#' This linters wrapper is very useful when creating linters
#' for lint_dir or lint_package
#'
#' @param ... all linters that need to be exclude with extra rules
#' @param root Base directory for relative filename resolution.
#' @inheritParams lint
#'
#' @evalRd rd_tags("exclude_wrapper_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @examples
#'
#' # will produce lints
#' lint(
#'   text = strrep("x", 23L),
#'   linters = exclude_wrapper_linter(line_length_linter(20), exclusions = c("inst/", "vignettes/"))
#' )
#'
#' lint(
#'   text = paste0(strrep("x", 23L), "\nany(!x))"),
#'   linters = exclude_wrapper_linter(
#'     line_length_linter(20), outer_negation_linter(),
#'     exclusions = c("inst/", "vignettes/")
#'   )
#' )
#'
#' # okay
#' lint(
#'   text = strrep("x", 23L),
#'   linters = exclude_wrapper_linter(
#'     line_length_linter(20),
#'     exclusions = c(normalizePath(tempdir(), TRUE)),
#'     pattern = "^file"
#'    )
#' )
#'
#' @export
exclude_wrapper_linter <- function(
    ...,
    exclusions,
    pattern = rex::rex(".",
                       one_of("Rr"),
                       or("", "html", "md", "nw", "rst", "tex", "txt"),
                       end),
    root = getwd()) {

  dots <- list(...)
  exclusions <- normalize_exclusions(
    exclusions,
    root = root,
    pattern = pattern
  )

  Linter(function(source_expression) {
    filename <- source_expression$filename
    if (is.character(filename) && (filename %in% names(exclusions))) {
      return(list())
    }
    lapply(dots, function(linter_fun) {
      linter_fun(source_expression)
    })
  })
}
