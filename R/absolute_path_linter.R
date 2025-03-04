#' Absolute path linter
#'
#' Check that no absolute paths are used (e.g. "/var", "C:\\System", "~/docs").
#'
#' @param lax Less stringent linting, leading to fewer false positives.
#' If `TRUE`, only lint path strings, which
#'
#' * contain at least two path elements, with one having at least two characters and
#' * contain only alphanumeric chars (including UTF-8), spaces, and win32-allowed punctuation
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'R"--[/blah/file.txt]--"',
#'   linters = absolute_path_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'R"(./blah)"',
#'   linters = absolute_path_linter()
#' )
#'
#' @evalRd rd_tags("absolute_path_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - [nonportable_path_linter()]
#' @export
absolute_path_linter <- function(lax = TRUE) {
  path_linter_factory(
    path_function = function(path) {
      is_absolute_path(path) && is_valid_long_path(path, lax)
    },
    message = "Do not use absolute paths."
  )
}
