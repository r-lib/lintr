#' Non-portable path linter
#'
#' Check that [file.path()] is used to construct safe and portable paths.
#'
#' @inheritParams absolute_path_linter
#' @evalRd rd_tags("nonportable_path_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - [absolute_path_linter()]
#' @export
nonportable_path_linter <- function(lax = TRUE) {
  path_linter_factory(
    path_function = function(path) {
      is_path(path) && is_valid_long_path(path, lax) && path != "/" &&
        re_matches(path, rex(one_of("/", "\\")))
    },
    message = "Use file.path() to construct portable file paths."
  )
}
