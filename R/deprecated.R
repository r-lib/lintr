#' Deprecated functions
#'
#' Functions that have been deprecated and replaced by newer ones. They will be removed in an
#' upcoming version of \pkg{lintr} and should thus not be used anymore.
#'
#' @name lintr-deprecated
#' @param source_file source file object from \code{\link{get_source_expressions}}
#' @include object_name_linters.R
NULL

lintr_deprecated <- function(old, new = NULL, version = NULL,
                             type = "Function") {
  msg <- c(
    c(type, " ", old, " was deprecated"),
    if (length(version)) {
      c(" in lintr version ", version)
    },
    ". ",
    if (length(new)) {
      c("Use ", new, " instead.")
    }
  )
  msg <- paste0(msg, collapse = "")
  warning(msg, call. = FALSE, domain = NA)
}


##################################################


#' @describeIn lintr-deprecated checks that no absolute paths are used.
#' @export
absolute_paths_linter <- function(source_file) {
  lintr_deprecated("'absolute_paths_linter'", "'absolute_path_linter'", "1.0.0.9001")
  absolute_path_linter(lax = TRUE)(source_file)
}
class(absolute_paths_linter) <- "linter"
attr(absolute_paths_linter, "name") <- "absolute_paths_linter"


#' @describeIn lintr-deprecated Check there are no trailing semicolons.
#' @export
trailing_semicolons_linter <- function(source_file) {
  lintr_deprecated("'trailing_semicolons_linter'", "'semicolon_terminator_linter'", "1.0.0.9001")
  semicolon_terminator_linter(semicolon = "trailing")(source_file)
}
class(trailing_semicolons_linter) <- "linter"
attr(trailing_semicolons_linter, "name") <- "trailing_semicolons_linter"


#' @describeIn lintr-deprecated Check that objects are not in camelCase.
#' @export
camel_case_linter <- make_object_linter(function(source_file, parsed) {

  lintr_deprecated("'camel_case_linter'", "'object_name_linter'", "1.0.0.9001")

  is_camel_case <- re_matches(parsed$text, rex(lower, upper))

  if (is_camel_case &&
      !is_external_reference(source_file, parsed$id) &&
      !is_base_function(parsed$text)) {
    object_lint(source_file,
                parsed,
                "Variable and function names should be all lowercase.")
  }
}, name = "camel_case_linter")


#' @describeIn lintr-deprecated Check that objects are not in snake_case.
#' @export
snake_case_linter <- make_object_linter(function(source_file, parsed) {

  lintr_deprecated("'snake_case_linter'", "'object_name_linter'", "1.0.0.9001")

  is_snake_case <- re_matches(parsed$text, rex(alnum, "_", alnum))

  if (is_snake_case &&
      !is_external_reference(source_file, parsed$id) &&
      !is_base_function(parsed$text)) {
    object_lint(source_file,
                parsed,
                "Variable and function names should not use underscores.")
  }
}, name = "snake_case_linter")


#' @describeIn lintr-deprecated check that objects do not have.multiple.dots.
#' @export
multiple_dots_linter <- make_object_linter(function(source_file, parsed) {

  lintr_deprecated("'multiple_dots_linter'", "'object_name_linter'", "1.0.0.9001")

  has_multiple_dots <- re_matches(parsed$text, rex(".", something, "."))
  if (has_multiple_dots &&
      !is_external_reference(source_file, parsed$id) &&
      !is_base_function(parsed$text)) {
    object_lint(source_file,
                parsed,
                "Words within variable and function names should be separated by '_' rather than '.'.")
  }
}, name = "multiple_dots_linter")
