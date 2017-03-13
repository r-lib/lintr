#' Deprecated functions
#'
#' Functions that have been deprecated and replaced by newer ones. They will be removed in an
#' upcoming version of \pkg{lintr} and should thus not be used anymore.
#'
#' @name lintr-deprecated
#' @inheritParams linters
#' @include object_name_linters.R
NULL

lintr_deprecated <- function(old, new, version) {
  msg <- sprintf(
    "'%s' was deprecated in lintr version %s. Use '%s' instead. See help(\"lintr-deprecated\").",
    old, version, new)
  warning(msg, call. = FALSE, domain = NA)
}


##################################################


#' @describeIn lintr-deprecated checks that no absolute paths are used.
#' @export
absolute_paths_linter <- function(source_file) {
  lintr_deprecated("absolute_paths_linter", "absolute_path_linter", "1.0.0.9001")
  absolute_path_linter(lax = TRUE)(source_file)
}


#' @describeIn lintr-deprecated check there are no trailing semicolons.
#' @export
trailing_semicolons_linter <- function(source_file) {
  lintr_deprecated("trailing_semicolons_linter", "semicolon_terminator_linter", "1.0.0.9001")
  semicolon_terminator_linter(type = "trailing")(source_file)
}


#' @describeIn linters check that objects are not in camelCase.
#' @export
camel_case_linter <- make_object_linter(function(source_file, parsed) {

  lintr_deprecated("camel_case_linter", "object_name_linter", "1.0.0.9001")

  is_camel_case <- re_matches(parsed$text, rex(lower, upper))

  if (is_camel_case &&
      !is_external_reference(source_file, parsed$id) &&
      !is_base_function(parsed$text)) {
    object_lint(source_file,
                parsed,
                "Variable and function names should be all lowercase.",
                "camel_case_linter")
  }
})


#' @describeIn lintr-deprecated check that objects are not in snake_case.
#' @export
snake_case_linter <- make_object_linter(function(source_file, parsed) {

  lintr_deprecated("snake_case_linter", "object_name_linter", "1.0.0.9001")

  is_snake_case <- re_matches(parsed$text, rex(alnum, "_", alnum))

  if (is_snake_case &&
      !is_external_reference(source_file, parsed$id) &&
      !is_base_function(parsed$text)) {
    object_lint(source_file,
                parsed,
                "Variable and function names should not use underscores.",
                "snake_case_linter")
  }
})


#' @describeIn lintr-deprecated check that objects do not have.multiple.dots.
#' @export
multiple_dots_linter <- make_object_linter(function(source_file, parsed) {

  lintr_deprecated("multiple_dots_linter", "object_name_linter", "1.0.0.9001")

  has_multiple_dots <- re_matches(parsed$text, rex(".", something, "."))
  if (has_multiple_dots &&
      !is_external_reference(source_file, parsed$id) &&
      !is_base_function(parsed$text)) {
    object_lint(source_file,
                parsed,
                "Words within variable and function names should be separated by '_' rather than '.'.",
                "multiple_dots_linter")
  }
})

