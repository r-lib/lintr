make_object_linter <- function(fun) {
  function(source_file) {

    pkg_name <- pkg_name(find_package(dirname(source_file$filename)))

    pkg_objects <- if (!is.null(pkg_name)) {
      ls(envir=getNamespace(pkg_name))
    }

    attached_nms <-
      c(unlist(lapply(search(), ls)),
        "...",
        pkg_objects)

    lapply(ids_with_token(source_file, re = rex(start, "SYMBOL" %if_next_isnt% "_SUB")),

      function(id) {

        parsed <- source_file$parsed_content[as.character(id), ]

        name <- parsed$text

        if (!name %in% attached_nms) {
          fun(source_file, id)
        }
      })
  }
}

object_lint <- function(source_file, id, message) {

  parsed <- source_file$parsed_content[as.character(id), ]
  Lint(
    filename = source_file$filename,
    line_number = parsed$line1,
    column_number = parsed$col1,
    type = "style",
    message = message,
    line = getSrcLines(source_file, parsed$line1, parsed$line1),
    ranges = list(c(parsed$col1, parsed$col2))
    )
}

#' @describeIn linters check that objects are not in camelCase.
#' @export
object_camel_case_linter <- make_object_linter(function(source_file, id) {

  parsed <- source_file$parsed_content[as.character(id), ]

  name <- parsed$text

  sibling_tokens <- source_file$parsed_content[
    as.character(siblings(source_file$parsed_content, id)),
    "token"]

  is_camel_case <- !any(sibling_tokens %in% c("NS_GET", "NS_GET_INT")) &&
    re_matches(name, rex(lower, upper))

  if (is_camel_case) {
    object_lint(source_file,
      id,
      "Variable and function names should be all lowercase.")
  }

})

#' @describeIn linters check that objects are not in snake_case.
#' @export
object_snake_case_linter <- make_object_linter(function(source_file, id) {

  parsed <- source_file$parsed_content[as.character(id), ]

  name <- parsed$text

  sibling_tokens <- source_file$parsed_content[
    as.character(siblings(source_file$parsed_content, id)),
    "token"]

  is_snake_case <- !any(sibling_tokens %in% c("NS_GET", "NS_GET_INT")) &&
    re_matches(name, rex(alnum, "_", alnum))

  if (is_snake_case) {
    object_lint(source_file,
      id,
      "Variable and function names should not use underscores.")
  }

})

#' @describeIn linters check that objects do not have.multiple.dots.
#' @export
object_multiple_dots_linter <- make_object_linter(function(source_file, id) {

  parsed <- source_file$parsed_content[as.character(id), ]

  name <- parsed$text

  has_multiple_dots <- re_matches(name, rex(".", something, "."))
  if (has_multiple_dots) {
    object_lint(source_file,
      id,
      "Words within variable and function names should be separated by '_' rather than '.'.")
  }

})

#' @describeIn linters check that objects do are not very long.not
#' have.multiple.dots.
#' @export
object_length_linter <- function(length = 20L) {
  make_object_linter(function(source_file, id) {

    parsed <- source_file$parsed_content[as.character(id), ]

    name <- parsed$text

    is_very_long <- nchar(name) > length
    if (is_very_long) {
      object_lint(source_file,
        id,
        paste0("Variable and function names should not be longer than ", length, " characters."))
    }
  })
}
