make_object_linter <- function(fun) {
  function(source_file) {

    pkg_name <- pkg_name(find_package(dirname(source_file$filename)))

    pkg_objects <- if (!is.null(pkg_name)) {
      envir <- try(getNamespace(pkg_name), silent = TRUE)

      if (!inherits(envir, "try-error")) {
        ls(envir = envir)
      }
    }

    attached_nms <-
      c(unlist(lapply(search(), ls)),
        "...",
        pkg_objects)

    lapply(
      ids_with_token(source_file,
        rex(start, "SYMBOL" %if_next_isnt% "_SUB"),
        fun = re_matches),

      function(id) {

        parsed <- with_id(source_file, id)

        if (!parsed$text %in% attached_nms) {
          fun(source_file, parsed)
        }
      })
  }
}

object_lint <- function(source_file, parsed, message, type) {

  Lint(
    filename = source_file$filename,
    line_number = parsed$line1,
    column_number = parsed$col1,
    type = "style",
    message = message,
    line = source_file$lines[as.character(parsed$line1)],
    ranges = list(c(parsed$col1, parsed$col2)),
    linter = type
    )
}

#' @describeIn linters check that objects are not in camelCase.
#' @export
camel_case_linter <- make_object_linter(function(source_file, parsed) {

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

#' @describeIn linters check that objects are not in snake_case.
#' @export
snake_case_linter <- make_object_linter(function(source_file, parsed) {

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

#' @describeIn linters check that objects do not have.multiple.dots.
#' @export
multiple_dots_linter <- make_object_linter(function(source_file, parsed) {

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

#' @describeIn linters check that objects do are not very long.not
#' have.multiple.dots.
#' @export
object_length_linter <- function(length = 20L) {
  make_object_linter(function(source_file, parsed) {

    is_very_long <- nchar(parsed$text) > length
    if (is_very_long &&
        !is_external_reference(source_file, parsed$id) &&
        !is_base_function(parsed$text)) {
        object_lint(source_file,
          parsed,
          paste0("Variable and function names should not be longer than ", length, " characters."),
          "object_length_linter")
      }
  })
}

is_external_reference <- function(source_file, id) {
  sibling_tokens <- with_id(source_file, siblings(source_file$parsed_content, id, 1))$token

  any(sibling_tokens %in% c("NS_GET", "NS_GET_INT"))
}

base_pkgs <- c(
               "base",
               "tools",
               "utils",
               "grDevices",
               "graphics",
               "stats",
               "datasets",
               "methods",
               "grid",
               "splines",
               "stats4",
               "compiler",
               "parallel",
               "MASS",
               "lattice",
               "Matrix",
               "nlme",
               "survival",
               "boot",
               "cluster",
               "codetools",
               "foreign",
               "KernSmooth",
               "rpart",
               "class",
               "nnet",
               "spatial",
               "mgcv"
               )

base_funs <- unlist(lapply(base_pkgs,
    function(x) {
      name <- try(getNamespace(x))
      if (!inherits(name, "try-error")) {
        ls(name, all.names = TRUE)
      }
    }))

is_base_function <- function(x) {
  x %in% base_funs
}
