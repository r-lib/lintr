#' @describeIn linters check that objects
#' \enumerate{
#'   \item Are never camelCase
#'   \item Are separated by \code{_} rather than \code{.}
#'   \item Are not more than \code{width} characters
#' }
#' @export
object_name_linter <- function(width = 20L) {
  function(source_file) {

    pkg_name <- pkg_name(find_package(dirname(source_file$filename)))

    pkg_objects <- if (!is.null(pkg_name)) {
      ls(envir=getNamespace(pkg_name))
    }

    attached_nms <-
      c(unlist(lapply(search(), ls)),
        "...",
        pkg_objects)

    object_linter <- function(parsed, message) {
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

    lapply(ids_with_token(source_file, re = rex(start, "SYMBOL" %if_next_isnt% "_SUB")),

      function(id) {

        parsed <- source_file$parsed_content[as.character(id), ]

        name <- parsed$text
        sibling_tokens <- source_file$parsed_content[
          as.character(siblings(source_file$parsed_content, id)),
          "token"]

        res <- list()
        if (!name %in% attached_nms) {

          is_camel_case <- !any(sibling_tokens %in% c("NS_GET", "NS_GET_INT")) &&
            re_matches(name, rex(lower, upper))

          if (is_camel_case) {
            res[[length(res) + 1L]] <- object_linter(parsed, "Variable and function names should be all lowercase.")
          }

          has_multiple_dots <- re_matches(name, rex(".", something, "."))
          if (has_multiple_dots) {
            res[[length(res) + 1L]] <- object_linter(parsed, "Words within variable and function names should be separated by '_' rather than '.'.")
          }

          is_very_long <- nchar(name) > width
          if (is_very_long) {
            res[[length(res) + 1L]] <- object_linter(parsed, paste0("Variable and function names should not be longer than ", width, " characters."))
          }
        }

        res

      })
  }
}
