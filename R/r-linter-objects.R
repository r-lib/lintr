#' @describeIn linters check that objects
#' \enumerate{
#'   \item Are never camelCase
#'   \item Are separated by \code{_} rather than \code{.}
#'   \item Are not more than \code{width} characters
#' }
object_name_linter <- function(width = 20L) {
  function(source_file) {

    #devtools::load_all(find_package())
    attached_nms <-
      c(unlist(lapply(search(), ls)),
        "...")


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

    lapply(which(
        re_matches(source_file$parsed_content$token,
          rex(start, "SYMBOL"))),

      function(id) {

        parsed <- source_file$parsed_content[id, ]

        name <- parsed$text

        res <- list()
        if (!name %in% attached_nms) {
          is_camel_case <- re_matches(name, rex(lower, upper))
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
