#' Pipe consistency linter
#'
#' Check that all pipes are consistent (either all %>% or all |>).
#'
#' Checks consistency across an entire file, not just a single
#' expression.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "1:3 |> mean() %>% as.character()",
#'   linters = pipe_consistency_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "1:3 %>% mean() %>% as.character()",
#'   linters = pipe_consistency_linter()
#' )
#'
#' lint(
#'  text = "1:3 |> mean() |> as.character()",
#'  linters = pipe_consistency_linter()
#' )
#' @evalRd rd_tags("pipe_consistency_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
pipe_consistency_linter <- function() {
  xpath_magrittir <- "//SPECIAL[text() = '%>%']"
  xpath_native <- "//PIPE"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    match_magrittr <- xml2::xml_find_all(xml, xpath_magrittir)
    match_native <- xml2::xml_find_all(xml, xpath_native)

    n_magrittr <- length(match_magrittr)
    n_native <- length(match_native)

    if (n_magrittr == 0L || n_native == 0L) {
      return(list())
    }

    xml_nodes_to_lints(
      xml = c(match_magrittr, match_native),
      source_expression = source_expression,
      lint_message = "Use consistent pipe operators (either all %>% or all |>).",
      type = "style"
    )
  })
}
