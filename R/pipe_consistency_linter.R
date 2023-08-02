#' Pipe consistency linter
#'
#' Check that pipe operators are used consistently by file, or optionally
#' specify one valid pipe operator.
#'
#' @param pipe Which pipe operator is valid (either `"%>%"` or `"|>"`). By default
#' (`"auto"`), the linter has no preference but will check that each file uses
#' only one type of pipe operator.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "1:3 |> mean() %>% as.character()",
#'   linters = pipe_consistency_linter()
#' )
#'
#' lint(
#'   text = "1:3 %>% mean() %>% as.character()",
#'   linters = pipe_consistency_linter("|>")
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
pipe_consistency_linter <- function(pipe = c("auto", "%>%", "|>")) {
  pipe <- match.arg(pipe)

  xpath_magrittr <- "//SPECIAL[text() = '%>%']"
  xpath_native <- "//PIPE"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    match_magrittr <- xml2::xml_find_all(xml, xpath_magrittr)
    match_native <- xml2::xml_find_all(xml, xpath_native)

    n_magrittr <- length(match_magrittr)
    n_native <- length(match_native)

    if (pipe == "auto" && n_magrittr > 0L && n_native > 0L) {
      xml_nodes_to_lints(
        xml = c(match_magrittr, match_native),
        source_expression = source_expression,
        lint_message = sprintf(
          "Found %d instances of %%>%% and %d instances of |>. Stick to one pipe operator.",
          n_magrittr,
          n_native
        ),
        type = "style"
      )
    } else if (pipe == "%>%" && n_native > 0L) {
      xml_nodes_to_lints(
        xml = match_native,
        source_expression = source_expression,
        lint_message = "Use magrittr pipe operator (%>%) instead of native pipe operator (|>).",
        type = "style"
      )
    } else if (pipe == "|>" && n_magrittr > 0L) {
      xml_nodes_to_lints(
        xml = match_magrittr,
        source_expression = source_expression,
        lint_message = "Use native pipe operator (|>) instead of magrittr pipe operator (%>%).",
        type = "style"
      )
    } else {
      list()
    }
  })
}
