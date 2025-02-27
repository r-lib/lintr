#' Pipe consistency linter
#'
#' Check that the recommended pipe operator is used, or more conservatively that
#'   pipes are consistent by file.
#'
#' @param pipe Which pipe operator is valid (either `"%>%"` or `"|>"`). The default
#'   is the native pipe (`|>`). `"auto"` will instead
#'   only enforce consistency, i.e., that in any given file there is only one pipe.
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
#'   text = "1:3 |> mean() |> as.character()",
#'   linters = pipe_consistency_linter()
#' )
#'
#' lint(
#'   text = "1:3 %>% mean() %>% as.character()",
#'   linters = pipe_consistency_linter("%>%")
#' )
#' @evalRd rd_tags("pipe_consistency_linter")
#' @seealso
#'  - [linters] for a complete list of linters available in lintr.
#'  - <https://style.tidyverse.org/pipes.html#magrittr>
#' @export
pipe_consistency_linter <- function(pipe = c("|>", "auto", "%>%")) {
  pipe <- match.arg(pipe)

  xpath_magrittr <- glue("//SPECIAL[{ xp_text_in_table(magrittr_pipes) }]")
  xpath_native <- "//PIPE"

  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content

    match_magrittr <- xml_find_all(xml, xpath_magrittr)
    match_native <- xml_find_all(xml, xpath_native)

    n_magrittr <- length(match_magrittr)
    n_native <- length(match_native)

    if (pipe == "auto" && n_magrittr > 0L && n_native > 0L) {
      xml_nodes_to_lints(
        xml = c(match_magrittr, match_native),
        source_expression = source_expression,
        lint_message = paste(
          "Stick to one pipe operator; found", n_magrittr, "instances of %>% and", n_native, "instances of |>."
        ),
        type = "style"
      )
    } else if (pipe == "%>%" && n_native > 0L) {
      xml_nodes_to_lints(
        xml = match_native,
        source_expression = source_expression,
        lint_message = "Use the %>% pipe operator instead of the |> pipe operator.",
        type = "style"
      )
    } else if (pipe == "|>" && n_magrittr > 0L) {
      xml_nodes_to_lints(
        xml = match_magrittr,
        source_expression = source_expression,
        lint_message = "Use the |> pipe operator instead of the %>% pipe operator.",
        type = "style"
      )
    } else {
      list()
    }
  })
}
