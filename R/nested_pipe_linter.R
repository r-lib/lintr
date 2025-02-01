#' Block usage of pipes nested inside other calls
#'
#' Nesting pipes harms readability; extract sub-steps to separate variables,
#'   append further pipeline steps, or otherwise refactor such usage away.
#'
#' @param allow_inline Logical, default `TRUE`, in which case only "inner"
#'   pipelines which span more than one line are linted. If `FALSE`, even
#'   "inner" pipelines that fit in one line are linted.
#' @param allow_outer_calls Character vector dictating which "outer"
#'   calls to exempt from the requirement to unnest (see examples). Defaults
#'   to [try()], [tryCatch()], and [withCallingHandlers()].
#'
#' @examples
#' # will produce lints
#' code <- "df1 %>%\n  inner_join(df2 %>%\n    select(a, b)\n  )"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = nested_pipe_linter()
#' )
#'
#' lint(
#'   text = "df1 %>% inner_join(df2 %>% select(a, b))",
#'   linters = nested_pipe_linter(allow_inline = FALSE)
#' )
#'
#' lint(
#'   text = "tryCatch(x %>% filter(grp == 'a'), error = identity)",
#'   linters = nested_pipe_linter(allow_outer_calls = character())
#' )
#'
#' # okay
#' lint(
#'   text = "df1 %>% inner_join(df2 %>% select(a, b))",
#'   linters = nested_pipe_linter()
#' )
#'
#' code <- "df1 %>%\n  inner_join(df2 %>%\n    select(a, b)\n  )"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = nested_pipe_linter(allow_outer_calls = "inner_join")
#' )
#'
#' lint(
#'   text = "tryCatch(x %>% filter(grp == 'a'), error = identity)",
#'   linters = nested_pipe_linter()
#' )
#'
#' @evalRd rd_tags("nested_pipe_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
nested_pipe_linter <- function(
  allow_inline = TRUE,
  allow_outer_calls = c("try", "tryCatch", "withCallingHandlers")
) {
  multiline_and <- if (allow_inline) "@line1 != @line2 and" else ""
  xpath <- glue("
  (//PIPE | //SPECIAL[{ xp_text_in_table(magrittr_pipes) }])
    /parent::expr[{multiline_and} preceding-sibling::expr/SYMBOL_FUNCTION_CALL[
      not({ xp_text_in_table(allow_outer_calls) })
      and (
        text() != 'switch'
        or parent::expr
          /following-sibling::expr[1]
          /*[self::PIPE or self::SPECIAL[{ xp_text_in_table(magrittr_pipes) }]]
      )
    ]]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "Don't nest pipes inside other calls.",
      type = "warning"
    )
  })
}
