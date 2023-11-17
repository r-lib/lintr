#' Block usage of pipes nested inside other calls
#'
#' Nesting pipes harms readability; extract sub-steps to separate variables,
#'   append further pipeline steps, or otherwise refactor such usage away.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "df1 %>% inner_join(df2 %>% select(a, b))",
#'   linters = nested_pipe_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "tryCatch(x %>% filter(grp == 'a'), error = identity)",
#'   linters = nested_pipe_linter()
#' )
#'
#' @evalRd rd_tags("nested_pipe_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @include shared_constants.R
#' @export
nested_pipe_linter <- make_linter_from_xpath(
  xpath = glue("
  (//PIPE | //SPECIAL[{ xp_text_in_table(magrittr_pipes) }])
    /parent::expr[preceding-sibling::expr[SYMBOL_FUNCTION_CALL[
      not(text() = 'try' or text() = 'tryCatch')
      and (
        text() != 'switch'
        or parent::expr
          /following-sibling::expr[1]
          /*[self::PIPE or self::SPECIAL[{ xp_text_in_table(magrittr_pipes) }]]
      )
    ]]]
  "),
  lint_message = "Don't nest pipes inside other calls."
)
