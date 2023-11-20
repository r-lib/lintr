#' Require an explicit call to else in a terminal if statement
#'
#' When an `if` statement in R is missing `else`, the alternative is implicitly
#'   set to `NULL`. When the `if` statement comes at the end of a function
#'   definition, then, there is an implicit return of `NULL`.
#'
#' @evalRd rd_tags("implicit_else_return_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @include shared_constants.R
#' @export
implicit_else_return_linter <- local({
  exempt_functions <- xp_text_in_table(return_not_needed_funs)
  fun_expr_cond <- glue(
    "not(preceding-sibling::expr/SYMBOL[{ exempt_functions }])"
  )

  # for inline functions, terminal <expr> is a sibling of <FUNCTION>, otherwise
  #   it's a descendant of the <expr> following <FUNCTION>
  xpath <- glue("
  //FUNCTION[parent::expr[{fun_expr_cond}]]
    /following-sibling::expr[
      (position() = last() and IF and not(ELSE))
      or expr[position() = last() and IF and not(ELSE)]
    ]
  ")

  make_linter_from_xpath(
    xpath = xpath,
    lint_message = paste(
      "All functions with terminal if statements must have a corresponding terminal else clause,",
      "or else a new explicit return() after the if statement."
    )
  )
})
