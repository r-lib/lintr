# Helpers for lintr tests

single_quote <- function(x) paste0("'", x, "'")
double_quote <- function(x) paste0('"', x, '"')

#' Trim some leading whitespace
#'
#' Trim `num` characters from the start of every line in `x`, or auto-detect to remove the maximum number whitespace
#' from all lines while preserving relative indentation
#'
#' @param x a string containing newlines
#' @param num number of characters to strip from the start of each line.
#' `NULL` will auto-detect this based on the minimum number of leading spaces greater than one.
#'
#' @return A modified version of `x` with `num` characters removed from the start of every line and with a possible
#' leading and trailing blank line removed.
#'
#' @examples
#' my_var <- local({
#'   out <- trim_some("
#'     This will be the content
#'     of the file where
#'     only the following line
#'       is indented by two spaces.
#'   ")
#' })
#'
#' stopifnot(identical(
#'   my_var,
#'   "This will be the content\nof the file where\nonly the following line\n  is indented by two spaces."
#' ))
trim_some <- function(x, num = NULL) {
  x <- rex::re_substitutes(
    x,
    rex::rex(list(start, any_blanks, newline) %or% list(newline, any_blanks, end)),
    replacement = "",
    global = TRUE
  )

  if (is.null(num)) {
    ms <- rex::re_matches(x, "^\\s+", locations = TRUE, global = TRUE, options = "multi-line")[[1L]]
    num <- min(ms$end - ms$start) + 1L
  }

  rex::re_substitutes(x, rex::rex(start, n_times(any, num)), "", global = TRUE, options = "multi-line")
}

expect_environments_equal <- function(actual, expected) {
  act <- quasi_label(rlang::enquo(actual), arg = "actual")
  exp <- quasi_label(rlang::enquo(expected), arg = "expected")

  act$objs <- ls(act$val, all.names = TRUE)
  exp$objs <- ls(exp$val, all.names = TRUE)

  act_not_exp <- setdiff(act$objs, exp$objs)
  if (length(act_not_exp) > 6L) act_not_exp <- c(utils::head(act_not_exp, 6L), "...")
  expect(
    length(act_not_exp) == 0L,
    sprintf("Objects found in %s but not %s: %s", act$lab, exp$lab, toString(act_not_exp))
  )

  exp_not_act <- setdiff(act$objs, exp$objs)
  if (length(exp_not_act) > 6L) exp_not_act <- c(utils::head(exp_not_act, 6L), "...")
  expect(
    length(exp_not_act) == 0L,
    sprintf("Objects found in %s but not %s: %s", exp$lab, act$lab, toString(exp_not_act))
  )

  for (obj in intersect(act$objs, exp$objs))
    expect_identical(
      act$val[[obj]], exp$val[[obj]],
      label = sprintf("%s$%s", act$lab, obj),
      expected.label = sprintf("%s$%s", exp$lab, obj)
    )

  invisible(act$val)
}
