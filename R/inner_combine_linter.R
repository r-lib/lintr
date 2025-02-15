#' Require `c()` to be applied before relatively expensive vectorized functions
#'
#' `as.Date(c(a, b))` is logically equivalent to `c(as.Date(a), as.Date(b))`.
#'   The same equivalence holds for several other vectorized functions like
#'   [as.POSIXct()] and math functions like [sin()]. The former is to be
#'   preferred so that the most expensive part of the operation ([as.Date()])
#'   is applied only once.
#'
#' Note that [strptime()] has one idiosyncrasy to be aware of, namely that
#'   auto-detected `format=` is set by the first matching input, which means
#'   that a case like `c(as.POSIXct("2024-01-01"), as.POSIXct("2024-01-01 01:02:03"))`
#'   gives different results to `as.POSIXct(c("2024-01-01", "2024-01-01 01:02:03"))`.
#'   This false positive is rare; a workaround where possible is to use
#'   consistent formatting, i.e., `"2024-01-01 00:00:00"` in the example.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "c(log10(x), log10(y), log10(z))",
#'   linters = inner_combine_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "log10(c(x, y, z))",
#'   linters = inner_combine_linter()
#' )
#'
#' lint(
#'   text = "c(log(x, base = 10), log10(x, base = 2))",
#'   linters = inner_combine_linter()
#' )
#'
#' @evalRd rd_tags("inner_combine_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
inner_combine_linter <- function() {
  # these don't take any other arguments (except maybe by non-default
  #   methods), so don't need to check equality of other arguments
  no_arg_vectorized_funs <- c(
    "sin", "cos", "tan", "sinpi", "cospi", "tanpi", "asin", "acos", "atan",
    "log2", "log10", "log1p", "exp", "expm1",
    "sqrt", "abs"
  )

  # TODO(#2468): Try and make this XPath less brittle/more extensible.

  # See ?as.Date, ?as.POSIXct. tryFormats is not explicitly in any default
  #   POSIXct method, but it is in as.Date.character and as.POSIXlt.character --
  #   the latter is what actually gets invoked when running as.POSIXct
  #   on a character. So it is indeed an argument by pass-through.
  date_args <- c("format", "origin", "tz", "tryFormats")
  date_funs <- c("as.Date", "as.POSIXct", "as.POSIXlt")

  # See ?log. Only these two take a 'base' argument.
  log_funs <- c("log", "logb")
  log_args <- "base"

  # See ?lubridate::ymd and ?lubridate::ymd_hms
  lubridate_args <- c("quiet", "tz", "locale", "truncated")
  lubridate_funs <- c(
    "ymd", "ydm", "mdy", "myd", "dmy", "dym",
    "yq", "ym", "my",
    "ymd_hms", "ymd_hm", "ymd_h", "dmy_hms", "dmy_hm", "dmy_h",
    "mdy_hms", "mdy_hm", "mdy_h", "ydm_hms", "ydm_hm", "ydm_h"
  )

  date_args_cond <- build_arg_condition(date_funs, date_args)
  log_args_cond <- build_arg_condition(log_funs, log_args)
  lubridate_args_cond <- build_arg_condition(lubridate_funs, lubridate_args)

  c_expr_cond <- xp_and(
    sprintf(
      "expr[1][SYMBOL_FUNCTION_CALL[%s]]",
      xp_text_in_table(c(no_arg_vectorized_funs, date_funs, log_funs, lubridate_funs))
    ),
    "not(following-sibling::expr[not(expr[1][SYMBOL_FUNCTION_CALL])])",
    "not(expr[1]/SYMBOL_FUNCTION_CALL != following-sibling::expr/expr[1]/SYMBOL_FUNCTION_CALL)",
    date_args_cond,
    log_args_cond,
    lubridate_args_cond
  )
  xpath <- glue("
  self::*[count(following-sibling::expr) > 1]
    /following-sibling::expr[1][ {c_expr_cond} ]
    /parent::expr
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls("c")
    bad_expr <- xml_find_all(xml_calls, xpath)

    matched_call <- xp_call_name(bad_expr, depth = 2L)
    lint_message <- paste(
      "Combine inputs to vectorized functions first to take full advantage of vectorization, e.g.,",
      sprintf(
        "%1$s(c(x, y)) only runs the more expensive %1$s() once as compared to c(%1$s(x), %1$s(y)).",
        matched_call
      )
    )
    xml_nodes_to_lints(bad_expr, source_expression = source_expression, lint_message, type = "warning")
  })
}

#' Make the XPath condition ensuring an argument matches across calls
#'
#' @param arg Character scalar naming an argument
#' @noRd
arg_match_condition <- function(arg) {
  this_symbol <- sprintf("SYMBOL_SUB[text() = '%s']", arg)
  following_symbol <- sprintf("following-sibling::expr/%s", this_symbol)
  next_expr <- "following-sibling::expr[1]"
  xp_or(
    sprintf("not(%s) and not(%s)", this_symbol, following_symbol),
    xp_and(
      this_symbol,
      following_symbol,
      sprintf(
        "not(%1$s/%3$s != %2$s/%3$s)",
        this_symbol, following_symbol, next_expr
      )
    )
  )
}

build_arg_condition <- function(calls, arguments) {
  xp_or(
    sprintf("not(expr[1][SYMBOL_FUNCTION_CALL[%s]])", xp_text_in_table(calls)),
    "not(EQ_SUB) and not(following-sibling::expr/EQ_SUB)",
    xp_and(vapply(arguments, arg_match_condition, character(1L)))
  )
}
