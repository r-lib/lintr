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
  vectorized_funs <- c(
    "sin", "cos", "tan", "sinpi", "cospi", "tanpi", "asin", "acos", "atan",
    "log", "logb", "log2", "log10", "log1p", "exp", "expm1",
    "sqrt", "abs",
    "as.Date", "as.POSIXct", "as.POSIXlt",
    "ymd", "ydm", "mdy", "myd", "dmy", "dym",
    "yq", "ym", "my",
    "ymd_hms", "ymd_hm", "ymd_h", "dmy_hms", "dmy_hm", "dmy_h",
    "mdy_hms", "mdy_hm", "mdy_h", "ydm_hms", "ydm_hm", "ydm_h"
  )

  c_expr_cond <- xp_and(
    "following-sibling::expr",
    sprintf("expr[1][SYMBOL_FUNCTION_CALL[%s]]", xp_text_in_table(vectorized_funs)),
    "not(following-sibling::expr[not(expr[1][SYMBOL_FUNCTION_CALL])])",
    "not(expr[1]/SYMBOL_FUNCTION_CALL != following-sibling::expr/expr[1]/SYMBOL_FUNCTION_CALL)"
  )
  xpath <- glue("following-sibling::expr[1][ {c_expr_cond} ]/parent::expr")

  extract_extra_args <- function(call_node) {
    call_args <- xml_find_all_(call_node, "expr[position() > 1]")
    arg_names <- get_r_string(call_args, "preceding-sibling::*[1][self::EQ_SUB]/preceding-sibling::*[1]")
    x_idx <- match("x", arg_names, nomatch = match(NA_character_, arg_names, nomatch = 0L))
    keep <- seq_along(call_args) != x_idx
    arg_names <- arg_names[keep]
    arg_values <- xml_text(call_args[keep])
    ord <- order(arg_names)
    c(arg_names[ord], arg_values[ord])
  }

  has_matching_args <- function(c_expr) {
    inner_calls <- xml_find_all_(c_expr, "expr[position() > 1]")
    first_args <- extract_extra_args(inner_calls[[1L]])
    for (call in inner_calls[-1L]) {
      if (!identical(extract_extra_args(call), first_args)) {
        return(FALSE)
      }
    }
    TRUE
  }

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls("c")
    bad_expr <- xml_calls |>
      xml_find_all_(xpath) |>
      strip_comments_from_subtree()
    bad_expr <- bad_expr[vapply(bad_expr, has_matching_args, logical(1L))]

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
