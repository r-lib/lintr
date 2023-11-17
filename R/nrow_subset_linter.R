#' Block usage of `nrow(subset(x, .))`
#'
#' Using `nrow(subset(x, condition))` to count the instances where `condition`
#'   applies inefficiently requires doing a full subset of `x` just to
#'   count the number of rows in the resulting subset.
#' There are a number of equivalent expressions that don't require the full
#'   subset, e.g. `with(x, sum(condition))` (or, more generically,
#'   `with(x, sum(condition, na.rm = TRUE))`).
#' The same can be said of other versions of this like
#'   `nrow(DT[(condition)])` for subsetting a `data.table` or
# "   `DT %>% filter(condition) %>% nrow()`.
#'
#' @evalRd rd_tags("nrow_subset_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
nrow_subset_linter <- make_linter_from_xpath(
  xpath = "
  //SYMBOL_FUNCTION_CALL[text() = 'subset']
    /parent::expr
    /parent::expr
    /parent::expr[expr/SYMBOL_FUNCTION_CALL[text() = 'nrow']]
  ",
  lint_message = paste(
    "Use arithmetic to count the number of rows satisfying a condition,",
    "rather than fully subsetting the table and counting the resulting rows.",
    "For example, replace nrow(subset(x, is_treatment))",
    "with sum(x$is_treatment). NB: use na.rm = TRUE if `is_treatment` has",
    "missing values."
  )
)
