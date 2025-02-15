#' Block usage of `nrow(subset(x, .))`
#'
#' Using `nrow(subset(x, condition))` to count the instances where `condition`
#'   applies inefficiently requires doing a full subset of `x` just to
#'   count the number of rows in the resulting subset.
#' There are a number of equivalent expressions that don't require the full
#'   subset, e.g. `with(x, sum(condition))` (or, more generically,
#'   `with(x, sum(condition, na.rm = TRUE))`).
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "nrow(subset(x, is_treatment))",
#'   linters = nrow_subset_linter()
#' )
#'
#' lint(
#'   text = "nrow(filter(x, is_treatment))",
#'   linters = nrow_subset_linter()
#' )
#'
#' lint(
#'   text = "x %>% filter(x, is_treatment) %>% nrow()",
#'   linters = nrow_subset_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "with(x, sum(is_treatment, na.rm = TRUE))",
#'   linters = nrow_subset_linter()
#' )
#'
#' @evalRd rd_tags("nrow_subset_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @include shared_constants.R
#' @export
nrow_subset_linter <- make_linter_from_function_xpath(
  function_names = c("subset", "filter"),
  xpath = glue("
  parent::expr
    /parent::expr[
      expr/SYMBOL_FUNCTION_CALL[text() = 'nrow']
      or (self::expr | parent::expr)[
        (PIPE or SPECIAL[{ xp_text_in_table(setdiff(magrittr_pipes, c('%$%', '%<>%'))) }])
        and expr/expr/SYMBOL_FUNCTION_CALL[text() = 'nrow']
      ]
    ]
  "),
  lint_message = paste(
    "Use arithmetic to count the number of rows satisfying a condition,",
    "rather than fully subsetting the data.frame and counting the resulting rows.",
    "For example, replace nrow(subset(x, is_treatment)) with sum(x$is_treatment).",
    "NB: use na.rm = TRUE if `is_treatment` has missing values."
  )
)
