#' Force `&&` conditions to be written separately where appropriate
#'
#' For readability of test outputs, testing only one thing per call to
#'   [testthat::expect_true()] is preferable, i.e.,
#'   `expect_true(A); expect_true(B)` is better than `expect_true(A && B)`, and
#'   `expect_false(A); expect_false(B)` is better than `expect_false(A || B)`.
#'
#' Similar reasoning applies to `&&` usage inside [base::stopifnot()] and `assertthat::assert_that()` calls.
#'
#' Relatedly, `dplyr::filter(DF, A & B)` is the same as `dplyr::filter(DF, A, B)`, but the latter will be more readable
#'   / easier to format for long conditions. Note that this linter assumes usages of `filter()` are `dplyr::filter()`;
#'   if you're using another function named `filter()`, e.g. [stats::filter()], please namespace-qualify it to avoid
#'   false positives. You can omit linting `filter()` expressions altogether via `allow_filter = TRUE`.
#'
#' @param allow_named_stopifnot Logical, `TRUE` by default. If `FALSE`, "named" calls to `stopifnot()`,
#'   available since R 4.0.0 to provide helpful messages for test failures, are also linted.
#' @param allow_filter Character naming the method for linting calls to `filter()`. The default, `"never"`, means
#'   `filter()` and `dplyr::filter()` calls are linted; `"not_dplyr"` means only `dplyr::filter()` calls are linted;
#'   and `"always"` means no calls to `filter()` are linted. Calls like `stats::filter()` are never linted.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "expect_true(x && y)",
#'   linters = conjunct_test_linter()
#' )
#'
#' lint(
#'   text = "expect_false(x || (y && z))",
#'   linters = conjunct_test_linter()
#' )
#'
#' lint(
#'   text = "stopifnot('x must be a logical scalar' = length(x) == 1 && is.logical(x) && !is.na(x))",
#'   linters = conjunct_test_linter(allow_named_stopifnot = FALSE)
#' )
#'
#' lint(
#'   text = "dplyr::filter(mtcars, mpg > 20 & vs == 0)",
#'   linters = conjunct_test_linter()
#' )
#'
#' lint(
#'   text = "filter(mtcars, mpg > 20 & vs == 0)",
#'   linters = conjunct_test_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "expect_true(x || (y && z))",
#'   linters = conjunct_test_linter()
#' )
#'
#' lint(
#'   text = 'stopifnot("x must be a logical scalar" = length(x) == 1 && is.logical(x) && !is.na(x))',
#'   linters = conjunct_test_linter(allow_named_stopifnot = TRUE)
#' )
#'
#' lint(
#'   text = "dplyr::filter(mtcars, mpg > 20 & vs == 0)",
#'   linters = conjunct_test_linter(allow_filter = "always")
#' )
#'
#' lint(
#'   text = "filter(mtcars, mpg > 20 & vs == 0)",
#'   linters = conjunct_test_linter(allow_filter = "not_dplyr")
#' )
#'
#' lint(
#'   text = "stats::filter(mtcars$cyl, mtcars$mpg > 20 & mtcars$vs == 0)",
#'   linters = conjunct_test_linter()
#' )
#'
#' @evalRd rd_tags("conjunct_test_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
conjunct_test_linter <- function(allow_named_stopifnot = TRUE,
                                 allow_filter = c("never", "not_dplyr", "always")) {
  allow_filter <- match.arg(allow_filter)

  expect_true_assert_that_xpath <- "
  following-sibling::expr[1][AND2]
    /parent::expr
  "
  named_stopifnot_condition <-
    if (allow_named_stopifnot) "and not(preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB])" else ""
  stopifnot_xpath <- glue("
  following-sibling::expr[1][AND2 {named_stopifnot_condition}]
    /parent::expr
  ")
  expect_false_xpath <- "
  following-sibling::expr[1][OR2]
    /parent::expr
  "

  filter_ns_cond <- switch(allow_filter,
    never = "not(SYMBOL_PACKAGE[text() != 'dplyr'])",
    not_dplyr = "SYMBOL_PACKAGE[text() = 'dplyr']",
    always = "true"
  )
  filter_xpath <- glue("
  self::*[{ filter_ns_cond }]
    /parent::expr
    /expr[AND]
  ")

  Linter(linter_level = "file", function(source_expression) {
    # need the full file to also catch usages at the top level
    expect_true_assert_that_calls <- source_expression$xml_find_function_calls(c("expect_true", "assert_that"))
    stopifnot_calls <- source_expression$xml_find_function_calls("stopifnot")
    expect_false_calls <- source_expression$xml_find_function_calls("expect_false")
    test_expr <- combine_nodesets(
      xml_find_all(expect_true_assert_that_calls, expect_true_assert_that_xpath),
      xml_find_all(stopifnot_calls, stopifnot_xpath),
      xml_find_all(expect_false_calls, expect_false_xpath)
    )

    matched_fun <- xp_call_name(test_expr)
    operator <- xml_find_chr(test_expr, "string(expr/*[self::AND2 or self::OR2])")
    replacement_fmt <- ifelse(
      matched_fun %in% c("expect_true", "expect_false"),
      "Write multiple expectations like %1$s(A) and %1$s(B)",
      "Write multiple conditions like %s(A, B)"
    )
    lint_message <- paste(
      # as.character() needed for 0-lint case where ifelse(logical(0)) returns logical(0)
      sprintf(as.character(replacement_fmt), matched_fun),
      sprintf("instead of %s(A %s B).", matched_fun, operator),
      "The latter will produce better error messages in the case of failure."
    )
    lints <- xml_nodes_to_lints(
      test_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )

    if (allow_filter != "always") {
      xml_calls <- source_expression$xml_find_function_calls("filter")
      filter_expr <- xml_find_all(xml_calls, filter_xpath)

      filter_lints <- xml_nodes_to_lints(
        filter_expr,
        source_expression = source_expression,
        lint_message = "Use dplyr::filter(DF, A, B) instead of dplyr::filter(DF, A & B).",
        type = "warning"
      )

      lints <- c(lints, filter_lints)
    }

    lints
  })
}
