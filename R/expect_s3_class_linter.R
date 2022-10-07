#' Require usage of `expect_s3_class()`
#'
#' [testthat::expect_s3_class()] exists specifically for testing the class
#'   of S3 objects. [testthat::expect_equal()], [testthat::expect_identical()],
#'   and [testthat::expect_true()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "expect_equal(class(x), 'data.frame')",
#'   linters = expect_s3_class_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "expect_s3_class(x, 'data.frame')",
#'   linters = expect_s3_class_linter()
#' )
#'
#' @evalRd rd_tags("expect_s3_class_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_s3_class_linter <- function() {
  # (1) expect_{equal,identical}(class(x), C)
  # (2) expect_true(is.<class>(x)) and expect_true(inherits(x, C))
  expect_equal_identical_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical']
    /parent::expr
    /following-sibling::expr[
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'class']]
      and (position() = 1 or preceding-sibling::expr[STR_CONST])
    ]
    /parent::expr[not(SYMBOL_SUB[text() = 'info' or text() = 'label' or text() = 'expected.label'])]
  "
  is_class_call <- xp_text_in_table(c(is_s3_class_calls, "inherits"))
  expect_true_xpath <- glue::glue("
  //SYMBOL_FUNCTION_CALL[text() = 'expect_true']
    /parent::expr
    /following-sibling::expr[1][expr[1][SYMBOL_FUNCTION_CALL[ {is_class_call} ]]]
    /parent::expr[not(SYMBOL_SUB[text() = 'info' or text() = 'label'])]
  ")
  xpath <- paste(expect_equal_identical_xpath, "|", expect_true_xpath)

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)
    matched_function <- xp_call_name(bad_expr)
    msg <- ifelse(
      matched_function %in% c("expect_equal", "expect_identical"),
      sprintf("expect_s3_class(x, k) is better than %s(class(x), k).", matched_function),
      "expect_s3_class(x, k) is better than expect_true(is.<k>(x)) or expect_true(inherits(x, k))."
    )
    xml_nodes_to_lints(
      bad_expr,
      source_expression,
      lint_message = paste(msg, "Note also expect_s4_class() available for testing S4 objects."),
      type = "warning"
    )
  })
}

# NB: there is no easy way to make an exhaustive list of places where an
#   is.<x> call can be replaced by expect_s3_class(); this list was manually
#   populated from the default R packages by inspection. For example,
#   is.matrix(x) cannot be replaced by expect_s3_class(x, "matrix") because
#   it is not actually an S3 class (is.object(x) is not TRUE).
#   Further, there are functions named is.<x> that have nothing to do with
#   object type, e.g. is.finite(), is.nan(), or is.R().
is_s3_class_calls <- paste0("is.", c(
  # base
  "data.frame", "factor", "numeric_version", "ordered", "package_version", "qr", "table",
  #      utils grDevices     tcltk    tcltk    grid    grid
  "relistable", "raster", "tclObj", "tkwin", "grob", "unit",
  # stats
  "mts", "stepfun", "ts", "tskernel"
))

#' Require usage of `expect_s4_class(x, k)` over `expect_true(is(x, k))`
#'
#' [testthat::expect_s4_class()] exists specifically for testing the class
#'   of S4 objects. [testthat::expect_true()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "expect_true(is(x, 'data.frame'))",
#'   linters = expect_s4_class_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "expect_s4_class(x, 'data.frame')",
#'   linters = expect_s4_class_linter()
#' )
#'
#' @evalRd rd_tags("expect_s4_class_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_s4_class_linter <- function() {
  # require 2 expressions because methods::is(x) alone is a valid call, even
  #   though the character output wouldn't make any sense for expect_true().
  xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'expect_true']
    /parent::expr
    /following-sibling::expr[1][count(expr) = 3 and expr[1][SYMBOL_FUNCTION_CALL[text() = 'is']]]
    /parent::expr[not(SYMBOL_SUB[text() = 'info' or text() = 'label'])]
  "

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    # TODO(michaelchirico): also catch expect_{equal,identical}(methods::is(x), k).
    #   this seems empirically rare, but didn't check many S4-heavy packages.

    bad_expr <- xml2::xml_find_all(xml, xpath)
    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste(
        "expect_s4_class(x, k) is better than expect_true(is(x, k)).",
        "Note also expect_s3_class() available for testing S3 objects."
      ),
      type = "warning"
    )
  })
}
