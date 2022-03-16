#' Require usage of expect_s3_class()
#'
#' [testthat::expect_s3_class()] exists specifically for testing the class
#'   of S3 objects. [testthat::expect_equal()], [testthat::expect_identical()],
#'   and [testthat::expect_true()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @evalRd rd_tags("expect_s3_class_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_s3_class_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    # (1) expect_{equal,identical}(class(x), C)
    # (2) expect_true(is.<class>(x)) and expect_true(inherits(x, C))
    is_class_call <- xp_text_in_table(c(is_s3_class_calls, "inherits")) # nolint: object_usage_linter. TODO(#942): fix this.
    xpath <- glue::glue("//expr[
      (
        SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical']
        and following-sibling::expr[
          expr[SYMBOL_FUNCTION_CALL[text() = 'class']]
          and (position() = 1 or preceding-sibling::expr[STR_CONST])
        ]
      ) or (
        SYMBOL_FUNCTION_CALL[text() = 'expect_true']
        and following-sibling::expr[1][expr[SYMBOL_FUNCTION_CALL[ {is_class_call} ]]]
      )
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)
    return(lapply(bad_expr, gen_expect_s3_class_lint, source_file))
  })
}

# NB: there is no easy way to make an exhaustive list of places where an
#   is.<x> call can be replaced by expect_s3_class(); this list was manually
#   populated from the default R packages by inspection. For example,
#   is.matrix(x) cannot be replaced by expect_s3_class(x, "matrix") because
#   it is not actually an S3 class (is.object(x) is not TRUE since there
#   is no class set for a matrix [I am not sure if this changes in R 4].
#   Further, there are functions named is.<x> that have nothing to do with
#   object type, e.g. is.finite(), is.nan(), or is.R().
is_s3_class_calls <- paste0("is.", c(
  # base
  "data.frame", "factor", "numeric_version",
  "ordered", "package_version", "qr", "table",
  #      utils grDevices     tcltk    tcltk    grid    grid
  "relistable", "raster", "tclObj", "tkwin", "grob", "unit",
  # stats
  "mts", "stepfun", "ts", "tskernel"
))

gen_expect_s3_class_lint <- function(expr, source_file) {
  matched_function <- xml2::xml_text(xml2::xml_find_first(expr, "SYMBOL_FUNCTION_CALL"))
  if (matched_function %in% c("expect_equal", "expect_identical")) {
    lint_msg <- sprintf("expect_s3_class(x, k) is better than %s(class(x), k).", matched_function)
  } else {
    lint_msg <- "expect_s3_class(x, k) is better than expect_true(is.<k>(x)) or expect_true(inherits(x, k))."
  }
  lint_msg <- paste(lint_msg, "Note also expect_s4_class() available for testing S4 objects.")
  xml_nodes_to_lint(expr, source_file, lint_msg, type = "warning")
}

#' Require usage of expect_s4_class(x, k) over expect_true(is(x, k))
#'
#' [testthat::expect_s4_class()] exists specifically for testing the class
#'   of S4 objects. [testthat::expect_true()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @evalRd rd_tags("expect_s3_class_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_s4_class_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    # TODO(michaelchirico): also catch expect_{equal,identical}(methods::is(x), k).
    #   there are no hits for this on google3 as of now.

    # require 2 expressions because methods::is(x) alone is a valid call, even
    #   though the character output wouldn't make any sense for expect_true().
    xpath <- "//expr[
      SYMBOL_FUNCTION_CALL[text() = 'expect_true']
      and following-sibling::expr[1][count(expr) = 3 and expr[SYMBOL_FUNCTION_CALL[text() = 'is']]]
    ]"

    bad_expr <- xml2::xml_find_all(xml, xpath)
    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      message = paste(
        "expect_s4_class(x, k) is better than expect_true(is(x, k)).",
        "Note also expect_s3_class() available for testing S3 objects."
      ),
      type = "warning"
    ))
  })
}
