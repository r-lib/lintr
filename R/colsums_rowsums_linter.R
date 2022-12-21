#' Require usage of `colSums(x)` or `rowSums(x)` over `apply(x, ., sum)`
#'
#' [colSums()] and [rowSums()] are clearer and more performant alternatives to
#' `apply(x, 2, sum)` and `apply(x, 1, sum)` respectively in the case of 2D
#' arrays, or matrices
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "apply(x, 1, sum)",
#'   linters = colsums_rowsums_linter()
#' )
#'
#' lint(
#'   text = "apply(x, 2, sum)",
#'   linters = colsums_rowsums_linter()
#' )
#'
#' lint(
#'   text = "apply(x, 2, sum, na.rm = TRUE)",
#'   linters = colsums_rowsums_linter()
#' )
#'
#' @evalRd rd_tags("expect_named_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
colsums_rowsums_linter <- function() {

  # mean() and sum() have very different signatures so we treat them separately.
  # sum() takes values to sum over via ..., has just one extra argument and is not a generic
  # mean() is a generic, takes values to average via a single argument, and can have extra arguments
  sums_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'apply']
    /parent::expr
    /following-sibling::expr[
      SYMBOL[text() = 'sum']
      and (position() = 3)
    ]
    /parent::expr
  "

  # Since mean() is a generic, we make sure that we only lint cases with arguments
  # supported by colMeans() and rowMeans(), i.e., na.rm
  means_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'apply']
    /parent::expr
    /following-sibling::expr[
      SYMBOL[text() = 'mean']
      and (position() = 3)
    ]
    /parent::expr[
      count(expr) = 4
      or (count(expr) = 5 and SYMBOL_SUB[text() = 'na.rm'])
    ]
  "

  xpath <- glue::glue("{sums_xpath} | {means_xpath}")

  # This doesn't handle the case when MARGIN and FUN are named and in a different position
  # but this should be relatively rate
  var_xpath  <- "expr[position() = 2]"
  margin_xpath <- "expr[position() = 3]"
  fun_xpath <- "expr[position() = 4]"

  craft_msg <- function(var, margin, fun) {
    if (is.na(xml2::xml_find_first(margin, "OP-COLON"))) {
      l1 <- xml2::xml_double(margin)
      l2 <- NULL
    } else {
      l1 <- xml2::xml_double(xml2::xml_find_first(margin, "expr[1]"))
      l2 <- xml2::xml_double(xml2::xml_find_first(margin, "expr[2]"))
    }

    if (l1 == 1) {
      if (is.null(l2)) {
        reco <- glue::glue("row{fun}s({var})")
      } else {
        reco <- glue::glue("row{fun}s({var}, dims = {l2})")
      }
    } else {
      if (is.null(l2)) {
        l2 <- l1
      }
      reco <- glue::glue("row{fun}s(col{fun}s({var}, dims = {l1 - 1}), dims = {l2 - l1 + 1})")
    }
  }

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    var <- xml2::xml_text(xml2::xml_find_all(bad_expr, var_xpath))

    fun <- xml2::xml_text(xml2::xml_find_all(bad_expr, fun_xpath))
    fun <- tools::toTitleCase(fun)

    margin <- xml2::xml_find_all(bad_expr, margin_xpath)

    recos <- lapply(seq_along(bad_expr), function(i) {
      craft_msg(var[i], margin[i], fun[i])
    })

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = sprintf("Use %1$s rather than %2$s", recos, get_r_string(bad_expr)),
      type = "warning"
    )


  })
}
