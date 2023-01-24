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
#'   linters = matrix_apply_linter()
#' )
#'
#' lint(
#'   text = "apply(x, 2, sum)",
#'   linters = matrix_apply_linter()
#' )
#'
#' lint(
#'   text = "apply(x, 2, sum, na.rm = TRUE)",
#'   linters = matrix_apply_linter()
#' )
#'
#' lint(
#'   text = "apply(x, 2:4, sum)",
#'   linters = matrix_apply_linter()
#' )
#'
#' @evalRd rd_tags("matrix_apply_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
matrix_apply_linter <- function() {

  # mean() and sum() have very different signatures so we treat them separately.
  # sum() takes values to sum over via ..., has just one extra argument and is not a generic
  # mean() is a generic, takes values to average via a single argument, and can have extra arguments
  #
  # Currently supported values for MARGIN: scalar numeric and vector of contiguous values created by : (OP-COLON)
  sums_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'apply']
    /parent::expr
    /following-sibling::expr[
      NUM_CONST or OP-COLON/preceding-sibling::expr[NUM_CONST]/following-sibling::expr[NUM_CONST]
      and (position() = 2)
    ]
    /following-sibling::expr[
      SYMBOL[text() = 'sum']
      and (position() = 1)
    ]
    /parent::expr
  "

  # Since mean() is a generic, we make sure that we only lint cases with arguments
  # supported by colMeans() and rowMeans(), i.e., na.rm
  means_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'apply']
    /parent::expr
    /following-sibling::expr[
      NUM_CONST or OP-COLON/preceding-sibling::expr[NUM_CONST]/following-sibling::expr[NUM_CONST]
      and (position() = 2)
    ]
    /following-sibling::expr[
      SYMBOL[text() = 'mean']
      and (position() = 1)
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

    narm_val <- xml2::xml_text(
      xml2::xml_find_first(bad_expr, "SYMBOL_SUB[text() = 'na.rm']/following-sibling::expr")
    )

    recos <- Map(craft_colsums_rowsums_msg, var, margin, fun, narm_val)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = sprintf("Use %1$s rather than %2$s", recos, get_r_string(bad_expr)),
      type = "warning"
    )
  })
}

craft_colsums_rowsums_msg <- function(var, margin, fun, narm_val) {

  if (is.na(xml2::xml_find_first(margin, "OP-COLON"))) {
    l1 <- xml2::xml_text(margin)
    l2 <- NULL
  } else {
    l1 <- xml2::xml_text(xml2::xml_find_first(margin, "expr[1]"))
    l2 <- xml2::xml_text(xml2::xml_find_first(margin, "expr[2]"))
  }

  # See #1764 for details about various cases. In short:
  # - If apply(., 1:l2, sum) -> rowSums(., dims = l2)
  # - If apply(., l1:l2, sum) -> rowSums(colSums(., dims = l1 - 1), dims = l2 - l1 + 1)
  # - This last case can be simplified to a simple colSums() call if l2 = length(dim(.))
  # - dims argument can be dropped if equals to 1. This notably is the case for matrices
  if (is.null(l2)) {
    l2 <- l1
  }

  # We don't want warnings when converted as NAs
  l1 <- suppressWarnings(as.integer(re_substitutes(l1, "L$", "")))
  l2 <- suppressWarnings(as.integer(re_substitutes(l2, "L$", "")))

  if (!is.na(narm_val)) {
    narm <- glue::glue(", na.rm = {narm_val}")
  } else {
    narm <- ""
  }

  if (identical(l1, 1L)) {
    reco <- glue::glue("row{fun}s({var}{narm}, dims = {l2})")
  } else {
    reco <- glue::glue(
      "row{fun}s(col{fun}s({var}{narm}, dims = {l1 - 1}), dims = {l2 - l1 + 1})",
      " or ",
      "col{fun}s({var}{narm}, dims = {l1 - 1}) if {var} has {l2} dimensions"
    )
  }

  # It's easier to remove this after the fact, rather than having never ending if/elses
  reco <- gsub(", dims = 1", "", reco, fixed = TRUE)

  return(reco)
}
