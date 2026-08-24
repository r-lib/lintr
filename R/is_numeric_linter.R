#' Redirect `is.numeric(x) || is.integer(x)` to just use `is.numeric(x)`
#'
#' [is.numeric()] returns `TRUE` when `typeof(x)` is `double` or `integer` --
#'   testing `is.numeric(x) || is.integer(x)` is thus redundant.
#'
#' NB: This linter plays well with [class_equals_linter()], which can help
#'   avoid further `is.numeric()` equivalents like
#'   `any(class(x) == c("numeric", "integer"))`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "is.numeric(y) || is.integer(y)",
#'   linters = is_numeric_linter()
#' )
#'
#' lint(
#'   text = 'class(z) %in% c("numeric", "integer")',
#'   linters = is_numeric_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "is.numeric(y) || is.factor(y)",
#'   linters = is_numeric_linter()
#' )
#'
#' lint(
#'   text = 'class(z) %in% c("numeric", "integer", "factor")',
#'   linters = is_numeric_linter()
#' )
#'
#' @evalRd rd_tags("is_numeric_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
is_numeric_linter <- function() {
  # TODO(#2469): This should also cover is.double(x) || is.integer(x).
  # TODO(#2470): Consider usages with class(), typeof(), or inherits().

  is_or_root <- function(node) {
    parent <- node
    repeat {
      parent <- xml_find_first_(parent, "parent::expr")
      if (is.na(parent)) {
        return(TRUE)
      }
      if (xml_find_lgl_(parent, "boolean(OR2)")) {
        return(FALSE)
      }
      if (!xml_find_lgl_(parent, is_paren_expr_xpath)) {
        return(TRUE)
      }
    }
  }

  extract_is_numeric_arg <- function(node) {
    fn <- xml_find_chr_(node, "string(expr[1]/SYMBOL_FUNCTION_CALL[text() = 'is.numeric' or text() = 'is.integer'])")
    arg <- if (nzchar(fn)) xml_find_chr_(node, "string(expr[2])") else ""
    list(fn = fn, arg = arg)
  }

  check_or_tree <- function(node) {
    node <- unwrap_parens(node)

    if (xml_find_lgl_(node, "boolean(OR2)")) {
      exprs <- xml_find_all_(node, "expr")
      left_res <- check_or_tree(exprs[[1L]])
      right_res <- check_or_tree(exprs[[2L]])

      lints <- c(
        left_res$lints, right_res$lints,
        lints_from_matching_args(node, left_res, right_res)
      )

      return(list(
        lints = lints,
        num_args = c(left_res$num_args, right_res$num_args),
        int_args = c(left_res$int_args, right_res$int_args)
      ))
    }

    leaf_info <- extract_is_numeric_arg(node)

    list(
      lints = list(),
      num_args = if (leaf_info$fn == "is.numeric") leaf_info$arg else character(),
      int_args = if (leaf_info$fn == "is.integer") leaf_info$arg else character()
    )
  }

  # testing class(x) %in% c("numeric", "integer")
  class_xpath <- "
  //SPECIAL[
    text() = '%in%'
    and following-sibling::expr[
      expr/SYMBOL_FUNCTION_CALL[text() = 'c']
      and count(expr/STR_CONST) = 2
      and count(expr) = 3
    ]
    and preceding-sibling::expr/expr/SYMBOL_FUNCTION_CALL[text() = 'class']
  ]
    /parent::expr
  "

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    calls <- source_expression$xml_find_function_calls(c("is.numeric", "is.integer"), keep_names = TRUE)
    has_both_calls <- all(c("is.numeric", "is.integer") %in% names(calls))

    or_lints <- list()
    if (has_both_calls) {
      all_or <- xml_find_all_(xml, "//OR2/parent::expr")
      or_expr <- all_or[vapply(all_or, is_or_root, logical(1L))] |>
        strip_comments_from_subtree() |>
        lapply(\(root) check_or_tree(root)$lints) |>
        unlist(recursive = FALSE)
      or_lints <- xml_nodes_to_lints(
        or_expr,
        source_expression = source_expression,
        lint_message = paste(
          "Use `is.numeric(x)` instead of the equivalent `is.numeric(x) || is.integer(x)`.",
          "Use is.double(x) to test for objects stored as 64-bit floating point."
        ),
        type = "warning"
      )
    }

    class_expr <- xml_find_all_(xml, class_xpath)
    if (length(class_expr) > 0L) {
      str1 <- get_r_string(class_expr, "expr[2]/expr[2]/STR_CONST")
      str2 <- get_r_string(class_expr, "expr[2]/expr[3]/STR_CONST")
      is_lintable <- (str1 == "integer" & str2 == "numeric") | (str1 == "numeric" & str2 == "integer")
      class_expr <- class_expr[is_lintable]
    }
    class_lints <- xml_nodes_to_lints(
      class_expr,
      source_expression = source_expression,
      lint_message = paste(
        'Use is.numeric(x) instead of class(x) %in% c("integer", "numeric").',
        "Use is.double(x) to test for objects stored as 64-bit floating point."
      ),
      type = "warning"
    )

    c(or_lints, class_lints)
  })
}

lints_from_matching_args <- function(node, left_res, right_res) {
  if (any(left_res$num_args %in% right_res$int_args)) {
    return(list(node))
  }
  if (any(left_res$int_args %in% right_res$num_args)) {
    return(list(node))
  }
  NULL
}
