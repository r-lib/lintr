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

    or_lints <- list()
    if ("is.numeric" %in% names(calls) && "is.integer" %in% names(calls)) {
      all_or <- xml_find_all_(xml, "//OR2/parent::expr")
      or_roots <- all_or[vapply(all_or, is_or_root, logical(1L))]
      or_lint_nodes <- list()
      for (root in or_roots) {
        res <- check_or_tree(root)
        or_lint_nodes <- c(or_lint_nodes, res$lints)
      }
      or_lints <- xml_nodes_to_lints(
        or_lint_nodes,
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
      class_strings <- c(
        get_r_string(class_expr, "expr[2]/expr[2]/STR_CONST"),
        get_r_string(class_expr, "expr[2]/expr[3]/STR_CONST")
      )
      is_lintable <- "integer" %in% class_strings && "numeric" %in% class_strings
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

unwrap_parens <- function(node) {
  while (xml_find_num_(node, "count(OP-LEFT-PAREN | OP-RIGHT-PAREN | expr)") == 3L) {
    node <- xml_find_first_(node, "expr")
  }
  node
}

is_or_root <- function(node) {
  parent <- xml_find_first_(node, "parent::expr")
  while (!is.na(parent)) {
    if (xml_find_num_(parent, "count(OR2)") > 0L) {
      return(FALSE)
    }
    if (xml_find_num_(parent, "count(OP-LEFT-PAREN | OP-RIGHT-PAREN | expr)") != 3L) {
      break
    }
    parent <- xml_find_first_(parent, "parent::expr")
  }
  TRUE
}

extract_is_numeric_arg <- function(node) {
  fn_node <- xml_find_first_(
    node,
    "self::expr[
      expr[1]/SYMBOL_FUNCTION_CALL[text() = 'is.numeric' or text() = 'is.integer']
      and count(expr) = 2
      and not(SYMBOL_SUB[text() != 'x'])
      and not(EQ_SUB/preceding-sibling::STR_CONST[text() != \"'x'\" and text() != '\"x\"'])
    ]/expr[1]/SYMBOL_FUNCTION_CALL"
  )
  if (is.na(fn_node)) {
    return(NULL)
  }
  fn <- xml_text(fn_node)
  arg_node <- xml_find_first_(node, "expr[2]")
  list(fn = fn, arg = xml2lang(arg_node))
}

matches_any <- function(args1, args2) {
  for (a1 in args1) {
    for (a2 in args2) {
      if (identical(a1, a2)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

has_cross_redundancy <- function(left_res, right_res) {
  matches_any(left_res$num_args, right_res$int_args) ||
    matches_any(left_res$int_args, right_res$num_args)
}

check_or_tree <- function(node) {
  node <- unwrap_parens(node)

  # nolint next: implicit_assignment_linter. Allows us to reduce nesting.
  if (xml_find_num_(node, "count(OR2)") > 0L && length(exprs <- xml_find_all_(node, "expr")) == 2L) {
    left_res <- check_or_tree(exprs[[1L]])
    right_res <- check_or_tree(exprs[[2L]])

    lints <- c(left_res$lints, right_res$lints)
    if (has_cross_redundancy(left_res, right_res)) {
      lints <- c(lints, list(node))
    }

    return(list(
      lints = lints,
      num_args = c(left_res$num_args, right_res$num_args),
      int_args = c(left_res$int_args, right_res$int_args)
    ))
  }

  leaf_info <- extract_is_numeric_arg(node)
  if (is.null(leaf_info)) {
    return(list(lints = list(), num_args = list(), int_args = list()))
  }

  list(
    lints = list(),
    num_args = if (leaf_info$fn == "is.numeric") list(leaf_info$arg) else list(),
    int_args = if (leaf_info$fn == "is.integer") list(leaf_info$arg) else list()
  )
}
