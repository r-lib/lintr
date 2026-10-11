#' Redirect `is.numeric(x) || is.integer(x)` to just use `is.numeric(x)`
#'
#' [is.numeric()] returns `TRUE` when `typeof(x)` is `double` or `integer` --
#'   testing `is.numeric(x) || is.integer(x)`, `is.numeric(x) || is.double(x)`,
#'   `is.double(x) || is.integer(x)`, `class(x) %in% c("numeric", "integer")`,
#'   `typeof(x) %in% c("double", "integer")`, or `inherits(x, c("numeric", "integer"))`
#'   is thus redundant.
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
#' lint(
#'   text = 'typeof(z) %in% c("double", "integer")',
#'   linters = is_numeric_linter()
#' )
#'
#' lint(
#'   text = 'inherits(z, c("numeric", "integer"))',
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
# nolint next: cyclocomp_linter. Helpers in factory inflate complexity (gaborcsardi/cyclocomp#30).
is_numeric_linter <- function() {
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
    fn <- xml_find_chr_(
      node,
      "string(expr[1]/SYMBOL_FUNCTION_CALL[text() = 'is.numeric' or text() = 'is.double' or text() = 'is.integer'])"
    )
    arg <- if (nzchar(fn)) xml_find_chr_(node, "string(expr[2])") else ""
    list(fn = fn, arg = arg)
  }

  lints_from_matching_args <- function(node, left_res, right_res) {
    if (any(left_res$num_args %in% right_res$int_args) || any(left_res$int_args %in% right_res$num_args)) {
      return(list("is.numeric(x) || is.integer(x)" = node))
    }
    if (any(left_res$dbl_args %in% right_res$int_args) || any(left_res$int_args %in% right_res$dbl_args)) {
      return(list("is.double(x) || is.integer(x)" = node))
    }
    if (any(left_res$num_args %in% right_res$dbl_args) || any(left_res$dbl_args %in% right_res$num_args)) {
      return(list("is.numeric(x) || is.double(x)" = node))
    }
    NULL
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
        dbl_args = c(left_res$dbl_args, right_res$dbl_args),
        int_args = c(left_res$int_args, right_res$int_args)
      ))
    }

    leaf_info <- extract_is_numeric_arg(node)

    list(
      lints = list(),
      num_args = leaf_info$arg[leaf_info$fn == "is.numeric"],
      dbl_args = leaf_info$arg[leaf_info$fn == "is.double"],
      int_args = leaf_info$arg[leaf_info$fn == "is.integer"]
    )
  }

  # testing class(x) %in% c("numeric", "integer") or typeof(x) %in% c("double", "integer")
  in_xpath <- "
  //SPECIAL[
    text() = '%in%'
    and following-sibling::expr[
      expr/SYMBOL_FUNCTION_CALL[text() = 'c']
      and count(expr/STR_CONST) = 2
      and count(expr) = 3
    ]
    and preceding-sibling::expr/expr/SYMBOL_FUNCTION_CALL[text() = 'class' or text() = 'typeof']
  ]
    /parent::expr
  "

  inherits_xpath <- "
  following-sibling::expr[
    position() = 2
    and expr[1]/SYMBOL_FUNCTION_CALL[text() = 'c']
    and count(expr/STR_CONST) = 2
    and count(expr) = 3
    and not(following-sibling::expr[not(NUM_CONST[text() = 'FALSE'])])
  ]
    /parent::expr
  "

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    calls <- source_expression$xml_find_function_calls(
      c("is.numeric", "is.double", "is.integer", "inherits"),
      keep_names = TRUE
    )
    has_multiple_is_calls <- sum(c("is.numeric", "is.double", "is.integer") %in% names(calls)) >= 2L

    or_lints <- list()
    if (has_multiple_is_calls) {
      all_or <- xml_find_all_(xml, "//OR2/parent::expr")
      or_expr <- all_or[vapply(all_or, is_or_root, logical(1L))] |>
        strip_comments_from_subtree() |>
        lapply(\(root) check_or_tree(root)$lints) |>
        unlist(recursive = FALSE)
      or_lints <- xml_nodes_to_lints(
        or_expr,
        source_expression = source_expression,
        lint_message = sprintf(
          paste(
            "Use `is.numeric(x)` instead of the equivalent `%s`.",
            "Use is.double(x) to test for objects stored as 64-bit floating point."
          ),
          names(or_expr)
        ),
        type = "warning"
      )
    }

    in_expr <- xml_find_all_(xml, in_xpath)
    in_fn <- character()
    other_type <- character()
    if (length(in_expr) > 0L) {
      in_fn <- xml_find_chr_(in_expr, "string(expr[1]/expr[1]/SYMBOL_FUNCTION_CALL)")
      other_type <- ifelse(in_fn == "class", "numeric", "double")
      str1 <- get_r_string(in_expr, "expr[2]/expr[2]/STR_CONST")
      str2 <- get_r_string(in_expr, "expr[2]/expr[3]/STR_CONST")
      keep <- is_unordered_pair(str1, str2, "integer", other_type)
      in_expr <- in_expr[keep]
      in_fn <- in_fn[keep]
      other_type <- other_type[keep]
    }
    in_lints <- xml_nodes_to_lints(
      in_expr,
      source_expression = source_expression,
      lint_message = sprintf(
        paste(
          'Use is.numeric(x) instead of %s(x) %%in%% c("integer", "%s").',
          "Use is.double(x) to test for objects stored as 64-bit floating point."
        ),
        in_fn,
        other_type
      ),
      type = "warning"
    )

    inherits_expr <- xml_find_all_(calls[names(calls) == "inherits"], inherits_xpath)
    if (length(inherits_expr) > 0L) {
      str1 <- get_r_string(inherits_expr, "expr[3]/expr[2]/STR_CONST")
      str2 <- get_r_string(inherits_expr, "expr[3]/expr[3]/STR_CONST")
      inherits_expr <- inherits_expr[is_unordered_pair(str1, str2, "integer", "numeric")]
    }
    inherits_lints <- xml_nodes_to_lints(
      inherits_expr,
      source_expression = source_expression,
      lint_message = paste(
        'Use is.numeric(x) instead of inherits(x, c("integer", "numeric")).',
        "Use is.double(x) to test for objects stored as 64-bit floating point."
      ),
      type = "warning"
    )

    c(or_lints, in_lints, inherits_lints)
  })
}

is_unordered_pair <- function(str1, str2, type1, type2) {
  (str1 == type1 & str2 == type2) | (str1 == type2 & str2 == type1)
}
