#' Decimal fraction integer linter
#'
#' Check for use of `as.integer(x * <float>)` and suggest an integer arithmetic
#' equivalent like `as.integer((x * 1461L)%/%4L)`.
#'
#' @examples
#' # will produce a lint
#' lint(
#'   text = "as.integer(x * 365.25)",
#'   linters = decimal_fraction_linter()
#' )
#'
#' lint(
#'   text = "as.integer(365.25 * x)",
#'   linters = decimal_fraction_linter()
#' )
#'
#' lint(
#'   text = "as.integer(x * 0.1)",
#'   linters = decimal_fraction_linter()
#' )
#'
#' lint(
#'   text = "as.integer(x / 0.13)",
#'   linters = decimal_fraction_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "as.integer(x * 365)",
#'   linters = decimal_fraction_linter()
#' )
#'
#' @evalRd rd_tags("decimal_fraction_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
decimal_fraction_linter <- function() {
  Linter(linter_level = "expression", function(source_expression) {
    tracked_funs <- c("as.integer", "ceiling", "floor", "trunc")
    xml_calls <- source_expression$xml_find_function_calls(tracked_funs)
    if (length(xml_calls) == 0L) {
      return(list())
    }

    # Match as.integer calls where the argument is a simple binary expr with a
    # decimal literal (including 1e-3-style) so we can safely suggest integer
    # arithmetic replacements.
    # bad_expr narrows to as.integer(<binary>) so we can safely rewrite and
    # pinpoint the literal; more complex expressions are left alone.
    bad_expr <- xml_find_all(
      xml_calls,
      "parent::expr[
        expr[2][
          (
            OP-STAR
            and expr//NUM_CONST
          ) or (
            OP-SLASH
            and expr[2]//NUM_CONST
          )
        ]
        and expr[2]//NUM_CONST[
          contains(text(), '.')
          or contains(translate(text(), 'E', 'e'), 'e-')
        ]
      ]"
    )
    if (length(bad_expr) == 0L) {
      return(list())
    }

    bad_expr <- strip_comments_from_subtree(bad_expr)
    bad_expr <- xml_find_all(
      bad_expr,
      "self::expr[expr[2][count(expr) = 2]]"
    )
    if (length(bad_expr) == 0L) {
      return(list())
    }

    # arg_expr is the argument expression to as.integer(<arg>).
    arg_expr <- xml_find_all(bad_expr, "expr[2]")
    expr_children <- xml_find_all(arg_expr, "./expr")
    is_numeric_child <- vapply(expr_children, function(expr) {
      !is.na(xml_find_first(expr, ".//NUM_CONST"))
    }, logical(1L))
    num_expr <- xml_find_all(expr_children[is_numeric_child], ".//NUM_CONST")
    # num_text is the literal string of the decimal constant we parse into a fraction.
    num_text <- xml_text(num_expr)
    other_expr <- expr_children[!is_numeric_child]
    # other_factor is the non-numeric operand (e.g., x or x + y) kept in the replacement.
    other_factor <- xml_text(other_expr)
    operator <- vapply(arg_expr, function(expr) {
      xml_name(xml_find_first(expr, "./OP-STAR | ./OP-SLASH"))
    }, character(1L))
    # Unary minus is its own node in the XML AST, so we need to inspect ancestors
    # to preserve sign in the replacement suggestion.
    is_negative <- vapply(num_expr, function(expr) {
      !is.na(xml_find_first(expr, "ancestor::expr[OP-MINUS and count(expr) = 1]"))
    }, logical(1L))

    # Skip nodes where we failed to identify the non-numeric operand; otherwise
    # we could build a malformed suggestion.
    ok <- nzchar(other_factor)
    bad_expr <- bad_expr[ok]
    num_text <- num_text[ok]
    other_factor <- other_factor[ok]
    operator <- operator[ok]
    is_negative <- is_negative[ok]
    if (length(bad_expr) == 0L) {
      return(list())
    }

    fractions <- lapply(num_text, decimal_literal_to_fraction)
    has_fraction <- vapply(fractions, Negate(is.null), logical(1L))
    bad_expr <- bad_expr[has_fraction]
    other_factor <- other_factor[has_fraction]
    fractions <- fractions[has_fraction]
    operator <- operator[has_fraction]
    is_negative <- is_negative[has_fraction]
    if (length(bad_expr) == 0L) {
      return(list())
    }

    call_names <- xp_call_name(bad_expr, depth = 1L)
    lint_message <- vapply(seq_along(bad_expr), function(idx) {
      fraction <- fractions[[idx]]
      sign_factor <- ifelse(is_negative[[idx]], -1L, 1L)
      numerator <- sign_factor * fraction[["numerator"]]
      denominator <- fraction[["denominator"]]
      if (operator[[idx]] == "OP-SLASH") {
        numerator <- sign_factor * fraction[["denominator"]]
        denominator <- fraction[["numerator"]]
      }
      has_mult_part <- numerator != 1L
      has_div_part <- denominator != 1L
      mult_part <- if (has_mult_part) sprintf(" * %dL", numerator) else ""
      div_part <- if (has_div_part) sprintf(" / %dL", denominator) else ""
      replacement_template <- if (has_mult_part && has_div_part) "%s((%s%s)%s)" else "%s(%s%s%s)"
      replacement <- sprintf(
        replacement_template,
        call_names[[idx]],
        other_factor[[idx]],
        mult_part,
        div_part
      )
      sprintf(
        "Use %s to avoid floating-point rounding.",
        replacement
      )
    }, character(1L))

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning",
      column_number_xpath = "number(./expr[2]//NUM_CONST/@col1)",
      range_start_xpath = "number(./expr[2]//NUM_CONST/@col1)",
      range_end_xpath = "number(./expr[2]//NUM_CONST/@col2)"
    )
  })
}

#' Convert a decimal literal to a reduced fraction
#'
#' @param number Character scalar decimal literal.
#' @return A list with `numerator` and `denominator`, or `NULL` if the literal is unsuitable.
#' @keywords internal
#' @rdname decimal_fraction_helpers
decimal_literal_to_fraction <- function(number) {
  split_exp <- strsplit(number, "[eE]", perl = TRUE)[[1L]]
  mantissa <- split_exp[[1L]]
  exponent <- if (length(split_exp) > 1L) as.integer(split_exp[[2L]]) else 0L
  if (should_skip_decimal_fraction(mantissa, exponent)) {
    return(NULL)
  }

  parts <- strsplit(mantissa, ".", fixed = TRUE)[[1L]]
  integer_part <- parts[[1L]]
  fractional_part <- if (length(parts) > 1L) parts[[2L]] else ""
  integer_part <- ifelse(nzchar(integer_part), integer_part, "0")

  # Use decimal digits for numerator/denominator to avoid floating-point drift.
  numerator <- as.integer(paste0(integer_part, fractional_part))
  if (is.na(numerator)) {
    return(NULL)
  }
  denominator <- 10.0^nchar(fractional_part)
  if (exponent < 0L) {
    denominator <- denominator * 10.0^abs(exponent)
  }
  reduce_fraction(numerator, denominator)
}

#' Decide whether to skip fraction conversion
#'
#' @param mantissa Character scalar mantissa.
#' @param exponent Integer exponent from scientific notation.
#' @return `TRUE` when the literal should be ignored.
#' @keywords internal
#' @rdname decimal_fraction_helpers
should_skip_decimal_fraction <- function(mantissa, exponent) {
  if (!grepl(".", mantissa, fixed = TRUE)) {
    return(exponent >= 0L)
  }
  # Positive exponents create integer-like values (e.g., 3.2e2) that we skip.
  if (exponent > 0L) {
    return(TRUE)
  }
  if (exponent >= 0L && !grepl("[1-9]", mantissa)) {
    return(TRUE)
  }
  FALSE
}

#' Normalize a fraction into reduced integer form
#'
#' @param numerator Integer numerator.
#' @param denominator Integer denominator.
#' @return A reduced fraction list or `NULL` if invalid.
#' @keywords internal
#' @rdname decimal_fraction_helpers
reduce_fraction <- function(numerator, denominator) {
  if (abs(numerator) > .Machine[["integer.max"]] || denominator > .Machine[["integer.max"]]) {
    return(NULL)
  }
  numerator <- as.integer(numerator)
  denominator <- as.integer(denominator)
  if (denominator == 0L) {
    return(NULL)
  }
  divisor <- integer_gcd(abs(numerator), denominator)
  numerator <- numerator %/% divisor
  denominator <- denominator %/% divisor
  if (denominator == 1L) {
    return(NULL)
  }

  list(numerator = numerator, denominator = denominator)
}

#' Compute the greatest common divisor for integers
#'
#' @param a Integer.
#' @param b Integer.
#' @return Integer greatest common divisor.
#' @keywords internal
#' @rdname decimal_fraction_helpers
integer_gcd <- function(a, b) {
  while (b != 0L) {
    tmp <- b
    b <- a %% b
    a <- tmp
  }
  abs(a)
}
