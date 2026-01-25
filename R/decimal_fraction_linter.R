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
#' @param lint_exact_binary Logical, whether to lint values exactly representable in binary.
#' @export
decimal_fraction_linter <- function(lint_exact_binary = FALSE) {
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

    filtered <- filter_decimal_fraction_inputs(
      bad_expr = bad_expr,
      num_text = num_text,
      other_factor = other_factor,
      operator = operator,
      is_negative = is_negative
    )
    if (length(filtered[["bad_expr"]]) == 0L) {
      return(list())
    }

    fractions <- lapply(filtered[["num_text"]], function(number) {
      parts <- extract_decimal_parts(number)
      if (!lint_exact_binary && is_exact_binary_literal(parts[["fractional_part"]])) {
        return(NULL)
      }
      convert_parts_to_fraction(
        integer_part = parts[["integer_part"]],
        fractional_part = parts[["fractional_part"]]
      )
    })
    filtered <- filter_decimal_fractions(
      bad_expr = filtered[["bad_expr"]],
      other_factor = filtered[["other_factor"]],
      operator = filtered[["operator"]],
      is_negative = filtered[["is_negative"]],
      fractions = fractions
    )
    if (length(filtered[["bad_expr"]]) == 0L) {
      return(list())
    }

    call_names <- xp_call_name(filtered[["bad_expr"]], depth = 1L)
    lint_message <- build_decimal_fraction_message(
      fractions = filtered[["fractions"]],
      call_names = call_names,
      other_factor = filtered[["other_factor"]],
      operator = filtered[["operator"]],
      is_negative = filtered[["is_negative"]]
    )

    xml_nodes_to_lints(
      filtered[["bad_expr"]],
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning",
      column_number_xpath = "number(./expr[2]//NUM_CONST/@col1)",
      range_start_xpath = "number(./expr[2]//NUM_CONST/@col1)",
      range_end_xpath = "number(./expr[2]//NUM_CONST/@col2)"
    )
  })
}

filter_decimal_fraction_inputs <- function(bad_expr, num_text, other_factor, operator, is_negative) {
  ok <- nzchar(other_factor)
  list(
    bad_expr = bad_expr[ok],
    num_text = num_text[ok],
    other_factor = other_factor[ok],
    operator = operator[ok],
    is_negative = is_negative[ok]
  )
}

filter_decimal_fractions <- function(bad_expr, other_factor, operator, is_negative, fractions) {
  has_fraction <- vapply(fractions, Negate(is.null), logical(1L))
  list(
    bad_expr = bad_expr[has_fraction],
    other_factor = other_factor[has_fraction],
    operator = operator[has_fraction],
    is_negative = is_negative[has_fraction],
    fractions = fractions[has_fraction]
  )
}

build_decimal_fraction_message <- function(fractions, call_names, other_factor, operator, is_negative) {
  vapply(seq_along(fractions), function(idx) {
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
}

#' Determine if a decimal literal is exactly representable in binary
#'
#' @param fractional_part Character scalar containing fractional digits.
#' @return `TRUE` when the literal is exactly representable in binary.
#' @keywords internal
#' @rdname decimal_fraction_helpers
is_exact_binary_literal <- function(fractional_part) {
  if (fractional_part == "") {
    return(TRUE)
  }
  fractional_digits <- as.integer(fractional_part)

  digits <- nchar(fractional_part)
  v5 <- 0L
  while (fractional_digits %% 5L == 0L) {
    fractional_digits <- fractional_digits / 5L
    v5 <- v5 + 1L
  }

  v5 >= digits
}

#' Extract decimal literal parts with exponent applied
#'
#' @param number Character scalar decimal literal.
#' @return A list with `integer_part` and `fractional_part`, or `NULL` if invalid.
#' @keywords internal
#' @rdname decimal_fraction_helpers
extract_decimal_parts <- function(number) {
  split_exp <- strsplit(number, "[eE]", perl = TRUE)[[1L]]
  mantissa <- split_exp[[1L]]
  exponent <- if (length(split_exp) > 1L) as.integer(split_exp[[2L]]) else 0L
  sign_char <- if (grepl("^[-+]", mantissa)) substr(mantissa, 1L, 1L) else ""
  mantissa <- sub("^[-+]", "", mantissa)

  parts <- strsplit(mantissa, ".", fixed = TRUE)[[1L]]
  integer_part <- parts[[1L]]
  fractional_part <- if (length(parts) > 1L) parts[[2L]] else ""
  integer_part <- ifelse(nzchar(integer_part), integer_part, "0")

  digits_text <- paste0(integer_part, fractional_part)
  decimal_pos <- nchar(integer_part)
  new_decimal_pos <- decimal_pos + exponent
  if (new_decimal_pos >= nchar(digits_text)) {
    integer_part <- paste0(digits_text, strrep("0", new_decimal_pos - nchar(digits_text)))
    fractional_part <- ""
  } else if (new_decimal_pos <= 0L) {
    integer_part <- "0"
    fractional_part <- paste0(strrep("0", abs(new_decimal_pos)), digits_text)
  } else {
    integer_part <- substr(digits_text, 1L, new_decimal_pos)
    fractional_part <- substr(digits_text, new_decimal_pos + 1L, nchar(digits_text))
  }
  if (sign_char != "") {
    integer_part <- paste0(sign_char, integer_part)
  }

  fractional_part <- sub("0+$", "", fractional_part)
  list(integer_part = integer_part, fractional_part = fractional_part)
}

#' Convert decimal literal parts to a reduced fraction
#'
#' @param integer_part Character scalar integer part.
#' @param fractional_part Character scalar fractional part.
#' @return A list with `numerator` and `denominator`, or `NULL` if invalid.
#' @keywords internal
#' @rdname decimal_fraction_helpers
convert_parts_to_fraction <- function(integer_part, fractional_part) {
  numerator <- as.integer(paste0(integer_part, fractional_part))
  denominator <- 10.0^nchar(fractional_part)
  reduce_fraction(numerator, denominator)
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
