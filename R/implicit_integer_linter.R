#' Implicit integer linter
#'
#' Check that integers are explicitly typed using the form `1L` instead of `1`.
#'
#' @param allow_colon Logical, default `FALSE`. If `TRUE`, expressions involving `:`
#'   won't throw a lint regardless of whether the inputs are implicitly integers.
#' @examples
#' # will produce lints
#' lint(
#'   text = "x <- 1",
#'   linters = implicit_integer_linter()
#' )
#'
#' lint(
#'   text = "x[2]",
#'   linters = implicit_integer_linter()
#' )
#'
#' lint(
#'   text = "1:10",
#'   linters = implicit_integer_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x <- 1.0",
#'   linters = implicit_integer_linter()
#' )
#'
#' lint(
#'   text = "x <- 1L",
#'   linters = implicit_integer_linter()
#' )
#'
#' lint(
#'   text = "x[2L]",
#'   linters = implicit_integer_linter()
#' )
#'
#' lint(
#'   text = "1:10",
#'   linters = implicit_integer_linter(allow_colon = TRUE)
#' )
#'
#' @evalRd rd_tags("implicit_integer_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
implicit_integer_linter <- function(allow_colon = FALSE) {
  if (allow_colon) {
    xpath <- "//NUM_CONST[not(parent::expr/parent::expr[
      OP-COLON
      or (OP-MINUS and parent::expr/OP-COLON)
    ])]"
  } else {
    xpath <- "//NUM_CONST"
  }
  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content

    number_expr <- xml_find_all(xml, xpath)
    number <- xml_text(number_expr)
    lint_idx <- is_implicit_integer(number)
    number_expr <- number_expr[lint_idx]
    number <- number[lint_idx]
    is_negative <- !is.na(xml_find_first(number_expr, "parent::expr/preceding-sibling::OP-MINUS"))

    lint_message <-
      sprintf("Use %1$dL or %1$d.0 to avoid implicit integers.", ((-1L)^is_negative) * as.integer(number))

    xml_nodes_to_lints(
      number_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "style",
      column_number_xpath = "number(./@col2 + 1)", # mark at end
      range_end_xpath = "number(./@col2 + 1)" # end after number for easy fixing (enter "L" or ".0")
    )
  })
}

is_implicit_integer <- function(s) {
  # styler: off
  is_implicit <- !re_matches(s, rex(or(
    group(start, upper),  # Inf, NaN and logicals (TRUE, FALSE, NA, NA_*)
    group(start, "0x"),   # hexadecimals, e.g. 0x0f, 0x1p+0 or 0x1.ec6666666p+6
    dot,                  # doubles with a decimal point, e.g. 1.0 or 0.945
    group("i", end),      # complex type, e.g. 1+3i
    group("L", end)       # integer type, e.g. 62L or 1e3L
  )))
  # styler: on

  # only keep numbers that are within the range representable by R, and that are whole
  n <- as.double(s[is_implicit])
  is_implicit[is_implicit] <- abs(n) <= .Machine[["integer.max"]] & floor(n) == n
  is_implicit
}
