#' Implicit integer linter
#'
#' Check that integers are explicitly typed using the form `1L` instead of `1`.
#'
#' @evalRd rd_tags("implicit_integer_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
implicit_integer_linter <- function() {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    numbers <- xml2::xml_find_all(xml, "//NUM_CONST")

    xml_nodes_to_lints(
      numbers[is_implicit_integer(xml2::xml_text(numbers))],
      source_expression = source_expression,
      lint_message = "Integers should not be implicit. Use the form 1L for integers or 1.0 for doubles.",
      type = "style",
      offset = 1L
    )
  })
}

is_implicit_integer <- function(s) {
  is_implicit <- !re_matches(s, rex(or(
    group(start, upper),  # Inf, NaN and logicals (TRUE, FALSE, NA, NA_*)
    group(start, "0x"),   # hexadecimals, e.g. 0x0f, 0x1p+0 or 0x1.ec6666666p+6
    dot,                  # doubles with a decimal point, e.g. 1.0 or 0.945
    group("i", end),      # complex type, e.g. 1+3i
    group("L", end)       # integer type, e.g. 62L or 1e3L
  )))
  # only keep numbers that are within the range representable by R, and that are whole
  n <- as.double(s[is_implicit])
  is_implicit[is_implicit] <- abs(n) <= .Machine[["integer.max"]]  &  floor(n) == n
  is_implicit
}
