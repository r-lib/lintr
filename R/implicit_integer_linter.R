#' @describeIn linters  Check that integers are explicitly typed using the form \code{1L} instead of
#'                      \code{1}.
#' @export
implicit_integer_linter <- function() {
  Linter(function(source_file) {
    lapply(
      ids_with_token(source_file, "NUM_CONST"),  # for numbers (finite/Inf/NaN) and logicals
      function(id) {
        token <- with_id(source_file, id)
        if (is_implicit_integer(token[["text"]])) {
          line_num <- token[["line2"]]
          end_col_num <- token[["col2"]]
          start_col_num <- token[["col1"]]
          Lint(
            filename = source_file[["filename"]],
            line_number = line_num,
            column_number = end_col_num + 1L,
            type = "style",
            message =
              "Integers should not be implicit. Use the form 1L for integers or 1.0 for doubles.",
            line = source_file[["lines"]][[as.character(line_num)]],
            ranges = list(c(start_col_num, end_col_num))
          )
        }
      }
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
