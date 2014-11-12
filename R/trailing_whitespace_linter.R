#' @describeIn linters check there are no trailing whitespace characters.
#' @export
trailing_whitespace_linter <- function(source_file) {
  res <- re_matches(source_file$lines,
    rex(capture(name = "space", some_of(" ", regex("\\t"))), or(newline, end)),
    global = TRUE,
    locations = TRUE)

  lapply(seq_along(lines), function(line_number) {

      mapply(
        FUN = function(start, end) {
          if (is.na(start)) {
            return()
          }
          Lint(
            filename = source_file$filename,
            line_number = line_number,
            column_number = start,
            type = "style",
            message = "Trailing whitespace is superfluous.",
            line = source_file$lines[line_number],
            ranges = list(c(start, end))
            )
        },
        start = res[[line_number]]$space.start,
        end = res[[line_number]]$space.end,
        SIMPLIFY = FALSE
        )
  })

}
