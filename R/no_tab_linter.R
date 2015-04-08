#' @describeIn linters check that only spaces are used, never tabs.
#' @export
no_tab_linter <- function(source_file) {
  res <- re_matches(source_file$lines,
    rex("\t" %>% one_or_more()), locations = TRUE, global = TRUE)

  lapply(seq_along(source_file$lines), function(line_number) {
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
          message = "Use two spaces to indent, never tabs.",
          line = source_file$lines[line_number],

          # R outputs tabs with 8 spaces
          # TODO: this is incorrect for embedded tabs, I am not going to fix it.
          ranges = list(c(start, end)),
          linter = "no_tab_linter"
          )
      },
      res[[line_number]]$start,
      res[[line_number]]$end,
      SIMPLIFY = FALSE
      )
    })
}
