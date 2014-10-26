#' @describeIn linters check that only spaces are used, never tabs.
no_tab_linter <- function(source_file) {
  res <- re_matches(source_file$stripped_comments,
    rex("\t" %>% one_or_more()), locations = TRUE, global = TRUE)[[1]]

  res <- res[!is.na(res$start)]

  mapply(
    FUN = function(start, end) {
      col_start <- source_file$find_column(start)
      col_end <- source_file$find_column(end)
      line_num <- source_file$find_line(start)

      Lint(
        filename = source_file$filename,
        line_number = line_num,
        column_number = col_start,
        type = "style",
        message = "Use two spaces to indent, never tabs.",
        line = getSrcLines(source_file, line_num, line_num),

        # R outputs tabs with 8 spaces
        # TODO: this is incorrect for embedded tabs, I am not going to fix it.
        ranges = list(c(col_start, (col_end - col_start + 1L) * 8L))
        )
    },
  res$start,
  res$end,
  SIMPLIFY = FALSE
  )
}
