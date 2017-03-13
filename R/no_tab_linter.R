#' @describeIn linters check that only spaces are used, never tabs.
#' @export
no_tab_linter <- function(source_file) {
  all_res <- re_matches(
    source_file$lines,
    rex(start, zero_or_more(regex("\\s")), one_or_more("\t")),
    locations = TRUE,
    global = TRUE
  )
  names(all_res) <- as.character(seq_along(all_res))

  # filter out lines with no matches
  all_res <- all_res[
    vapply(all_res, function(line_match) {!is.na(line_match[["start"]][[1L]])}, logical(1L))
  ]

  Map(
    function(line_res, line_num) {
      lapply(
        split(line_res, seq_len(nrow(line_res))),
        function(res) {
          Lint(
            filename = source_file$filename,
            line_number = line_num,
            column_number = res[["start"]],
            type = "style",
            message = "Use spaces to indent, not tabs.",
            line = source_file$lines[line_num],

            # R outputs tabs with 8 spaces
            # TODO: this is incorrect for embedded tabs, I am not going to fix it.
            ranges = list(c(res[["start"]], res[["end"]])),
            linter = "no_tab_linter"
          )
        }
      )
    },
    all_res,
    names(all_res)
  )
}
