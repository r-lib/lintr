#' @describeIn linters check that only spaces are used for indentation, not tabs.
#' @export
no_tab_linter <- function(source_file) {
  all_matches <- re_matches(
    source_file$lines,
    rex(start, zero_or_more(regex("\\s")), one_or_more("\t")),
    locations = TRUE,
    global = TRUE
  )
  line_numbers <- as.integer(names(source_file$lines))

  Map(
    function(line_matches, line_number) {
      lapply(
        split(line_matches, seq_len(nrow(line_matches))),
        function(match) {
          start <- match[["start"]]
          if (is.na(start)) {
            return()
          }
          end <- match[["end"]]
          Lint(
            filename = source_file$filename,
            line_number = line_number,
            column_number = start,
            type = "style",
            message = "Use spaces to indent, not tabs.",
            line = source_file$lines[[as.character(line_number)]],

            # R outputs tabs with 8 spaces
            # TODO: this is incorrect for embedded tabs, I am not going to fix it.
            ranges = list(c(start, end)),
            linter = "no_tab_linter"
          )
        }
      )
    },
    all_matches,
    line_numbers
  )
}
