#' @describeIn linters check that there is a space between right
#' parenthesis and an opening curly brace.
#' @export
paren_brace_linter <- function(source_file) {
  all_matches <- re_matches(
    source_file[["lines"]],
    rex("){"),
    locations = TRUE,
    global = TRUE
  )
  line_numbers <- as.integer(names(source_file[["lines"]]))

  Map(
    function(line_matches, line_number) {
      lapply(
        split(line_matches, seq_len(nrow(line_matches))),
        function(.match) {
          start <- .match[["start"]]
          if (is.na(start)) {
            return()
          }
          end <- .match[["end"]]
          Lint(
            filename = source_file[["filename"]],
            line_number = line_number,
            column_number = start,
            type = "style",
            message = "There should be a space between right parenthesis and an opening curly brace.",
            line = source_file[["lines"]][[as.character(line_number)]],
            ranges = list(c(start, end)),
            linter = "paren_brace_linter"
          )
        }
      )
    },
    all_matches,
    line_numbers
  )
}
