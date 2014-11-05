#' @describeIn linters check that parentheses and square brackets do not have
#' spaces directly inside them.
#' @export
spaces_inside_linter <- function(source_file) {

  # using a regex here as checking each token is horribly slow
  re <- rex(list(one_of("[("), " ") %or% list(" " %if_prev_isnt% ",", one_of("])")))

  res <- re_matches(source_file$content, re, global = TRUE, locations = TRUE)[[1]]

  if (!is.na(res$start[1])) {
    lapply(seq_len(NROW(res)),
      function(itr) {
        line_number <- source_file$find_line(res$start[itr])
        column_number <- source_file$find_column(res$start[itr])
        column_end <- source_file$find_column(res$end[itr])

        line <- getSrcLines(source_file, line_number, line_number)

        # we need to make sure that the closing are not at the start of a new
        # line (which is valid).
        start_of_line <- re_matches(line, rex(start, spaces, one_of("])")), locations = TRUE)

        if (is.na(start_of_line$start) || start_of_line$end != column_end) {

          Lint(
            filename = source_file$filename,
            line_number = line_number,
            column_number = column_number,
            type = "style",
            message = "Do not place spaces around code in parentheses or square brackets.",
            line = line
            )
        }

      })
  }
}

