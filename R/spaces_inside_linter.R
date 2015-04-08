#' @describeIn linters check that parentheses and square brackets do not have
#' spaces directly inside them.
#' @export
spaces_inside_linter <- function(source_file) {
  tokens <- c(
    "'('",
    "')'",
    "'['",
    "']'")

  # using a regex here as checking each token is horribly slow
  re <- rex(list(one_of("[("), " ") %or% list(" " %if_prev_isnt% ",", one_of("])")))

  res <- re_matches(source_file$lines, re, global = TRUE, locations = TRUE)

  lapply(seq_along(res), function(line_number) {

    mapply(
      FUN = function(start, end) {
        if (is.na(start)) {
          return()
        }

        line <- unname(source_file$lines[line_number])

        # we need to make sure that the closing are not at the start of a new
        # line (which is valid).
        start_of_line <- re_matches(line, rex(start, spaces, one_of("])")), locations = TRUE)

        if (is.na(start_of_line$start) || start_of_line$end != end) {
          is_token <-
            any(source_file$parsed_content$line1 == line_number &
               (source_file$parsed_content$col1 == end |
                source_file$parsed_content$col1 == start) &
              source_file$parsed_content$token %in% tokens)

          if (is_token) {

            Lint(
              filename = source_file$filename,
              line_number = line_number,
              column_number = start,
              type = "style",
              message = "Do not place spaces around code in parentheses or square brackets.",
              line = line,
              linter = "spaces_inside_linter"
              )
          }
        }
      },
      start = res[[line_number]]$start,
      end = res[[line_number]]$end,
      SIMPLIFY = FALSE
      )
    })
}
