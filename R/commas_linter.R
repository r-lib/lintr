#' @describeIn linters check that all commas are followed by spaces, but do not
#' have spaces before them.
#' @export
commas_linter <- function(source_file) {

  re <- rex(list(one_or_more(" "), ",") %or% list(",", non_space))

  res <- re_matches(source_file$content, re, global = TRUE, locations = TRUE)[[1]]

  if (!is.na(res$start[1])) {
    lapply(seq_len(NROW(res)),
      function(itr) {

      lints <- list()

      line_number <- source_file$find_line(res$start[itr])

      start <- source_file$find_column(res$start[itr])
      end <- source_file$find_column(res$end[itr])
      line <- source_file$lines[line_number]

      comma_loc <- start + re_matches(substr(line, start, end), rex(","), locations = TRUE)$start - 1L

      space_before <- unname(substr(line, comma_loc - 1L, comma_loc - 1L)) %==% " "

      if (space_before) {

        has_token <- any(source_file$parsed_content$line1 == line_number &
          source_file$parsed_content$col1 == comma_loc &
          source_file$parsed_content$token == "','")

        start_of_line <- re_matches(line, rex(start, spaces, ","))

        if (has_token && !start_of_line) {

          lints[[length(lints) + 1L]] <-
            Lint(
              filename = source_file$filename,
              line_number = line_number,
              column_number = comma_loc,
              type = "style",
              message = "Commas should never have a space before.",
              line = line,
              ranges = list(c(start, end))
              )
        }
      }

      # we still need to check if there is a non-space after
      non_space_after <- re_matches(substr(line, comma_loc + 1L, comma_loc + 1L), rex(non_space))

      if (non_space_after) {

        has_token <- any(source_file$parsed_content$line1 == line_number &
          source_file$parsed_content$col1 == comma_loc &
          source_file$parsed_content$token == "','")

        if (has_token) {

          lints[[length(lints) + 1L]] <-
            Lint(
              filename = source_file$filename,
              line_number = line_number,
              column_number = comma_loc,
              type = "style",
              message = "Commas should always have a space after.",
              line = line
              )
        }

      }

      lints
    })
  }
}
