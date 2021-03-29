#' @describeIn linters Check that parentheses and square brackets do not have
#' spaces directly inside them.
#' @export
spaces_inside_linter <- function() {
  Linter(function(source_file) {
    tokens <- c(
      "'('",
      "')'",
      "'['",
      "']'")

    # using a regex here as checking each token is horribly slow
    re <- rex(
      list(one_of("[("), one_or_more(" "), none_of(end %or% "#" %or% " ")) %or%
        list(" " %if_prev_isnt% ",", one_of("])"))
    )
    all_matches <- re_matches(source_file$lines, re, global = TRUE, locations = TRUE)
    line_numbers <- as.integer(names(source_file$lines))

    Map(
      function(line_matches, line_number) {
        apply(
          line_matches,
          1L,
          function(match) {
            start <- match[["start"]]
            if (is.na(start)) {
              return()
            }
            end <- match[["end"]]
            line <- source_file$lines[[as.character(line_number)]]

            # make sure that the closing is not at the start of a new line (which is valid).
            start_of_line <- re_matches(line, rex(start, spaces, one_of("])")), locations = TRUE)

            if (is.na(start_of_line$start) || start_of_line$end != end) {
              pc <- source_file$parsed_content
              is_token <-
                any(pc[["line1"]] == line_number &
                      (pc[["col1"]] == end | pc[["col1"]] == start) &
                      pc[["token"]] %in% tokens)

              if (is_token) {
                Lint(
                  filename = source_file$filename,
                  line_number = line_number,
                  column_number = if (substr(line, start, start) == " ") start else start + 1L,
                  type = "style",
                  message = "Do not place spaces around code in parentheses or square brackets.",
                  line = line
                )
              }
            }
          }
        )
      },
      all_matches,
      line_numbers
    )
  })
}
