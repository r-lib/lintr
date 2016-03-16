#' @include utils.R
#' @describeIn linters check that a certain kind of indentation is used
#' @export
indentation_linter <- function(spaces, linter_name = "indentation_linter") function(source_file) {
  if (spaces %==% FALSE || spaces %==% 0L) {
    allowed <- "\t"
    message <- "Use tabs to indent."
  } else {
    allowed <- rex(n_times(" ", spaces))
    message <- sprintf("Use %s spaces to indent.", digit_to_word(spaces))
  }

  res <- re_matches(source_file$lines,
    rex(start, zero_or_more(allowed, type = 'possessive'), some_of(" ", "\t")),
    locations = TRUE, global = TRUE)

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
          message = message,
          line = source_file$lines[line_number],

          # R outputs tabs with 8 spaces
          # TODO: this is incorrect for embedded tabs, I am not going to fix it.
          ranges = list(c(start, end)),
          linter = linter_name
          )
      },
      res[[line_number]]$start,
      res[[line_number]]$end,
      SIMPLIFY = FALSE
      )
    })
}

#' @describeIn linters check that only spaces are used, never tabs.
#' @export
no_tab_linter <- indentation_linter(2L, "no_tab_linter")
