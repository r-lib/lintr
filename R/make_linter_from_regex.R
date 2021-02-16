make_linter_from_regex <- function(regex,
                                   lint_type,
                                   lint_msg,
                                   ignore_strings = TRUE) {

  # If a regex-based linter is found, only flag those lints that occur within
  # a relevant section of source code
  .in_ignorable_position <- function(source_file, line_number, match) {
    ignore_strings && in_string(source_file, line_number, match)
  }

  function() {
    Linter(function(source_file) {
      all_matches <- re_matches(
        source_file[["lines"]],
        regex,
        locations = TRUE,
        global = TRUE
      )

      line_numbers <- as.integer(names(source_file[["lines"]]))

      Map(
        function(line_matches, line_number) {
          lapply(
            split(line_matches, seq_len(nrow(line_matches))),
            function(.match) {
              if (is.na(.match[["start"]]) ||
                .in_ignorable_position(source_file, line_number, .match)) {
                return()
              }
              start <- .match[["start"]]
              end <- .match[["end"]]
              Lint(
                filename = source_file[["filename"]],
                line_number = line_number,
                column_number = start,
                type = lint_type,
                message = lint_msg,
                line = source_file[["lines"]][[as.character(line_number)]],
                ranges = list(c(start, end))
              )
            }
          )
        },
        all_matches,
        line_numbers
      )
    })
  }
}

#' Determine if a regex match is covered by an expression in a source_file
#'
#' @param   source_file   A source_file
#' @param   line_number,match   The position where a regex match was observed.
#'   match must have entries "start" and "end".
#' @param   token_type    Restrict analysis to tokens of this type, for example,
#'   with token_type = "STR_CONST" you can check that a regex match occurs
#'   within a string
#' @noRd

is_match_covered <- function(source_file, line_number, match, token_type = NULL) {
  pc <- source_file[["parsed_content"]]
  if (!is.null(token_type)) {
    pc <- pc[pc[["token"]] == token_type, ]
  }
  covering_rows <- pc[["line1"]] <= line_number & pc[["line2"]] >= line_number
  pc_cover <- pc[covering_rows, ]

  any_single_line_covers <- function() {
    x <- pc_cover[pc_cover[["line1"]] == pc_cover[["line2"]], ]
    any(
      x[["col1"]] <= match[["start"]] & x[["col2"]] >= match[["end"]]
    )
  }

  any_multi_line_covers <- function() {
    x <- pc_cover[pc_cover[["line1"]] < pc_cover[["line2"]], ]
    any(
      (x[["line1"]] < line_number & x[["line2"]] > line_number) |
        (x[["line1"]] == line_number & x[["col1"]] <= match[["start"]]) |
        (x[["line2"]] == line_number & x[["col2"]] >= match[["end"]])
    )
  }

  any_single_line_covers() || any_multi_line_covers()
}

in_string <- function(source_file, line_number, match) {
  # do any of the strings in the parsed content contain the matched regex?

  is_match_covered(source_file, line_number, match, "STR_CONST")
}
