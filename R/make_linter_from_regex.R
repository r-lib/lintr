make_linter_from_regex <- function(regex,
                                   lint_name,
                                   lint_type,
                                   lint_msg,
                                   ignore_comments = TRUE,
                                   ignore_strings = TRUE) {

  # If a regex-based linter is found, only flag those lints that occur within
  # a relevant section of source code
  .in_ignorable_position <- function(source_file, line_number, match) {
    is.na(match[["start"]]) ||
      (ignore_comments && in_comment(source_file, line_number, match)) ||
      (ignore_strings && in_string(source_file, line_number, match))
  }

  linter <- function(source_file) {
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
            if (.in_ignorable_position(source_file, line_number, .match)) {
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
              ranges = list(c(start, end)),
              linter = lint_name
            )
          }
        )
      },
      all_matches,
      line_numbers
    )
  }

  linter
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
  if (! is.null(token_type)) {
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

in_comment <- function(source_file, line_number, match) {
  # For code of the form "abc <- 123 # some comment", there are two runs
  # through a given linter: first with a source_file generated from
  # "abc <- 123", and secondly with a source_file from the comment

  # If a regex matches inside a post-code comment, then the "match" will not
  # overlap the code that precedes the comment: for now we return TRUE for
  # these instances

  # TODO: filter out code blocks that do not cover the "match" before running
  # this function then use is_match_covered(..., "COMMENT")

  # TODO: check this handles "f <- function(x, #\tblah\n    y) {}"

  pc <- source_file[["parsed_content"]]

  non_comment_rows <- which(pc[["token"]] != "COMMENT")
  pc_no_comments <- pc[non_comment_rows, ]

  if (nrow(pc_no_comments) == 0 ||
    match[["start"]] > max(pc_no_comments[["col2"]])) {
    TRUE
  } else {
    FALSE
  }
}

in_string <- function(source_file, line_number, match) {
  # do any of the strings in the parsed content contain the matched regex?

  is_match_covered(source_file, line_number, match, "STR_CONST")
}
