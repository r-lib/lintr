make_linter_from_regex <- function(regex,
                                   lint_type,
                                   lint_msg) {
  function() { # nocov: only run at namespace load time
    Linter(linter_level = "file", function(source_expression) {
      all_matches <- re_matches(
        source_expression[["file_lines"]],
        regex,
        locations = TRUE,
        global = FALSE
      )
      all_matches <- all_matches[!is.na(all_matches$start), ]
      all_matches$line_number <- as.integer(rownames(all_matches))

      matches_by_row <- split(all_matches, seq_len(nrow(all_matches)))

      lints <- lapply(matches_by_row, function(.match) {
        if (is_match_covered(.match, source_expression)) {
          return()
        }
        Lint(
          filename = source_expression[["filename"]],
          line_number = .match$line_number,
          type = lint_type,
          message = lint_msg,
          line = source_expression[["file_lines"]][[rownames(.match)]],
          ranges = list(c(.match$start, .match$end))
        )
      })
      lints[lengths(lints) > 0L]
    })
  } # nocov: ditto opening brace
}

#' Determine if a regex match is covered by an expression in a source_expression
#'
#' @param match The position where a regex match was observed.
#'   It must have the following elements: `"start"`, `"end"`, and `"line_number"`.
#' @param source_expression A source_expression.
#' @param token_type Restrict analysis to tokens of this type, for example,
#'   with `token_type = "STR_CONST"` you can check that a regex match occurs
#'   within a string.
#' @noRd
is_match_covered <- function(match, source_expression, token_type = "STR_CONST") {
  line_number <- match$line_number
  pc <- source_expression[["full_parsed_content"]]
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
