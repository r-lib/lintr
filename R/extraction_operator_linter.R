#' @describeIn linters  Check that the `[[` operator is used when extracting a single element from
#'                      an object, not `[` (subsetting) nor `$` (interactive use).
#' @export
extraction_operator_linter <- function() {
  Linter(function(source_file) {
    tokens <- source_file[["parsed_content"]] <- filter_out_token_type(source_file[["parsed_content"]], "expr")

    lapply(
      ids_with_token(source_file, c("'$'", "'['"), fun = `%in%`),
      function(token_num) {
        if (is_dollar_extract(token_num, tokens) || is_bracket_extract(token_num, tokens)) {
          token <- with_id(source_file, token_num)
          start_col_num <- token[["col1"]]
          end_col_num <- token[["col2"]]
          line_num <- token[["line1"]]
          line <- source_file[["lines"]][[as.character(line_num)]]

          Lint(
            filename = source_file[["filename"]],
            line_number = line_num,
            column_number = start_col_num,
            type = "warning",
            message = sprintf("Use `[[` instead of `%s`  to extract an element.", token[["text"]]),
            line = line,
            ranges = list(c(start_col_num, end_col_num))
          )
        }
      }
    )
  })
}

is_dollar_extract <- function(token_line_num, tokens) {
  # A "$" that is not preceded by a ReferenceClass ".self" or R6 class "self" object.
  tokens[token_line_num, "text"] == "$" &&
    re_substitutes(tokens[token_line_num - 1L, "text"], rex(start, maybe(dot)), "") != "self"
}

is_bracket_extract <- function(token_line_num, tokens) {
  # The sequence "[" + zero or more "+" symbols + a constant + "]".
  inside_tokens <- get_tokens_in_parentheses(token_line_num, tokens)
  if (all(is.na(inside_tokens))) {
    FALSE
  } else {
    start_line <- 1L
    while (inside_tokens[start_line, "text"] == "+") {
      start_line <- start_line + 1L
    }
    inside_tokens <- inside_tokens[start_line:nrow(inside_tokens), ]
    nrow(inside_tokens) == 1L &&
      inside_tokens[1L, "token"] %in% c("STR_CONST", "NUM_CONST", "NULL_CONST")
  }
}
