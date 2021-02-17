#' @describeIn linters  Check that the \code{c} function is not used without arguments nor with a
#'                      single constant.
#' @export
unneeded_concatenation_linter <- function() {
  Linter(function(source_file) {
    tokens <- source_file[["parsed_content"]] <-
      filter_out_token_type(source_file[["parsed_content"]], "expr")
    msg_empty <- "Unneeded concatenation without arguments. Replace the \"c\" call by NULL or vector()."
    msg_const <- "Unneeded concatenation of a constant. Remove the \"c\" call."
    lapply(
      ids_with_token(source_file, "SYMBOL_FUNCTION_CALL"),
      function(token_num) {
        num_args <- get_num_concat_args(token_num, tokens)
        if (num_args == 0L || num_args == 1L) {
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
            message = if (num_args) msg_const else msg_empty,
            line = line,
            ranges = list(c(start_col_num, end_col_num))
          )
        }
      }
    )
  })
}

get_num_concat_args <- function(token_num, tokens) {
  # Detect the sequence "c" + "(" + optional constant + ")" and return a number:
  #   -1 if not a concatenation call
  #    0 if a concatenation without arguments
  #    1 if a concatenation with a single constant argument
  #    2 if a concatenation in other cases
  open_paren_num <- token_num + 1L
  if (tokens[token_num, "text"] == "c" &&
    tokens[open_paren_num, "token"] == "'('") {
    token_args <- get_tokens_in_parentheses(open_paren_num, tokens)
    preceding_pipe <- check_for_pipe(token_num, tokens)
    num_token_args <- nrow(token_args)
    if (!num_token_args) {
      0L + preceding_pipe
    } else if (num_token_args == 1L) {
      if (token_args[1L, "token"] %in% c("STR_CONST", "NUM_CONST", "NULL_CONST")) {
        1L + preceding_pipe
      } else {
        2L
      }
    } else {
      2L
    }
  } else {
    -1L
  }
}

get_tokens_in_parentheses <- function(open_paren_line_num, tokens) {
  # Return the tokens enclosed by the opening parenthesis/bracket at the given line, or NA.
  open_paren_token <- tokens[open_paren_line_num, ]
  open_paren_text <- open_paren_token[["text"]]
  close_paren_token <- tail(get_sibling_tokens(open_paren_token, tokens), 1L)
  close_paren_text <- close_paren_token[["text"]]
  close_paren_line_num <- which(rownames(tokens) == rownames(close_paren_token))
  if (are_matching_parentheses(open_paren_text, close_paren_text)) {
    range <- if (open_paren_line_num + 1L == close_paren_line_num) {
      integer()
    } else {
      (open_paren_line_num + 1L):(close_paren_line_num - 1L)
    }
    tokens[range, ]
  } else {
    NA
  }
}

are_matching_parentheses <- function(open_paren_text, close_paren_text) {
  isTRUE(
    match(open_paren_text, c("(", "{", "[", "[[")) ==
      match(close_paren_text, c(")", "}", "]", "]]"))
  )
}

get_sibling_tokens <- function(child, tokens) {
  # Get all siblings of the given child token (i.e. that have the same parent id)
  tokens[tokens[, "parent"] == child[["parent"]], ]
}


filter_out_token_type <- function(tokens, type) {
  tokens[tokens[["token"]] != type, ]
}


check_for_pipe <- function(token_num, tokens) {
  # Checks for concatenation in a magrittr pipeline. If found the argument
  # count should be incremented by one, thanks to magrittr's implicit first
  # argument.
  if (token_num < 2) {
    0L
  } else {
    as.integer(tokens$text[token_num - 1] == "%>%")
  }
}
