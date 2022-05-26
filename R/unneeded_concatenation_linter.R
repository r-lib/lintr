#' Unneeded concatenation linter
#'
#' Check that the `c` function is not used without arguments nor with a single constant.
#'
#' @evalRd rd_tags("unneeded_concatenation_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unneeded_concatenation_linter <- function() {
  msg_empty <- paste(
    "Unneeded concatenation without arguments.",
    'Replace the "c" call by NULL or, whenever possible,',
    "vector() seeded with the correct type and/or length."
  )
  msg_const <- 'Unneeded concatenation of a constant. Remove the "c" call.'

  constant_nodes_in_c <- paste0("self::", c("STR_CONST", "NUM_CONST", "NULL_CONST"))
  non_constant_cond <- glue::glue("not(descendant::*[{xp_or(constant_nodes_in_c)}])")

  to_pipe_xpath <- "
    ./parent::expr/preceding-sibling::*[1][
      self::PIPE or
      self::SPECIAL[text() = '%>%']
    ]
  "
  xpath_call <- glue::glue("
    //expr[
      SYMBOL_FUNCTION_CALL[text() = 'c'] and
      not(following-sibling::expr[{non_constant_cond}]) and
      not({to_pipe_xpath}/preceding-sibling::expr[1][{non_constant_cond}])
    ]
  ")
  num_args_xpath <- "count(./following-sibling::expr)"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content
    c_calls <- xml2::xml_find_all(xml, xpath_call)
    num_args <- as.integer(!is.na(xml2::xml_find_first(c_calls, to_pipe_xpath))) +
      as.integer(xml2::xml_find_num(c_calls, num_args_xpath))
    is_unneeded <- num_args <= 1L
    c_calls <- c_calls[is_unneeded]
    num_args <- num_args[is_unneeded]
    msg <- ifelse(num_args == 0L, msg_empty, msg_const)

    xml_nodes_to_lints(
      c_calls,
      source_expression = source_expression,
      lint_message = msg
    )
  })
}

# TODO remove all these helpers once extraction_operator_linter is refactored (#1260)
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
