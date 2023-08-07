#' Block unnecessary quoting in calls
#'
#' Any valid symbol can be used as a keyword argument to an R function call.
#'   Sometimes, it is necessary to quote (or backtick) an argument that is
#'   not an otherwise valid symbol (e.g. creating a vector whose names have
#'   spaces); besides this edge case, quoting should not be done.
#'
#' The most common source of violation for this is creating named vectors,
#'   lists, or data.frame-alikes, but it can be observed in other calls as well.
#'
#' Similar reasoning applies to extractions with `$` or `@`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'data.frame("a" = 1)',,
#'   linters = keyword_quote_linter()
#' )
#'
#' lint(
#'   text = "data.frame(`a` = 1)",
#'   linters = keyword_quote_linter()
#' )
#'
#' lint(
#'   text = 'my_list$"key"',
#'   linters = keyword_quote_linter()
#' )
#'
#' lint(
#'   text = 's4obj@"key"',
#'   linters = keyword_quote_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "data.frame(`a b` = 1)",
#'   linters = keyword_quote_linter()
#' )
#'
#' lint(
#'   text = 'my_list$`a b`',
#'   linters = keyword_quote_linter()
#' )
#'
#' @evalRd rd_tags("keyword_quote_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
# TODO(michaelchirico): offer a stricter version of this that
#   requires backticks to be used for non-syntactic names (i.e., not quotes).
#   Here are the relevant xpaths:
#   //expr[expr[SYMBOL_FUNCTION_CALL]]/SYMBOL_SUB[starts-with(text(), '`')]
#   //expr[expr[SYMBOL_FUNCTION_CALL]]/STR_CONST[{is_quoted(text())}]
keyword_quote_linter <- function() {
  # NB: xml2 uses xpath 1.0 which doesn't support matches() for regex, so we
  #   have to jump out of xpath to complete this lint.
  # It's also a bit tough to get the escaping through R and then xpath to
  #   work as intended, hence the rather verbose declaration here.
  quote_cond <- xp_or(
    "starts-with(text(), '\"')",
    "starts-with(text(), '`')",
    'starts-with(text(), "\'")'
  )
  # SYMBOL_SUB for backticks, STR_CONST for quoted names
  call_arg_xpath <- glue("
  //SYMBOL_FUNCTION_CALL
    /parent::expr
    /parent::expr
    /*[(self::SYMBOL_SUB or self::STR_CONST) and {quote_cond}]
  ")

  # also exclude $ or @, which are handled below
  assignment_xpath <- "
  (//EQ_ASSIGN | //LEFT_ASSIGN[text() != ':='])
    /preceding-sibling::expr[
      not(OP-DOLLAR or OP-AT)
      and (STR_CONST or SYMBOL[starts-with(text(), '`')])
    ]
  "

  extraction_xpath <- "
    (//OP-DOLLAR | //OP-AT)/following-sibling::STR_CONST
    | //OP-DOLLAR/following-sibling::SYMBOL[starts-with(text(), '`')]
    | //OP-AT/following-sibling::SLOT[starts-with(text(), '`')]
  "

  return(Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    call_arg_expr <- xml_find_all(xml, call_arg_xpath)

    invalid_call_quoting <- is_valid_r_name(get_r_string(call_arg_expr))

    call_arg_lints <- xml_nodes_to_lints(
      call_arg_expr[invalid_call_quoting],
      source_expression = source_expression,
      lint_message =
        "Only quote named arguments to functions if necessary, i.e., the name is not a valid R symbol (see ?make.names).",
      type = "warning"
    )

    assignment_expr <- xml_find_all(xml, assignment_xpath)

    invalid_assignment_quoting <- is_valid_r_name(get_r_string(assignment_expr), no_quote = TRUE)

    assignment_lints <- xml_nodes_to_lints(
      assignment_expr[invalid_assignment_quoting],
      source_expression = source_expression,
      lint_message = paste(
        "Only quote targets of assignment if necessary, i.e., the name is not a valid R symbol (see ?make.names).",
        "If necessary, use backticks to create non-syntactic names, not quotes."
      ),
      type = "warning"
    )

    extraction_expr <- xml_find_all(xml, extraction_xpath)

    invalid_extraction_quoting <-
      is_valid_r_name(get_r_string(extraction_expr), no_quote = TRUE)

    extraction_expr <- extraction_expr[invalid_extraction_quoting]
    extractor <- xml_find_chr(extraction_expr, "string(preceding-sibling::*[1])")
    gen_extractor <- ifelse(extractor == "$", "[[", "slot()")

    extraction_lints <- xml_nodes_to_lints(
      extraction_expr[invalid_extraction_quoting],
      source_expression = source_expression,
      lint_message = paste(
        "Only quote targets of extraction with", extractor, "if necessary, i.e.,",
        "the name is not a valid R symbol (see ?make.names).",
        "If necessary, use backticks to create non-syntactic names, not quotes,",
        "or use", gen_extractor, "to extract by string."
      ),
      type = "warning"
    )

    return(c(call_arg_lints, assignment_lints, extraction_lints))
  }))
}

# from ?Reserved
r_reserved_words <- c(
  "if", "else", "repeat", "while", "function", "for", "in", "next", "break",
  "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA",
  "NA_integer_", "NA_real_", "NA_complex_", "NA_character_"
)

#' Check if a string could be assigned as an R variable.
#'
#'   considered valid, i.e., anything wrapped in '' or "" is linted.
#'
#' See [make.names()] for the description of syntactically valid names in R.
#'   we could also replace this with `make.names(x) == x`
#' @noRd
is_valid_r_name <- function(x, no_quote = FALSE) {
  if (no_quote) {
    bad_quote <- !startsWith(x, "`")
  } else {
    bad_quote <- FALSE
  }
  is_valid_symbol <- grepl("^([a-zA-Z][a-zA-Z0-9._]*|[.]|[.][a-zA-Z._][a-zA-Z0-9._]*)$", x)
  return(bad_quote | (is_valid_symbol & !(x %in% r_reserved_words)))
}
