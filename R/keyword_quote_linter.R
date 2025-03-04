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
#'   text = 'data.frame("a" = 1)',
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
#'   text = "my_list$`a b`",
#'   linters = keyword_quote_linter()
#' )
#'
#' @evalRd rd_tags("keyword_quote_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
keyword_quote_linter <- function() {
  # Check if a string could be assigned as an R variable.
  #
  # See [make.names()] for the description of syntactically valid names in R.
  is_valid_r_name <- function(x) make.names(x) == x

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
  parent::expr
    /*[(self::SYMBOL_SUB or self::STR_CONST) and {quote_cond}]
  ")

  # also exclude $ or @, which are handled below
  assignment_candidate_cond <- "
    not(OP-DOLLAR or OP-AT)
    and (STR_CONST or SYMBOL[starts-with(text(), '`')])
  "
  assignment_xpath <- glue("
  (//EQ_ASSIGN | //LEFT_ASSIGN[text() != ':='])
    /preceding-sibling::expr[{ assignment_candidate_cond }]
  | //RIGHT_ASSIGN/following-sibling::expr[{ assignment_candidate_cond }]
  ")

  extraction_xpath <- "
    (//OP-DOLLAR | //OP-AT)/following-sibling::STR_CONST
    | //OP-DOLLAR/following-sibling::SYMBOL[starts-with(text(), '`')]
    | //OP-AT/following-sibling::SLOT[starts-with(text(), '`')]
  "

  no_quote_msg <- "Use backticks to create non-syntactic names, not quotes."
  clarification <- "i.e., if the name is not a valid R symbol (see ?make.names)."

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    xml_calls <- source_expression$xml_find_function_calls(NULL)

    call_arg_expr <- xml_find_all(xml_calls, call_arg_xpath)

    invalid_call_quoting <- is_valid_r_name(get_r_string(call_arg_expr))

    call_arg_lints <- xml_nodes_to_lints(
      call_arg_expr[invalid_call_quoting],
      source_expression = source_expression,
      lint_message = paste("Only quote named arguments to functions if necessary,", clarification),
      type = "warning"
    )

    assignment_expr <- xml_find_all(xml, assignment_xpath)

    invalid_assignment_quoting <- is_valid_r_name(get_r_string(assignment_expr))
    # NB: XPath is such that there is exactly 1 node per match, making xml_children() ideal.
    #   xml_child() gets it wrong for 0 (an error) and >1 match.
    assignment_to_string <- xml_name(xml_children(assignment_expr)) == "STR_CONST"

    string_assignment_lints <- xml_nodes_to_lints(
      assignment_expr[assignment_to_string & !invalid_assignment_quoting],
      source_expression = source_expression,
      lint_message = no_quote_msg,
      type = "warning"
    )

    assignment_lints <- xml_nodes_to_lints(
      assignment_expr[invalid_assignment_quoting],
      source_expression = source_expression,
      lint_message = paste("Only quote targets of assignment if necessary,", clarification),
      type = "warning"
    )

    extraction_expr <- xml_find_all(xml, extraction_xpath)

    invalid_extraction_quoting <- is_valid_r_name(get_r_string(extraction_expr))
    extraction_of_string <- xml_name(extraction_expr) == "STR_CONST"

    string_extraction_lints <- xml_nodes_to_lints(
      extraction_expr[extraction_of_string & !invalid_extraction_quoting],
      source_expression = source_expression,
      lint_message = no_quote_msg,
      type = "warning"
    )

    extraction_expr <- extraction_expr[invalid_extraction_quoting]
    extractor <- xml_find_chr(extraction_expr, "string(preceding-sibling::*[1])")
    gen_extractor <- ifelse(extractor == "$", "[[", "slot()")

    extraction_lints <- xml_nodes_to_lints(
      extraction_expr,
      source_expression = source_expression,
      lint_message = paste(
        "Only quote targets of extraction with", extractor, "if necessary,", clarification,
        "Use backticks to create non-syntactic names, or use", gen_extractor, "to extract by string."
      ),
      type = "warning"
    )

    c(call_arg_lints, string_assignment_lints, assignment_lints, string_extraction_lints, extraction_lints)
  })
}
