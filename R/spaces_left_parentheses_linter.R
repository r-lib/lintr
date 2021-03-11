#' @describeIn linters Check that all left parentheses have a space before them
#' unless they are in a function call.
#' @export
spaces_left_parentheses_linter <- function() {
  Linter(function(source_file) {

    if (is.null(source_file$xml_parsed_content)) return(list())

    xml <- source_file$xml_parsed_content

    # apply the lint by requiring a gap in three cases:
    #   (1) if/while loop conditions, e.g. 'if(x>2) { }'
    #   (2) for loop conditions, e.g. 'for(i in 1:5) { }' [very similar to (1) in code but different in XML]
    #   (3) non-unary infix operators, e.g. 'x&(y | z)'

    # -2 on LHS because, when RHS matches nothing, +2 tricks the condition into returning true...
    #   while @start will always be there, the @end may not be
    if_while_cond <- "@start - 2 != preceding-sibling::*[self::IF or self::WHILE]/@end"
    for_cond <- "@start - 2 != parent::forcond/preceding-sibling::FOR/@end"

    # see infix_spaces_linter.R; preceding-sibling::* is for unary operators like -(a)
    infix_selves <- paste0("self::", names(infix_tokens), "[preceding-sibling::*]", collapse = " or ")
    # preceding-symbol::* catches (1) function definitions and (2) function calls
    infix_cond <- sprintf(
      "not(preceding-sibling::*) and (@start - 2 != parent::expr/preceding-sibling::*[%s]/@end)",
      infix_selves
    )
    xpath <- sprintf(
      "//OP-LEFT-PAREN[(%s) or (%s) or (%s)]",
      if_while_cond, for_cond, infix_cond
    )

    bad_paren <- xml2::xml_find_all(xml, xpath)

    lapply(bad_paren, xml_nodes_to_lint, source_file,
           message = "Place a space before left parenthesis, except in a function call.",
           type = "style")
  })
}
