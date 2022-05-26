#' Spaces before parentheses linter
#'
#' Check that all left parentheses have a space before them unless they are in a function call.
#'
#' @evalRd rd_tags("spaces_left_parentheses_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#parentheses>
#' @export
spaces_left_parentheses_linter <- function() {
  file_level_xpath <- "//OP-LEFT-PAREN[@start - 1 = ancestor::expr/preceding-sibling::OP-SEMICOLON/@end]"

  # apply the lint by requiring a gap in three cases:
  #   (1) if/while loop conditions, e.g. 'if(x>2) { }', including 'else('
  #   (2) for loop conditions, e.g. 'for(i in 1:5) { }' [very similar to (1) in code but different in XML]
  #   (3) non-unary infix operators, e.g. 'x&(y | z)', and including commas, braces, e.g. 'c(a,(a+b))'

  # -1 on LHS because, when RHS matches nothing, +1 tricks the condition into returning true...
  #   while @start will always be there, the @end may not be.
  if_while_cond <- "@start - 1 = preceding-sibling::*[self::IF or self::WHILE]/@end"
  for_cond <- "@start - 1 = parent::forcond/preceding-sibling::FOR/@end"

  # see infix_spaces_linter.R; preceding-sibling::* is needed for unary operators where '-(a)' is ok
  unary_nodes <- infix_metadata[infix_metadata$unary, "xml_tag"]
  unary_selves <- paste0("self::", unary_nodes, "[preceding-sibling::*]", collapse = " or ")
  binary_nodes <- c(
    infix_metadata[infix_metadata$low_precedence & !infix_metadata$unary, "xml_tag"],
    "OP-COMMA", "OP-LEFT-BRACE", "ELSE", "IN"
  )
  binary_selves <- paste0("self::", binary_nodes, collapse = " or ")
  # preceding-symbol::* catches (1) function definitions and (2) function calls
  # ancestor::expr needed for nested RHS expressions, e.g. 'y1<-(abs(yn)>90)*1'
  infix_cond <- sprintf(
    "not(preceding-sibling::*) and (@start - 1 = ancestor::expr/preceding-sibling::*[%s]/@end)",
    paste(unary_selves, "or", binary_selves)
  )
  expression_level_xpath <- sprintf(
    "//OP-LEFT-PAREN[(%s) or (%s) or (%s)]",
    if_while_cond, for_cond, infix_cond
  )

  Linter(function(source_expression) {

    # else/if structure for trees that are missing XML content (both global & local)
    if (is_lint_level(source_expression, "file")) {
      # 'x = 1;(x + 2)' can't be detected from the expression-level tree
      xml <- source_expression$full_xml_parsed_content
      xpath <- file_level_xpath
    } else {

      xml <- source_expression$xml_parsed_content
      xpath <- expression_level_xpath
    }

    bad_paren <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_paren,
      source_expression,
      lint_message = "Place a space before left parenthesis, except in a function call.",
      type = "style"
    )
  })
}
