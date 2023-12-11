#' Infix spaces linter
#'
#' Check that infix operators are surrounded by spaces. Enforces the corresponding Tidyverse style guide rule;
#'   see <https://style.tidyverse.org/syntax.html#infix-operators>.
#'
#' @param exclude_operators Character vector of operators to exclude from consideration for linting.
#'   Default is to include the following "low-precedence" operators:
#'   `+`, `-`, `~`, `>`, `>=`, `<`, `<=`, `==`, `!=`, `&`, `&&`, `|`, `||`, `<-`, `:=`, `<<-`, `->`, `->>`,
#'   `=`, `/`, `*`, and any infix operator (exclude infixes by passing `"%%"`). Note that `"="` here includes
#'   three different operators, from the parser's point of view. To lint only some of these, pass the
#'   corresponding parse tags (i.e., some of `"EQ_ASSIGN"`, `"EQ_SUB"`, and `"EQ_FORMALS"`; see
#'   [utils::getParseData()]).
#' @param allow_multiple_spaces Logical, default `TRUE`. If `FALSE`, usage like `x  =  2` will also be linted;
#'   excluded by default because such usage can sometimes be used for better code alignment, as is allowed
#'   by the style guide.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x<-1L",
#'   linters = infix_spaces_linter()
#' )
#'
#' lint(
#'   text = "1:4 %>%sum()",
#'   linters = infix_spaces_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x <- 1L",
#'   linters = infix_spaces_linter()
#' )
#'
#' lint(
#'   text = "1:4 %>% sum()",
#'   linters = infix_spaces_linter()
#' )
#'
#' code_lines <- "
#' ab     <- 1L
#' abcdef <- 2L
#' "
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = infix_spaces_linter(allow_multiple_spaces = TRUE)
#' )
#'
#' lint(
#'   text = "a||b",
#'   linters = infix_spaces_linter(exclude_operators = "||")
#' )
#'
#' lint(
#'   text = "sum(1:10, na.rm=TRUE)",
#'   linters = infix_spaces_linter(exclude_operators = "EQ_SUB")
#' )
#'
#' @evalRd rd_tags("infix_spaces_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#infix-operators>
#' @export
infix_spaces_linter <- function(exclude_operators = NULL, allow_multiple_spaces = TRUE) {
  if (allow_multiple_spaces) {
    op <- "<"
    lint_message <- "Put spaces around all infix operators."
  } else {
    op <- "!="
    lint_message <- "Put exactly one space on each side of infix operators."
  }

  infix_tokens <- infix_metadata$xml_tag_exact[
    infix_metadata$low_precedence &
      !infix_metadata$string_value %in% exclude_operators &
      # parse_tag, not xml_tag, since the former is easier for the user to discover with getParseData()
      !infix_metadata$parse_tag %in% exclude_operators
  ]

  # NB: preceding-sibling::* and not preceding-sibling::expr because
  #   of the foo(a=1) case, where the tree is <SYMBOL_SUB><EQ_SUB><expr>
  # NB: parent::*[count(expr | SYMBOL_SUB)) > 1] for the unary case, e.g. x[-1]
  #  SYMBOL_SUB for case with missing argument like alist(a =)
  # NB: the last not() disables lints inside box::use() declarations
  global_xpath <- paste0("//", infix_tokens, collapse = "|")
  xpath <- glue("({global_xpath})[
    parent::*[count(expr | SYMBOL_SUB) > 1]
    and (
      (
        @line1 = preceding-sibling::*[1]/@line2
        and @start {op} preceding-sibling::*[1]/@end + 2
      ) or (
        @line1 = following-sibling::*[1]/@line1
        and following-sibling::*[1]/@start {op} @end + 2
      )
    )
    and not(
      self::OP-SLASH[
        ancestor::expr/preceding-sibling::OP-LEFT-PAREN/preceding-sibling::expr[
          ./SYMBOL_PACKAGE[text() = 'box'] and
          ./SYMBOL_FUNCTION_CALL[text() = 'use']
        ]
      ]
    )
  ]")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "style"
    )
  })
}
