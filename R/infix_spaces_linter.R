# some metadata about infix operators on the R parse tree.
#   xml_tag gives the XML tag as returned by xmlparsedata::xml_parse_data().
#   r_string gives the operator as you would write it in R code.
# NB: this metadata is used elsewhere in lintr, e.g. spaces_left_parentheses_linter.
#   because of that, even though some rows of this table are currently unused, but
#   we keep them around because it's useful to keep this info in one place.
infix_metadata <- data.frame(stringsAsFactors = FALSE, matrix(byrow = TRUE, ncol = 2L, c(
  "OP-PLUS", "+",
  "OP-MINUS", "-",
  "OP-TILDE", "~",
  "GT", ">",
  "GE", ">=",
  "LT", "<",
  "LE", "<=",
  "EQ", "==",
  "NE", "!=",
  "AND", "&",
  "OR", "|",
  "AND2", "&&",
  "OR2", "||",
  "LEFT_ASSIGN", "<-",  # also includes := and <<-
  "RIGHT_ASSIGN", "->", # also includes ->>
  "EQ_ASSIGN", "=",
  "EQ_SUB", "=",        # in calls: foo(x = 1)
  "EQ_FORMALS", "=",    # in definitions: function(x = 1)
  "SPECIAL", "%%",
  "OP-SLASH", "/",
  "OP-STAR", "*",
  "OP-COMMA", ",",
  "OP-CARET", "^",      # also includes **
  "OP-AT", "@",
  "OP-EXCLAMATION", "!",
  "OP-COLON", ":",
  "NS_GET", "::",
  "NS_GET_INT", ":::",
  "OP-LEFT-BRACE", "{",
  "OP-LEFT-BRACKET", "[",
  "LBB", "[[",
  "OP-LEFT-PAREN", "(",
  "OP-QUESTION", "?",
  NULL
)))
names(infix_metadata) <- c("xml_tag", "string_value")
# utils::getParseData()'s designation for the tokens wouldn't be valid as XML tags
infix_metadata$parse_tag <- ifelse(
  startsWith(infix_metadata$xml_tag, "OP-"),
  quote_wrap(infix_metadata$string_value, "'"),
  infix_metadata$xml_tag
)
# treated separately because spacing rules are different for unary operators
infix_metadata$unary <- infix_metadata$xml_tag %in% c("OP-PLUS", "OP-MINUS", "OP-TILDE")
# high-precedence operators are ignored by this linter; see
#   https://style.tidyverse.org/syntax.html#infix-operators
infix_metadata$low_precedence <- infix_metadata$string_value %in% c(
  "+", "-", "~", ">", ">=", "<", "<=", "==", "!=", "&", "&&", "|", "||", "<-", "->", "=", "%%", "/", "*"
)
# comparators come up in several lints
infix_metadata$comparator <- infix_metadata$string_value %in% c("<", "<=", ">", ">=", "==", "!=")

#' Infix spaces linter
#'
#' Check that infix operators are surrounded by spaces. Enforces the corresponding Tidyverse style guide rule;
#'   see <https://style.tidyverse.org/syntax.html#infix-operators>.
#'
#' @param exclude_operators Character vector of operators to exlude from consideration for linting.
#'   Default is to include the following "low-precedence" operators:
#'   `+`, `-`, `~`, `>`, `>=`, `<`, `<=`, `==`, `!=`, `&`, `&&`, `|`, `||`, `<-`, `:=`, `<<-`, `->`, `->>`,
#'   `=`, `/`, `*`, and any infix operator (exclude infixes by passing `"%%"`). Note that `<-`, `:=`, and `<<-`
#'   are included/excluded as a group (indicated by passing `"<-"`), as are `->` and `->>` (_viz_, `"->"`),
#'   and that `=` for assignment and for setting arguments in calls are treated the same.
#' @param allow_multiple_spaces Logical, default `TRUE`. If `FALSE`, usage like `x  =  2` will also be linted;
#'   excluded by default because such usage can sometimes be used for better code alignment, as is allowed
#'   by the style guide.
#' @evalRd rd_tags("infix_spaces_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#infix-operators>
#' @export
infix_spaces_linter <- function(exclude_operators = NULL, allow_multiple_spaces = TRUE) {
  if (allow_multiple_spaces) {
    op <- "<"
    lint_message <- "Put at least one space around each side of infix operators."
  } else {
    op <- "!="
    lint_message <- "Put exactly one space on each side of infix operators."
  }
  Linter(function(source_file) {
    if (is.null(source_file$xml_parsed_content)) return(list())

    xml <- source_file$xml_parsed_content
    infix_tokens <- infix_metadata[
      infix_metadata$low_precedence & !infix_metadata$string_value %in% exclude_operators,
      "xml_tag"
    ]

    # NB: preceding-sibling::* and not preceding-sibling::expr because
    #   of the foo(a=1) case, where the tree is <SYMBOL_SUB><EQ_SUB><expr>
    # NB: position() > 1 for the unary case, e.g. x[-1]
    xpath <- glue::glue("//*[
      ({xp_or(paste0('self::', infix_tokens))})
      and position() > 1
      and (
        (
          @line1 = preceding-sibling::*[1]/@line1
          and @col1 {op} preceding-sibling::*[1]/@col2 + 2
        ) or (
          @line1 = following-sibling::*[1]/@line1
          and following-sibling::*[1]/@col1 {op} @col2 + 2
        )
      )
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)
    
    lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = lint_message,
      type = "style"
    )
  })
}
