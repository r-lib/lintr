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
  # NB: sQuote(x, "'") doesn't work on older R versions
  paste0("'", infix_metadata$string_value, "'"),
  infix_metadata$xml_tag
)
# treated separately because spacing rules are different for unary operators
infix_metadata$unary <- infix_metadata$xml_tag %in% c("OP-PLUS", "OP-MINUS", "OP-TILDE")
# high-precedence operators are ignored by this linter; see
#   https://style.tidyverse.org/syntax.html#infix-operators
infix_metadata$low_precedence <- infix_metadata$string_value %in% c(
  "+", "-", "~", ">", ">=", "<", "<=", "==", "!=", "&", "&&", "|", "||", "<-", "->", "=", "%%", "/", "*"
)

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
#' @evalRd rd_tags("infix_spaces_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
infix_spaces_linter <- function(exclude_operators = NULL) {
  Linter(function(source_file) {
    infix_tokens <- infix_metadata[
      with(infix_metadata, low_precedence & !string_value %in% exclude_operators),
      "parse_tag"
    ]
    lapply(
      ids_with_token(source_file, infix_tokens, fun = `%in%`),
      function(id) {
        parsed <- with_id(source_file, id)

        line <- source_file$lines[as.character(parsed$line1)]

        around_operator <- substr(line, parsed$col1 - 1L, parsed$col2 + 1L)
        non_space_before <- re_matches(around_operator, rex(start, non_space))
        newline_after <- unname(nchar(line)) %==% parsed$col2
        non_space_after <- re_matches(around_operator, rex(non_space, end))

        if (non_space_before || (!newline_after && non_space_after)) {

          # we only should check spacing if the operator is infix,
          # which only happens if there is more than one sibling
          is_infix <- length(siblings(source_file$parsed_content, parsed$id, 1)) > 1L

          start <- end <- parsed$col1

          if (is_infix) {
            if (non_space_before) {
              start <- parsed$col1 - 1L
            }
            if (non_space_after) {
              end <- parsed$col2 + 1L
            }

            Lint(
              filename = source_file$filename,
              line_number = parsed$line1,
              column_number = parsed$col1,
              type = "style",
              message = "Put spaces around all infix operators.",
              line = line,
              ranges = list(c(start, end))
            )
          }
        }
      })
  })
}
