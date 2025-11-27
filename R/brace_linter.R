#' Brace linter
#'
#' Perform various style checks related to placement and spacing of curly braces:
#'
#'  - Opening curly braces are never on their own line and are always followed by a newline.
#'  - Opening curly braces have a space before them.
#'  - Closing curly braces are on their own line unless they are followed by an `else`.
#'  - Closing curly braces in `if` conditions are on the same line as the corresponding `else`.
#'  - Either both or neither branch in `if`/`else` use curly braces, i.e., either both branches use `{...}` or neither
#'    does.
#'  - Function bodies are wrapped in curly braces.
#'
#' @param allow_single_line If `TRUE`, allow an open and closed curly pair on the same line.
#' @param function_bodies When to require function bodies to be wrapped in curly braces. One of
#'   - `"always"` to require braces around all function bodies, including inline functions,
#'   - `"not_inline"` to require braces when a function body does not start on the same line as its signature,
#'   - `"multi_line"` (the default) to require braces when a function definition spans multiple lines,
#'   - `"never"` to never require braces in function bodies.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "f <- function() { 1 }",
#'   linters = brace_linter()
#' )
#'
#' writeLines("if (TRUE) {\n return(1) }")
#' lint(
#'   text = "if (TRUE) {\n return(1) }",
#'   linters = brace_linter()
#' )
#'
#' # okay
#' writeLines("f <- function() {\n  1\n}")
#' lint(
#'   text = "f <- function() {\n  1\n}",
#'   linters = brace_linter()
#' )
#'
#' writeLines("if (TRUE) { \n return(1) \n}")
#' lint(
#'   text = "if (TRUE) { \n return(1) \n}",
#'   linters = brace_linter()
#' )
#'
#' # customizing using arguments
#' writeLines("if (TRUE) { return(1) }")
#' lint(
#'   text = "if (TRUE) { return(1) }",
#'   linters = brace_linter(allow_single_line = TRUE)
#' )
#' @evalRd rd_tags("brace_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#indenting>
#' - <https://style.tidyverse.org/syntax.html#if-statements>
#' @export
brace_linter <- function(allow_single_line = FALSE,
                         function_bodies = c("multi_line", "always", "not_inline", "never")) {
  function_bodies <- match.arg(function_bodies)

  xp_cond_open <- xp_and(c(
    # matching } is on same line
    if (isTRUE(allow_single_line)) {
      "(@line1 != following-sibling::OP-LEFT-BRACE/@line1)"
    },
    # double curly
    "not(
      (@line1 = parent::expr/preceding-sibling::OP-LEFT-BRACE/@line1)
      or (@line1 = following-sibling::expr/OP-LEFT-BRACE/@line1)
    )",
    # allow `(`, `,` and `%>%` on preceding line
    #
    # note that '{' is not supported in RHS call of base-R's native pipe (`|>`),
    # so no exception needs to be made for this operator
    glue("not(
      @line1 > parent::expr/preceding-sibling::*[not(self::COMMENT)][1][
        self::OP-LEFT-PAREN
        or self::OP-COMMA
        or (self::SPECIAL and ({xp_text_in_table(magrittr_pipes)}) )
      ]/@line2
    )")
  ))

  # TODO(#1103): if c_style_braces is TRUE, invert the preceding-sibling condition
  xp_open_curly <- glue("//OP-LEFT-BRACE[
    { xp_cond_open }
    and (
      not(@line1 = parent::expr/preceding-sibling::*/@line2)
      or @line1 = following-sibling::*[1][not(self::COMMENT or self::OP-RIGHT-BRACE)]/@line1
    )
  ]")

  xp_open_preceding <- "parent::expr/preceding-sibling::*[1][self::OP-RIGHT-PAREN or self::ELSE or self::REPEAT]"

  xp_paren_brace <- glue("//OP-LEFT-BRACE[
    @line1 = { xp_open_preceding }/@line1
    and @col1 = { xp_open_preceding }/@col2 + 1
  ]")

  xp_cond_closed <- xp_and(c(
    # matching { is on same line
    if (isTRUE(allow_single_line)) {
      "(@line1 != preceding-sibling::OP-LEFT-BRACE/@line1)"
    },
    # immediately followed by ",", "]" or ")"
    "not(
      @line1 = ancestor::expr/following-sibling::*[1][
        self::OP-COMMA or self::OP-RIGHT-BRACKET or self::OP-RIGHT-PAREN
      ]
        /@line1
    )",
    # double curly
    "not(
      (@line1 = parent::expr/following-sibling::OP-RIGHT-BRACE/@line1)
      or (@line1 = preceding-sibling::expr/OP-RIGHT-BRACE/@line1)
    )"
  ))

  # TODO(#1103): if c_style_braces is TRUE, skip the not(ELSE) condition
  xp_closed_curly <- glue("//OP-RIGHT-BRACE[
    { xp_cond_closed }
    and (
      (@line1 = preceding-sibling::*[1][not(self::OP-LEFT-BRACE)]/@line2)
      or (@line1 = parent::expr/following-sibling::*[not(self::COMMENT)][1][not(self::ELSE)]/@line1)
    )
  ]")

  xp_else_closed_curly <- "preceding-sibling::IF/following-sibling::expr[2]/OP-RIGHT-BRACE"
  # need to (?) repeat previous_curly_path since != will return true if there is
  #   no such node. ditto for approach with not(@line1 = ...).
  # TODO(#1103): if c_style_braces is TRUE, this needs to be @line2 + 1
  xp_else_same_line <- glue("//ELSE[{xp_else_closed_curly} and @line1 != {xp_else_closed_curly}/@line2]")

  if (function_bodies != "never") {
    xp_cond_function_brace <- switch(
      function_bodies,
      always = "1",
      multi_line = "@line1 != @line2",
      not_inline = "@line1 != expr/@line1"
    )

    xp_function_brace <- glue(
      "(//FUNCTION | //OP-LAMBDA)/parent::expr[{xp_cond_function_brace} and not(expr/OP-LEFT-BRACE)]"
    )

    msg_function_brace <- switch(
      function_bodies,
      always = "Wrap function bodies in curly braces.",
      multi_line = "Wrap multi-line function bodies in curly braces.",
      not_inline = "Wrap function bodies starting on a new line in curly braces."
    )
  }

  # if (x) { ... } else if (y) { ... } else { ... } is OK; fully exact pairing
  #   of if/else would require this to be
  #   if (x) { ... } else { if (y) { ... } else { ... } } since there's no
  #   elif operator/token in R, which is pretty unseemly
  xp_if_else_match_brace <- "
  //IF[
    following-sibling::expr[2][OP-LEFT-BRACE]
    and
      following-sibling::ELSE
        /following-sibling::expr[1][not(OP-LEFT-BRACE or IF/following-sibling::expr[2][OP-LEFT-BRACE])]
  ]

  |

  //ELSE[
    following-sibling::expr[1][OP-LEFT-BRACE]
    and preceding-sibling::IF/following-sibling::expr[2][not(OP-LEFT-BRACE)]
  ]
  "

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    lints <- list()

    lints <- c(
      lints,
      xml_nodes_to_lints(
        xml_find_all(xml, xp_open_curly),
        source_expression = source_expression,
        lint_message =
          "Opening curly braces should never go on their own line and should always be followed by a new line."
      )
    )

    lints <- c(
      lints,
      xml_nodes_to_lints(
        xml_find_all(xml, xp_paren_brace),
        source_expression = source_expression,
        lint_message = "There should be a space before an opening curly brace."
      )
    )

    lints <- c(
      lints,
      xml_nodes_to_lints(
        xml_find_all(xml, xp_closed_curly),
        source_expression = source_expression,
        lint_message =
          "Closing curly-braces should always be on their own line, unless they are followed by an else."
      )
    )

    lints <- c(
      lints,
      xml_nodes_to_lints(
        xml_find_all(xml, xp_else_same_line),
        source_expression = source_expression,
        lint_message = "`else` should come on the same line as the previous `}`."
      )
    )
    if (function_bodies != "never") {
      lints <- c(
        lints,
        xml_nodes_to_lints(
          xml_find_all(xml, xp_function_brace),
          source_expression = source_expression,
          lint_message = msg_function_brace
        )
      )
    }

    lints <- c(
      lints,
      xml_nodes_to_lints(
        xml_find_all(xml, xp_if_else_match_brace),
        source_expression = source_expression,
        lint_message = "Either both or neither branch in `if`/`else` should use curly braces."
      )
    )

    lints
  })
}
