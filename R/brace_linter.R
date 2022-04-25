#' Brace linter
#'
#' Perform various style checks related to placement and spacing of curly braces:
#'
#'  - Opening curly braces are never on their own line and are always followed by a newline.
#'  - Closing curly braces are on their own line unless they are followed by an `else`.
#'  - Closing curly braces in `if` conditions are on the same line as the corresponding `else`.
#'  - Either both or neither branch in `if`/`else` use curly braces, i.e., either both branches use `{...}` or neither
#'    does.
#'  - Functions spanning multiple lines use curly braces.
#'
#' @param allow_single_line if `TRUE`, allow an open and closed curly pair on the same line.
#'
#' @evalRd rd_tags("brace_linter")
#' @seealso [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#indenting> \cr
#'   <https://style.tidyverse.org/syntax.html#if-statements>
#' @export
brace_linter <- function(allow_single_line = FALSE) {
  Linter(function(source_expression) {
    if (length(source_expression$xml_parsed_content) == 0L) {
      return(list())
    }

    lints <- list()

    xp_cond_open <- xp_and(c(
      # matching } is on same line
      if (isTRUE(allow_single_line)) {
        "(@line1 != following-sibling::OP-LEFT-BRACE/@line1)"
      },
      # double curly
      "not(
        (@line1 = parent::expr/preceding-sibling::OP-LEFT-BRACE/@line1) or
        (@line1 = following-sibling::expr/OP-LEFT-BRACE/@line1)
      )"
    ))

    # TODO (AshesITR): if c_style_braces is TRUE, invert the preceding-sibling condition
    xp_open_curly <- glue::glue("//OP-LEFT-BRACE[
      { xp_cond_open } and (
        not(@line1 = parent::expr/preceding-sibling::*/@line2) or
        @line1 = following-sibling::*[1][not(self::COMMENT)]/@line1
      )
    ]")

    lints <- c(lints, lapply(
      xml2::xml_find_all(source_expression$xml_parsed_content, xp_open_curly),
      xml_nodes_to_lint,
      source_file = source_expression,
      lint_message = paste(
        "Opening curly braces should never go on their own line and",
        "should always be followed by a new line."
      )
    ))

    xp_cond_closed <- xp_and(c(
      # matching { is on same line
      if (isTRUE(allow_single_line)) {
        "(@line1 != preceding-sibling::OP-LEFT-BRACE/@line1)"
      },
      # immediately followed by "," or ")"
      "not(@line1 = ancestor::expr/following-sibling::*[1][self::OP-COMMA or self::OP-RIGHT-PAREN]/@line1)",
      # double curly
      "not(
        (@line1 = parent::expr/following-sibling::OP-RIGHT-BRACE/@line1) or
        (@line1 = preceding-sibling::expr/OP-RIGHT-BRACE/@line1)
      )"
    ))

    # TODO (AshesITR): if c_style_braces is TRUE, skip the not(ELSE) condition
    xp_closed_curly <- glue::glue("//OP-RIGHT-BRACE[
      { xp_cond_closed } and (
        (@line1 = preceding-sibling::*/@line2) or
        (@line1 = parent::expr/following-sibling::*[1][not(self::ELSE)]/@line1)
      )
    ]")

    lints <- c(lints, lapply(
      xml2::xml_find_all(source_expression$xml_parsed_content, xp_closed_curly),
      xml_nodes_to_lint,
      source_file = source_expression,
      lint_message = paste(
        "Closing curly-braces should always be on their own line,",
        "unless they are followed by an else."
      )
    ))

    xp_else_closed_curly <- "preceding-sibling::IF/following-sibling::expr[2]/OP-RIGHT-BRACE"
    # need to (?) repeat previous_curly_path since != will return true if there is
    #   no such node. ditto for approach with not(@line1 = ...).
    # TODO (AshesITR): if c_style_braces is TRUE, this needs to be @line2 + 1
    xp_else_same_line <- glue::glue("//ELSE[{xp_else_closed_curly} and @line1 != {xp_else_closed_curly}/@line2]")

    lints <- c(lints, lapply(
      xml2::xml_find_all(source_expression$xml_parsed_content, xp_else_same_line),
      xml_nodes_to_lint,
      source_file = source_expression,
      lint_message = "`else` should come on the same line as the previous `}`."
    ))

    xp_function_brace <- "//expr[FUNCTION and @line1 != @line2 and not(expr[OP-LEFT-BRACE])]"

    lints <- c(lints, lapply(
      xml2::xml_find_all(source_expression$xml_parsed_content, xp_function_brace),
      xml_nodes_to_lint,
      source_file = source_expression,
      lint_message = "Any function spanning multiple lines should use curly braces."
    ))

    # if (x) { ... } else if (y) { ... } else { ... } is OK; fully exact pairing
    #   of if/else would require this to be
    #   if (x) { ... } else { if (y) { ... } else { ... } } since there's no
    #   elif operator/token in R, which is pretty unseemly
    xp_if_else_match_brace <- "
    //IF[
      following-sibling::expr[2][OP-LEFT-BRACE]
      and following-sibling::ELSE
          /following-sibling::expr[1][not(OP-LEFT-BRACE or IF/following-sibling::expr[2][OP-LEFT-BRACE])]
    ]

    |

    //ELSE[
      following-sibling::expr[1][OP-LEFT-BRACE]
      and preceding-sibling::IF/following-sibling::expr[2][not(OP-LEFT-BRACE)]
    ]
    "

    lints <- c(lints, lapply(
      xml2::xml_find_all(source_expression$xml_parsed_content, xp_if_else_match_brace),
      xml_nodes_to_lint,
      source_file = source_expression,
      lint_message = "Either both or neither branch in `if`/`else` should use curly braces."
    ))

    lints
  })
}
