#' Check that indentation is consistent
#'
#' @param indent Block indentation level, used for multi-line code blocks (`{ ... }`), function calls (`( ... )`) and
#' extractions (`[ ... ]`, `[[ ... ]]`).
#'
#' @evalRd rd_tags("indentation_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
indentation_linter <- function(indent = 2L) {
  paren_tokens <- c("OP-LEFT-PAREN", "OP-LEFT-BRACKET", "LBB")
  paren_tokens_right <- c("OP-RIGHT-PAREN", "OP-RIGHT-BRACKET", "OP-RIGHT-BRACKET")
  infix_tokens <- setdiff(infix_metadata$xml_tag, c("OP-LEFT-BRACE", "OP-COMMA", paren_tokens))

  xp_paren_blocks <- glue::glue(
    "//{paren_tokens}[
      @line1 != following-sibling::*[1]/@line1 and
      @line1 + 1 < following-sibling::{paren_tokens_right}/@line1
    ]"
  )
  xp_paren_block_ends <- glue::glue("number(following-sibling::{paren_tokens_right}/@line1)")

  xp_hanging_blocks <- glue::glue(
    "//{paren_tokens}[
      @line1 = following-sibling::*[1]/@line1 and
      not(@line1 = following-sibling::*[@line2 > @line1]/@line1) and
      @line1 < following-sibling::{paren_tokens_right}/@line1
    ]"
  )
  xp_hanging_block_ends <- glue::glue("number(following-sibling::{paren_tokens_right}/preceding-sibling::*[1]/@line2)")

  xp_operator_blocks <- paste(glue::glue("//{infix_tokens}[@line1 != following-sibling::*[1]/@line1]"), collapse = " | ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    indent_levels <- rex::re_matches(source_expression$lines, rex::rex(start, any_spaces), locations = TRUE)[, "end"]
    expected_indent_levels <- integer(length(indent_levels))
    is_hanging <- logical(length(indent_levels))

    xml <- source_expression$xml_parsed_content
    # Indentation increases by 1 for:
    #  - { } blocks that span multiple lines
    #  - ( ), [ ], or [[ ]] calls that span multiple lines
    #     + if a token follows (, a hanging indent is required until )
    #     + if there is no token following ( on the same line, a block indent is required until )
    #  - binary operators where the second arguments starts on a new line

    # 1. find block indents
    brace_blocks <- xml2::xml_find_all(xml, "//OP-LEFT-BRACE/parent::expr[@line1 + 1 < @line2]")
    for (block in brace_blocks) {
      to_indent <- seq(
        from = as.integer(xml2::xml_attr(block, "line1")) + 1L,
        to = as.integer(xml2::xml_attr(block, "line2")) - 1L
      ) - source_expression$line + 1L
      expected_indent_levels[to_indent] <- expected_indent_levels[to_indent] + 1L
    }

    for (i in seq_along(xp_paren_blocks)) {
      paren_blocks <- xml2::xml_find_all(xml, xp_paren_blocks[i])
      for (block in paren_blocks) {
        to_indent <- seq(
          from = as.integer(xml2::xml_attr(block, "line1")) + 1L,
          to = as.integer(xml2::xml_find_num(block, xp_paren_block_ends[i])) - 1L
        ) - source_expression$line + 1L
        expected_indent_levels[to_indent] <- expected_indent_levels[to_indent] + 1L
      }
    }

    operator_blocks <- xml2::xml_find_all(xml, xp_operator_blocks)
    to_indent <- xml2::xml_find_num(operator_blocks, "number(./@line1 + 1)") - source_expression$line + 1L
    expected_indent_levels[to_indent] <- expected_indent_levels[to_indent] + 1L

    expected_indent_levels <- expected_indent_levels * indent

    # 2. find hanging indents
    for (i in seq_along(xp_hanging_blocks)) {
      hanging_parens <- xml2::xml_find_all(xml, xp_hanging_blocks[i])
      for (block in hanging_parens) {
        to_indent <- seq(
          from = as.integer(xml2::xml_attr(block, "line1")) + 1L,
          to = as.integer(xml2::xml_find_num(block, xp_hanging_block_ends[i]))
        ) - source_expression$line + 1L
        expected_indent_levels[to_indent] <- as.integer(xml2::xml_attr(block, "col2"))
        is_hanging[to_indent] <- TRUE
      }
    }

    # Only lint non-empty lines if the indentation level doesn't match.
    bad <- which(indent_levels != expected_indent_levels & nzchar(source_expression$lines))
    mapply(
      function(line, actual, expected, is_hanging) {
        msg <- if (is_hanging) {
          "Hanging indent should be %d spaces but is %d spaces."
        } else {
          "Indentation should be %d spaces but is %d spaces."
        }
        Lint(
          filename = source_expression$filename,
          line_number = as.integer(names(source_expression$lines)[[line]]),
          column_number = actual,
          type = "style",
          message = sprintf(msg, expected, actual),
          line = source_expression$lines[[line]],
          ranges = list(sort(c(expected, actual)))
        )
      },
      line = bad,
      actual = indent_levels[bad],
      expected = expected_indent_levels[bad],
      is_hanging = is_hanging[bad],
      SIMPLIFY = FALSE
    )
  })
}