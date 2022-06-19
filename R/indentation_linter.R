#' Check that indentation is consistent
#'
#' @param indent Block indentation level, used for multi-line code blocks (`{ ... }`), function calls (`( ... )`) and
#'   extractions (`[ ... ]`, `[[ ... ]]`).
#' @param use_hybrid_indent Require a block indent for multi-line function calls if there are only unnamed arguments
#'   in the first line?
#'   ```r
#'   # complies with use_hybrid_indent = TRUE:
#'   map(x, f,
#'     additional_arg = 42
#'   )
#'
#'   # complies with use_hybrid_indent = FALSE:
#'   map(x, f,
#'       additional_arg = 42)
#'   ```
#'
#' @evalRd rd_tags("indentation_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
indentation_linter <- function(indent = 2L, use_hybrid_indent = TRUE) {
  paren_tokens <- c("OP-LEFT-BRACE", "OP-LEFT-PAREN", "OP-LEFT-BRACKET", "LBB")
  paren_tokens_right <- c("OP-RIGHT-BRACE", "OP-RIGHT-PAREN", "OP-RIGHT-BRACKET", "OP-RIGHT-BRACKET")
  infix_tokens <- setdiff(infix_metadata$xml_tag, c("OP-LEFT-BRACE", "OP-COMMA", paren_tokens))
  no_paren_keywords <- c("ELSE", "REPEAT")
  keyword_tokens <- c("FUNCTION", "IF", "FOR", "WHILE")

  xp_last_on_line <- "@line1 != following-sibling::*[not(self::COMMENT)][1]/@line1"

  if (use_hybrid_indent) {
    xp_is_not_hanging <- glue::glue(
      "self::*[{xp_last_on_line} or (self::OP-LEFT-PAREN and @line1 != following-sibling::EQ_SUB/@line1)]"
    )
  } else {
    xp_is_not_hanging <- glue::glue("self::*[{xp_last_on_line}]")
  }

  xp_block_ends <- paste0(
    "number(",
    paste(
      c(
        glue::glue("self::{paren_tokens}/following-sibling::{paren_tokens_right}/preceding-sibling::*[1]/@line2"),
        glue::glue("self::*[{xp_and(paste0('not(self::', paren_tokens, ')'))}]/following-sibling::SYMBOL_FUNCTION_CALL/
                      parent::expr/following-sibling::expr[1]/@line2"),
        glue::glue("self::*[
                      {xp_and(paste0('not(self::', paren_tokens, ')'))} and
                      not(following-sibling::SYMBOL_FUNCTION_CALL)
                    ]/following-sibling::*[1]/@line2")
      ),
      collapse = " | "
    ),
    ")"
  )

  xp_indent_changes <- paste(
    c(
      glue::glue("//{paren_tokens}[not(@line1 = following-sibling::expr[
                    @line2 > @line1 and
                    ({xp_or(paste0('descendant::', paren_tokens, '[', xp_last_on_line, ']'))})
                  ]/@line1)]"),
      glue::glue("//{infix_tokens}[{xp_last_on_line}]"),
      glue::glue("//{no_paren_keywords}[{xp_last_on_line}]"),
      glue::glue("//{keyword_tokens}/following-sibling::OP-RIGHT-PAREN[{xp_last_on_line}]")
    ),
    collapse = " | "
  )

  xp_multiline_string <- "//STR_CONST[@line1 < @line2]"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content
    # Indentation increases by 1 for:
    #  - { } blocks that span multiple lines
    #  - ( ), [ ], or [[ ]] calls that span multiple lines
    #     + if a token follows (, a hanging indent is required until )
    #     + if there is no token following ( on the same line, a block indent is required until )
    #  - binary operators where the second arguments starts on a new line

    indent_levels <- rex::re_matches(source_expression$lines, rex::rex(start, any_spaces), locations = TRUE)[, "end"]
    expected_indent_levels <- integer(length(indent_levels))
    is_hanging <- logical(length(indent_levels))

    indent_changes <- xml2::xml_find_all(xml, xp_indent_changes)
    for (change in indent_changes) {
      change_starts_hanging <- length(xml2::xml_find_first(change, xp_is_not_hanging)) == 0L
      change_begin <- as.integer(xml2::xml_attr(change, "line1")) + 1L
      change_end <- xml2::xml_find_num(change, xp_block_ends)
      if (change_begin <= change_end) {
        to_indent <- seq(from = change_begin, to = change_end) - source_expression$line + 1L
        if (change_starts_hanging) {
          expected_indent_levels[to_indent] <- as.integer(xml2::xml_attr(change, "col2"))
          is_hanging[to_indent] <- TRUE
        } else {
          expected_indent_levels[to_indent] <- expected_indent_levels[to_indent] + indent
          is_hanging[to_indent] <- FALSE
        }
      }
    }

    in_str_const <- logical(length(indent_levels))
    multiline_strings <- xml2::xml_find_all(xml, xp_multiline_string)
    for (string in multiline_strings) {
      is_in_str <- seq(
        from = as.integer(xml2::xml_attr(string, "line1")) + 1L,
        to = as.integer(xml2::xml_attr(string, "line2"))
      ) - source_expression$line + 1L
      in_str_const[is_in_str] <- TRUE
    }

    # Only lint non-empty lines if the indentation level doesn't match.
    bad_lines <- which(indent_levels != expected_indent_levels & nzchar(source_expression$lines) & !in_str_const)
    if (length(bad_lines)) {
      lint_messages <- sprintf(
        "%s should be %d spaces but is %d spaces.",
        ifelse(is_hanging[bad_lines], "Hanging indent", "Indentation"),
        expected_indent_levels[bad_lines],
        indent_levels[bad_lines]
      )
      lint_lines <- unname(as.integer(names(source_expression$lines)[bad_lines]))
      lint_ranges <- cbind(
        pmin(expected_indent_levels[bad_lines] + 1L, indent_levels[bad_lines]),
        pmax(expected_indent_levels[bad_lines], indent_levels[bad_lines])
      )
      Map(
        Lint,
        filename = source_expression$filename,
        line_number = lint_lines,
        column_number = indent_levels[bad_lines],
        type = "style",
        message = lint_messages,
        line = unname(source_expression$lines[bad_lines]),
        ranges = apply(lint_ranges, 1L, list, simplify = FALSE)
      )
    } else {
      list()
    }
  })
}
