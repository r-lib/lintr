#' Check that indentation is consistent
#'
#' @param indent Number of spaces, that a code block should be indented by relative to its parent code block.
#'   Used for multi-line code blocks (`{ ... }`), function calls (`( ... )`) and extractions (`[ ... ]`, `[[ ... ]]`).
#'   Defaults to 2.
#' @param hanging_indent_style Indentation style for multi-line function calls with arguments in their first line.
#'   Defaults to tidyverse style, i.e. a block indent is used if the function call terminates with `)` on a separate
#'   line and a hanging indent if not.
#'   Note that function multi-line function calls without arguments on their first line will always be expected to have
#'   block-indented arguments.
#'   If `hanging_indent_style` is `"tidy"`, multi-line function definitions are expected to be double-indented if the
#'   first line of the function definition contains no arguments and the closing parenthesis is not on its own line.
#'
#'   ```r
#'   # complies to any style
#'   map(
#'     x,
#'     f,
#'     additional_arg = 42
#'   )
#'
#'   # complies to "tidy" and "never"
#'   map(x, f,
#'     additional_arg = 42
#'   )
#'
#'   # complies to "always"
#'   map(x, f,
#'       additional_arg = 42
#'   )
#'
#'   # complies to "tidy" and "always"
#'   map(x, f,
#'       additional_arg = 42)
#'
#'   # complies to "never"
#'   map(x, f,
#'     additional_arg = 42)
#'
#'   # complies to "tidy"
#'   function(
#'       a,
#'       b) {
#'     # body
#'   }
#'   ```
#'
#' @examples
#' # will produce lints
#' code_lines <- "if (TRUE) {\n1 + 1\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = indentation_linter()
#' )
#'
#' code_lines <- "if (TRUE) {\n    1 + 1\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = indentation_linter()
#' )
#'
#' code_lines <- "map(x, f,\n  additional_arg = 42\n)"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = indentation_linter(hanging_indent_style = "always")
#' )
#'
#' code_lines <- "map(x, f,\n    additional_arg = 42)"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = indentation_linter(hanging_indent_style = "never")
#' )
#'
#' # okay
#' code_lines <- "map(x, f,\n  additional_arg = 42\n)"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = indentation_linter()
#' )
#'
#' code_lines <- "if (TRUE) {\n    1 + 1\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = indentation_linter(indent = 4)
#' )
#'
#' @evalRd rd_tags("indentation_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#indenting>
#' - <https://style.tidyverse.org/functions.html#long-lines-1>
#'
#' @export
indentation_linter <- function(indent = 2L, hanging_indent_style = c("tidy", "always", "never")) {
  paren_tokens_left <- c("OP-LEFT-BRACE", "OP-LEFT-PAREN", "OP-LEFT-BRACKET", "LBB")
  paren_tokens_right <- c("OP-RIGHT-BRACE", "OP-RIGHT-PAREN", "OP-RIGHT-BRACKET", "OP-RIGHT-BRACKET")
  infix_tokens <- setdiff(infix_metadata$xml_tag, c("OP-LEFT-BRACE", "OP-COMMA", paren_tokens_left))
  no_paren_keywords <- c("ELSE", "REPEAT")
  keyword_tokens <- c("FUNCTION", "IF", "FOR", "WHILE")

  xp_last_on_line <- "@line1 != following-sibling::*[not(self::COMMENT)][1]/@line1"

  hanging_indent_style <- match.arg(hanging_indent_style)

  if (hanging_indent_style == "tidy") {
    find_indent_type <- build_indentation_style_tidy()
  } else if (hanging_indent_style == "always") {
    find_indent_type <- build_indentation_style_always()
  } else { # "never"
    find_indent_type <- function(change) {
      "block"
    }
  }

  xp_block_ends <- paste0(
    "number(",
    paste(
      c(
        glue::glue("self::{paren_tokens_left}/following-sibling::{paren_tokens_right}/preceding-sibling::*[1]/@line2"),
        glue::glue("self::*[{xp_and(paste0('not(self::', paren_tokens_left, ')'))}]
                      /following-sibling::SYMBOL_FUNCTION_CALL/parent::expr/following-sibling::expr[1]/@line2"),
        glue::glue("self::*[
                      {xp_and(paste0('not(self::', paren_tokens_left, ')'))} and
                      not(following-sibling::SYMBOL_FUNCTION_CALL)
                    ]/following-sibling::*[not(self::COMMENT)][1]/@line2")
      ),
      collapse = " | "
    ),
    ")"
  )

  xp_indent_changes <- paste(
    c(
      glue::glue("//{paren_tokens_left}[not(@line1 = following-sibling::expr[
                    @line2 > @line1 and
                    ({xp_or(paste0('descendant::', paren_tokens_left, '[', xp_last_on_line, ']'))})
                  ]/@line1)]"),
      glue::glue("//{infix_tokens}[{xp_last_on_line}]"),
      glue::glue("//{no_paren_keywords}[{xp_last_on_line}]"),
      glue::glue("//{keyword_tokens}/following-sibling::OP-RIGHT-PAREN[
                    {xp_last_on_line} and
                    not(following-sibling::expr[1][OP-LEFT-BRACE])
                  ]")
    ),
    collapse = " | "
  )

  xp_multiline_string <- "//STR_CONST[@line1 < @line2]"

  Linter(function(source_expression) {
    # must run on file level because a line can contain multiple expressions, losing indentation information, e.g.
    #
    #> fun(
    #    a) # comment
    #
    # will have "# comment" as a separate expression
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content
    # Indentation increases by 1 for:
    #  - { } blocks that span multiple lines
    #  - ( ), [ ], or [[ ]] calls that span multiple lines
    #     + if a token follows (, a hanging indent is required until )
    #     + if there is no token following ( on the same line, a block indent is required until )
    #  - binary operators where the second arguments starts on a new line

    indent_levels <- rex::re_matches(
      source_expression$file_lines,
      rex::rex(start, any_spaces), locations = TRUE
    )[, "end"]
    expected_indent_levels <- integer(length(indent_levels))
    is_hanging <- logical(length(indent_levels))

    indent_changes <- xml2::xml_find_all(xml, xp_indent_changes)
    for (change in indent_changes) {
      change_type <- find_indent_type(change)
      change_begin <- as.integer(xml2::xml_attr(change, "line1")) + 1L
      change_end <- xml2::xml_find_num(change, xp_block_ends)
      if (change_begin <= change_end) {
        to_indent <- seq(from = change_begin, to = change_end)
        if (change_type == "hanging") {
          expected_indent_levels[to_indent] <- as.integer(xml2::xml_attr(change, "col2"))
          is_hanging[to_indent] <- TRUE
        } else { # block or double
          if (change_type == "double") {
            expected_indent_levels[to_indent] <- expected_indent_levels[to_indent] + 2L * indent
          } else {
            expected_indent_levels[to_indent] <- expected_indent_levels[to_indent] + indent
          }
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
      )
      in_str_const[is_in_str] <- TRUE
    }

    # Only lint non-empty lines if the indentation level doesn't match.
    bad_lines <- which(indent_levels != expected_indent_levels &
                         nzchar(trimws(source_expression$file_lines)) &
                         !in_str_const)
    if (length(bad_lines)) {
      # Suppress consecutive lints with the same indentation difference, to not generate an excessive number of lints
      is_consecutive_lint <- c(FALSE, diff(bad_lines) == 1L)
      indent_diff <- expected_indent_levels[bad_lines] - indent_levels[bad_lines]
      is_same_diff <- c(FALSE, diff(indent_diff) == 0L)

      bad_lines <- bad_lines[!(is_consecutive_lint & is_same_diff)]

      lint_messages <- sprintf(
        "%s should be %d spaces but is %d spaces.",
        ifelse(is_hanging[bad_lines], "Hanging indent", "Indentation"),
        expected_indent_levels[bad_lines],
        indent_levels[bad_lines]
      )
      lint_lines <- unname(as.integer(names(source_expression$file_lines)[bad_lines]))
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
        line = unname(source_expression$file_lines[bad_lines]),
        ranges = apply(lint_ranges, 1L, list, simplify = FALSE)
      )
    } else {
      list()
    }
  })
}
# nolint end

build_indentation_style_tidy <- function() {
  paren_tokens_left <- c("OP-LEFT-BRACE", "OP-LEFT-PAREN", "OP-LEFT-BRACKET", "LBB")
  paren_tokens_right <- c("OP-RIGHT-BRACE", "OP-RIGHT-PAREN", "OP-RIGHT-BRACKET", "OP-RIGHT-BRACKET")
  xp_last_on_line <- "@line1 != following-sibling::*[not(self::COMMENT)][1]/@line1"

  # double indent is tidyverse style for function definitions
  # triggered only if the closing parenthesis of the function definition is not on its own line and the opening
  # parenthesis has no arguments behind it.
  # this allows both of these styles:
  #
  #> function(
  #>     a,
  #>     b) {
  #>   body
  #> }
  #
  #> function(
  #>   a,
  #>   b
  #> ) {
  #>   body
  #> }
  xp_is_double_indent <- "
    parent::expr[FUNCTION and not(@line1 = SYMBOL_FORMALS/@line1)]
      /OP-RIGHT-PAREN[@line1 = preceding-sibling::*[not(self::COMMENT)][1]/@line2]
  "
  xp_is_not_hanging <- paste(
    c(
      glue::glue(
        "self::{paren_tokens_left}/following-sibling::{paren_tokens_right}[@line1 > preceding-sibling::*[1]/@line2]"
      ),
      glue::glue("self::*[{xp_and(paste0('not(self::', paren_tokens_left, ')'))} and {xp_last_on_line}]")
    ),
    collapse = " | "
  )

  function(change) {
    if (length(xml2::xml_find_first(change, xp_is_double_indent)) > 0L) {
      "double"
    } else if (length(xml2::xml_find_first(change, xp_is_not_hanging)) == 0L) {
      "hanging"
    } else {
      "block"
    }
  }
}

build_indentation_style_always <- function() {
  xp_is_not_hanging <- "self::*[@line1 != following-sibling::*[not(self::COMMENT)][1]/@line1]"

  function(change) {
    if (length(xml2::xml_find_first(change, xp_is_not_hanging)) == 0L) {
      "hanging"
    } else {
      "block"
    }
  }
}
