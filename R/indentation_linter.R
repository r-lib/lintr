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
#' @param assignment_as_infix Treat `<-` as a regular (i.e. left-associative) infix operator?
#'   This means, that infix operators on the right hand side of an assignment do not trigger a second level of
#'   indentation:
#'   ```r
#'   # complies to any style
#'   variable <- a %+%
#'     b %+%
#'     c
#'
#'   # complies to assignment_as_infix = TRUE
#'   variable <-
#'     a %+%
#'     b %+%
#'     c
#'
#'   # complies to assignment_as_infix = FALSE
#'   variable <-
#'     a %+%
#'       b %+%
#'       c
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
indentation_linter <- function(indent = 2L, hanging_indent_style = c("tidy", "always", "never"),
                               assignment_as_infix = TRUE) {
  paren_tokens_left <- c("OP-LEFT-BRACE", "OP-LEFT-PAREN", "OP-LEFT-BRACKET", "LBB")
  paren_tokens_right <- c("OP-RIGHT-BRACE", "OP-RIGHT-PAREN", "OP-RIGHT-BRACKET", "OP-RIGHT-BRACKET")
  infix_tokens <- setdiff(infix_metadata$xml_tag, c("OP-LEFT-BRACE", "OP-COMMA", paren_tokens_left))
  no_paren_keywords <- c("ELSE", "REPEAT")
  keyword_tokens <- c("FUNCTION", "OP-LAMBDA", "IF", "FOR", "WHILE")

  xp_last_on_line <- "@line1 != following-sibling::*[not(self::COMMENT)][1]/@line1"

  hanging_indent_style <- match.arg(hanging_indent_style)

  find_indent_type <- switch(hanging_indent_style,
    tidy = build_indentation_style_tidy(),
    always = build_indentation_style_always(),
    never = function(change) "block"
  )

  if (isTRUE(assignment_as_infix)) {
    suppressing_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN", "EQ_SUB", "EQ_FORMALS")
    xp_suppress <- glue("preceding-sibling::{suppressing_tokens}[{xp_last_on_line}]")

    restoring_tokens <- c("expr[SYMBOL_FUNCTION_CALL]", "OP-LEFT-BRACE")
    xp_restore <- glue("preceding-sibling::{restoring_tokens}")

    # match the first ancestor expr that is either
    #  * a suppressing token (<- or =) or
    #  * a restoring token (braces or a function call)
    # suppress the indent if the matched ancestor is a suppressing token
    infix_condition <- glue("
      and not(ancestor::expr[{xp_or(c(xp_suppress, xp_restore))}][1][{xp_or(xp_suppress)}])
    ")
  } else {
    infix_condition <- ""
  }

  xp_block_ends <- paste0(
    "number(",
    paste(
      c(
        glue("self::{paren_tokens_left}/following-sibling::{paren_tokens_right}/preceding-sibling::*[1]/@line2"),
        glue("
          self::*[{xp_and(paste0('not(self::', paren_tokens_left, ')'))}]
            /following-sibling::SYMBOL_FUNCTION_CALL
            /parent::expr
            /following-sibling::expr[1]
            /@line2
        "),
        glue("
          self::*[
            {xp_and(paste0('not(self::', paren_tokens_left, ')'))}
            and not(following-sibling::SYMBOL_FUNCTION_CALL)
          ]
            /following-sibling::*[not(self::COMMENT)][1]
            /@line2
        ")
      ),
      collapse = " | "
    ),
    ")"
  )

  global_nodes <- function(nodes) paste0("//", nodes, collapse = "|")
  xp_indent_changes <- paste(
    c(
      glue("//{paren_tokens_left}[not(
        @line1 = following-sibling::expr[
          @line2 > @line1 and
          ({xp_or(paste0('descendant::', paren_tokens_left, '[', xp_last_on_line, ']'))})
        ]/@line1
      )]"),
      glue("({ global_nodes(infix_tokens) })[{xp_last_on_line}{infix_condition}]"),
      glue("({ global_nodes(no_paren_keywords) })[{xp_last_on_line}]"),
      glue("
        ({ global_nodes(keyword_tokens) })
          /following-sibling::OP-RIGHT-PAREN[
            {xp_last_on_line} and
            not(following-sibling::expr[1][OP-LEFT-BRACE])
          ]
      ")
    ),
    collapse = " | "
  )

  xp_multiline_string <- "//STR_CONST[@line1 < @line2]"

  Linter(linter_level = "file", function(source_expression) {
    # must run on file level because a line can contain multiple expressions, losing indentation information, e.g.
    #
    #> fun(
    #    a) # comment
    #
    # will have "# comment" as a separate expression

    xml <- source_expression$full_xml_parsed_content

    # Indentation increases by 1 for:
    #  - { } blocks that span multiple lines
    #  - ( ), [ ], or [[ ]] calls that span multiple lines
    #     + if a token follows (, a hanging indent is required until )
    #     + if there is no token following ( on the same line, a block indent is required until )
    #  - binary operators where the second arguments starts on a new line

    indent_levels <- re_matches(
      source_expression$file_lines,
      rex(start, any_spaces),
      locations = TRUE
    )[, "end"]
    expected_indent_levels <- integer(length(indent_levels))
    is_hanging <- logical(length(indent_levels))

    indent_changes <- xml_find_all(xml, xp_indent_changes)
    for (change in indent_changes) {
      change_type <- find_indent_type(change)
      change_begin <- as.integer(xml_attr(change, "line1")) + 1L
      change_end <- xml_find_num(change, xp_block_ends)
      if (isTRUE(change_begin <= change_end)) {
        to_indent <- seq(from = change_begin, to = change_end)
        expected_indent_levels[to_indent] <- find_new_indent(
          current_indent = expected_indent_levels[to_indent],
          change_type = change_type,
          indent = indent,
          hanging_indent = as.integer(xml_attr(change, "col2"))
        )
        is_hanging[to_indent] <- change_type == "hanging"
      }
    }

    in_str_const <- logical(length(indent_levels))
    multiline_strings <- xml_find_all(xml, xp_multiline_string)
    for (string in multiline_strings) {
      is_in_str <- seq(
        from = as.integer(xml_attr(string, "line1")) + 1L,
        to = as.integer(xml_attr(string, "line2"))
      )
      in_str_const[is_in_str] <- TRUE
    }

    # Only lint non-empty lines if the indentation level doesn't match.
    bad_lines <- which(indent_levels != expected_indent_levels &
                         nzchar(trimws(source_expression$file_lines)) &
                         !in_str_const)
    if (length(bad_lines) > 0L) {
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
        # when indent_levels==0, need to start ranges at column 1.
        pmax(
          pmin(expected_indent_levels[bad_lines] + 1L, indent_levels[bad_lines]),
          1L
        ),
        # If the expected indent is larger than the current line width, the lint range would become invalid.
        # Therefore, limit range end to end of line.
        pmin(
          pmax(expected_indent_levels[bad_lines], indent_levels[bad_lines]),
          nchar(source_expression$file_lines[bad_lines]) + 1L
        )
      )
      Map(
        Lint,
        filename = source_expression$filename,
        line_number = lint_lines,
        column_number = indent_levels[bad_lines],
        type = "style",
        message = lint_messages,
        line = unname(source_expression$file_lines[bad_lines]),
        # TODO(#2467): Use ranges = apply(lint_ranges, 1L, list, simplify = FALSE).
        ranges = lapply(
          seq_along(bad_lines),
          function(i) {
            list(lint_ranges[i, ])
          }
        )
      )
    } else {
      list()
    }
  })
}

find_new_indent <- function(current_indent, change_type, indent, hanging_indent) {
  switch(change_type,
    suppress = current_indent,
    hanging = hanging_indent,
    double = current_indent + 2L * indent,
    block = current_indent + indent
  )
}

build_indentation_style_tidy <- function() {
  paren_tokens_left <- c("OP-LEFT-BRACE", "OP-LEFT-PAREN", "OP-LEFT-BRACKET", "LBB")
  paren_tokens_right <- c("OP-RIGHT-BRACE", "OP-RIGHT-PAREN", "OP-RIGHT-BRACKET", "OP-RIGHT-BRACKET")
  xp_last_on_line <- "@line1 != following-sibling::*[not(self::COMMENT)][1]/@line1"
  xp_inner_expr <- "preceding-sibling::*[1][self::expr and expr[SYMBOL_FUNCTION_CALL]]/*[not(self::COMMENT)]"

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
    parent::expr[(FUNCTION or OP-LAMBDA) and not(@line1 = SYMBOL_FORMALS/@line1)]
      /OP-RIGHT-PAREN[@line1 = preceding-sibling::*[not(self::COMMENT)][1]/@line2]
  "

  xp_suppress <- paste(
    glue("
        self::{paren_tokens_left}[
          @line1 = following-sibling::{paren_tokens_right}/{xp_inner_expr}[position() = 1]/@line1
        ]/following-sibling::{paren_tokens_right}[
          @line1 > {xp_inner_expr}[position() = last() - 1]/@line2
        ]"),
    collapse = " | "
  )

  xp_is_not_hanging <- paste(
    c(
      glue(
        "self::{paren_tokens_left}/following-sibling::{paren_tokens_right}[@line1 > preceding-sibling::*[1]/@line2]"
      ),
      glue("self::*[{xp_and(paste0('not(self::', paren_tokens_left, ')'))} and {xp_last_on_line}]")
    ),
    collapse = " | "
  )

  function(change) {
    if (length(xml_find_first(change, xp_is_double_indent)) > 0L) {
      "double"
    } else if (length(xml_find_first(change, xp_suppress)) > 0L) {
      "suppress"
    } else if (length(xml_find_first(change, xp_is_not_hanging)) == 0L) {
      "hanging"
    } else {
      "block"
    }
  }
}

build_indentation_style_always <- function() {
  paren_tokens_left <- c("OP-LEFT-BRACE", "OP-LEFT-PAREN", "OP-LEFT-BRACKET", "LBB")
  paren_tokens_right <- c("OP-RIGHT-BRACE", "OP-RIGHT-PAREN", "OP-RIGHT-BRACKET", "OP-RIGHT-BRACKET")
  xp_last_on_line <- "@line1 != following-sibling::*[not(self::COMMENT)][1]/@line1"

  xp_is_not_hanging <- paste(
    c(
      glue("
        self::{paren_tokens_left}[{xp_last_on_line}]/
          following-sibling::{paren_tokens_right}[@line1 > preceding-sibling::*[1]/@line2]
      "),
      glue("self::*[{xp_and(paste0('not(self::', paren_tokens_left, ')'))} and {xp_last_on_line}]")
    ),
    collapse = " | "
  )

  function(change) {
    if (length(xml_find_first(change, xp_is_not_hanging)) == 0L) {
      "hanging"
    } else {
      "block"
    }
  }
}
