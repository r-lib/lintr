#' Check that indentation is consistent
#'
#' @param indent Number of spaces, that a code block should be indented by relative to its parent code block.
#'   Used for multi-line code blocks (`{ ... }`), function calls (`( ... )`) and extractions (`[ ... ]`, `[[ ... ]]`).
#'   Defaults to 2.
#' @param hanging_indent_style Indentation style for multi-line function calls with arguments in their first line.
#'   Defaults to tidyverse style, i.e. a block indent is used if the function call terminates with `)` on a separate
#'   line and a hanging indent if not.
#'   Note that multi-line function calls without arguments on their first line will always be expected to have
#'   block-indented arguments.
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
#'     a,
#'     b
#'   ) {
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
  paren_token_right_names <- c(`)` = "parenthesis", `}` = "brace", `]` = "bracket")
  infix_tokens <- setdiff(infix_metadata$xml_tag, c("OP-LEFT-BRACE", "OP-COMMA", paren_tokens_left))
  no_paren_keywords <- c("ELSE", "REPEAT")
  keyword_tokens <- c("FUNCTION", "OP-LAMBDA", "IF", "WHILE")

  xp_last_on_line <- "@line1 != following-sibling::*[not(self::COMMENT)][1]/@line1"
  xp_self_last_on_line <- glue("self::*[{xp_last_on_line}]")

  xp_following_right_paren <-
    glue("following-sibling::*[{xp_or(paste0('self::', paren_tokens_right))}][1]")

  hanging_indent_style <- match.arg(hanging_indent_style)

  find_indent_type <- switch(hanging_indent_style,
    tidy = build_indentation_style_tidy(),
    always = build_indentation_style_always(),
    never = \(change) "block"
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
            /following-sibling::*[
              self::SYMBOL_FUNCTION_CALL
              or self::SLOT[parent::expr/following-sibling::OP-LEFT-PAREN]
            ]
            /parent::expr
            /following-sibling::expr[1]
            /@line2
        "),
        glue("
          self::*[
            {xp_and(paste0('not(self::', paren_tokens_left, ')'))}
            and not(following-sibling::*[
              self::SYMBOL_FUNCTION_CALL
              or self::SLOT[parent::expr/following-sibling::OP-LEFT-PAREN]
            ])
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
      "),
      # FOR loop is strange, #2564
      glue("
        //forcond[
          OP-RIGHT-PAREN/{xp_last_on_line}
          and not(following-sibling::expr[1]/OP-LEFT-BRACE)
        ]
      ")
    ),
    collapse = " | "
  )

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

    indent_changes <- xml_find_all_(xml, xp_indent_changes)
    change_types <- vapply(indent_changes, find_indent_type, character(1L))
    change_begins <- as.integer(xml_attr_(indent_changes, "line1")) + 1L
    change_ends <- xml_find_num_(indent_changes, xp_block_ends)
    col2s <- as.integer(xml_attr_(indent_changes, "col2"))
    indent_change_metadata <- compute_indent_changes(
      indent_changes, change_types, change_begins, change_ends, col2s, indent,
      length(indent_levels), xp_self_last_on_line, xp_following_right_paren
    )
    expected_indent_levels <- indent_change_metadata$expected_indent_levels
    is_hanging <- indent_change_metadata$is_hanging
    hanging_indent_cols <- indent_change_metadata$hanging_indent_cols
    bad_closing_list <- indent_change_metadata$bad_closing_list

    in_str_const <- rep(FALSE, length(indent_levels))
    in_str_const[is_in_str_const(xml)] <- TRUE

    # Only lint non-empty lines if the indentation level doesn't match.
    # TODO: remove styler ignore directives once tidyverse/style/issues/197 is resolved
    # styler: off
    bad_lines <- which(indent_levels != expected_indent_levels &
                         nzchar(trimws(source_expression$file_lines)) &
                         !in_str_const)
    # styler: on
    if (length(bad_lines) == 0L && length(bad_closing_list) == 0L) {
      return(list())
    }

    # Suppress consecutive lints with the same indentation difference, to not generate an excessive number of lints
    if (length(bad_lines) > 0L) {
      is_consecutive_lint <- c(FALSE, diff(bad_lines) == 1L)
      indent_diff <- expected_indent_levels[bad_lines] - indent_levels[bad_lines]
      is_same_diff <- c(FALSE, diff(indent_diff) == 0L)
      bad_lines <- bad_lines[!(is_consecutive_lint & is_same_diff)]
    }

    if (length(bad_lines) > 0L) {
      is_misindented_hanging <- hanging_indent_cols[bad_lines] > 0L &
        indent_levels[bad_lines] == hanging_indent_cols[bad_lines]

      lint_messages <- sprintf(
        ifelse(
          is_misindented_hanging,
          "%s should be %d spaces but is %d spaces (or start argument on previous line).",
          "%s should be %d spaces but is %d spaces."
        ),
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
      lint_ranges_list <- apply(lint_ranges, 1L, list, simplify = FALSE)
      lint_cols <- indent_levels[bad_lines]
    } else {
      lint_messages <- character()
      lint_lines <- integer()
      lint_ranges_list <- list()
      lint_cols <- integer()
    }

    res <- incorporate_closing_lints(
      bad_closing_list,
      indent_change_metadata$bad_closing_block_begins,
      indent_change_metadata$bad_closing_block_ends,
      lint_lines, lint_cols, lint_messages, lint_ranges_list,
      paren_token_right_names
    )
    lint_lines <- res$lint_lines
    lint_cols <- res$lint_cols
    lint_messages <- res$lint_messages
    lint_ranges_list <- res$lint_ranges_list

    if (length(lint_lines) == 0L) {
      return(list())
    }

    Map(
      Lint,
      filename = source_expression$filename,
      line_number = lint_lines,
      column_number = lint_cols,
      type = "style",
      message = lint_messages,
      line = unname(source_expression$file_lines[lint_lines]),
      ranges = lint_ranges_list
    )
  })
}

check_bad_closing_node <- function(node, end_line, xp_self_last_on_line, xp_following_right_paren) {
  if (length(xml_find_first_(node, xp_self_last_on_line)) == 0L) {
    return(NULL)
  }
  closing_node <- xml_find_first_(node, xp_following_right_paren)
  if (length(closing_node) == 0L || is.na(xml_attr_(closing_node, "line1"))) {
    return(NULL)
  }
  if (as.integer(xml_attr_(closing_node, "line1")) == end_line) {
    closing_node
  } else {
    NULL
  }
}

compute_indent_changes <- function(indent_changes, change_types, change_begins, change_ends, col2s, indent,
                                 n_lines, xp_self_last_on_line, xp_following_right_paren) {
  expected_indent_levels <- integer(n_lines)
  is_hanging <- logical(n_lines)
  hanging_indent_cols <- integer(n_lines)
  bad_closing_list <- list()
  bad_closing_block_begins <- integer()
  bad_closing_block_ends <- integer()

  for (ii in which(change_begins <= change_ends)) {
    to_indent <- seq(from = change_begins[ii], to = change_ends[ii])
    expected_indent_levels[to_indent] <- find_new_indent(
      current_indent = expected_indent_levels[to_indent],
      change_type = change_types[ii],
      indent = indent,
      hanging_indent = col2s[ii]
    )
    is_hanging[to_indent] <- change_types[ii] == "hanging"
    if (change_types[ii] == "block") {
      hanging_indent_cols[to_indent] <- col2s[ii]
      closing_node <- check_bad_closing_node(
        indent_changes[[ii]], change_ends[ii], xp_self_last_on_line, xp_following_right_paren
      )
      if (!is.null(closing_node)) {
        bad_closing_list[[length(bad_closing_list) + 1L]] <- closing_node
        bad_closing_block_begins <- c(bad_closing_block_begins, change_begins[ii])
        bad_closing_block_ends <- c(bad_closing_block_ends, change_ends[ii])
      }
    }
  }

  list(
    expected_indent_levels = expected_indent_levels,
    is_hanging = is_hanging,
    hanging_indent_cols = hanging_indent_cols,
    bad_closing_list = bad_closing_list,
    bad_closing_block_begins = bad_closing_block_begins,
    bad_closing_block_ends = bad_closing_block_ends
  )
}

is_in_str_const <- function(xml) {
  multiline_strings <- xml_find_all_(xml, "//STR_CONST[@line1 < @line2]")
  line1 <- as.integer(xml_attr_(multiline_strings, "line1"))
  line2 <- as.integer(xml_attr_(multiline_strings, "line2"))
  unlist(Map(`:`, line1, line2))
}

incorporate_closing_lints <- function(bad_closing_list, bad_closing_block_begins, bad_closing_block_ends,
                                      lint_lines, lint_cols, lint_messages, lint_ranges_list,
                                      paren_token_right_names) {
  if (length(bad_closing_list) == 0L) {
    return(list(
      lint_lines = lint_lines,
      lint_cols = lint_cols,
      lint_messages = lint_messages,
      lint_ranges_list = lint_ranges_list
    ))
  }

  for (jj in seq_along(bad_closing_list)) {
    in_block <- which(lint_lines >= bad_closing_block_begins[jj] & lint_lines <= bad_closing_block_ends[jj])
    if (length(in_block) > 0L) {
      first_idx <- in_block[1L]
      closing_text <- xml_text(bad_closing_list[[jj]])
      lint_messages[first_idx] <- sprintf(
        "%s; closing %s '%s' should be on its own line.",
        sub("\\.$", "", lint_messages[first_idx]),
        paren_token_right_names[closing_text],
        closing_text
      )
    } else {
      closing_node <- bad_closing_list[[jj]]
      closing_line <- as.integer(xml_attr_(closing_node, "line1"))
      closing_col1 <- as.integer(xml_attr_(closing_node, "col1"))
      closing_col2 <- as.integer(xml_attr_(closing_node, "col2"))
      closing_text <- xml_text(closing_node)
      closing_message <- sprintf(
        "Closing %s '%s' should be on its own line for block-indented calls.",
        paren_token_right_names[closing_text], closing_text
      )
      lint_lines <- c(lint_lines, closing_line)
      lint_cols <- c(lint_cols, closing_col1)
      lint_messages <- c(lint_messages, closing_message)
      lint_ranges_list <- c(lint_ranges_list, list(list(c(closing_col1, closing_col2))))
    }
  }

  list(
    lint_lines = lint_lines,
    lint_cols = lint_cols,
    lint_messages = lint_messages,
    lint_ranges_list = lint_ranges_list
  )
}

find_new_indent <- function(current_indent, change_type, indent, hanging_indent) {
  switch(change_type,
    suppress = current_indent,
    hanging = hanging_indent,
    block = current_indent + indent
  )
}

build_indentation_style_tidy <- function() {
  paren_tokens_left <- c("OP-LEFT-BRACE", "OP-LEFT-PAREN", "OP-LEFT-BRACKET", "LBB")
  paren_tokens_right <- c("OP-RIGHT-BRACE", "OP-RIGHT-PAREN", "OP-RIGHT-BRACKET", "OP-RIGHT-BRACKET")
  xp_last_on_line <- "@line1 != following-sibling::*[not(self::COMMENT)][1]/@line1"
  xp_inner_expr <- "preceding-sibling::*[1][self::expr and expr[SYMBOL_FUNCTION_CALL]]/*[not(self::COMMENT)]"

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
      glue("
        self::{paren_tokens_left}
          /following-sibling::{paren_tokens_right}[@line1 > preceding-sibling::*[1]/@line2]
      "),
      glue("self::*[{xp_and(paste0('not(self::', paren_tokens_left, ')'))} and {xp_last_on_line}]"),
      glue("self::{paren_tokens_left}[parent::expr[FUNCTION or OP-LAMBDA] and {xp_last_on_line}]")
    ),
    collapse = "\n|  "
  )

  function(change) {
    if (length(xml_find_first_(change, xp_suppress)) > 0L) {
      "suppress"
    } else if (length(xml_find_first_(change, xp_is_not_hanging)) == 0L) {
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
    if (length(xml_find_first_(change, xp_is_not_hanging)) == 0L) {
      "hanging"
    } else {
      "block"
    }
  }
}
