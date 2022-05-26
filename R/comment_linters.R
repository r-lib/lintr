ops <- list(
  "+",
  #"-",
  "=",
  "==",
  "!=",
  "<=",
  ">=",
  "<-",
  "<<-",
  "<",
  ">",
  "->",
  "->>",
  "%%",
  "/",
  "^",
  "*",
  "**",
  "|",
  "||",
  "&",
  "&&",
  rex("%", except_any_of("%"), "%"))

#' Commented code linter
#'
#' Check that there is no commented code outside roxygen blocks.
#'
#' @evalRd rd_tags("commented_code_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
commented_code_linter <- function() {
  code_candidate_regex <- rex(
    some_of("#"),
    any_spaces,
    capture(
      name = "code",
      # except("'"),
      anything,
      or(
        some_of("{}[]"), # code-like parentheses
        or(ops), # any operator
        group(graphs, "(", anything, ")"), # a function call
        group("!", alphas) # a negation
      ),
      anything
    )
  )
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }
    all_comment_nodes <- xml2::xml_find_all(source_expression$full_xml_parsed_content, "//COMMENT")
    all_comments <- xml2::xml_text(all_comment_nodes)
    code_candidates <- re_matches(all_comments, code_candidate_regex, global = FALSE, locations = TRUE)

    # TODO(michaelchirico): convert this to xml_nodes_to_lint()?
    lapply(rownames(na.omit(code_candidates)), function(code_candidate) {
      is_parsable <- parsable(code_candidates[code_candidate, "code"])
      if (is_parsable) {
        comment_node <- all_comment_nodes[[as.integer(code_candidate)]]
        line_number <- as.integer(xml2::xml_attr(comment_node, "line1"))
        column_offset <- as.integer(xml2::xml_attr(comment_node, "col1")) - 1L

        column_number <- column_offset + code_candidates[code_candidate, "code.start"]
        Lint(
          filename = source_expression$filename,
          line_number = line_number,
          column_number = column_number,
          type = "style",
          message = "Commented code should be removed.",
          line = source_expression$file_lines[line_number],
          ranges = list(c(column_number, column_offset + code_candidates[code_candidate, "code.end"]))
        )
      }
    })
  })
}

# is given text parsable
parsable <- function(x) {
  res <- try_silently(parse(text = x))
  !inherits(res, "try-error")
}


#' TODO comment linter
#'
#' Check that the source contains no TODO comments (case-insensitive).
#'
#' @param todo Vector of strings that identify TODO comments.
#'
#' @evalRd rd_tags("todo_comment_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
todo_comment_linter <- function(todo = c("todo", "fixme")) {
  todo_comment_regex <- rex(one_or_more("#"), any_spaces, or(todo))
  Linter(function(source_expression) {
    tokens <- with_id(source_expression, ids_with_token(source_expression, "COMMENT"))
    are_todo <- re_matches(tokens[["text"]], todo_comment_regex, ignore.case = TRUE)
    tokens <- tokens[are_todo, ]
    lapply(
      split(tokens, seq_len(nrow(tokens))),
      function(token) {
        Lint(
          filename = source_expression[["filename"]],
          line_number = token[["line1"]],
          column_number = token[["col1"]],
          type = "style",
          message = "TODO comments should be removed.",
          line = source_expression[["lines"]][[as.character(token[["line1"]])]],
          ranges = list(c(token[["col1"]], token[["col2"]]))
        )
      }
    )
  })
}
