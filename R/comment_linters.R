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

#' @describeIn linters Check that there is no commented code outside roxygen
#' blocks.
#' @export
commented_code_linter <- function() {
  Linter(function(source_file) {
    if (is.null(source_file$full_xml_parsed_content)) return(list())
    all_comment_nodes <- xml2::xml_find_all(source_file$full_xml_parsed_content, "//COMMENT")
    all_comments <- xml2::xml_text(all_comment_nodes)
    code_candidates <- re_matches(
      all_comments,
      rex(some_of("#"), any_spaces,
          capture(name = "code",
                  # except("'"),
                  anything,
                  or(some_of("{}[]"), # code-like parentheses
                     or(ops), # any operator
                     group(graphs, "(", anything, ")"), # a function call
                     group("!", alphas) # a negation
                  ),
                  anything
          )
      ),
      global = FALSE, locations = TRUE)

    lapply(rownames(na.omit(code_candidates)), function(code_candidate) {
      is_parsable <- parsable(code_candidates[code_candidate, "code"])
      if (is_parsable) {
        comment_node <- all_comment_nodes[[as.integer(code_candidate)]]
        line_number <- as.integer(xml2::xml_attr(comment_node, "line1"))
        column_offset <- as.integer(xml2::xml_attr(comment_node, "col1")) - 1L

        Lint(
          filename = source_file$filename,
          line_number = line_number,
          column_number = column_offset + code_candidates[code_candidate, "code.start"],
          type = "style",
          message = "Commented code should be removed.",
          line = source_file$file_lines[line_number],
          ranges = list(column_offset + c(code_candidates[code_candidate, "code.start"],
                                          code_candidates[code_candidate, "code.end"]))
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


#' @describeIn linters  Check that the source contains no TODO comments (case-insensitive).
#' @param todo  Vector of strings that identify TODO comments.
#' @export
todo_comment_linter <- function(todo = c("todo", "fixme")) {
  Linter(function(source_file) {
    tokens <- with_id(source_file, ids_with_token(source_file, "COMMENT"))
    are_todo <- re_matches(tokens[["text"]], rex(one_or_more("#"), any_spaces, or(todo)), ignore.case = TRUE)
    tokens <- tokens[are_todo, ]
    lapply(
      split(tokens, seq_len(nrow(tokens))),
      function(token) {
        Lint(
          filename = source_file[["filename"]],
          line_number = token[["line1"]],
          column_number = token[["col1"]],
          type = "style",
          message = "TODO comments should be removed.",
          line = source_file[["lines"]][[as.character(token[["line1"]])]],
          ranges = list(c(token[["col1"]], token[["col2"]]))
        )
      }
    )
  })
}
