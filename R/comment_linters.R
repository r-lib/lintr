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
    is_parsable <- which(vapply(code_candidates[, "code"], parsable, logical(1L)))

    lint_list <- xml_nodes_to_lints(
      all_comment_nodes[is_parsable],
      source_expression = source_expression,
      lint_message = "Commented code should be removed."
    )

    # Location info needs updating
    for (i in seq_along(lint_list)) {
      rng <- lint_list[[i]]$ranges[[1L]]

      rng[2L] <- rng[1L] + code_candidates[is_parsable[i], "code.end"] - 1L
      rng[1L] <- rng[1L] + code_candidates[is_parsable[i], "code.start"] - 1L

      lint_list[[i]]$column_number <- rng[1L]
      lint_list[[i]]$ranges <- list(rng)
    }

    lint_list
  })
}

# is given text parsable
parsable <- function(x) {
  if (anyNA(x)) {
    return(FALSE)
  }
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
