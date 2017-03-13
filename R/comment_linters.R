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
#' blocks
#' @export
commented_code_linter <- function(source_file) {
  res <- re_matches(source_file$file_lines,
                    rex("#", any_spaces,
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

  line_numbers <- rownames(na.omit(res))
  lapply(line_numbers, function(line_number) {
    line <- source_file$file_lines[as.numeric(line_number)]
    is_parsable <- parsable(substr(line,
                                   res[line_number, "code.start"],
                                   res[line_number, "code.end"]))
    if (is_parsable) {
      Lint(
        filename = source_file$filename,
        line_number = line_number,
        column_number = res[line_number, "code.start"],
        type = "style",
        message = "Commented code should be removed.",
        line = line,
        linter = "commented_code_linter",
        ranges = list(c(res[line_number, "code.start"], res[line_number, "code.end"]))
        )
    }
  })
}

# is given text parsable
parsable <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  res <- try(parse(text = x), silent = TRUE)

  !inherits(res, "try-error")
}



#' @describeIn linters  Check that the source contains no TODO comments (case-insensitive).
#' @param todo  Vector of strings that identify TODO comments.
#' @export
todo_comment_linter <- function(todo=c("todo", "fixme")) {
  local({
    regex <- rex(capture(name="match", one_or_more("#"), any_spaces, or(todo)))
    code_locs <- integer()
    function(source_file) {
      file_lines <- source_file[["file_lines"]]
      if (is.null(file_lines)) {
        # code section: register where expression ends on the line
        code_locs <<- register_code_locations(source_file, code_locs)
      } else {
        # non-code section: detect and report TODOs
        # workaround issue https://github.com/kevinushey/rex/issues/50 :
        if (length(file_lines) == 0L) {file_lines <- ""}
        res <- gather_matches(
          re_matches(file_lines, regex, global=TRUE, locations=TRUE, ignore.case=TRUE))
        apply(
          res,
          1L,
          function(match) {
            if (!is.logical(match)) {
              # skip NA lines
              start_col_num <- as.integer(match[["match.start"]])
              end_col_num <- as.integer(match[["match.end"]])
              line_num <- as.integer(match[["line"]])
              line <- file_lines[[as.numeric(line_num)]]
              if (is_located_outside_code(line_num, start_col_num, code_locs)) {
                # the todo is not in a code block
                Lint(
                  filename = source_file[["filename"]],
                  line_number = line_num,
                  column_number = start_col_num,
                  type = "style",
                  message = "TODO comments should be removed.",
                  line = line,
                  linter = "todo_comment_linter",
                  ranges = list(c(start_col_num, end_col_num))
                )
              }
            }
          }
        )
      }
    }
  })
}

register_code_locations <- function(source_file, locs) {
  tokens <- source_file[["parsed_content"]]
  line_nums <- sort(unique(tokens[, "line2"]))
  max_line_num <- line_nums[[length(line_nums)]]
  for (line_num in seq.int(line_nums[[1L]], max_line_num)) {
    # cannot merge with c() because it could introduce duplicate names in the vector
    line_tokens <- tokens[tokens[, "line2"] == line_num, ]
    line_comment_tokens <- line_tokens[line_tokens[, "token"] == "COMMENT", ]
    col <- if (line_num < max_line_num) {
      if (nrow(line_comment_tokens)) {
        min(line_comment_tokens[, "col1"]) - 1L                  # column just before comment
      } else {
        nchar(source_file[["lines"]][[as.character(line_num)]])  # last column of the line
      }
    } else {
      max(line_tokens[, "col2"])                                 # column where code stops
    }
    locs[[as.character(line_num)]] <- col  # cannot use c(): it could add duplicate names in vector
  }
  locs
}

is_located_outside_code <- function(line, col, code_locs) {
  !line %in% names(code_locs) ||           # no code on this line
    col > code_locs[[as.character(line)]]  # column located after code on this line
}

gather_matches <- function(res) {
  # Input : list of dataframes containing the positions of matched regex capture groups
  # Output: single dataframe with a new column for the "line" number
  res <- Map(
    function(line_res, line_num) {
      if (is.na(line_res[1L, 1L])) {
        data.frame()
      } else {
        line_res[, "line"] <- rep_len(line_num, nrow(line_res))
        line_res
      }
    },
    res,
    seq_along(res)
  )
  do.call(rbind, res)
}
