ops <- list(
  "+",
  "-",
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

#' @describeIn linters checks that there is no commented code outside roxygen
#' blocks
#' @export
commented_code_linter <- function(source_file) {
  res <- re_matches(source_file$file_lines,
                    rex("#", any_spaces,
                        capture(name = "code",
                          except("'"), anything,
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
