#' @describeIn linters checks that there is no commented code outside roxygen
#'   blocks
#' @export
commented_code_linter <- function(source_file) {
  res <- re_matches(source_file$file_lines,
                    rex("#", except("'"), anything,
                        or(some_of("{}[]"), # code-like parentheses
                           "<-", "->", "==", # an assignment
                           group(graphs, "(", anything, ")"), # a function call
                           group("!", alphas) # a negation
                        )
                    ),
                    global = TRUE, locations = TRUE)
  line_number <- length(source_file$file_lines)
  lints <- list()
  while (line_number > 0L && !is.na(res[[line_number]][["start"]])) {
    lints[[length(lints) + 1L]] <- Lint(
      filename = source_file$filename,
      line_number = line_number,
      column_number = res[[line_number]][["start"]],
      type = "style",
      message = "Commented code should be removed.",
      line = source_file$file_lines[[line_number]],
      linter = "commented_code_linter"
    )
    line_number <- line_number - 1L
  }
  invisible(lints)
}
