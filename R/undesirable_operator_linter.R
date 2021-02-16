op_types <- c(
  "LEFT_ASSIGN", "EQ_ASSIGN", "EQ_SUB", "RIGHT_ASSIGN",            # <<- <- = -> ->>
  "'$'", "'@'", "'['", "']'", "LBB",                               # $ @ [ ] [[
  "'-'", "'+'", "'!'", "'~'", "'?'",                               # - + ! ~ ?
  "':'", "'*'", "'/'", "'^'", "'~'", "'?'", "'%'", "SPECIAL",      # : * / ^ ~ ? % %*%
  "GT", "GE", "LT", "LE", "EQ", "NE", "AND", "OR", "AND2", "OR2",  # > >= < <= == != & | && ||
  "NS_GET", "NS_GET_INT"                                           # :: :::
)


#' @describeIn linters  Report the use of undesirable operators, e.g. \code{`:::`} or \code{`<<-`}
#'                      and suggest an alternative.
#' @param op  Named character vector, where the names are the names of the undesirable operators,
#'            and the values are the text for the alternative operator to use (or \code{NA}).
#' @export
undesirable_operator_linter <- function(op = default_undesirable_operators) {
  Linter(function(source_file) {
    lapply(
      ids_with_token(source_file, op_types, fun = `%in%`),
      function(id) {
        token <- with_id(source_file, id)
        op_name <- token[["text"]]
        if (op_name %in% names(op)) {
          line_num <- token[["line1"]]
          start_col_num <- token[["col1"]]
          end_col_num <- token[["col2"]]
          msg <- sprintf("Operator `%s` is undesirable.", op_name)
          alt_op <- op[[op_name]]
          if (!is.na(alt_op)) {
            msg <- c(msg, sprintf("As an alternative, %s.", alt_op))
          }

          Lint(
            filename = source_file[["filename"]],
            line_number = line_num,
            column_number = start_col_num,
            type = "warning",
            message = paste0(msg, collapse = " "),
            line = source_file[["lines"]][[as.character(line_num)]],
            ranges = list(c(start_col_num, end_col_num))
          )
        }
      }
    )
  })
}
