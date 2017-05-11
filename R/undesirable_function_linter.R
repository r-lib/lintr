#' @describeIn linters  Report the use of undesirable functions, e.g. \code{return}, \code{options},
#'                      or \code{sapply} and suggest an alternative.
#' @param fun  Named character vector, where the names are the names of the undesirable functions,
#'             and the values are the text for the alternative function to use (or \code{NA}).
#' @export
undesirable_function_linter <- function(fun = default_undesirable_functions) {
  Linter(function(source_file) {
    lapply(
      ids_with_token(source_file, c("SYMBOL_FUNCTION_CALL", "SYMBOL"), fun=`%in%`),
      function(id) {
        token <- with_id(source_file, id)
        fun_name <- token[["text"]]
        if (fun_name %in% names(fun)) {
          line_num <- token[["line1"]]
          start_col_num <- token[["col1"]]
          end_col_num <- token[["col2"]]
          msg <- sprintf("Function \"%s\" is undesirable.", fun_name)
          alt_fun <- fun[[fun_name]]
          if (!is.na(alt_fun)) {
            msg <- c(msg, sprintf("As an alternative, %s.", alt_fun))
          }
          Lint(
            filename = source_file[["filename"]],
            line_number = line_num,
            column_number = start_col_num,
            type = "warning",
            message = paste0(msg, collapse=" "),
            line = source_file[["lines"]][[as.character(line_num)]],
            ranges = list(c(start_col_num, end_col_num)),
            linter = "undesirable_function_linter"
          )
        }
      }
    )
  })
}
