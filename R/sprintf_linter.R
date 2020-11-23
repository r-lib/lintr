#' @describeIn linters checks for inconsistent number of arguments or
#'    arguments with incompatible types in \code{sprintf} calls.
#' @export
sprintf_linter <- function(source_file) {
  if (!length(source_file$full_parsed_content) || is.null(source_file$xml_parsed_content)) return(list())

  xml <- source_file$xml_parsed_content

  xpath <- "//expr[
    expr/SYMBOL_FUNCTION_CALL[text() = 'sprintf'] and
    OP-LEFT-PAREN/following-sibling::expr[1]/STR_CONST
  ]"

  sprintf_calls <- xml2::xml_find_all(xml, xpath)

  get_range_text <- function(content, line1, col1, line2, col2) {
    text <- content[line1:line2]
    text[[1]] <- substr(text[[1]], col1, nchar(text[[1]]))
    if (line2 > line1) {
      nlines <- length(text)
      text[[nlines]] <- substr(text[[nlines]], col2, nchar(text[[nlines]]))
    }
    text
  }

  line1 <- as.integer(xml2::xml_attr(sprintf_calls, "line1"))
  col1 <- as.integer(xml2::xml_attr(sprintf_calls, "col1"))
  line2 <- as.integer(xml2::xml_attr(sprintf_calls, "line2"))
  col2 <- as.integer(xml2::xml_attr(sprintf_calls, "col2"))

  is_missing <- function(x) {
    is.symbol(x) && !nzchar(x)
  }

  result <- .mapply(function(line1, col1, line2, col2) {
    text <- get_range_text(source_file$file_lines, line1, col1, line2, col2)
    expr <- tryCatch(parse(text = text, keep.source = FALSE)[[1]], error = function(e) NULL)
    if (is.call(expr) && is.language(expr[[1]]) && is.character(expr[[2]])) {
      if (length(expr) >= 3) {
        for (i in 3:length(expr)) {
          if (!is_missing(expr[[i]]) && !is.atomic(expr[[i]])) {
            expr[[i]] <- 0
          }
        }
      }

      res <- tryCatch(eval(expr, envir = baseenv()), error = function(e) e)
      if (inherits(res, "error")) {
        Lint(
          filename = source_file$filename,
          line_number = line1,
          column_number = col1,
          type = "warning",
          message = conditionMessage(res),
          line = source_file$file_lines[line1],
          ranges = list(c(col1, col2)),
          linter = "sprintf_linter"
        )
      }
    }
  }, list(line1, col1, line2, col2), NULL)

  result[vapply(result, is.list, logical(1L))]
}
