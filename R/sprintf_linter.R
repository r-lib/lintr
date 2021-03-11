#' @describeIn linters checks for inconsistent number of arguments or
#'    arguments with incompatible types in \code{sprintf} calls.
#' @export
sprintf_linter <- function() {
  Linter(function(source_file) {
    if (is.null(source_file$full_xml_parsed_content)) return(list())

    xml <- source_file$full_xml_parsed_content

    xpath <- "//expr[
    expr/SYMBOL_FUNCTION_CALL[text() = 'sprintf'] and
    OP-LEFT-PAREN/following-sibling::expr[1]/STR_CONST
  ]"

    sprintf_calls <- xml2::xml_find_all(xml, xpath)

    get_range_text <- function(content, line1, col1, line2, col2) {
      lines <- content[line1:line2]
      lines[length(lines)] <- substr(lines[length(lines)], 1L, col2)
      lines[1] <- substr(lines[1], col1, nchar(lines[1]))
      lines
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
      if (is.call(expr) &&
        is.language(expr[[1]]) &&
        is.character(expr[[2]])) {
        if (length(expr) >= 3) {
          for (i in 3:length(expr)) {
            if (!is_missing(expr[[i]]) && !is.atomic(expr[[i]])) {
              expr[[i]] <- 0
            }
          }
        }

        res <- tryCatch(eval(expr, envir = baseenv()), warning = identity, error = identity)
        if (inherits(res, "condition")) {
          Lint(
            filename = source_file$filename,
            line_number = line1,
            column_number = col1,
            type = "warning",
            message = conditionMessage(res),
            line = source_file$file_lines[line1],
            ranges = list(c(col1, col2))
          )
        }
      }
    }, list(line1, col1, line2, col2), NULL)

    result[vapply(result, is.list, logical(1L))]
  })
}
