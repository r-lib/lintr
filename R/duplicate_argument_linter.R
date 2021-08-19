#' @describeIn linters checks for duplicate arguments in function calls.
#' @param except a character vector of function names as exceptions.
#' @export
duplicate_argument_linter <- function(except = character()) {
  Linter(function(source_file) {

    if (is.null(source_file$full_xml_parsed_content)) return(list())

    xml <- source_file$full_xml_parsed_content

    xpath <- "//expr[EQ_SUB]"

    calls <- xml2::xml_find_all(xml, xpath)

    if (length(except)) {
      calls_names <- xml2::xml_find_first(calls, "expr[1][count(*)=1]/*[1]")
      calls_text <- vapply(
        parse(text = xml2::xml_text(calls_names)),
        as.character, character(1L)
      )
      calls <- calls[!(calls_text %in% except)]
    }

    result <- lapply(calls, function(call) {
      args <- xml2::xml_find_all(call, "EQ_SUB/preceding-sibling::*[1]")
      args_text <- vapply(
        parse(text = xml2::xml_text(args), keep.source = FALSE),
        as.character, character(1L)
      )
      is_dup_arg <- duplicated(args_text)
      dup_args <- args[is_dup_arg]
      if (length(dup_args)) {
        line1 <- as.integer(xml2::xml_attr(dup_args, "line1"))
        line2 <- as.integer(xml2::xml_attr(dup_args, "line2"))
        col1 <- as.integer(xml2::xml_attr(dup_args, "col1"))
        col2 <- as.integer(xml2::xml_attr(dup_args, "col2"))
        col2[line2 > line1] <- nchar(source_file$file_lines[line1[line2 > line1]])
        lapply(seq_along(dup_args), function(i) {
          Lint(
            filename = source_file$filename,
            line_number = line1[[i]],
            column_number = col1[[i]],
            type = "warning",
            message = "Duplicate arguments in function call.",
            line = source_file$file_lines[line1[[i]]],
            ranges = list(c(col1[[i]], col2[[i]]))
          )
        })
      }
    })

    unlist(result, recursive = FALSE, use.names = FALSE)
  })
}
