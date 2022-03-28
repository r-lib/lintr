#' Sequence linter
#'
#' Check for `1:length(...)`, `1:nrow(...)`, `1:ncol(...)`, `1:NROW(...)` and `1:NCOL(...)` expressions.
#' These often cause bugs when the right-hand side is zero.
#' It is safer to use [base::seq_len()] or [base::seq_along()] instead.
#'
#' @evalRd rd_tags("seq_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
seq_linter <- function() {
  Linter(function(source_file) {

    if (is.null(source_file$xml_parsed_content)) return(list())

    xml <- source_file$xml_parsed_content

    bad_funcs <- c("length", "nrow", "ncol", "NROW", "NCOL", "dim")

    xpath <- glue::glue("//expr[
      expr[NUM_CONST[text() =  '1' or text() =  '1L']]
      and OP-COLON
      and expr[expr[(expr|self::*)[SYMBOL_FUNCTION_CALL[ {xp_text_in_table(bad_funcs)} ]]]]
    ]")

    badx <- xml2::xml_find_all(xml, xpath)

    ## The actual order of the nodes is document order
    ## In practice we need to handle length(x):1
    get_fun <- function(x, n) {
      funcall <- xml2::xml_children(xml2::xml_children(x)[[n]])
      fun <- gsub("\\(.*\\)", "(...)", trim_ws(xml2::xml_text(funcall[[1]])))
      if (!fun %in% bad_funcs) fun else paste0(fun, "(...)")
    }

    ## Unfortunately the more natural lapply(badx, ...) does not work,
    ## because badx looses its class for length() and/or [[
    lapply(
      seq_along(badx),
      function(i) {
        x <- badx[[i]]
        f1 <- get_fun(x, 1)
        f2 <- get_fun(x, 3)
        line1 <- xml2::xml_attr(x, "line1")
        col1 <- xml2::xml_attr(x, "col1")
        col2 <- xml2::xml_attr(x, "col1")
        Lint(
          filename = source_file$filename,
          line_number = as.integer(line1),
          column_number = as.integer(col1),
          type = "warning",
          message = paste0(f1, ":", f2, " is likely to be wrong in the empty ",
                           "edge case, use seq_len."),
          line = source_file$lines[line1],
          ranges = list(c(as.integer(col1), as.integer(col2)))
        )
      }
    )
  })
}
