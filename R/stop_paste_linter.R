#' @describeIn linters check for \code{stop(paste(...))},
#'   \code{message(paste0(...))}, \code{warning(paste(...))}, and similar
#'   invocations. These usages are basically redundant and interfere with
#'   efforts to make the package messaging portable across human languages.
#' @export
stop_paste_linter <- function(source_file) {

  if (!length(source_file$parsed_content)) return(list())

  xml <- source_file$xml_parsed_content
  if ("sep" %in% source_file$parsed_content$text) browser()

  translators <- c("message", "warning", "stop")
  concats <- c("paste", "paste0")
  bad_seps <- c('""', '" "')

  expr_cond_fmt <- paste0(
    "expr[SYMBOL_FUNCTION_CALL[%s]] and ",
    "expr[expr[SYMBOL_FUNCTION_CALL[%s]] and ",
    "  not(SYMBOL_SUB[text() = 'collapse']) and ",
    "  (not(SYMBOL_SUB[text() = 'sep']) or ",
    "    SYMBOL_SUB[text() = 'sep' and ",
    "      following-sibling::expr[STR_CONST[%s]]])] and ",
    "not(OP-COMMA)"
  )
  xpath <- sprintf(
    sprintf("//expr[%s]", expr_cond_fmt),
    paste("text() =", dQuote(translators, '"'), collapse = " or "),
    paste("text() =", dQuote(concats, '"'), collapse = " or "),
    paste("text() =", sQuote(bad_seps, "'"), collapse = " or ")
  )

  badx <- xml2::xml_find_all(xml, xpath)

  get_fun <- function(x, n) {
    funcall <- xml2::xml_children(xml2::xml_children(x)[[n]])
    if (!length(funcall)) return(NULL)
    gsub("\\(.*\\)", "", trim_ws(xml2::xml_text(funcall[[1]])))
  }

  ## Unfortunately the more natural lapply(badx, ...) does not work,
  ## because badx looses its class for length() and/or [[
  lapply(
    seq_along(badx),
    function(i) {
      x <- badx[[i]]
      f1 <- get_fun(x, 1L)
      f2 <- get_fun(x, 3L)
      line1 <- xml2::xml_attr(x, "line1")
      col1 <- as.integer(xml2::xml_attr(x, "col1"))
      col2 <- as.integer(xml2::xml_attr(x, "col2"))
      Lint(
        filename = source_file$filename,
        line_number = as.character(line1),
        column_number = col1,
        type = "warning",
        message = paste0("Avoid ", f1, "(", f2,
          "(...)) expressions, just use ", f1, "(...)."),
        line = source_file$lines[line1],
        ranges = list(c(col1, col2)),
        linter = "stop_paste_linter"
      )
    }
  )
}
