#' Semicolon linter
#'
#' Check that no semicolons terminate expressions.
#'
#' @param allow_compound Logical, default `FALSE`. If `TRUE`, "compound"
#'   semicolons (e.g. as in `x; y`, i.e., on the same line of code) are allowed.
#' @param allow_trailing Logical, default `FALSE`. If `TRUE`, "trailing"
#'   semicolons (i.e., those that terminate lines of code) are allowed.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "a <- 1;",
#'   linters = semicolon_linter()
#' )
#'
#' lint(
#'   text = "a <- 1; b <- 1",
#'   linters = semicolon_linter()
#' )
#'
#' lint(
#'   text = "function() { a <- 1; b <- 1 }",
#'   linters = semicolon_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "a <- 1",
#'   linters = semicolon_linter()
#' )
#'
#' lint(
#'   text = "a <- 1;",
#'   linters = semicolon_linter(allow_trailing = TRUE)
#' )
#'
#' code_lines <- "a <- 1\nb <- 1"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = semicolon_linter()
#' )
#'
#' lint(
#'   text = "a <- 1; b <- 1",
#'   linters = semicolon_linter(allow_compound = TRUE)
#' )
#'
#' code_lines <- "function() { \n  a <- 1\n  b <- 1\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = semicolon_linter()
#' )
#'
#' @evalRd rd_tags("semicolon_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#semicolons>
#' @export
semicolon_linter <- function(allow_compound = FALSE, allow_trailing = FALSE) {
  msg_trailing <- "Remove trailing semicolons."
  msg_compound <- "Replace compound semicolons by a newline."

  if (allow_compound && allow_trailing) {
    cli_abort(c(
      x = "At least one of {.arg allow_compound} or {.arg allow_trailing} must be `FALSE`.",
      i = "No lints can be generated otherwise."
    ))
  } else if (allow_compound && !allow_trailing) {
    # lint only trailing
    xpath <- "//OP-SEMICOLON[not(@line1 = following-sibling::*[1]/@line1)]"
    msg <- msg_trailing # nolint: object_usage. (usage in linter, since need_detection == FALSE)
    need_detection <- FALSE
  } else if (!allow_compound && allow_trailing) {
    # lint only compound
    xpath <- "//OP-SEMICOLON[@line1 = following-sibling::*[1]/@line1]"
    msg <- msg_compound # nolint: object_usage. (usage in linter, since need_detection == FALSE)
    need_detection <- FALSE
  } else {
    # lint all
    xpath <- "//OP-SEMICOLON"
    need_detection <- TRUE
  }
  compound_xpath <- "self::*[@line1 = following-sibling::*[1]/@line1]"

  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content

    bad_exprs <- xml_find_all(xml, xpath)
    if (need_detection) {
      is_trailing <- is.na(xml_find_first(bad_exprs, compound_xpath))
      msg <- ifelse(is_trailing, msg_trailing, msg_compound)
    }

    xml_nodes_to_lints(
      bad_exprs,
      source_expression = source_expression,
      lint_message = msg
    )
  })
}
