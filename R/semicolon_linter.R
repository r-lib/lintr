#' Semicolon linter
#'
#' Check that no semicolons terminate expressions.
#'
#' @param allow_compound Logical, default `FALSE`. If `TRUE`, "compound"
#'   semicolons (e.g. as in `x; y`, i.e., on the same line of code) are allowed.
#' @param allow_trailing Logical, default `FALSE`. If `TRUE`, "trailing"
#'   semicolons (i.e., those that terminate lines of code) are allowed.
#' @evalRd rd_tags("semicolon_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#semicolons>
#' @export
semicolon_linter <- function(allow_compound = FALSE, allow_trailing = FALSE) {
  msg_trailing <- "Trailing semicolons are not needed."
  msg_compound <- "Compound semicolons are discouraged. Replace them by a newline."

  if (allow_compound && allow_trailing) {
    stop("At least one of `allow_compound` or `allow_trailing` must be FALSE, otherwise no lints can be generated.")
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

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content
    bad_exprs <- xml2::xml_find_all(xml, xpath)
    if (need_detection) {
      is_trailing <- is.na(xml2::xml_find_first(bad_exprs, compound_xpath))
      msg <- ifelse(is_trailing, msg_trailing, msg_compound)
    }

    xml_nodes_to_lints(
      bad_exprs,
      source_expression = source_expression,
      lint_message = msg
    )
  })
}

#' @rdname semicolon_linter
#' @param semicolon A character vector defining which semicolons to report:
#' \describe{
#'   \item{compound}{Semicolons that separate two statements on the same line.}
#'   \item{trailing}{Semicolons following the last statement on the line.}
#' }
#' @export
semicolon_terminator_linter <- function(semicolon = c("compound", "trailing")) {
  lintr_deprecated(
    old = "semicolon_terminator_linter",
    new = "semicolon_linter",
    version = "3.0.0",
    type = "Linter"
  )
  semicolon <- match.arg(semicolon, several.ok = TRUE)
  allow_compound <- !"compound" %in% semicolon
  allow_trailing <- !"trailing" %in% semicolon
  semicolon_linter(allow_compound, allow_trailing)
}
