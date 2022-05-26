#' `sprintf` linter
#'
#' Check for an inconsistent number of arguments or arguments with incompatible types (for literal arguments) in
#' `sprintf` calls.
#'
#' @evalRd rd_tags("sprintf_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
sprintf_linter <- function() {
  xpath <- "//expr[
    expr/SYMBOL_FUNCTION_CALL[text() = 'sprintf'] and
    (
      OP-LEFT-PAREN/following-sibling::expr[1]/STR_CONST or
      SYMBOL_SUB[text() = 'fmt']/following-sibling::expr[1]/STR_CONST
    ) and
    not(expr/SYMBOL[text() = '...'])
  ]"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    sprintf_calls <- xml2::xml_find_all(xml, xpath)

    message <- vapply(sprintf_calls, capture_sprintf_warning, character(1L))

    has_message <- !is.na(message)
    xml_nodes_to_lints(
      sprintf_calls[has_message],
      source_expression = source_expression,
      lint_message = message[has_message],
      type = "warning"
    )
  })
}

#' Zap sprintf() call to contain only constants
#'
#' Set all extra arguments to 0L if they aren't a constant
#'
#' @param parsed_expr A parsed `sprintf()` call
#'
#' @return A `sprintf()` call with all non-constants replaced by `0L`
#' (which is compatible with all sprintf format specifiers)
#'
#' @noRd
zap_extra_args <- function(parsed_expr) {
  is_missing <- function(x) {
    is.symbol(x) && !nzchar(x)
  }

  if ("fmt" %in% names(parsed_expr)) {
    fmt_loc <- which(names(parsed_expr) == "fmt")
  } else {
    fmt_loc <- 2L
  }

  if (length(parsed_expr) >= 3L) {
    for (i in setdiff(seq_along(parsed_expr), c(1L, fmt_loc))) {
      if (!is_missing(parsed_expr[[i]]) && !is.atomic(parsed_expr[[i]])) {
        parsed_expr[[i]] <- 0L
      }
    }
  }
  parsed_expr
}

#' Anticipate warnings of a sprintf() call
#'
#' Try running a static sprintf() call to determine whether it will produce warnings or errors due to format
#' misspecification
#'
#' @param xml An XML node representing a `sprintf()` call (i.e. the `<expr>` node containing the call)
#'
#' @return A string, either `NA_character_` or the text of generated errors and warnings from the `sprintf()` call when
#' replacing all dynamic components by 0, which is compatible with all format specifiers.
#'
#' @noRd
capture_sprintf_warning <- function(xml) {
  text <- xml2::xml_text(xml)
  parsed_expr <- try_silently(parse(text = text, keep.source = FALSE)[[1L]])
  parsed_expr <- zap_extra_args(parsed_expr)
  res <- tryCatch(eval(parsed_expr, envir = baseenv()), warning = identity, error = identity)
  if (inherits(res, "condition")) {
    conditionMessage(res)
  } else {
    NA_character_
  }
}
