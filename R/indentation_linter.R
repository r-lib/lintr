#'
#' @param indent The number of characters to indent each expression.
#' @param outermost_only Whether all offending lines should be reported as lint,
#'   or only the outermost expression. Default is \code{TRUE}, only the
#'   outermost expressions.
#'
#' @describeIn linters Indent nested expressions and return to the parent
#'   indentation for closing multi-line expressions with curly-braces.
#'
#' @export
indentation_linter <- function(indent = 2L, outermost_only = TRUE) {
  function(source_file) {
    exprs <- source_file$parsed_content

    # short-circuit on global expression as to not double-lint indentations
    if (is.null(exprs)) return(list())

    # only use one of redundant wrapping expressions and terminal symbols
    exprs <- exprs[order(exprs$line1, exprs$col1, -exprs$parent),]
    exprs <- exprs[!duplicated(exprs[c("line1", "col1", "line2", "col2")]),]
    exprs <- exprs[exprs$parent > 0,]

    # short-circuit on empty expression content
    if (!nrow(exprs)) return(list())

    # calculate minimum indentation across expressions starting on same line
    exprs$line_indent <- pmin(exprs$col1, exprs$col2)
    exprs_indent <- do.call(
      rbind,
      lapply(
        split(exprs, exprs$line1),
        subset,
        seq_along(line_indent) == which.min(line_indent)))

    # associate per-line minimum indentation level of each line with all
    # expressions starting on that line
    exprs <- merge(
      exprs[-which(names(exprs) %in% "line_indent")],
      exprs_indent[c("line1", "line_indent")],
      by = "line1")

    # merge on parent id to associate parent scope indentation level
    exprs <- merge(
      exprs,
      exprs,
      by.x = "parent",
      by.y = "id",
      suffixes = c("", ".par"))

    # calculate indentation relative to parent
    exprs$rel_indent <- with(exprs, line_indent - line_indent.par)
    exprs$bad_indent <- with(exprs, line1 != line1.par & rel_indent != indent)

    # calculate linty indenting
    ignored_tokens <- c("')'", "'}'", "','")
    linty_closing_curly <- with(exprs, token == "'}'" & rel_indent != 0)
    linty_expr <- with(exprs, !token %in% ignored_tokens & bad_indent)

    # only lint the first offending expression per line
    linty_expr <- linty_expr & !duplicated(exprs[,"line1"] * linty_expr)

    # filter out nested linty expressions to avoid cascading indentation lints
    if (outermost_only && any(linty_expr)) {
      linty_lines <- unique(unlist(apply(exprs[linty_expr,], 1L, function(row) {
        tail(row["line1"]:row["line2"], -1L)
      })))
      linty_expr <- linty_expr & !exprs$line1 %in% linty_lines
      linty_expr <- !duplicated(linty_expr * exprs$line1) & linty_expr
    }

    closing_curly_indent_lints <- mapply(
      Lint,
      line_number = exprs[linty_closing_curly, "line1"],
      column_number = exprs[linty_closing_curly, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = paste0(
          "Closing curly-braces should be at the same indentation level as ",
          "the parent expression."),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    expr_indent_lints <- mapply(
      Lint,
      line_number = exprs[linty_expr, "line1"],
      column_number = exprs[linty_expr, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = sprintf(
          paste0(
            "Expressions that wrap to a new line should be indented by %d ",
            "characters."),
          indent),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    flatten_lints(list(
      closing_curly_indent_lints,
      expr_indent_lints))
  }
}
