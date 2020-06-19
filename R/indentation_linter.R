#'
#' @param indent The number of characters to indent each expression.
#' @param parent_only Whether all offending lines should be reported as lint, or
#'   only the outermost offending parent expression. Default is \code{TRUE},
#'   only the outermost expressions will be flagged.
#'
#' @describeIn linters Indent nested expressions and return to the parent
#'   indentation for closing multi-line expressions with curly-braces.
#'
#' @export
indentation_linter <- function(indent = 2L, parent_only = TRUE,
                               func_header_to_open_paren = TRUE,
                               func_call_closing_paren = TRUE) {

  function(source_file) {
    exprs <- source_file$parsed_content

    # short-circuit on global expression as to not double-lint indentations
    if (is.null(exprs)) return(list())

    # build a data.frame of parent expressions for each function call
    function_tokens <- exprs[exprs$token == "FUNCTION",]

    # only use one of redundant wrapping expressions and terminal symbols,
    # prefer non-wrapping expressions
    exprs <- exprs[order(exprs$line1, exprs$col1, -exprs$parent),]
    exprs <- exprs[!duplicated(exprs[c("line1", "col1", "line2", "col2")]),]
    # exprs <- exprs[exprs$parent > 0,]

    # flag components of a function header
    exprs$is_func_top_level <- exprs$parent %in% function_tokens$parent

    # short-circuit on empty expression content
    if (!nrow(exprs)) return(list())

    # calculate minimum indentation across expressions starting on same line
    exprs$line_indent <- exprs$col1
    exprs_indent <- do.call(
      rbind,
      lapply(
        split(exprs, exprs$line1, drop = TRUE),
        subset,
        seq_along(line_indent) == which.min(line_indent)))

    # calculate indentation at the start of each function header
    func_header_indent <- exprs[exprs$token == "FUNCTION",]
    # TODO: this assumes no space between 'function' and '('
    func_header_indent$func_indent = func_header_indent$col2 + 2L

    # associate per-line minimum indentation level of each line with all
    # expressions starting on that line
    exprs <- merge(
      exprs[-which(names(exprs) %in% "line_indent")],
      exprs_indent[c("line1", "line_indent")],
      by = "line1")
    exprs <- merge(
      exprs,
      func_header_indent[c("parent", "func_indent")],
      by = "parent",
      all.x = TRUE)

    # update line indent for multi-line function headers
    #  - `func_line_indent` represent per-line func header indentation
    #  - `line_indent` represents indentation level for beginning of func header
    exprs <- exprs[order(exprs$parent, exprs$line1, exprs$col1),]
    is_header <- exprs$parent %in% func_header_indent$parent
    exprs[is_header, "func_line_indent"] <- exprs[is_header, "line_indent"]
    exprs[is_header, "line_indent"] <- unlist(lapply(
      split(exprs[is_header,], exprs[is_header, "parent"]),
      function(i) rep(min(i$line_indent), nrow(i))))

    # merge on parent id to associate parent scope indentation level
    exprs <- merge(
      exprs,
      exprs,
      by.x = "parent",
      by.y = "id",
      suffixes = c("", ".par"),
      all.x = TRUE)

    # calculate indentation relative to parent
    exprs$rel_indent <- with(exprs, line_indent - line_indent.par)
    exprs$rel_func_indent <- with(exprs, func_line_indent - func_indent)
    exprs$bad_indent <- with(exprs, line1 != line1.par & rel_indent != indent)

    # calculate linty indenting
    ignored_tokens <- c("')'", "'}'", "','", "SYMBOL_FORMALS", "COMMENT")
    linty <- with(exprs, list(
      closing_curly = token == "'}'" & rel_indent != 0L,
      closing_paren = token == "')'" & !is_func_top_level & rel_indent != 0L,
      hanging_func_header = line1 != line1.par &
        token == "SYMBOL_FORMALS" &
        rel_func_indent != 0L,
      indent = !is_func_top_level & !token %in% ignored_tokens & bad_indent))

    # filter out NAs from linty results, caused by missing parent
    linty <- lapply(linty, vapply, isTRUE, logical(1L))

    # filter out nested linty expressions to avoid cascading indentation lints
    if (parent_only && any(linty$indent)) {
      nested_linty_lines <- unique(unlist(apply(
        exprs[linty$indent,],
        1L,
        function(row) tail(row["line1"]:row["line2"], -1L))))
      linty$indent <- linty$indent & !exprs$line1 %in% nested_linty_lines
    }

    hanging_func_header_lints <- mapply(
      Lint,
      line_number = exprs[linty$hanging_func_header, "line1"],
      column_number = exprs[linty$hanging_func_header, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = paste0(
          "Function arguments that wrap to a new line should be aligned with ",
          "the function header's opening parenthesis."),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    closing_curly_indent_lints <- mapply(
      Lint,
      line_number = exprs[linty$closing_curly, "line1"],
      column_number = exprs[linty$closing_curly, "col1"],
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

    closing_paren_indent_lints <- mapply(
      Lint,
      line_number = exprs[linty$closing_paren, "line1"],
      column_number = exprs[linty$closing_paren, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = paste0(
          "Closing parenthesis of multi-line function calls should be at the ",
          "same indentation level as the parent expression."),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    expr_indent_lints <- mapply(
      Lint,
      line_number = exprs[linty$indent, "line1"],
      column_number = exprs[linty$indent, "col1"],
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
      if (func_header_to_open_paren) hanging_func_header_lints,
      if (func_call_closing_paren) closing_paren_indent_lints,
      closing_curly_indent_lints,
      expr_indent_lints))
  }
}
