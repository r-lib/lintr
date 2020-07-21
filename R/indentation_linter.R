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
    # short-circuit on global expression as to not double-lint indentations
    if (is.null(source_file$parsed_content)) return(list())
    pc <- add_indentation_data(source_file$parsed_content)

    # specify symbols to ignore for generalized indentation
    ignored_tokens <- c("')'", "'}'", "','", "SYMBOL_FORMALS", "COMMENT")

    # calculate linty indenting
    linty <- with(pc, list(
      closing_curly = token == "'}'" & rel_indent != 0L,
      closing_paren = token == "')'" & !is_func_top_level & rel_indent != 0L,
      generalized_func_header = line1 != line1.par &
        token == "SYMBOL_FORMALS" &
        rel_func_indent != indent,
      hanging_func_header = line1 != line1.par &
        token == "SYMBOL_FORMALS" &
        rel_func_open_paren_indent != 0L,
      indent = !is_func_top_level &
        !token %in% ignored_tokens &
        line1 != line1.par &
        rel_indent != indent))

    # filter out NAs from linty results, caused by missing parent
    linty <- lapply(linty, vapply, isTRUE, logical(1L))

    # filter out nested linty expressions to avoid cascading indentation lints
    if (parent_only && any(linty$indent)) {
      nested_linty_lines <- unique(unlist(apply(
        pc[linty$indent,],
        1L,
        function(row) tail(row["line1"]:row["line2"], -1L))))
      linty$indent <- linty$indent & !pc$line1 %in% nested_linty_lines
    }

    hanging_func_header_lints <- mapply(
      Lint,
      line_number = pc[linty$hanging_func_header, "line1"],
      column_number = pc[linty$hanging_func_header, "col1"],
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

    generalizedd_func_header_lints <- mapply(
      Lint,
      line_number = pc[linty$generalized_func_header, "line1"],
      column_number = pc[linty$generalized_func_header, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = sprintf(
          paste0(
            "Function arguments that wrap to a new line should be indented by ",
            "%d characters."),
          indent),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    closing_curly_indent_lints <- mapply(
      Lint,
      line_number = pc[linty$closing_curly, "line1"],
      column_number = pc[linty$closing_curly, "col1"],
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
      line_number = pc[linty$closing_paren, "line1"],
      column_number = pc[linty$closing_paren, "col1"],
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
      line_number = pc[linty$indent, "line1"],
      column_number = pc[linty$indent, "col1"],
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

    header_indent_lints <- if (func_header_to_open_paren) {
      hanging_func_header_lints
    } else {
      generalizedd_func_header_lints
    }

    flatten_lints(list(
      header_indent_lints,
      if (func_call_closing_paren) closing_paren_indent_lints,
      closing_curly_indent_lints,
      expr_indent_lints))
  }
}


#' Add indentation data to the parsed content
#'
#' @param pc parsed_content as produced by \code{get_source_file}
#' @return a modified parsed_content data.frame with additional fields
#'   relating to line indentation, function header indentation and indentation
#'   relative to a parent expression's line.
#'
add_indentation_data <- function(pc) {
  # build a data.frame of parent expressions for each function call
  function_tokens <- pc[pc$token == "FUNCTION",]

  # only use one of redundant wrapping expressions and terminal symbols,
  # prefer non-wrapping expressions
  pc <- pc[order(pc$line1, pc$col1, -pc$parent),]
  pc <- pc[!duplicated(pc[c("line1", "col1", "line2", "col2")]),]

  # flag components of a function header
  pc$is_func_top_level <- pc$parent %in% function_tokens$parent

  # short-circuit on empty expression content
  if (!nrow(pc)) return(list())

  # calculate minimum indentation across expressions starting on same line
  pc$line_indent <- pc$col1
  pc_indent <- do.call(
    rbind,
    lapply(
      split(pc, pc$line1, drop = TRUE),
      subset,
      seq_along(line_indent) == which.min(line_indent)))

  # calculate indentation at the start of each function header
  func_header_indent <- pc[pc$token == "FUNCTION",]
  # TODO: this assumes no space between 'function' and '('
  func_header_indent$func_indent = func_header_indent$col1
  func_header_indent$func_open_paren_indent = func_header_indent$col2 + 2L

  # associate per-line minimum indentation level of each line with all
  # expressions starting on that line
  pc <- merge(
    pc[-which(names(pc) %in% "line_indent")],
    pc_indent[c("line1", "line_indent")],
    by = "line1")
  pc <- merge(
    pc,
    func_header_indent[c("parent", "func_indent", "func_open_paren_indent")],
    by = "parent",
    all.x = TRUE)

  # update line indent for multi-line function headers
  #  - `func_line_indent` represent per-line func header indentation
  #  - `line_indent` represents indentation level for beginning of func header
  pc <- pc[order(pc$parent, pc$line1, pc$col1),]
  is_header <- pc$parent %in% func_header_indent$parent
  pc[is_header, "func_line_indent"] <- pc[is_header, "line_indent"]
  pc[is_header, "line_indent"] <- unlist(lapply(
    split(pc[is_header,], pc[is_header, "parent"]),
    function(i) rep(min(i$line_indent), nrow(i))))

  # merge on parent id to associate parent scope indentation level
  pc <- merge(
    pc,
    pc,
    by.x = "parent",
    by.y = "id",
    suffixes = c("", ".par"),
    all.x = TRUE)

  # calculate indentation relative to parent
  pc$rel_indent <- with(pc, line_indent - line_indent.par)
  pc$rel_func_indent <- with(pc, func_line_indent - line_indent)
  pc$rel_func_open_paren_indent <- with(pc, func_line_indent - func_open_paren_indent)

  attr(pc$line_indent, "label") <- "indentation of the line on which an expression begins"
  attr(pc$func_line_indent, "label") <- "indentation of lines within the function header"
  attr(pc$func_open_paren_indent, "label") <- "indentation level following the opening parenthesis of a function header"
  attr(pc$rel_indent, "label") <- "indentation relative to the parent expression in characters"
  attr(pc$rel_func_indent, "label") <- "indentation relative to the function declaration parent expression in characters"
  attr(pc$rel_func_open_paren_indent, "label") <- "indentation relative to the opening parenthesis function header"

  pc
}
