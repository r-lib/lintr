#'
#' @param indent The number of characters to indent each expression.
#' @param outermost_only Whether all offending lines should be reported as lint,
#'   or only the outermost offending expression. Default is \code{TRUE}, only
#'   the outermost expressions will be flagged.
#' @param func_header_to_open_paren Whether function header parameters should be
#'   wrapped to the indentation level of the first parameter, immediately
#'   following an opening parenthesis or should otherwise be treated as a
#'   regular expression, indenting to one indentation level. The default is
#'   \code{TRUE}, to follow the tidyverse style guide, indenting to the first
#'   parameter.
#' @param func_call_closing_paren Whether closing parenthesis of function calls
#'   should
#'
#' @describeIn linters Indent nested expressions and return to the parent
#'   indentation for closing multi-line expressions with curly-braces.
#'
#' @export
indentation_linter <- function(indent = 2L, outermost_only = TRUE,
                               func_header_to_open_paren = TRUE,
                               func_call_closing_paren = TRUE,
                               expr_closing_curly = TRUE) {

  function(source_file) {
    # short-circuit on global expression as to not double-lint indentations
    if (is.null(source_file$parsed_content)) return(list())
    pc <- add_indentation_data(source_file$parsed_content)

    # specify symbols to ignore for generalized indentation
    ignored_tokens <- c("')'", "'}'", "','", "SYMBOL_FORMALS", "COMMENT")

    # calculate linty indenting
    linty <- with(pc, list(
      closing_curly =
        token == "'}'" &
        line_indent != base_indent.par,
      closing_paren =
        token == "')'" &
        !is_kw_expr &   # ignore closing parens from FUNCTION, IF, FOR, WHILE
        !token.par %in% "forcond" &  # handle FOR conditions parsing explicitly
        line_indent != line_indent.par,
      generalized_func_header =
        line1 != line1.par &
        token == "SYMBOL_FORMALS" &
        line_indent != kw_expr_line_indent + indent,
      hanging_func_header =
        line1 != line1.par &
        token == "SYMBOL_FORMALS" &
        line_indent != kw_open_paren_indent + 1L,
      indent =
        !token %in% ignored_tokens &
        !is_kw_expr &  # ignore closing parens from FUNCTION, IF, FOR, WHILE
        line1 != line1.par &
        line_indent != base_indent.par + indent))

    # filter out NAs from linty results, caused by missing parent
    linty <- lapply(linty, vapply, isTRUE, logical(1L))

    # filter out nested linty expressions to avoid cascading indentation lints
    if (outermost_only && any(linty$indent)) {
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
      if (expr_closing_curly) closing_curly_indent_lints,
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
  pc <- pc[!duplicated(pc[c("line1", "col1", "line2", "col2")]),]
  pc <- pc[order(pc$line1, pc$col1, pc$parent),]

  # construct new parsed content df with columns for indentation data
  pc <- cbind(pc,
    line_indent = numeric(1L),
    base_indent = numeric(1L),
    is_kw_expr = logical(1L),
    kw_open_paren_indent = numeric(1L),
    kw_expr_line_indent = numeric(1L))

  pc$line_indent <- as.numeric(unlist(lapply(
    split(pc, pc$line1, drop = TRUE),
    function(i) rep(min(i$col1), nrow(i)))))

  # calculate indentation at the start of each keyword's opening
  # parenthesis (e.g. `if (_`, `for (_`)
  kw_tokens <- pc[pc$token %in% c("FUNCTION", "IF", "FOR", "forcond", "WHILE"),]
  pc$is_kw_expr <- pc$parent %in% unique(kw_tokens$parent)

  # for each keyword syntax block, find opening paren and indentation
  kw_data <- do.call(rbind, lapply(
    split(pc[pc$is_kw_expr,], pc[pc$is_kw_expr, "parent"]),
    function(i) {
      kw_open_paren_indent_i <- c(i[i$token == "'('", "col2"], NA_real_)[1]
      kw_expr_line_indent_i <- min(i$line_indent)
      c(kw_open_paren_indent_i, kw_expr_line_indent_i, nrow(i))
    }))
  # build index to replicate across all expressions of each keyword syntax block
  kw_indx <- rep(seq_along(kw_data[, 3L]), times = kw_data[, 3L])

  pc[pc$is_kw_expr, "kw_open_paren_indent"] <- as.numeric(kw_data[kw_indx, 1L])
  pc[pc$is_kw_expr, "kw_expr_line_indent"] <- as.numeric(kw_data[kw_indx, 2L])
  pc$base_indent <- ifelse(pc$is_kw_expr, pc$kw_expr_line_indent, pc$line_indent)

  # associate parent information for nested expressions
  pc <- merge(
    pc,
    pc,
    by.x = "parent",
    by.y = "id",
    suffixes = c("", ".par"),
    all.x = TRUE)

  pc
}
