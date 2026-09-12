#' Infix spaces linter
#'
#' Check that infix operators are surrounded by spaces. Enforces the corresponding Tidyverse style guide rule;
#'   see <https://style.tidyverse.org/syntax.html#infix-operators>. Spacing can be customized per operator,
#'   e.g. to require no space around `=` in named arguments as in the Bioconductor style guide.
#'
#' @param default_style Spacing style for operators not listed in `overrides`. `"multiple"` (the default)
#'   requires at least one space on each side, so that usage like `x  =  2` is allowed for code alignment;
#'   `"one"` requires exactly one space on each side.
#' @param overrides Named list of character vectors assigning a spacing style to specific operators. Names are
#'   the style, one of `"multiple"`, `"one"`, `"none"` (no space on either side) or `"any"` (not linted), and
#'   values are the operators, e.g. `list(none = c("EQ_SUB", "EQ_FORMALS"), any = "%%")`. Each operator can
#'   appear only once.
#'   The linter considers the following "low-precedence" operators:
#'   `+`, `-`, `~`, `>`, `>=`, `<`, `<=`, `==`, `!=`, `&`, `&&`, `|`, `||`, `<-`, `:=`, `<<-`, `->`, `->>`,
#'   `=`, `/`, `*`, `|>`, and any infix operator (refer to infixes by `"%%"`). Note that `"="` here includes
#'   three different operators, from the parser's point of view. To target only some of these, pass the
#'   corresponding parse tags (i.e., some of `"EQ_ASSIGN"`, `"EQ_SUB"`, and `"EQ_FORMALS"`; see
#'   [utils::getParseData()]).
#' @param exclude_operators (Deprecated) Use `overrides = list(any = ...)` instead.
#' @param allow_multiple_spaces (Deprecated) Use `default_style = "one"` instead of `FALSE`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x<-1L",
#'   linters = infix_spaces_linter()
#' )
#'
#' lint(
#'   text = "1:4 %>%sum()",
#'   linters = infix_spaces_linter()
#' )
#'
#' lint(
#'   text = "sum(1:10, na.rm = TRUE)",
#'   linters = infix_spaces_linter(overrides = list(none = c("EQ_SUB", "EQ_FORMALS")))
#' )
#'
#' # okay
#' lint(
#'   text = "x <- 1L",
#'   linters = infix_spaces_linter()
#' )
#'
#' lint(
#'   text = "1:4 %>% sum()",
#'   linters = infix_spaces_linter()
#' )
#'
#' code_lines <- "
#' ab     <- 1L
#' abcdef <- 2L
#' "
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = infix_spaces_linter(default_style = "multiple")
#' )
#'
#' lint(
#'   text = "a||b",
#'   linters = infix_spaces_linter(overrides = list(any = "||"))
#' )
#'
#' lint(
#'   text = "sum(1:10, na.rm=TRUE)",
#'   linters = infix_spaces_linter(overrides = list(none = c("EQ_SUB", "EQ_FORMALS")))
#' )
#'
#' @evalRd rd_tags("infix_spaces_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#infix-operators>
#' - <https://contributions.bioconductor.org/r-code.html#use-of-space>
#' @export
infix_spaces_linter <- function(default_style = c("multiple", "one"),
                                overrides = NULL,
                                exclude_operators = NULL,
                                allow_multiple_spaces = NULL) {
  default_style <- match.arg(default_style)

  if (!is.null(allow_multiple_spaces)) {
    lintr_deprecated(
      "allow_multiple_spaces",
      "default_style",
      version = "3.5.0",
      type = "Argument"
    )
    default_style <- if (allow_multiple_spaces) "multiple" else "one"
  }
  if (!is.null(exclude_operators)) {
    lintr_deprecated(
      "exclude_operators",
      "overrides = list(any = ...)",
      version = "3.5.0",
      type = "Argument"
    )
    overrides[["any"]] <- c(overrides[["any"]], exclude_operators)
  }

  operator_styles <- resolve_infix_styles(default_style, overrides)

  # NB: preceding-sibling::* and not preceding-sibling::expr because
  #   of the foo(a=1) case, where the tree is <SYMBOL_SUB><EQ_SUB><expr>
  # NB: parent::*[count(expr | SYMBOL_SUB)) > 1] for the unary case, e.g. x[-1]
  #  SYMBOL_SUB for case with missing argument like alist(a =)
  # NB: the last not() disables lints inside box::use() declarations
  xpath_template <- "({global_xpath})[
    parent::*[count(expr | SYMBOL_SUB) > 1]
    and (
      (
        @line1 = preceding-sibling::*[1]/@line2
        and @start {op} preceding-sibling::*[1]/@end + 2
      ) or (
        @line1 = following-sibling::*[1]/@line1
        and following-sibling::*[1]/@start {op} @end + 2
      )
    )
    and not(
      self::OP-SLASH[
        ancestor::expr/preceding-sibling::OP-LEFT-PAREN/preceding-sibling::expr[
          ./SYMBOL_PACKAGE[text() = 'box'] and
          ./SYMBOL_FUNCTION_CALL[text() = 'use']
        ]
      ]
    )
  ]"

  # for each style, the comparison of the gap between the operator and its neighbor
  #   (@start - @end, which is 1 for adjacent tokens) which constitutes a lint
  style_metadata <- list(
    multiple = list(op = "<", lint_message = "Put spaces around all infix operators."),
    one = list(op = "!=", lint_message = "Put exactly one space on each side of infix operators."),
    none = list(op = ">=", lint_message = "Put no spaces around `%s`.")
  )

  xpaths <- lapply(names(style_metadata), function(style) {
    infix_tokens <- infix_metadata$xml_tag_exact[operator_styles == style]
    if (length(infix_tokens) == 0L) {
      return(NULL)
    }
    glue(
      xpath_template,
      global_xpath = paste0("//", infix_tokens, collapse = "|"),
      op = style_metadata[[style]]$op
    )
  })
  names(xpaths) <- names(style_metadata)
  xpaths <- Filter(Negate(is.null), xpaths)

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    lints <- lapply(names(xpaths), function(style) {
      bad_expr <- xml_find_all_(xml, xpaths[[style]])
      lint_message <- style_metadata[[style]]$lint_message
      if (style == "none") {
        lint_message <- sprintf(lint_message, xml_text(bad_expr))
      }
      xml_nodes_to_lints(
        bad_expr,
        source_expression = source_expression,
        lint_message = lint_message,
        type = "style"
      )
    })
    unlist(lints, recursive = FALSE)
  })
}

# map every low-precedence operator in infix_metadata to a spacing style,
#   as a character vector aligned with the rows of infix_metadata (NA for
#   high-precedence operators, which are never linted)
resolve_infix_styles <- function(default_style, overrides) {
  check_infix_overrides(overrides)

  styles <- ifelse(infix_metadata$low_precedence, default_style, NA_character_)
  matched <- logical(nrow(infix_metadata))
  for (style in names(overrides)) {
    for (operator in overrides[[style]]) {
      # parse_tag, not xml_tag, since the former is easier for the user to discover with getParseData()
      is_operator <- infix_metadata$low_precedence &
        (infix_metadata$string_value == operator | infix_metadata$parse_tag == operator)
      if (!any(is_operator)) {
        cli_abort(c(
          "Unknown operator {.str {operator}} in {.code overrides${style}}.",
          i = "See {.help infix_spaces_linter} for the operators that can be specified."
        ))
      }
      if (any(matched & is_operator)) {
        cli_abort("Operator {.str {operator}} is given more than once in {.arg overrides}.")
      }
      matched <- matched | is_operator
      styles[is_operator] <- style
    }
  }
  styles
}

check_infix_overrides <- function(overrides) {
  if (is.null(overrides)) {
    return(invisible())
  }
  if (!is.list(overrides) || is.null(names(overrides)) || !all(nzchar(names(overrides)))) {
    cli_abort("{.arg overrides} must be a named list.")
  }
  valid_styles <- c("multiple", "one", "none", "any")
  invalid_styles <- setdiff(names(overrides), valid_styles)
  if (length(invalid_styles) > 0L) {
    cli_abort(c(
      "Names of {.arg overrides} must be among {.str {valid_styles}}.",
      i = "Found {.str {invalid_styles}}."
    ))
  }
  for (style in names(overrides)) {
    if (!is.character(overrides[[style]])) {
      cli_abort("{.code overrides${style}} must be a character vector.")
    }
  }
  invisible()
}
