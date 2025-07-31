#' Require usage of `startsWith()` and `endsWith()` over `grepl()`/`substr()` versions
#'
#' [startsWith()] is used to detect fixed initial substrings; it is more
#'   readable and more efficient than equivalents using [grepl()] or [substr()].
#'   c.f. `startsWith(x, "abc")`, `grepl("^abc", x)`,
#'   `substr(x, 1L, 3L) == "abc"`.
#'
#' Ditto for using [endsWith()] to detect fixed terminal substrings.
#'
#' Note that there is a difference in behavior between how `grepl()` and `startsWith()`
#'   (and `endsWith()`) handle missing values. In particular, for `grepl()`, `NA` inputs
#'   are considered `FALSE`, while for `startsWith()`, `NA` inputs have `NA` outputs.
#'   That means the strict equivalent of `grepl("^abc", x)` is
#'   `!is.na(x) & startsWith(x, "abc")`.
#'
#' We lint `grepl()` usages by default because the `!is.na()` version is more explicit
#'   with respect to `NA` handling -- though documented, the way `grepl()` handles
#'   missing inputs may be surprising to some users.
#'
#' @param allow_grepl Logical, default `FALSE`. If `TRUE`, usages with `grepl()`
#'   are ignored. Some authors may prefer the conciseness offered by `grepl()` whereby
#'   `NA` input maps to `FALSE` output, which doesn't have a direct equivalent
#'   with `startsWith()` or `endsWith()`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'grepl("^a", x)',
#'   linters = string_boundary_linter()
#' )
#'
#' lint(
#'   text = 'grepl("z$", x)',
#'   linters = string_boundary_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'startsWith(x, "a")',
#'   linters = string_boundary_linter()
#' )
#'
#' lint(
#'   text = 'endsWith(x, "z")',
#'   linters = string_boundary_linter()
#' )
#'
#' # If missing values are present, the suggested alternative wouldn't be strictly
#' # equivalent, so this linter can also be turned off in such cases.
#' lint(
#'   text = 'grepl("z$", x)',
#'   linters = string_boundary_linter(allow_grepl = TRUE)
#' )
#'
#' @evalRd rd_tags("string_boundary_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
string_boundary_linter <- function(allow_grepl = FALSE) {
  str_cond <- xp_and(
    "string-length(text()) > 3",
    "contains(text(), '^') or contains(text(), '$')"
  )
  str_detect_xpath <- glue("
  following-sibling::expr[2]
    /STR_CONST[ {str_cond} ]
  ")
  str_detect_message_map <- c(
    both = "Use == to check for an exact string match.",
    initial = "Use startsWith() to detect a fixed initial substring.",
    terminal = "Use endsWith() to detect a fixed terminal substring."
  )

  if (!allow_grepl) {
    grepl_xpath <- glue("
    parent::expr[
      not(SYMBOL_SUB[
        text() = 'ignore.case'
        and not(following-sibling::expr[1][NUM_CONST[text() = 'FALSE'] or SYMBOL[text() = 'F']])
      ])
      and not(SYMBOL_SUB[
        text() = 'fixed'
        and not(following-sibling::expr[1][NUM_CONST[text() = 'FALSE'] or SYMBOL[text() = 'F']])
      ])
    ]
      /expr[2]
      /STR_CONST[ {str_cond} ]
    ")
    grepl_lint_fmt <- paste(
      "Use !is.na(x) & %1$s(x, string) to detect a fixed %2$s substring, or,",
      "if missingness is not a concern, just %1$s()."
    )
    grepl_message_map <- c(
      both = "Use == to check for an exact string match.",
      initial = sprintf(grepl_lint_fmt, "startsWith", "initial"),
      terminal = sprintf(grepl_lint_fmt, "endsWith", "terminal")
    )
  }

  get_regex_lint_data <- function(xml, xpath) {
    expr <- xml_find_all(xml, xpath)
    patterns <- get_r_string(expr)
    initial_anchor <- startsWith(patterns, "^")
    terminal_anchor <- endsWith(patterns, "$")
    search_start <- 1L + initial_anchor
    search_end <- nchar(patterns) - terminal_anchor
    should_lint <- (initial_anchor | terminal_anchor) &
      is_not_regex(substr(patterns, search_start, search_end))
    initial_anchor <- initial_anchor[should_lint]
    terminal_anchor <- terminal_anchor[should_lint]

    lint_type <- character(length(initial_anchor))

    lint_type[initial_anchor & terminal_anchor] <- "both"
    lint_type[initial_anchor & !terminal_anchor] <- "initial"
    lint_type[!initial_anchor & terminal_anchor] <- "terminal"
    list(lint_expr = expr[should_lint], lint_type = lint_type)
  }

  string_comparison_xpath <- "self::*[(EQ or NE) and expr/STR_CONST]"
  substr_xpath <- glue("
  self::*[expr/expr[
    (
      position() = 3
      and NUM_CONST[text() = '1' or text() = '1L']
    ) or (
      position() = 4
      and expr[1][SYMBOL_FUNCTION_CALL[text() = 'nchar']]
      and expr[position() = 2] = preceding-sibling::expr[2]
    )
  ]]")

  substr_arg2_xpath <- "string(./expr[expr[1][SYMBOL_FUNCTION_CALL]]/expr[3])"

  Linter(linter_level = "expression", function(source_expression) {
    lints <- list()

    str_detect_lint_data <- get_regex_lint_data(
      source_expression$xml_find_function_calls("str_detect"),
      str_detect_xpath
    )
    str_detect_lint_message <- str_detect_message_map[str_detect_lint_data$lint_type]

    lints <- c(lints, xml_nodes_to_lints(
      str_detect_lint_data$lint_expr,
      source_expression = source_expression,
      lint_message = paste(str_detect_lint_message, "Doing so is more readable and more efficient."),
      type = "warning"
    ))

    if (!allow_grepl) {
      grepl_lint_data <- get_regex_lint_data(source_expression$xml_find_function_calls("grepl"), grepl_xpath)
      grepl_lint_message <- grepl_message_map[grepl_lint_data$lint_type]

      lints <- c(lints, xml_nodes_to_lints(
        grepl_lint_data$lint_expr,
        source_expression = source_expression,
        lint_message = paste(grepl_lint_message, "Doing so is more readable and more efficient."),
        type = "warning"
      ))
    }

    substr_calls <- xml_parent(xml_parent(
      source_expression$xml_find_function_calls(c("substr", "substring"))
    ))
    is_str_comparison <- !is.na(xml_find_first(substr_calls, string_comparison_xpath))
    substr_calls <- strip_comments_from_subtree(substr_calls[is_str_comparison])
    substr_expr <- xml_find_all(substr_calls, substr_xpath)
    substr_one <- xml_find_chr(substr_expr, substr_arg2_xpath) %in% c("1", "1L")
    substr_lint_message <- paste(
      ifelse(
        substr_one,
        "Use startsWith() to detect an initial substring.",
        "Use endsWith() to detect a terminal substring."
      ),
      "Doing so is more readable and more efficient."
    )

    lints <- c(lints, xml_nodes_to_lints(substr_expr, source_expression, substr_lint_message, type = "warning"))
    lints
  })
}
