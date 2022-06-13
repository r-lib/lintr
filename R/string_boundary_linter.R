#' Require usage of startsWith() and endsWith() over grepl()/substr() versions
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
#'   missing inputs may be surprising to some readers.
#'
#' @param allow_grepl Logical, default `FALSE`. If `TRUE`, usages with `grepl()`
#'   are ignored. Some authors may prefer the `NA` input to `FALSE` output
#'   conciseness offered by `grepl()`, which doesn't have a direct equivalent
#'   with `startsWith()` or `endsWith()`.
#' @evalRd rd_tags("string_boundary_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
string_boundary_linter <- function(allow_grepl = FALSE) {
  str_cond <- xp_and(
    "string-length(text()) > 3",
    "contains(text(), '^') or contains(text(), '$')"
  )
  str_detect_xpath <- glue::glue("//expr[
    expr[1][SYMBOL_FUNCTION_CALL[text() = 'str_detect']]
  ]/expr[3]/STR_CONST[ {str_cond} ]")

  if (!allow_grepl) {
    grepl_xpath <- glue::glue("//expr[
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'grepl']]
      and not(SYMBOL_SUB[
        text() = 'ignore.case'
        and not(following-sibling::expr[1][NUM_CONST[text() = 'FALSE'] or SYMBOL[text() = 'F']])
      ])
      and not(SYMBOL_SUB[
        text() = 'fixed'
        and not(following-sibling::expr[1][NUM_CONST[text() = 'FALSE'] or SYMBOL[text() = 'F']])
      ])
    ]/expr[2]/STR_CONST[ {str_cond} ]")
  }

  get_regex_lint_data <- function(xml, xpath) {
    expr <- xml2::xml_find_all(xml, xpath)
    patterns <- get_r_string(expr)
    initial_anchor <- startsWith(patterns, "^")
    search_start <- 1L + initial_anchor
    search_end <- nchar(patterns) - 1L + initial_anchor
    can_replace <- is_not_regex(substr(patterns, search_start, search_end))
    list(lint_expr = expr[can_replace], initial_anchor = initial_anchor[can_replace])
  }

  substr_xpath <- "//expr[
    (EQ or NE)
    and expr[STR_CONST]
    and expr[
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'substr' or text() = 'substring']]
      and expr[
        (
          position() = 3
          and NUM_CONST[text() = '1' or text() = '1L']
        ) or (
          position() = 4
          and expr[1][SYMBOL_FUNCTION_CALL[text() = 'nchar']]
          and expr[position() = 2] = preceding-sibling::expr[2]
        )
      ]
    ]
  ]"
  substr_arg2_xpath <- "string(./expr[expr[1][SYMBOL_FUNCTION_CALL]]/expr[3])"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content
    lints <- list()

    str_detect_lint_data <- get_regex_lint_data(xml, str_detect_xpath)
    str_detect_lint_message <- paste(
      ifelse(
        str_detect_lint_data$initial_anchor,
        "Use startsWith() to detect a fixed initial substring.",
        "Use endsWith() to detect a fixed terminal substring."
      ),
      "Doing so is more readable and more efficient."
    )

    lints <- c(lints, xml_nodes_to_lints(
      str_detect_lint_data$lint_expr,
      source_expression = source_expression,
      lint_message = str_detect_lint_message,
      type = "warning"
    ))

    if (!allow_grepl) {
      grepl_lint_data <- get_regex_lint_data(xml, grepl_xpath)
      grepl_replacement <- ifelse(grepl_lint_data$initial_anchor, "startsWith", "endsWith")
      grepl_type <- ifelse(grepl_lint_data$initial_anchor, "initial", "terminal")
      grepl_lint_message <- paste(
        sprintf(
          "Use !is.na(x) & %s(x, string) to detect a fixed %s substring, or, if missingness is not a concern, just %s.",
          grepl_replacement, grepl_type, grepl_replacement
        ),
        "Doing so is more readable and more efficient."
      )

      lints <- c(lints, xml_nodes_to_lints(
        grepl_lint_data$lint_expr,
        source_expression = source_expression,
        lint_message = grepl_lint_message,
        type = "warning"
      ))
    }

    substr_expr <- xml2::xml_find_all(xml, substr_xpath)
    substr_one <- xml2::xml_find_chr(substr_expr, substr_arg2_xpath) %in% c("1", "1L")
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
