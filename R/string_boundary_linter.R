#' Require usage of startsWith() and endsWith() over grepl()/substr() versions
#'
#' [startsWith()] is used to detect fixed initial substrings; it is more
#'   readable and more efficient than equivalents using [grepl()] or [substr()].
#'   c.f. `startsWith(x, "abc")`, `grepl("^abc", x)`,
#'   `substr(x, 1L, 3L) == "abc"`.
#'
#' Ditto for using [endsWith()] to detect fixed terminal substrings.
#'
#' @evalRd rd_tags("string_boundary_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
string_boundary_linter <- function() {
  str_cond <- xp_and(
    "string-length(text()) > 3",
    "contains(text(), '^') or contains(text(), '$')"
  )
  grepl_xpath <- glue::glue("//expr[
    expr[SYMBOL_FUNCTION_CALL[text() = 'grepl']]
    and not(SYMBOL_SUB[
      text() = 'ignore.case'
      and not(following-sibling::expr[1][NUM_CONST[text() = 'FALSE'] or SYMBOL[text() = 'F']])
    ])
    and not(SYMBOL_SUB[
      text() = 'fixed'
      and not(following-sibling::expr[1][NUM_CONST[text() = 'FALSE'] or SYMBOL[text() = 'F']])
    ])
  ]/expr[2]/STR_CONST[ {str_cond} ]")
  str_detect_xpath <- glue::glue("//expr[
    expr[SYMBOL_FUNCTION_CALL[text() = 'str_detect']]
  ]/expr[3]/STR_CONST[ {str_cond} ]")
  regex_xpath <- paste(grepl_xpath, "|", str_detect_xpath)

  substr_xpath <- "//expr[
    (EQ or NE)
    and expr[STR_CONST]
    and expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'substr' or text() = 'substring']]
      and expr[
        (
          position() = 3
          and NUM_CONST[text() = '1' or text() = '1L']
        ) or (
          position() = 4
          and expr[SYMBOL_FUNCTION_CALL[text() = 'nchar']]
          and expr[position() = 2] = preceding-sibling::expr[2]
        )
      ]
    ]
  ]"
  substr_arg2_xpath <- "string(./expr[expr[SYMBOL_FUNCTION_CALL]]/expr[3])"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    regex_expr <- xml2::xml_find_all(xml, regex_xpath)
    patterns <- get_r_string(regex_expr)
    initial_anchor <- startsWith(patterns, "^")
    search_start <- 1L + initial_anchor
    search_end <- nchar(patterns) - 1L + initial_anchor
    can_replace <- is_not_regex(substr(patterns, search_start, search_end))
    regex_expr <- regex_expr[can_replace]
    regex_lint_message <- paste(
      ifelse(
        initial_anchor[can_replace],
        "Use startsWith() to detect a fixed initial substring.",
        "Use endsWith() to detect a fixed terminal substring."
      ),
      "Doing so is more readable and more efficient.
    )

    regex_lints <- xml_nodes_to_lints(regex_expr, source_expression, regex_lint_message, type = "warning")

    substr_expr <- xml2::xml_find_all(xml, substr_xpath)
    substr_one <- xml2::xml_find_chr(substr_expr, substr_arg2_xpath) %in% c("1", "1L")
    substr_lint_message <- ifelse(
      substr_one,
      "Use startsWith() to detect an initial substring.",
      "Use endsWith() to detect a terminal substring."
    )

    substr_lints <- xml_nodes_to_lints(substr_expr, source_expression, substr_lint_message, type = "warning")
    return(c(regex_lints, substr_lints))
  })
}
