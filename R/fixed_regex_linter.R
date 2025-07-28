#' Require usage of `fixed=TRUE` in regular expressions where appropriate
#'
#' Invoking a regular expression engine is overkill for cases when the search
#'   pattern only involves static patterns.
#'
#' NB: for `stringr` functions, that means wrapping the pattern in `stringr::fixed()`.
#'
#' NB: this linter is likely not able to distinguish every possible case when
#'   a fixed regular expression is preferable, rather it seeks to identify
#'   likely cases. It should _never_ report false positives, however; please
#'   report false positives as an error.
#'
#' @param allow_unescaped Logical, default `FALSE`. If `TRUE`, only patterns that
#'   require regex escapes (e.g. `"\\$"` or `"[$]"`) will be linted. See examples.
#' @examples
#' # will produce lints
#' code_lines <- 'gsub("\\\\.", "", x)'
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = fixed_regex_linter()
#' )
#'
#' lint(
#'   text = 'grepl("a[*]b", x)',
#'   linters = fixed_regex_linter()
#' )
#'
#' lint(
#'   text = 'grepl("a[*]b", x)',
#'   linters = fixed_regex_linter(allow_unescaped = TRUE)
#' )
#'
#' code_lines <- 'stringr::str_subset(x, "\\\\$")'
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = fixed_regex_linter()
#' )
#'
#' lint(
#'   text = 'grepl("Munich", address)',
#'   linters = fixed_regex_linter()
#' )
#'
#' # okay
#' code_lines <- 'gsub("\\\\.", "", x, fixed = TRUE)'
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = fixed_regex_linter()
#' )
#'
#' lint(
#'   text = 'grepl("a*b", x, fixed = TRUE)',
#'   linters = fixed_regex_linter()
#' )
#'
#' lint(
#'   text = 'stringr::str_subset(x, stringr::fixed("$"))',
#'   linters = fixed_regex_linter()
#' )
#'
#' lint(
#'   text = 'grepl("Munich", address, fixed = TRUE)',
#'   linters = fixed_regex_linter()
#' )
#'
#' lint(
#'   text = 'grepl("Munich", address)',
#'   linters = fixed_regex_linter(allow_unescaped = TRUE)
#' )
#'
#' @evalRd rd_tags("fixed_regex_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
fixed_regex_linter <- function(allow_unescaped = FALSE) {
  # regular expression pattern is the first argument
  pos_1_regex_funs <- c(
    "grep", "grepv", "gsub", "sub", "regexec", "grepl", "regexpr", "gregexpr"
  )

  # regular expression pattern is the second argument
  pos_2_regex_funs <- c(
    # base functions.
    "strsplit",
    # data.table functions.
    "tstrsplit",
    # stringr functions.
    #   even though the user action is different
    #   (setting fixed=TRUE vs. wrapping stringr::fixed()),
    #   detection of the lint is the same
    "str_count", "str_detect", "str_ends", "str_extract", "str_extract_all",
    "str_locate", "str_locate_all", "str_match", "str_match_all",
    "str_remove", "str_remove_all", "str_replace", "str_replace_all",
    "str_split", "str_starts", "str_subset",
    "str_view", "str_view_all", "str_which"
  )

  pipes <- setdiff(magrittr_pipes, c("%$%", "%T>%"))
  in_pipe_cond <- glue("
    parent::expr/preceding-sibling::SPECIAL[{ xp_text_in_table(pipes) }]
    | parent::expr/preceding-sibling::PIPE
  ")

  # NB: strsplit doesn't have an ignore.case argument
  # NB: we intentionally exclude cases like gsub(x, c("a" = "b")), where "b" is fixed
  pos_1_xpath <- glue("
  self::*[
    not(following-sibling::SYMBOL_SUB[
      (text() = 'fixed' or text() = 'ignore.case')
      and following-sibling::expr[1][NUM_CONST[text() = 'TRUE'] or SYMBOL[text() = 'T']]
    ])
  ]
    /following-sibling::expr[
      (
        position() = 1
        and STR_CONST
        and not(EQ_SUB)
        and not({ in_pipe_cond })
      ) or (
        STR_CONST
        and preceding-sibling::*[not(self::COMMENT)][2][self::SYMBOL_SUB/text() = 'pattern']
      )
    ]
  ")
  pos_2_xpath <- glue("
  self::*[
    not(following-sibling::SYMBOL_SUB[
      text() = 'fixed'
      and following-sibling::expr[1][NUM_CONST[text() = 'TRUE'] or SYMBOL[text() = 'T']]
    ])
  ]
    /following-sibling::expr[
      position() = 2 - count({ in_pipe_cond })
      and STR_CONST
      and not(EQ_SUB)
    ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    pos_1_calls <- source_expression$xml_find_function_calls(pos_1_regex_funs)
    pos_2_calls <- source_expression$xml_find_function_calls(pos_2_regex_funs)
    patterns <- combine_nodesets(
      xml_find_all(pos_1_calls, pos_1_xpath),
      xml_find_all(pos_2_calls, pos_2_xpath)
    )
    pattern_strings <- get_r_string(patterns)

    is_static <- is_not_regex(pattern_strings, allow_unescaped)
    patterns <- patterns[is_static]
    pattern_strings <- pattern_strings[is_static]

    fixed_equivalent <- encodeString(get_fixed_string(pattern_strings), quote = '"', justify = "none")
    call_name <- xml_find_chr(patterns, "string(preceding-sibling::expr[last()]/SYMBOL_FUNCTION_CALL)")

    is_stringr <- startsWith(call_name, "str_")
    replacement_suggestion <- ifelse(
      is_stringr,
      sprintf("stringr::fixed(%s) as the pattern", fixed_equivalent),
      sprintf("%s with fixed = TRUE", fixed_equivalent)
    )
    msg <- paste(
      "Use", replacement_suggestion, "here. This regular expression is static, i.e.,",
      "its matches can be expressed as a fixed substring expression, which is faster to compute."
    )

    xml_nodes_to_lints(
      patterns,
      source_expression = source_expression,
      lint_message = msg,
      type = "warning"
    )
  })
}
