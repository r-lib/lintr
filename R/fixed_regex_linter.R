#' Require usage of `fixed=TRUE` in regular expressions where appropriate
#'
#' Invoking a regular expression engine is overkill for cases when the search
#'   pattern only involves static patterns.
#'
#' NB: for `stringr` functions, that means wrapping the pattern in `stringr::fixed()`.
#'
#' NB: This linter is likely not able to distinguish every possible case when
#'   a fixed regular expression is preferable, rather it seeks to identify
#'   likely cases. It should _never_ report false positives, however; please
#'   report false positives as an error.
#'
#' @evalRd rd_tags("fixed_regex_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
fixed_regex_linter <- function() {
  # regular expression pattern is the first argument
  pos_1_regex_funs <- xp_text_in_table(c(
    "grep", "gsub", "sub", "regexec", "grepl", "regexpr", "gregexpr"
  ))

  # regular expression pattern is the second argument
  pos_2_regex_funs <- xp_text_in_table(c(
    "strsplit", "tstrsplit",
    # stringr functions. even though the user action is different
    #   (setting fixed=TRUE vs. wrapping stringr::fixed()),
    #   detection of the lint is the same
    "str_count", "str_detect", "str_ends", "str_extract", "str_extract_all",
    "str_locate", "str_locate_all", "str_match", "str_match_all",
    "str_remove", "str_remove_all", "str_replace", "str_replace_all",
    "str_split", "str_starts", "str_subset",
    "str_view", "str_view_all", "str_which"
  ))

  # NB: strsplit doesn't have an ignore.case argument
  # NB: we intentionally exclude cases like gsub(x, c("a" = "b")), where "b" is fixed
  xpath <- glue::glue("//expr[1][
    SYMBOL_FUNCTION_CALL[ {pos_1_regex_funs} ]
    and not(following-sibling::SYMBOL_SUB[
      (text() = 'fixed' or text() = 'ignore.case')
      and following-sibling::expr[1][NUM_CONST[text() = 'TRUE'] or SYMBOL[text() = 'T']]
    ])
  ]
  /following-sibling::expr[1][STR_CONST and not(EQ_SUB)]
  |
  //expr[1][
    SYMBOL_FUNCTION_CALL[ {pos_2_regex_funs} ]
    and not(following-sibling::SYMBOL_SUB[
      text() = 'fixed'
      and following-sibling::expr[1][NUM_CONST[text() = 'TRUE'] or SYMBOL[text() = 'T']]
    ])
  ]
  /following-sibling::expr[2][STR_CONST and not(EQ_SUB)]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    patterns <- xml2::xml_find_all(xml, xpath)
    pattern_strings <- get_r_string(patterns)
    is_static <- is_not_regex(pattern_strings)

    fixed_equivalent <- encodeString(get_fixed_string(pattern_strings[is_static]), quote = '"', justify = "none")
    call_name <- xml2::xml_find_chr(patterns[is_static], "string(preceding-sibling::expr[last()]/SYMBOL_FUNCTION_CALL)")

    is_stringr <- startsWith(call_name, "str_")
    replacement <- ifelse(
      is_stringr,
      sprintf("stringr::fixed(%s)", fixed_equivalent),
      fixed_equivalent
    )
    msg <- paste(
      "This regular expression is static, i.e., its matches can be expressed as a fixed substring expression, which",
      "is faster to compute. Here, you can use",
      replacement, ifelse(is_stringr, "as the pattern.", "with fixed = TRUE.")
    )

    xml_nodes_to_lints(
      patterns[is_static],
      source_expression = source_expression,
      lint_message = msg,
      type = "warning"
    )
  })
}

rx_non_active_char <- rex::rex(none_of("^${(.*+?|[\\"))
rx_static_escape <- local({
  rx_char_escape <- rex::rex(or(
    group("\\", none_of(alnum)),
    group("\\x", between(xdigit, 1L, 2L)),
    group("\\", between("0":"7", 1L, 3L)),
    group("\\u{", between(xdigit, 1L, 4L), "}"),
    group("\\u", between(xdigit, 1L, 4L)),
    group("\\U{", between(xdigit, 1L, 8L), "}"),
    group("\\U", between(xdigit, 1L, 8L))
  ))
  rx_trivial_char_group <- rex::rex(
    "[",
    or(
      any,
      group("\\", none_of("dswDSW")), # character classes, e.g. \d are enabled in [] too if perl = TRUE
      rx_char_escape
    ),
    "]"
  )
  rex::rex(or(
    capture(rx_char_escape, name = "char_escape"),
    capture(rx_trivial_char_group, name = "trivial_char_group")
  ))
})

rx_static_token <- local({
  rex::rex(or(
    rx_non_active_char,
    rx_static_escape
  ))
})

rx_static_regex <- paste0("(?s)", rex::rex(start, zero_or_more(rx_static_token), end))
rx_first_static_token <- paste0("(?s)", rex::rex(start, zero_or_more(rx_non_active_char), rx_static_escape))

#' Determine whether a regex pattern actually uses regex patterns
#'
#' Note that is applies to the strings that are found on the XML parse tree,
#'   _not_ plain strings. This is important for backslash escaping, which
#'   happens at different layers of escaping than one might expect. So testing
#'   this function is best done through testing the expected results of a lint
#'   on a given file, rather than passing strings to this function, which can
#'   be confusing.
#'
#' @param str A character vector.
#' @return A logical vector, `TRUE` wherever `str` could be replaced by a
#'   string with `fixed = TRUE`.
#' @noRd
is_not_regex <- function(str) {
  # need to add single-line option to allow literal newlines
  grepl(rx_static_regex, str, perl = TRUE)
}

#' Compute a fixed string equivalent to a static regular expression
#'
#' @param static_regex A regex for which `is_not_regex()` returns `TRUE`
#' @return A string such that `grepl(static_regex, x)` is equivalent to
#' `grepl(get_fixed_string(static_regex), x, fixed = TRUE)`
#'
#' @noRd
get_fixed_string <- function(static_regex) {
  if (length(static_regex) == 0L) {
    return(character())
  } else if (length(static_regex) > 1L) {
    return(vapply(static_regex, get_fixed_string, character(1L)))
  }
  fixed_string <- ""
  current_match <- regexpr(rx_first_static_token, static_regex, perl = TRUE)
  while (current_match != -1L) {
    token_type <- attr(current_match, "capture.names")[attr(current_match, "capture.start") > 0L]
    token_start <- max(attr(current_match, "capture.start"))
    if (token_start > 1L) {
      fixed_string <- paste0(fixed_string, substr(static_regex, 1L, token_start - 1L))
    }
    consume_to <- attr(current_match, "match.length")
    token_content <- substr(static_regex, token_start, consume_to)
    fixed_string <- paste0(fixed_string, get_token_replacement(token_content, token_type))
    static_regex <- substr(static_regex, start = consume_to + 1L, stop = nchar(static_regex))
    current_match <- regexpr(rx_first_static_token, static_regex, perl = TRUE)
  }
  paste0(fixed_string, static_regex)
}

get_token_replacement <- function(token_content, token_type) {
  if (token_type == "trivial_char_group") {
    token_content <- substr(token_content, start = 2L, stop = nchar(token_content) - 1L)
    if (startsWith(token_content, "\\")) { # escape within trivial char group
      get_token_replacement(token_content, "char_escape")
    } else {
      token_content
    }
  } else { # char_escape token
    if (rex::re_matches(token_content, rex::rex("\\", one_of("^${}().*+?|[]\\<>:")))) {
      substr(token_content, start = 2L, stop = nchar(token_content))
    } else {
      eval(parse(text = paste0('"', token_content, '"')))
    }
  }
}
