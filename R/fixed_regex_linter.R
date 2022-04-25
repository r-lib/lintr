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
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

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
    xpath <- glue::glue("//expr[
      SYMBOL_FUNCTION_CALL[ {pos_1_regex_funs} ]
      and not(following-sibling::SYMBOL_SUB[
        (text() = 'fixed' or text() = 'ignore.case')
        and following-sibling::expr[1][NUM_CONST[text() = 'TRUE'] or SYMBOL[text() = 'T']]
      ])
    ]
    /following-sibling::expr[1][STR_CONST and not(EQ_SUB)]
    |
    //expr[
      SYMBOL_FUNCTION_CALL[ {pos_2_regex_funs} ]
      and not(following-sibling::SYMBOL_SUB[
        text() = 'fixed'
        and following-sibling::expr[1][NUM_CONST[text() = 'TRUE'] or SYMBOL[text() = 'T']]
      ])
    ]
    /following-sibling::expr[2][STR_CONST and not(EQ_SUB)]
    ")

    patterns <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      patterns[is_not_regex(xml2::xml_text(patterns))],
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = paste(
        "For static regular expression patterns, set `fixed = TRUE`.",
        "Note that this includes regular expressions that can be expressed as",
        "fixed patterns, e.g. [.] is really just . and \\$ is really just $",
        "if there are no other regular expression specials. For functions from",
        "the 'stringr' package, the way to declare a static string is to",
        "wrap the pattern in stringr::fixed().",
        "If this is being used in a dbplyr context (i.e., translated to sql),",
        "replace the regular expression with the `LIKE` operator using the",
        "`%LIKE%` infix function.",
        "Lastly, take care to remember that the `replacement` argument of",
        "`gsub()` is affected by the `fixed` argument as well."
      ),
      type = "warning"
    ))
  })
}

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
  # Handle string quoting and escaping using R directly
  str <- as.character(parse(text = str, keep.source = FALSE))

  rx_non_active_char <- rex::rex(none_of("^${(.*+?|[\\"))
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
      group("\\", any),
      rx_char_escape
    ),
    "]"
  )
  rx_static_token <- rex::rex(or(
    rx_non_active_char,
    rx_char_escape,
    rx_trivial_char_group
  ))
  rx_static_regex <- rex::rex(start, zero_or_more(rx_static_token), end)

  # need to add multi-line option to allow literal newlines
  grepl(paste0("(?m)", rx_static_regex), str, perl = TRUE)
}
