rx_non_active_char <- rex(none_of("^${(.*+?|[\\"))
rx_static_escape <- local({
  rx_char_escape <- rex(or(
    group("\\", none_of(alnum)),
    group("\\x", between(xdigit, 1L, 2L)),
    group("\\", between("0":"7", 1L, 3L)),
    group("\\u{", between(xdigit, 1L, 4L), "}"),
    group("\\u", between(xdigit, 1L, 4L)),
    group("\\U{", between(xdigit, 1L, 8L), "}"),
    group("\\U", between(xdigit, 1L, 8L))
  ))
  rx_trivial_char_group <- rex(
    "[",
    or(
      any,
      group("\\", none_of("dswDSW")), # character classes, e.g. \d are enabled in [] too if perl = TRUE
      rx_char_escape
    ),
    "]"
  )
  rex(or(
    rx_char_escape,
    rx_trivial_char_group
  ))
})

rx_static_token <- local({
  rex(or(
    rx_non_active_char,
    rx_static_escape
  ))
})

rx_unescaped_regex <- paste0("(?s)", rex(start, zero_or_more(rx_non_active_char), end))
rx_static_regex <- paste0("(?s)", rex(start, zero_or_more(rx_static_token), end))
rx_escapable_tokens <- "^${}().*+?|[]\\<>=:;/_-!@#%&,~"
rx_escapable_regex <- rex("\\", capture(one_of(rx_escapable_tokens)))
rx_trivial_char_group <- paste0(R"{(?s)(?<!\\)(?:\\\\)*\K\[}", rex(
  capture(or(
    group("\\", or(
      group("x", between(xdigit, 1L, 2L)),
      between("0":"7", 1L, 3L),
      group("u{", between(xdigit, 1L, 4L), "}"),
      group("u", between(xdigit, 1L, 4L)),
      group("U{", between(xdigit, 1L, 8L), "}"),
      group("U", between(xdigit, 1L, 8L)),
      none_of("dswDSW")
    )),
    any
  ))
), "\\]")

rx_esc_num <- paste0(
  R"{(?s)(?<!\\)(?:\\\\)*\K\\}",
  rex(or(
    group("x", between(xdigit, 1L, 2L)),
    group("u{", between(xdigit, 1L, 4L), "}"),
    group("u", between(xdigit, 1L, 4L)),
    group("U{", between(xdigit, 1L, 8L), "}"),
    group("U", between(xdigit, 1L, 8L)),
    between("0":"7", 1L, 3L)
  ))
)

decode_escapes <- function(s) {
  m <- gregexpr(rx_esc_num, s, perl = TRUE)
  matches <- regmatches(s, m)
  all_vec <- unlist(matches, use.names = FALSE)
  if (length(all_vec) == 0L) {
    return(s)
  }
  is_hex <- grepl("^\\\\[xuU]", all_vec)
  codes <- integer(length(all_vec))
  if (any(is_hex)) {
    codes[is_hex] <- strtoi(gsub("[^0-9a-fA-F]", "", all_vec[is_hex]), base = 16L)
  }
  if (!all(is_hex)) {
    codes[!is_hex] <- strtoi(substr(all_vec[!is_hex], 2L, 4L), base = 8L)
  }
  codes[is.na(codes)] <- 0L
  all_chars <- intToUtf8(codes, multiple = TRUE)
  regmatches(s, m) <- relist(all_chars, matches)
  s
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
is_not_regex <- function(str, allow_unescaped = FALSE) {
  # need to add single-line option to allow literal newlines
  if (allow_unescaped) {
    !grepl(rx_unescaped_regex, str, perl = TRUE)
  } else {
    grepl(rx_static_regex, str, perl = TRUE)
  }
}

#' Compute a fixed string equivalent to a static regular expression
#'
#' @param static_regex A character vector of regex patterns for which `is_not_regex()` returns `TRUE`.
#' @return A character vector of quoted strings such that `grepl(static_regex, x)` is equivalent to
#'   `eval(parse(text = sprintf("grepl(%s, x, fixed = TRUE)", get_fixed_string(static_regex))))`.
#'
#' @noRd
get_fixed_string <- function(static_regex) {
  if (length(static_regex) == 0L) {
    return(character())
  }
  static_regex <- static_regex |>
    gsub(pattern = rx_trivial_char_group, replacement = "\\1", perl = TRUE) |>
    decode_escapes() |>
    gsub(pattern = rx_escapable_regex, replacement = "\\1", perl = TRUE) |>
    encodeString(quote = '"', justify = "none")

  static_regex
}

# some metadata about infix operators on the R parse tree.
#   xml_tag gives the XML tag as returned by xmlparsedata::xml_parse_data().
#   r_string gives the operator as you would write it in R code.

# styler: off
infix_metadata <- data.frame(matrix(byrow = TRUE, ncol = 2L, c(
  "OP-PLUS",         "+",
  "OP-MINUS",        "-",
  "OP-TILDE",        "~",
  "GT",              ">",
  "GE",              ">=",
  "LT",              "<",
  "LE",              "<=",
  "EQ",              "==",
  "NE",              "!=",
  "AND",              "&",
  "OR",              "|",
  "AND2",            "&&",
  "OR2",             "||",
  "LEFT_ASSIGN",     "<-",
  "LEFT_ASSIGN",     ":=",
  "LEFT_ASSIGN",     "<<-",
  "RIGHT_ASSIGN",    "->",
  "RIGHT_ASSIGN",    "->>",
  "EQ_ASSIGN",       "=",
  "EQ_SUB",          "=",   # in calls: foo(x = 1)
  "EQ_FORMALS",      "=",   # in definitions: function(x = 1)
  "PIPE",            "|>",
  "SPECIAL",         "%%",
  "OP-SLASH",        "/",
  "OP-STAR",         "*",
  "OP-COMMA",        ",",
  "OP-CARET",        "^",
  "OP-CARET",        "**",
  "OP-AT",           "@",
  "OP-EXCLAMATION",  "!",
  "OP-COLON",        ":",
  "NS_GET",          "::",
  "NS_GET_INT",      ":::",
  "OP-LEFT-BRACE",   "{",
  "OP-LEFT-BRACKET", "[",
  "LBB",             "[[",
  "OP-LEFT-PAREN",   "(",
  "OP-QUESTION",     "?",
  "OP-DOLLAR",       "$",
  NULL
)))
# styler: on

names(infix_metadata) <- c("xml_tag", "string_value")
# utils::getParseData()'s designation for the tokens wouldn't be valid as XML tags
infix_metadata$parse_tag <- ifelse(
  startsWith(infix_metadata$xml_tag, "OP-"),
  sQuote(infix_metadata$string_value, "'"),
  infix_metadata$xml_tag
)
# treated separately because spacing rules are different for unary operators
infix_metadata$unary <- infix_metadata$xml_tag %in% c("OP-PLUS", "OP-MINUS", "OP-TILDE", "OP-EXCLAMATION")
# high-precedence operators are ignored by this linter; see
#   https://style.tidyverse.org/syntax.html#infix-operators
infix_metadata$low_precedence <- infix_metadata$string_value %in% c(
  "+", "-", "~", ">", ">=", "<", "<=", "==", "!=", "&", "&&", "|", "||",
  "<-", ":=", "<<-", "->", "->>", "=", "%%", "/", "*", "|>"
)
# comparators come up in several lints
infix_metadata$comparator <- infix_metadata$string_value %in% c("<", "<=", ">", ">=", "==", "!=")

# these XML nodes require checking the text() to disambiguate multiple operators using the same tag
infix_metadata$ambiguous_tag <- infix_metadata$xml_tag %in% infix_metadata$xml_tag[duplicated(infix_metadata$xml_tag)]
infix_metadata$xml_tag_exact <- infix_metadata$xml_tag
infix_metadata$xml_tag_exact[infix_metadata$ambiguous_tag] <- sprintf(
  "%s[text() = '%s']",
  infix_metadata$xml_tag_exact[infix_metadata$ambiguous_tag],
  infix_metadata$string_value[infix_metadata$ambiguous_tag]
)

# functions equivalent to base::ifelse() for linting purposes
ifelse_funs <- c("ifelse", "if_else", "fifelse")

object_name_xpath <- local({
  # search ancestor:: axis for assignments of symbols for
  #   cases like a$b$c. We only try to lint 'a' since 'b'
  #   and 'c' might be beyond the user's control to name.
  #   the tree structure for 'a$b$c <- 1' has 'a'
  #   at the 'bottom' of the <expr> list; it is distinguished
  #   from 'b' and 'c' by not having '$' as a sibling.
  # search parent:: axis for assignments of strings because
  #   the complicated nested assignment available for symbols
  #   is not possible for strings, though we do still have to
  #   be aware of cases like 'a$"b" <- 1'.
  xp_assignment_target_fmt <- "
    not(parent::expr[OP-DOLLAR or OP-AT])
    and %1$s::expr[
      following-sibling::LEFT_ASSIGN%2$s
      or preceding-sibling::RIGHT_ASSIGN
      or following-sibling::EQ_ASSIGN
    ]
    and not(%1$s::expr[
     preceding-sibling::OP-LEFT-BRACKET
     or preceding-sibling::LBB
    ])
  "

  # strings on LHS of := are only checked if they look like data.table usage DT[, "a" := ...]
  dt_walrus_cond <- "[
    text() != ':='
    or parent::expr/preceding-sibling::OP-LEFT-BRACKET
  ]"

  # either an argument supplied positionally, i.e., not like 'arg = val', or the call <expr>
  not_kwarg_cond <- "not(preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB])"

  glue(xp_strip_comments("
  //SYMBOL[ {sprintf(xp_assignment_target_fmt, 'ancestor', '')} ]
  |  //STR_CONST[
      ({sprintf(xp_assignment_target_fmt, 'parent', dt_walrus_cond)})
      or parent::expr
        /preceding-sibling::expr[1]
        /SYMBOL_FUNCTION_CALL[text() = 'setGeneric']
      (: x= argument is the first positional argument, if not given as x= :)
      or parent::expr[
        (
          ({not_kwarg_cond})
          and count(preceding-sibling::expr[{not_kwarg_cond}]) = 1
        )
        or preceding-sibling::SYMBOL_SUB[1][text() = 'x']
      ]
        /preceding-sibling::expr[last()]
        /SYMBOL_FUNCTION_CALL[text() = 'assign']
     ]
  |  //SYMBOL_FORMALS
  "))
})

# Remove quotes or other things from names
strip_names <- function(x) {
  x <- re_substitutes(x, rex(start, some_of(quote, "`", "%")), "")
  x <- re_substitutes(x, rex(some_of(quote, "`", "<", "-", "%"), end), "")
  x
}

#' Pull out symbols used in glue strings under the current sub-tree
#'
#' Required by any linter (e.g. [object_usage_linter()] / [unused_import_linter()])
#'   that lints based on whether certain symbols are present, to ensure any
#'   symbols only used inside glue strings are also visible to the linter.
#'
#' @param expr An XML AST
#' @param interpret_glue Logical, if `FALSE` return nothing.
#' @return A character vector of symbols (variables, infix operators, and
#'   function calls) found in glue calls under `expr`.
#' @noRd
extract_glued_symbols <- function(expr, interpret_glue = TRUE) {
  if (!isTRUE(interpret_glue)) {
    return(character())
  }
  # TODO(#2448): support more glue functions
  # NB: position() > 1 because position=1 is <expr><SYMBOL_FUNCTION_CALL>
  glue_call_xpath <- "
    descendant::SYMBOL_FUNCTION_CALL[text() = 'glue']
      /parent::expr
      /parent::expr[
        not(SYMBOL_SUB[text() = '.envir' or text() = '.transform'])
        and not(expr[position() > 1 and not(STR_CONST)])
      ]
  "
  glue_calls <- xml_find_all_(expr, glue_call_xpath)

  glued_symbols <- new.env(parent = emptyenv())
  for (glue_call in glue_calls) {
    parsed_call <- xml2lang(glue_call)
    parsed_call[[".envir"]] <- glued_symbols
    parsed_call[[".transformer"]] <- glue_symbol_extractor
    # #1459: syntax errors in glue'd code are ignored with warning, rather than crashing lint
    tryCatch(eval(parsed_call), error = glue_parse_failure_warning)
  }
  names(glued_symbols)
}

glue_parse_failure_warning <- function(cond) {
  cli_warn(c(
    x = "Evaluating glue expression while testing for local variable usage failed: {conditionMessage(cond)}",
    i = "Please ensure correct glue syntax, e.g., matched delimiters."
  ))
  NULL
}
glue_symbol_extractor <- function(text, envir, data) {
  symbols <- tryCatch(
    all.vars(parse(text = text), functions = TRUE),
    error = \(...) NULL,
    warning = \(...) NULL
  )
  for (sym in symbols) {
    assign(sym, NULL, envir = envir)
  }
  ""
}

magrittr_pipes <- c("%>%", "%!>%", "%T>%", "%$%", "%<>%")

purrr_mappers <- c(
  "map", "walk",
  "map_raw", "map_lgl", "map_int", "map_dbl", "map_chr", "map_vec",
  "map_df", "map_dfr", "map_dfc"
)

# see ?".onLoad", ?Startup, and ?quit.
#   All of .onLoad, .onAttach, and .onUnload are used in base packages,
#   and should be caught in is_base_function; they're included here for completeness / stability
#   (they don't strictly _have_ to be defined in base, so could in principle be removed).
#   .Last.sys and .First.sys are part of base itself, so aren't included here.
special_funs <- c(
  ".onLoad",
  ".onAttach",
  ".onUnload",
  ".onDetach",
  ".Last.lib",
  ".First",
  ".Last"
)

is_special_function <- function(x) {
  x %in% special_funs
}
