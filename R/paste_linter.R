#' Raise lints for several common poor usages of `paste()`
#'
#' @description
#'
#' The following issues are linted by default by this linter
#'   (see arguments for which can be de-activated optionally):
#'
#'  1. Block usage of [paste()] with `sep = ""`. [paste0()] is a faster, more concise alternative.
#'  2. Block usage of `paste()` or `paste0()` with `collapse = ", "`. [toString()] is a direct
#'     wrapper for this, and alternatives like [glue::glue_collapse()] might give better messages for humans.
#'  3. Block usage of `paste0()` that supplies `sep=` -- this is not a formal argument to `paste0`, and
#'     is likely to be a mistake.
#'  4. Block usage of `paste()` / `paste0()` combined with [rep()] that could be replaced by
#'     [strrep()]. `strrep()` can handle the task of building a block of repeated strings
#'     (e.g. often used to build "horizontal lines" for messages). This is both more readable and
#'     skips the (likely small) overhead of putting two strings into the global string cache when only one is needed.
#'
#'     Only target scalar usages -- `strrep` can handle more complicated cases (e.g. `strrep(letters, 26:1)`,
#'     but those aren't as easily translated from a `paste(collapse=)` call.
#'
#' @evalRd rd_tags("paste_linter")
#' @param allow_empty_sep Logical, default `FALSE`. If `TRUE`, usage of
#'   `paste()` with `sep = ""` is not linted.
#' @param allow_to_string Logical, default `FALSE`. If `TRUE`, usage of
#'   `paste()` and `paste0()` with `collapse = ", "` is not linted.
#' @param allow_file_path String, one of `"never"`, `"double_slash"`, or `"always"`; `"double_slash"` by default.
#'   If `"never"`, usage of `paste()` and `paste0()` to construct file paths is not linted. If `"double_slash"`,
#'   strings containing consecutive forward slashes will not lint. The main use case here is for URLs -- "paths" like
#'   `"https://"` will not induce lints, since constructing them with `file.path()` might be deemed unnatural.
#'   Lastly, if `"always"`, strings with consecutive forward slashes will also lint. Note that `"//"` is never linted
#'   when it comes at the beginning or end of the input, to avoid requiring empty inputs like
#'  `file.path("", ...)` or `file.path(..., "")`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'paste("a", "b", sep = "")',
#'   linters = paste_linter()
#' )
#'
#' lint(
#'   text = 'paste(c("a", "b"), collapse = ", ")',
#'   linters = paste_linter()
#' )
#'
#' lint(
#'   text = 'paste0(c("a", "b"), sep = " ")',
#'   linters = paste_linter()
#' )
#'
#' lint(
#'   text = 'paste0(rep("*", 10L), collapse = "")',
#'   linters = paste_linter()
#' )
#'
#' lint(
#'   text = 'paste0("http://site.com/", path)',
#'   linters = paste_linter(allow_file_path = "never")
#' )
#'
#' lint(
#'   text = 'paste0(x, collapse = "")',
#'   linters = paste_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'paste0("a", "b")',
#'   linters = paste_linter()
#' )
#'
#' lint(
#'   text = 'paste("a", "b", sep = "")',
#'   linters = paste_linter(allow_empty_sep = TRUE)
#' )
#'
#' lint(
#'   text = 'toString(c("a", "b"))',
#'   linters = paste_linter()
#' )
#'
#' lint(
#'   text = 'paste(c("a", "b"), collapse = ", ")',
#'   linters = paste_linter(allow_to_string = TRUE)
#' )
#'
#' lint(
#'   text = 'paste(c("a", "b"))',
#'   linters = paste_linter()
#' )
#'
#' lint(
#'   text = 'strrep("*", 10L)',
#'   linters = paste_linter()
#' )
#'
#' lint(
#'   text = 'paste0(year, "/", month, "/", day)',
#'   linters = paste_linter(allow_file_path = "always")
#' )
#'
#' lint(
#'   text = 'paste0("http://site.com/", path)',
#'   linters = paste_linter()
#' )
#'
#' lint(
#'   text = 'paste(x, collapse = "")',
#'   linters = paste_linter()
#' )
#'
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
paste_linter <- function(allow_empty_sep = FALSE,
                         allow_to_string = FALSE,
                         allow_file_path = c("double_slash", "always", "never")) {
  allow_file_path <- match.arg(allow_file_path)
  check_file_paths <- allow_file_path %in% c("double_slash", "never")

  paste_sep_xpath <- "
  parent::expr
    /following-sibling::SYMBOL_SUB[text() = 'sep' and following-sibling::expr[1][STR_CONST]]
    /parent::expr
  "

  to_string_xpath <- "
  parent::expr
    /parent::expr[
      count(expr) = 3
      and SYMBOL_SUB[text() = 'collapse']/following-sibling::expr[1][STR_CONST]
    ]
  "

  paste0_sep_xpath <- "
  parent::expr
    /following-sibling::SYMBOL_SUB[text() = 'sep']
    /parent::expr
  "

  paste_strrep_xpath <- "
  parent::expr[
    count(following-sibling::expr) = 2
    and following-sibling::expr[1][expr[1][SYMBOL_FUNCTION_CALL[text() = 'rep']] and expr[2][STR_CONST]]
    and following-sibling::SYMBOL_SUB[text() = 'collapse']
  ]/parent::expr
  "

  # Type II: paste0(x, "/", y, "/", z)
  #   NB: some conditions require evaluating the R string, only a few can be done in pure XPath. See below.
  paste0_file_path_xpath <- xp_strip_comments("
  parent::expr
    /parent::expr[
      (: exclude paste0(x) :)
      count(expr) > 2
      (: An expression matching _any_ of these conditions is _not_ a file path :)
      and not(
        (: Any numeric input :)
        expr/NUM_CONST
        (: A call using collapse= :)
        or SYMBOL_SUB[text() = 'collapse']
        (: Consecutive non-strings like paste0(x, y) :)
        or expr[(SYMBOL or expr) and following-sibling::expr[1][SYMBOL or expr]]
      )
    ]
  ")

  empty_paste_note <-
    'Note that paste() converts empty inputs to "", whereas file.path() leaves it empty.'

  paste0_collapse_xpath <- glue::glue("
  parent::expr
    /parent::expr[
      SYMBOL_SUB[text() = 'collapse']
      and count(expr) =
        3 - count(preceding-sibling::*[self::PIPE or self::SPECIAL[{ xp_text_in_table(magrittr_pipes) }]])
      and not(expr/SYMBOL[text() = '...'])
    ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    paste_calls <- source_expression$xml_find_function_calls("paste")
    paste0_calls <- source_expression$xml_find_function_calls("paste0")
    both_calls <- combine_nodesets(paste_calls, paste0_calls)

    optional_lints <- list()

    # Both of these look for paste(..., sep = "..."), differing in which 'sep' is linted,
    #   so run the expensive XPath search/R parse only once
    if (!allow_empty_sep || check_file_paths) {
      paste_sep_expr <- xml_find_all(paste_calls, paste_sep_xpath)
      paste_sep_value <- get_r_string(paste_sep_expr, xpath = "./SYMBOL_SUB[text() = 'sep']/following-sibling::expr[1]")
    }

    if (!allow_empty_sep) {
      optional_lints <- c(optional_lints, xml_nodes_to_lints(
        paste_sep_expr[!nzchar(paste_sep_value)],
        source_expression = source_expression,
        lint_message = 'paste0(...) is better than paste(..., sep = "").',
        type = "warning"
      ))
    }

    if (!allow_to_string) {
      # 3 expr: the function call, the argument, and collapse=
      to_string_expr <- xml_find_all(both_calls, to_string_xpath)
      collapse_value <- get_r_string(
        to_string_expr,
        xpath = "./SYMBOL_SUB[text() = 'collapse']/following-sibling::expr[1]"
      )

      optional_lints <- c(optional_lints, xml_nodes_to_lints(
        to_string_expr[collapse_value == ", "],
        source_expression = source_expression,
        lint_message = paste(
          'toString(.) is more expressive than paste(., collapse = ", ").',
          "Note also glue::glue_collapse() and and::and()",
          "for constructing human-readable / translation-friendly lists"
        ),
        type = "warning"
      ))
    }

    paste0_sep_expr <- xml_find_all(paste0_calls, paste0_sep_xpath)
    paste0_sep_lints <- xml_nodes_to_lints(
      paste0_sep_expr,
      source_expression = source_expression,
      lint_message = "sep= is not a formal argument to paste0(); did you mean to use paste(), or collapse=?",
      type = "warning"
    )

    paste_strrep_expr <- xml_find_all(both_calls, paste_strrep_xpath)
    collapse_arg <- get_r_string(paste_strrep_expr, "SYMBOL_SUB/following-sibling::expr[1]/STR_CONST")
    paste_strrep_expr <- paste_strrep_expr[!nzchar(collapse_arg)]
    paste_call <- xp_call_name(paste_strrep_expr)
    paste_strrep_lints <- xml_nodes_to_lints(
      paste_strrep_expr,
      source_expression = source_expression,
      lint_message = sprintf('strrep(x, times) is better than %s(rep(x, times), collapse = "").', paste_call),
      type = "warning"
    )

    paste0_collapse_expr <- xml_find_all(paste0_calls, paste0_collapse_xpath)
    paste0_collapse_lints <- xml_nodes_to_lints(
      paste0_collapse_expr,
      source_expression = source_expression,
      lint_message = "Use paste(), not paste0(), to collapse a character vector when sep= is not used.",
      type = "warning"
    )

    if (check_file_paths) {
      paste_sep_slash_expr <- paste_sep_expr[paste_sep_value == "/"]
      optional_lints <- c(optional_lints, xml_nodes_to_lints(
        # in addition to paste(..., sep = "/") we ensure collapse= is not present
        paste_sep_slash_expr[is.na(xml_find_first(paste_sep_slash_expr, "./SYMBOL_SUB[text() = 'collapse']"))],
        source_expression = source_expression,
        lint_message = paste(
          'Construct file paths with file.path(...) instead of paste(..., sep = "/").',
          'If you are using paste(sep = "/") to construct a date,',
          "consider using format() or lubridate helpers instead.",
          empty_paste_note
        ),
        type = "warning"
      ))

      paste0_file_path_expr <- xml_find_all(paste0_calls, paste0_file_path_xpath)
      is_file_path <-
        !vapply(paste0_file_path_expr, check_is_not_file_path, logical(1L), allow_file_path = allow_file_path)
      optional_lints <- c(optional_lints, xml_nodes_to_lints(
        paste0_file_path_expr[is_file_path],
        source_expression = source_expression,
        lint_message = paste(
          'Construct file paths with file.path(...) instead of paste0(x, "/", y, "/", z).',
          empty_paste_note
        ),
        type = "warning"
      ))
    }

    c(optional_lints, paste0_sep_lints, paste_strrep_lints, paste0_collapse_lints)
  })
}

check_is_not_file_path <- function(expr, allow_file_path) {
  arguments <- xml_find_all(expr, "expr[position() > 1]")

  is_string <- !is.na(xml_find_first(arguments, "STR_CONST"))
  string_values <- character(length(arguments))
  string_values[is_string] <- get_r_string(arguments[is_string])
  not_start_slash <- which(!startsWith(string_values, "/"))
  not_end_slash <- which(!endsWith(string_values, "/"))

  if (allow_file_path == "double_slash") {
    check_double_slash <- function(str) any(grepl("//", str, fixed = TRUE))
  } else {
    # always skip on strings starting/ending with //, since switching to
    #   file.path() would require file.path("", ...) or file.path(..., "")
    check_double_slash <- function(str) any(grepl("^//|//$", str))
  }

  # First input is '/', meaning file.path() would need to start with ''
  string_values[1L] == "/" ||
    # Last input is '/', meaning file.path() would need to end with ''
    string_values[length(string_values)] == "/" ||
    check_double_slash(string_values) ||
    # A string not ending with /, followed by non-string,
    #   or a string not starting with /, preceded by a non-string
    !all(is_string[c(not_end_slash + 1L, not_start_slash - 1L)], na.rm = TRUE) ||
    # A string not starting with / preceded by a string not ending with /
    any(not_start_slash %in% (not_end_slash + 1L))
}
