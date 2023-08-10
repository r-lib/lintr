#' Raise lints for several common poor usages of `paste()`
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
#' @param allow_file_path Logical, default `FALSE`. If `TRUE`, usage of
#'   `paste()` and `paste0()` to construct file paths is not linted.
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
#'   text = 'paste0(dir, "/", file)',
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
#'   linters = paste_linter(allow_file_path = TRUE)
#' )
#'
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
paste_linter <- function(allow_empty_sep = FALSE, allow_to_string = FALSE, allow_file_path = FALSE) {
  sep_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'paste']
    /parent::expr
    /following-sibling::SYMBOL_SUB[text() = 'sep' and following-sibling::expr[1][STR_CONST]]
    /parent::expr
  "

  to_string_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'paste' or text() = 'paste0']
    /parent::expr
    /parent::expr[
      count(expr) = 3
      and SYMBOL_SUB[text() = 'collapse']/following-sibling::expr[1][STR_CONST]
    ]
  "

  paste0_sep_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'paste0']
    /parent::expr
    /following-sibling::SYMBOL_SUB[text() = 'sep']
    /parent::expr
  "

  paste_strrep_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'paste' or text() = 'paste0']
    /parent::expr[
      count(following-sibling::expr) = 2
      and following-sibling::expr[1][expr[1][SYMBOL_FUNCTION_CALL[text() = 'rep']] and expr[2][STR_CONST]]
      and following-sibling::SYMBOL_SUB[text() = 'collapse']
    ]
    /parent::expr
  "

  slash_str <- sprintf("STR_CONST[%s]", xp_text_in_table(c("'/'", '"/"')))
  str_not_start_with_slash <-
    "STR_CONST[not(substring(text(), 2, 1) = '/')]"
  str_not_end_with_slash <-
    "STR_CONST[not(substring(text(), string-length(text()) - 1, 1) = '/')]"
  non_str <- "SYMBOL or expr"

  # Type I: paste(..., sep = "/")
  paste_file_path_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[text() = 'paste']
    /parent::expr
    /parent::expr[
      SYMBOL_SUB[text() = 'sep']/following-sibling::expr[1][{slash_str}]
      and not(SYMBOL_SUB[text() = 'collapse'])
    ]
  ")

  # Type II: paste0(x, "/", y, "/", z)
  #   more specifically, paste0 need not be replaced by file.path when
  #   (1) there are consecutive SYMBOL|expr (like paste0(x, y))
  #   (2) a STR_CONST precedes a SYMBOL|expr, but doesn't end with /
  #   (3) a STR_CONST follows a SYMBOL|expr, but doesn't start with /
  #   (4) there are consecutive STR_CONSTs but neither
  #       the first ends with / nor the second starts with /
  #   (5) there is only one argument
  #   (6) there is any NUM_CONST
  #   (7) collapse is used
  #   (8) / is the first character in an initial STR_CONST or the
  #       last character in a terminal STR_CONST, where file.path
  #       replacement would somewhat awkwardly look like
  #       file.path("", ...) and/or file.path(..., "")
  #   (9) a STR_CONST beginning or ending with >1 slashes (like "//x")
  # use count() to exclude paste0(x)
  # don't use starts-with() -- (1) there's no ends-with() in XPath 1.0 and
  #   (2) we need to cater for both ' and " quotations which ^ complexity
  # NB: XPath's substring() signature is (text, initial_index, n_characters)
  # an expression matching any of the not(...) conditions is _not_ a path
  paste0_file_path_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[text() = 'paste0']
    /parent::expr
    /parent::expr[
      count(expr) > 2
      and not(
        expr/NUM_CONST
        or SYMBOL_SUB[text() = 'collapse']
        or expr[2][{slash_str}]
        or expr[last()][{slash_str}]
        or expr/STR_CONST[
          substring(text(), 2, 2) = '//'
          or substring(text(), string-length(text()) - 2, 2) = '//'
        ]
        or expr[({non_str}) and following-sibling::expr[1][{non_str}]]
        or expr[
          {str_not_end_with_slash}
          and following-sibling::expr[1][
            {non_str}
            or {str_not_start_with_slash}
          ]
        ]
        or expr[
          {str_not_start_with_slash}
          and preceding-sibling::expr[1][{non_str}]
        ]
      )
    ]
  ")

  empty_paste_note <-
    'Note that paste() converts empty inputs to "", whereas file.path() leaves it empty.'

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content
    optional_lints <- list()

    if (!allow_empty_sep) {
      empty_sep_expr <- xml_find_all(xml, sep_xpath)
      sep_value <- get_r_string(empty_sep_expr, xpath = "./SYMBOL_SUB[text() = 'sep']/following-sibling::expr[1]")

      optional_lints <- c(optional_lints, xml_nodes_to_lints(
        empty_sep_expr[!nzchar(sep_value)],
        source_expression = source_expression,
        lint_message = 'paste0(...) is better than paste(..., sep = "").',
        type = "warning"
      ))
    }

    if (!allow_to_string) {
      # 3 expr: the function call, the argument, and collapse=
      to_string_expr <- xml_find_all(xml, to_string_xpath)
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

    paste0_sep_expr <- xml_find_all(xml, paste0_sep_xpath)
    paste0_sep_lints <- xml_nodes_to_lints(
      paste0_sep_expr,
      source_expression = source_expression,
      lint_message = "sep= is not a formal argument to paste0(); did you mean to use paste(), or collapse=?",
      type = "warning"
    )

    paste_strrep_expr <- xml_find_all(xml, paste_strrep_xpath)
    collapse_arg <- get_r_string(paste_strrep_expr, "SYMBOL_SUB/following-sibling::expr[1]/STR_CONST")
    paste_strrep_expr <- paste_strrep_expr[!nzchar(collapse_arg)]
    paste_call <- xp_call_name(paste_strrep_expr)
    paste_strrep_lints <- xml_nodes_to_lints(
      paste_strrep_expr,
      source_expression = source_expression,
      lint_message = sprintf('strrep(x, times) is better than %s(rep(x, times), collapse = "").', paste_call),
      type = "warning"
    )

    if (!allow_file_path) {
      paste_file_path_expr <- xml_find_all(xml, paste_file_path_xpath)
      optional_lints <- c(optional_lints, xml_nodes_to_lints(
        paste_file_path_expr,
        source_expression = source_expression,
        lint_message = paste(
          'Construct file paths with file.path(...) instead of paste(..., sep = "/").',
          'If you are using paste(sep = "/") to construct a date, consider using format() or lubridate helpers instead.',
          empty_paste_note
        ),
        type = "warning"
      ))

      paste0_file_path_expr <- xml_find_all(xml, paste0_file_path_xpath)
      optional_lints <- c(optional_lints, xml_nodes_to_lints(
        paste0_file_path_expr,
        source_expression = source_expression,
        lint_message = paste(
          'Construct file paths with file.path(...) instead of paste0(x, "/", y, "/", z).',
          empty_paste_note
        ),
        type = "warning"
      ))
    }

    c(optional_lints, paste0_sep_lints, paste_strrep_lints)
  })
}
