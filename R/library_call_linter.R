#' Library call linter
#'
#' This linter covers several rules related to [library()] calls:
#'
#'  - Enforce such calls to all be at the top of the script.
#'  - Block usage of argument `character.only`.
#'
#' @param allow_preamble Logical, default `TRUE`. If `FALSE`,
#'   no code is allowed to precede the first `library()` call,
#'   otherwise some setup code is allowed, but all `library()`
#'   calls must follow consecutively after the first one.
#' @examples
#' # will produce lints
#'
#' code <- "library(dplyr)\nprint('test')\nlibrary(tidyr)"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = library_call_linter()
#' )
#'
#' lint(
#'   text = "library('dplyr', character.only = TRUE)",
#'   linters = library_call_linter()
#' )
#'
#' code <- paste(
#'   "pkg <- c('dplyr', 'tibble')",
#'   "sapply(pkg, library, character.only = TRUE)",
#'   sep = "\n"
#' )
#' writeLines(code)
#' lint(
#'   text = code,
#'   library_call_linter()
#' )
#'
#' # okay
#' code <- "library(dplyr)\nprint('test')"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = library_call_linter()
#' )
#'
#' code <- '# comment\nlibrary(dplyr)'
#' lint(
#'   text = code,
#'   linters = library_call_linter()
#' )
#'
#' code <- paste(
#'   "foo <- function(pkg) {",
#'   "  sapply(pkg, library, character.only = TRUE)",
#'   "}",
#'   sep = "\n"
#' )
#' writeLines(code)
#' lint(
#'   text = code,
#'   library_call_linter()
#' )
#' 
#' @evalRd rd_tags("library_call_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
library_call_linter <- function(allow_preamble = TRUE) {
  attach_call <- "text() = 'library' or text() = 'require'"
  unsuppressed_call <- glue("not( {attach_call} or starts-with(text(), 'suppress'))")
  if (allow_preamble) {
    unsuppressed_call <- xp_and(
      unsuppressed_call,
      glue("@line1 > //SYMBOL_FUNCTION_CALL[{ attach_call }][1]/@line1")
    )
  }
  upfront_call_xpath <- glue("
    //SYMBOL_FUNCTION_CALL[{ attach_call }][last()]
      /preceding::expr
      /SYMBOL_FUNCTION_CALL[{ unsuppressed_call }][last()]
      /following::expr[SYMBOL_FUNCTION_CALL[{ attach_call }]]
      /parent::expr
  ")

  # STR_CONST: block library|require("..."), i.e., supplying a string literal
  # ancestor::expr[FUNCTION]: Skip usages inside functions a la {knitr}
  char_only_direct_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']
    /parent::expr
    /parent::expr[
      expr[2][STR_CONST]
      or (
        SYMBOL_SUB[text() = 'character.only']
        and not(ancestor::expr[FUNCTION])
      )
    ]
  "

  bad_indirect_funs <- c("do.call", "lapply", "sapply", "map", "walk")
  call_symbol_cond <- "
  SYMBOL[text() = 'library' or text() = 'require']
    or STR_CONST[text() = '\"library\"' or text() = '\"require\"']
  "
  char_only_indirect_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[{ xp_text_in_table(bad_indirect_funs) }]
    /parent::expr
    /parent::expr[
      not(ancestor::expr[FUNCTION])
      and expr[{ call_symbol_cond }]
    ]
  ")
  call_symbol_path <- glue("./expr[{call_symbol_cond}]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    upfront_call_expr <- xml_find_all(xml, upfront_call_xpath)

    call_name <- xp_call_name(upfront_call_expr)

    upfront_call_lints <- xml_nodes_to_lints(
      upfront_call_expr,
      source_expression = source_expression,
      lint_message = sprintf("Move all %s calls to the top of the script.", call_name),
      type = "warning"
    )

    char_only_direct_expr <- xml_find_all(xml, char_only_direct_xpath)
    char_only_direct_calls <- xp_call_name(char_only_direct_expr)
    character_only <-
      xml_find_first(char_only_direct_expr, "./SYMBOL_SUB[text() = 'character.only']")
    char_only_direct_msg_fmt <- ifelse(
      is.na(character_only),
      "Use symbols, not strings, in %s calls.",
      "Use symbols in %s calls to avoid the need for 'character.only'."
    )
    char_only_direct_msg <-
      sprintf(as.character(char_only_direct_msg_fmt), char_only_direct_calls)
    char_only_direct_lints <- xml_nodes_to_lints(
      char_only_direct_expr,
      source_expression = source_expression,
      lint_message = char_only_direct_msg,
      type = "warning"
    )

    char_only_indirect_expr <- xml_find_all(xml, char_only_indirect_xpath)
    char_only_indirect_lib_calls <- get_r_string(char_only_indirect_expr, call_symbol_path)
    char_only_indirect_loop_calls <- xp_call_name(char_only_indirect_expr)
    char_only_indirect_msg <- sprintf(
      "Call %s() directly, not vectorized with %s().",
      char_only_indirect_lib_calls, char_only_indirect_loop_calls
    )
    char_only_indirect_lints <- xml_nodes_to_lints(
      char_only_indirect_expr,
      source_expression = source_expression,
      lint_message = char_only_indirect_msg,
      type = "warning"
    )

    c(upfront_call_lints, char_only_direct_lints, char_only_indirect_lints)
  })
}
