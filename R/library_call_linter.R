#' Library call linter
#'
#' This linter covers several rules related to [library()] calls:
#'
#'  - Enforce such calls to all be at the top of the script.
#'  - Block usage of argument `character.only`, in particular
#'    for loading packages in a loop.
#'  - Block consecutive calls to `suppressMessages(library(.))`
#'    in favor of using [suppressMessages()] only once to suppress
#'    messages from all `library()` calls. Ditto [suppressPackageStartupMessages()].
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
#'   linters = library_call_linter()
#' )
#'
#' code <- "suppressMessages(library(dplyr))\nsuppressMessages(library(tidyr))"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = library_call_linter()
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
#' code <- "# comment\nlibrary(dplyr)"
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
#'   linters = library_call_linter()
#' )
#'
#' code <- "suppressMessages({\n  library(dplyr)\n  library(tidyr)\n})"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = library_call_linter()
#' )
#'
#' @evalRd rd_tags("library_call_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
library_call_linter <- function(allow_preamble = TRUE) {
  attach_calls <- c("library", "require")
  attach_call_cond <- xp_text_in_table(attach_calls)
  suppress_call_cond <- xp_text_in_table(c("suppressMessages", "suppressPackageStartupMessages"))

  unsuppressed_call_cond <- glue("not( {xp_or(attach_call_cond, suppress_call_cond)} )")
  if (allow_preamble) {
    unsuppressed_call_cond <- xp_and(
      unsuppressed_call_cond,
      glue("@line1 > //SYMBOL_FUNCTION_CALL[{ attach_call_cond }][1]/@line1")
    )
  }
  upfront_call_xpath <- glue("
    //SYMBOL_FUNCTION_CALL[{ attach_call_cond }][last()]
      /preceding::expr
      /SYMBOL_FUNCTION_CALL[{ unsuppressed_call_cond }][last()]
      /following::expr[SYMBOL_FUNCTION_CALL[{ attach_call_cond }]]
      /parent::expr
  ")

  # STR_CONST: block library|require("..."), i.e., supplying a string literal
  # ancestor::expr[FUNCTION]: Skip usages inside functions a la {knitr}
  char_only_direct_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[{attach_call_cond}]
    /parent::expr
    /parent::expr[
      expr[2][STR_CONST]
      or (
        SYMBOL_SUB[text() = 'character.only']
        and not(ancestor::expr[FUNCTION])
      )
    ]
  ")

  bad_indirect_funs <- c("do.call", "lapply", "sapply", "map", "walk")
  call_symbol_cond <- glue("SYMBOL[{attach_call_cond}] or STR_CONST")
  char_only_indirect_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[{ xp_text_in_table(bad_indirect_funs) }]
    /parent::expr
    /parent::expr[
      not(ancestor::expr[FUNCTION])
      and expr[{ call_symbol_cond }]
    ]
  ")
  call_symbol_path <- glue("./expr[{call_symbol_cond}]")

  attach_expr_cond <- glue("expr[expr/SYMBOL_FUNCTION_CALL[{attach_call_cond}]]")

  # Use `calls` in the first condition, not in the second, to prevent, e.g.,
  #   the first call matching calls[1] but the second matching calls[2].
  #   That is, ensure that calls[i] only matches a following call to calls[i].
  # match on the expr, not the SYMBOL_FUNCTION_CALL, to ensure
  #   namespace-qualified calls only match if the namespaces do.
  consecutive_suppress_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[{ suppress_call_cond }]
    /parent::expr
    /parent::expr[
      expr[SYMBOL_FUNCTION_CALL[{ suppress_call_cond }]] =
        following-sibling::expr[1][{attach_expr_cond}]/expr
      and {attach_expr_cond}
    ]
  ")

  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content

    upfront_call_expr <- xml_find_all(xml, upfront_call_xpath)

    upfront_call_name <- xp_call_name(upfront_call_expr)

    upfront_call_lints <- xml_nodes_to_lints(
      upfront_call_expr,
      source_expression = source_expression,
      lint_message = sprintf("Move all %s calls to the top of the script.", upfront_call_name),
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
    char_only_indirect_lib_calls <- vapply(
      char_only_indirect_expr,
      function(expr) {
        calls <- get_r_string(xml_find_all(expr, call_symbol_path))
        calls <- calls[calls %in% attach_calls]
        if (length(calls) == 1L) calls else NA_character_
      },
      character(1L)
    )

    # For STR_CONST entries, the XPath doesn't check the string value -- we use
    #   get_r_string() here to do that filter more robustly.
    is_attach_call <- !is.na(char_only_indirect_lib_calls)
    char_only_indirect_expr <- char_only_indirect_expr[is_attach_call]
    char_only_indirect_lib_calls <- char_only_indirect_lib_calls[is_attach_call]

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

    consecutive_suppress_expr <- xml_find_all(xml, consecutive_suppress_xpath)
    consecutive_suppress_call_text <- xp_call_name(consecutive_suppress_expr)
    consecutive_suppress_message <- glue(
      "Unify consecutive calls to {consecutive_suppress_call_text}(). ",
      "You can do so by writing all of the calls in one braced expression ",
      "like {consecutive_suppress_call_text}({{...}})."
    )
    consecutive_suppress_lints <- xml_nodes_to_lints(
      consecutive_suppress_expr,
      source_expression = source_expression,
      lint_message = consecutive_suppress_message,
      type = "warning"
    )

    c(upfront_call_lints, char_only_direct_lints, char_only_indirect_lints, consecutive_suppress_lints)
  })
}
