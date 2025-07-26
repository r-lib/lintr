#' Require correct `sprintf()` calls
#'
#' Check for an inconsistent number of arguments or arguments with incompatible types (for literal arguments) in
#' [sprintf()] calls.
#'
#' [gettextf()] calls are also included, since `gettextf()` is a thin wrapper around `sprintf()`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'sprintf("hello %s %s %d", x, y)',
#'   linters = sprintf_linter()
#' )
#'
#' lint(
#'   text = 'sprintf("hello")',
#'   linters = sprintf_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'sprintf("hello %s %s %d", x, y, z)',
#'   linters = sprintf_linter()
#' )
#'
#' lint(
#'   text = 'sprintf("hello %s %s %d", x, y, ...)',
#'   linters = sprintf_linter()
#' )
#'
#' @evalRd rd_tags("sprintf_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
sprintf_linter <- function() {
  call_xpath <- glue::glue("
  parent::expr[
    not(expr/SYMBOL[text() = '...'])
  ]")

  pipes <- setdiff(magrittr_pipes, "%$%")
  in_pipe_xpath <- glue("self::expr[
    preceding-sibling::*[1][self::PIPE or self::SPECIAL[{ xp_text_in_table(pipes) }]]
  ]")

  is_missing <- function(x) is.symbol(x) && !nzchar(x)

  # Zap sprintf() call to contain only constants
  #
  # Set all extra arguments to 0L if they aren't a constant
  #
  # @param parsed_expr A parsed `sprintf()` call
  #
  # @return A `sprintf()` call with all non-constants replaced by `0L`
  # (which is compatible with all sprintf format specifiers)
  zap_extra_args <- function(parsed_expr) {
    if ("fmt" %in% names(parsed_expr)) {
      fmt_loc <- which(names(parsed_expr) == "fmt")
    } else {
      fmt_loc <- 2L
    }

    if (length(parsed_expr) >= 3L) {
      for (i in setdiff(seq_along(parsed_expr), c(1L, fmt_loc))) {
        if (!is_missing(parsed_expr[[i]]) && !is.atomic(parsed_expr[[i]])) {
          parsed_expr[[i]] <- 0L
        }
      }
    }
    parsed_expr
  }

  # Anticipate warnings of a sprintf() call
  #
  # Try running a static sprintf() call to determine whether it will produce warnings or errors due to format
  # misspecification
  #
  # @param xml An XML node representing a `sprintf()` call (i.e. the `<expr>` node containing the call)
  #
  # @return A string, either `NA_character_` or the text of generated errors and warnings from the `sprintf()` call when
  # replacing all dynamic components by 0, which is compatible with all format specifiers.
  capture_sprintf_warning <- function(xml) {
    parsed_expr <- xml2lang(xml)
    # convert x %>% sprintf(...) to sprintf(x, ...)
    if (length(xml_find_first(xml, in_pipe_xpath)) > 0L) {
      arg_names <- names(parsed_expr)
      arg_idx <- 2L:length(parsed_expr)
      parsed_expr[arg_idx + 1L] <- parsed_expr[arg_idx]
      names(parsed_expr)[arg_idx + 1L] <- arg_names[arg_idx]
      parsed_expr[[2L]] <- xml2lang(xml_find_first(xml, "preceding-sibling::*[2]"))
      names(parsed_expr)[2L] <- ""
    }
    parsed_expr <- zap_extra_args(parsed_expr)
    res <- tryCatch(eval(parsed_expr, envir = baseenv()), warning = identity, error = identity)
    if (inherits(res, "condition")) {
      conditionMessage(res)
    } else {
      NA_character_
    }
  }

  Linter(linter_level = "file", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(c("sprintf", "gettextf"))
    sprintf_calls <- xml_find_all(xml_calls, call_xpath)
    in_pipeline <- !is.na(xml_find_first(sprintf_calls, in_pipe_xpath))

    fmt_by_name <- get_r_string(
      sprintf_calls,
      "SYMBOL_SUB[text() = 'fmt']/following-sibling::expr[1]/STR_CONST"
    )
    fmt_by_pos <- ifelse(
      in_pipeline,
      get_r_string(sprintf_calls, "preceding-sibling::*[2]/STR_CONST"),
      get_r_string(sprintf_calls, "OP-LEFT-PAREN/following-sibling::expr[1]/STR_CONST")
    )

    fmt <- ifelse(!is.na(fmt_by_name), fmt_by_name, fmt_by_pos)
    constant_fmt <- !is.na(fmt) & !grepl("%[^%]", fmt)

    constant_fmt_lint <- xml_nodes_to_lints(
      sprintf_calls[constant_fmt],
      source_expression = source_expression,
      lint_message = "A constant string is used and the sprintf call can be removed",
      type = "warning"
    )

    templated_sprintf_calls <- sprintf_calls[!constant_fmt & !is.na(fmt)]
    sprintf_warning <- vapply(templated_sprintf_calls, capture_sprintf_warning, character(1L))

    has_warning <- !is.na(sprintf_warning)
    invalid_sprintf_lint <- xml_nodes_to_lints(
      templated_sprintf_calls[has_warning],
      source_expression = source_expression,
      lint_message = sprintf_warning[has_warning],
      type = "warning"
    )

    c(constant_fmt_lint, invalid_sprintf_lint)
  })
}
