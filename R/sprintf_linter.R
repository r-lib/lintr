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
#' lint(
#'   text = 'sprintf(paste0(x, y))',
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
#' lint(
#'   text = 'paste0(x, y)',
#'   linters = sprintf_linter()
#' )
#'
#' @evalRd rd_tags("sprintf_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
sprintf_linter <- function() {
  call_xpath <- "
  parent::expr[
    not(expr/SYMBOL[text() = '...'])
  ]"

  pipes <- setdiff(magrittr_pipes, "%$%")
  in_pipe_xpath <- glue(
    "self::expr[
      preceding-sibling::*[not(self::COMMENT)][1][self::PIPE or self::SPECIAL[{ xp_text_in_table(pipes) }]]
    ]"
  )

  is_non_atomic <- function(x) !(is.symbol(x) && !nzchar(x)) && !is.atomic(x)

  find_fmt_loc <- function(parsed_expr) {
    arg_names <- names(parsed_expr)
    if ("fmt" %in% arg_names) {
      return(which(names(parsed_expr) == "fmt")[1L])
    }

    1L + match("", arg_names[-1L], nomatch = 1L)
  }

  zap_extra_args <- function(parsed_expr) {
    fmt_loc <- find_fmt_loc(parsed_expr)

    if (length(parsed_expr) < 3L) {
      return(parsed_expr)
    }
    arg_names <- names2(parsed_expr)
    domain_loc <- arg_names == "domain"
    parsed_expr[domain_loc] <- list(NULL)
    for (i in setdiff(2L:length(parsed_expr), c(fmt_loc, which(domain_loc)))) {
      if (is_non_atomic(parsed_expr[[i]])) {
        parsed_expr[[i]] <- 0L
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
    if (length(xml_find_first_(xml, in_pipe_xpath)) > 0L) {
      arg_names <- names(parsed_expr)
      arg_idx <- 2L:length(parsed_expr)
      parsed_expr[arg_idx + 1L] <- parsed_expr[arg_idx]
      names(parsed_expr)[arg_idx + 1L] <- arg_names[arg_idx]
      parsed_expr[[2L]] <- xml2lang(xml_find_first_(xml, "preceding-sibling::*[not(self::COMMENT)][2]"))
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
    sprintf_calls <- xml_find_all_(xml_calls, call_xpath)
    in_pipeline <- !is.na(xml_find_first_(sprintf_calls, in_pipe_xpath))

    fmt_by_name <- get_r_string(
      sprintf_calls,
      "SYMBOL_SUB[text() = 'fmt']/following-sibling::expr[1]/STR_CONST"
    )
    fmt_by_pos_xpath <- "
      expr[
        preceding-sibling::OP-LEFT-PAREN
        and not(preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB])
      ][1]
        /STR_CONST
    "
    fmt_by_pos <- ifelse(
      in_pipeline,
      get_r_string(sprintf_calls, "preceding-sibling::*[not(self::COMMENT)][2]/STR_CONST"),
      get_r_string(sprintf_calls, fmt_by_pos_xpath)
    )

    fmt <- ifelse(!is.na(fmt_by_name), fmt_by_name, fmt_by_pos)
    constant_fmt <- !is.na(fmt) & !grepl("%", gsub("%%", "", fmt, fixed = TRUE), fixed = TRUE)

    fct_name <- xp_call_name(sprintf_calls)

    num_args_xpath <- paste0(
      "./expr[preceding-sibling::OP-LEFT-PAREN and ",
      "not(preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB]/",
      "preceding-sibling::*[not(self::COMMENT)][1][self::SYMBOL_SUB[text() = 'domain']])]"
    )
    num_args_in_parens <- vapply(
      sprintf_calls,
      function(call) {
        length(xml_find_all_(call, num_args_xpath))
      },
      integer(1L)
    )
    num_args <- num_args_in_parens + as.integer(in_pipeline)

    single_arg <- (is.na(fmt) | constant_fmt) & num_args == 1L
    single_arg_lint <- xml_nodes_to_lints(
      sprintf_calls[single_arg],
      source_expression = source_expression,
      lint_message = sprintf("%s call can be removed when a single argument is provided.", fct_name[single_arg]),
      type = "warning"
    )

    constant_fmt_multi <- constant_fmt & num_args > 1L
    constant_fmt_lint <- xml_nodes_to_lints(
      sprintf_calls[constant_fmt_multi],
      source_expression = source_expression,
      lint_message = sprintf(
        "%s call can be removed when a constant string is provided.",
        fct_name[constant_fmt_multi]
      ),
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

    c(single_arg_lint, constant_fmt_lint, invalid_sprintf_lint)
  })
}
