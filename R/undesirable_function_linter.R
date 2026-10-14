#' Undesirable function linter
#'
#' Report the use of undesirable functions and suggest an alternative.
#'
#' @param fun Character vector of undesirable function names. Input can be any of three types:
#'   - Unnamed entries must be a character string specifying an undesirable function.
#'   - For named entries, the name specifies the undesirable function.
#'     + If the entry is a character string, it is used as a description of
#'       why a given function is undesirable
#'     + Otherwise, entries should be missing (`NA`)
#'   A generic message that the named function is undesirable is used if no
#'     specific description is provided.
#'   Input can also be a list of character strings for convenience.
#'   Setter functions (like `foo(x) <- y`) are distinguished from plain calls (`foo(x)`),
#'   and must be specified with `"<-"` (e.g. `"foo<-"`).
#'   Function names may be namespace-qualified (e.g. `"pkg::foo"` or `"pkg:::foo"`)
#'   to lint only calls qualified with that namespace.
#'
#'   Defaults to [default_undesirable_functions]. To make small customizations to this list,
#'   use [modify_defaults()].
#' @param symbol_is_undesirable Whether to consider the use of an undesirable function
#'   name as a symbol undesirable or not.
#'
#' @examples
#' # defaults for which functions are considered undesirable
#' names(default_undesirable_functions)
#'
#' # will produce lints
#' lint(
#'   text = "sapply(x, mean)",
#'   linters = undesirable_function_linter()
#' )
#'
#' lint(
#'   text = "log10(x)",
#'   linters = undesirable_function_linter(fun = c("log10" = NA))
#' )
#'
#' lint(
#'   text = "log10(x)",
#'   linters = undesirable_function_linter(fun = c("log10" = "use log()"))
#' )
#'
#' lint(
#'   text = 'dir <- "path/to/a/directory"',
#'   linters = undesirable_function_linter(fun = c("dir" = NA))
#' )
#'
#' lint(
#'   text = 'dir <- "path/to/a/directory"',
#'   linters = undesirable_function_linter(fun = "dir")
#' )
#'
#' # okay
#' lint(
#'   text = "vapply(x, mean, FUN.VALUE = numeric(1))",
#'   linters = undesirable_function_linter()
#' )
#'
#' lint(
#'   text = "log(x, base = 10)",
#'   linters = undesirable_function_linter(fun = c("log10" = "use log()"))
#' )
#'
#' lint(
#'   text = 'dir <- "path/to/a/directory"',
#'   linters = undesirable_function_linter(fun = c("dir" = NA), symbol_is_undesirable = FALSE)
#' )
#'
#' lint(
#'   text = 'dir <- "path/to/a/directory"',
#'   linters = undesirable_function_linter(fun = "dir", symbol_is_undesirable = FALSE)
#' )
#'
#' @evalRd rd_tags("undesirable_function_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
undesirable_function_linter <- function(fun = default_undesirable_functions,
                                        symbol_is_undesirable = TRUE) {
  if (is.list(fun)) fun <- unlist(fun)
  if (!is.logical(symbol_is_undesirable)) {
    cli_abort("{.arg symbol_is_undesirable} must be a logical, not {.obj_type_friendly {symbol_is_undesirable}}.")
  }
  # allow (uncoerced->implicitly logical) 'NA'
  if (length(fun) == 0L || !(is.character(fun) || all(is.na(fun)))) {
    cli_abort("{.arg fun} must be a non-empty character vector.")
  }

  implicit_idx <- !nzchar(names2(fun))
  if (any(implicit_idx)) {
    names(fun)[implicit_idx] <- fun[implicit_idx]
    is.na(fun) <- implicit_idx
  }
  fun_names <- names(fun)
  if (anyNA(fun_names)) {
    missing_idx <- which(is.na(fun_names)) # nolint: object_usage_linter. False positive.
    cli_abort(paste(
      "Unnamed elements of {.arg fun} must not be missing,",
      "but {.val {missing_idx}} {qty(length(missing_idx))} {?is/are}."
    ))
  }
  has_ns_prefix <- grepl("^[[:alnum:].]+:::?", fun_names)
  ns_prefix <- ifelse(has_ns_prefix, sub("^([[:alnum:].]+:::?).*$", "\\1", fun_names), "")
  base_fun_names <- gsub("^`|`$", "", sub("^[[:alnum:].]+:::?", "", fun_names))
  fun_names <- paste0(ns_prefix, base_fun_names)
  names(fun) <- fun_names
  lookup_names <- paste0(sub(":::$", "::", ns_prefix), base_fun_names)
  unique_base_funs <- unique(base_fun_names)
  has_ns <- any(has_ns_prefix)

  xp_condition <- xp_and(
    paste0(
      "not(parent::expr/preceding-sibling::expr[last()][SYMBOL_FUNCTION_CALL[",
      xp_text_in_table(c("library", "require")),
      "]])"
    ),
    "not(parent::expr[OP-DOLLAR or OP-AT])"
  )

  if (symbol_is_undesirable) {
    needs_backticks <- make.names(unique_base_funs) != unique_base_funs
    symbol_names <- unique_base_funs
    symbol_names[needs_backticks] <- sprintf("`%s`", symbol_names[needs_backticks])
    symbol_xpath <- glue("//SYMBOL[({xp_text_in_table(symbol_names)}) and {xp_condition}]")
  }
  call_xpath <- glue("SYMBOL_FUNCTION_CALL[{xp_condition}]")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    xml_calls <- source_expression$xml_find_function_calls(unique_base_funs, keep_names = TRUE)

    matched_call_nodes <- xml_find_first_(xml_calls, call_xpath)
    matched_call_nodes <- matched_call_nodes[!is.na(matched_call_nodes)]
    matched_nodes <- unname(matched_call_nodes)
    matched_fun <- names(matched_call_nodes)

    if (symbol_is_undesirable) {
      symbol_nodes <- xml_find_all_(xml, symbol_xpath)
      matched_nodes <- combine_nodesets(matched_nodes, symbol_nodes)
      matched_fun <- c(matched_fun, get_r_string(symbol_nodes))
    }

    if (has_ns) {
      call_pkg <- xml_find_chr_(matched_nodes, "string(preceding-sibling::SYMBOL_PACKAGE)")
      qualified_fun <- paste0(call_pkg, "::", matched_fun, recycle0 = TRUE)
      idx <- match(qualified_fun, lookup_names)
      unqualified_idx <- is.na(idx)
      idx[unqualified_idx] <- match(matched_fun[unqualified_idx], lookup_names)
      keep <- !is.na(idx)
      matched_nodes <- matched_nodes[keep]
      matched_fun <- fun_names[idx[keep]]
    }

    msgs <- vapply(
      stats::setNames(nm = unique(matched_fun)),
      function(fun_name) {
        msg <- sprintf('Avoid undesirable function "%s".', fun_name)
        alternative <- fun[[fun_name]]
        if (!is.na(alternative)) {
          msg <- paste(msg, sprintf("As an alternative, %s.", alternative))
        }
        msg
      },
      character(1L)
    )

    xml_nodes_to_lints(
      matched_nodes,
      source_expression = source_expression,
      lint_message = unname(msgs[matched_fun])
    )
  })
}
