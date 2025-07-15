#' Object usage linter
#'
#' Check that closures have the proper usage using [codetools::checkUsage()].
#' Note that this runs [base::eval()] on the code, so **do not use with untrusted code**.
#'
#' @param interpret_glue (Deprecated) If `TRUE`, interpret [glue::glue()] calls to avoid
#'   false positives caused by local variables which are only used in a glue expression.
#'   Provide `interpret_extensions` instead, see below.
#' @param interpret_extensions Character vector of extensions to interpret. These are meant to cover known cases where
#'   variables may be used in ways understood by the reader but not by `checkUsage()` to avoid false positives.
#'   Currently `"glue"` and `"rlang"` are supported, both of which are in the default.
#'   - For `glue`, examine [glue::glue()] calls.
#'   - For `rlang`, examine `.env$key` usages.
#' @param skip_with A logical. If `TRUE` (default), code in `with()` expressions
#'   will be skipped. This argument will be passed to `skipWith` argument of
#'   `codetools::checkUsage()`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "foo <- function() { x <- 1 }",
#'   linters = object_usage_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "foo <- function(x) { x <- 1 }",
#'   linters = object_usage_linter()
#' )
#'
#' lint(
#'   text = "foo <- function() { x <- 1; return(x) }",
#'   linters = object_usage_linter()
#' )
#' @evalRd rd_linters("package_development")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
object_usage_linter <- function(interpret_glue = NULL, interpret_extensions = c("glue", "rlang"), skip_with = TRUE) {
  if (!is.null(interpret_glue)) {
    lintr_deprecated(
      "interpret_glue",
      '"glue" in interpret_extensions',
      version = "3.3.0",
      type = "Argument",
      signal = "warning"
    )

    if (interpret_glue) {
      interpret_extensions <- union(interpret_extensions, "glue")
    } else {
      interpret_extensions <- setdiff(interpret_extensions, "glue")
    }
  }

  if (length(interpret_extensions) > 0L) {
    interpret_extensions <- match.arg(interpret_extensions, several.ok = TRUE)
  }

  # NB: difference across R versions in how EQ_ASSIGN is represented in the AST
  #   (under <expr_or_assign_or_help> or <equal_assign>)
  # NB: the repeated expr[2][FUNCTION] XPath has no performance impact, so the different direct assignment XPaths are
  #   split for better readability, see PR#1197
  # TODO(#1106): use //[...] to capture assignments in more scopes
  xpath_function_assignment <- "
    expr[LEFT_ASSIGN or EQ_ASSIGN]/expr[2][FUNCTION or OP-LAMBDA]
    | expr_or_assign_or_help[EQ_ASSIGN]/expr[2][FUNCTION or OP-LAMBDA]
    | equal_assign[EQ_ASSIGN]/expr[2][FUNCTION or OP-LAMBDA]
    | //SYMBOL_FUNCTION_CALL[text() = 'assign']/parent::expr/following-sibling::expr[2][FUNCTION or OP-LAMBDA]
    | //SYMBOL_FUNCTION_CALL[text() = 'setMethod']/parent::expr/following-sibling::expr[3][FUNCTION or OP-LAMBDA]
  "

  # not all instances of linted symbols are potential sources for the observed violations -- see #1914
  symbol_exclude_cond <- "preceding-sibling::OP-DOLLAR or preceding-sibling::OP-AT or ancestor::expr[OP-TILDE]"
  xpath_culprit_symbol <- glue("
    descendant::SYMBOL[not( {symbol_exclude_cond} )]
    | descendant::SYMBOL_FUNCTION_CALL[not( {symbol_exclude_cond} )]
    | descendant::SPECIAL
    | descendant::LEFT_ASSIGN[text() = ':=']
  ")

  Linter(linter_level = "file", function(source_expression) {
    pkg_name <- pkg_name(find_package(dirname(source_expression$filename)))

    declared_globals <- try_silently(globalVariables(package = pkg_name %||% globalenv()))

    xml <- source_expression$full_xml_parsed_content

    # Catch missing packages and report them as lints
    outer_env <- environment()
    outer_env$library_lints <- list()
    library_lint_hook <- function(lint_node, lint_msg) {
      outer_env$library_lints[[length(outer_env$library_lints) + 1L]] <- xml_nodes_to_lints(
        lint_node, source_expression = source_expression, lint_message = lint_msg, type = "warning"
      )
    }

    # run the following at run-time, not "compile" time to allow package structure to change
    env <- make_check_env(pkg_name, xml, library_lint_hook)

    fun_assignments <- xml_find_all(xml, xpath_function_assignment)

    lapply(fun_assignments, function(fun_assignment) {
      code <- get_content(lines = source_expression$content, fun_assignment)
      fun <- try_silently(eval(
        envir = env,
        parse(
          text = code,
          keep.source = TRUE
        )
      ))

      if (inherits(fun, "try-error")) {
        return()
      }
      known_used_symbols <- known_used_symbols(fun_assignment, interpret_extensions = interpret_extensions)
      res <- parse_check_usage(
        fun,
        known_used_symbols = known_used_symbols,
        declared_globals = declared_globals,
        start_line = as.integer(xml_attr(fun_assignment, "line1")),
        end_line = as.integer(xml_attr(fun_assignment, "line2")),
        skip_with = skip_with
      )

      res$name <- re_substitutes(res$name, rex("<-"), "")

      lintable_symbols <- xml_find_all(fun_assignment, xpath_culprit_symbol)

      lintable_symbol_names <- gsub("^`|`$", "", xml_text(lintable_symbols))
      lintable_symbol_lines <- as.integer(xml_attr(lintable_symbols, "line1"))

      matched_symbol <- vapply(
        seq_len(nrow(res)),
        function(i) {
          match(
            TRUE,
            lintable_symbol_names == res$name[i] &
              lintable_symbol_lines >= res$line1[i] &
              lintable_symbol_lines <= res$line2[i]
          )
        },
        integer(1L)
      )
      nodes <- unclass(lintable_symbols)[matched_symbol]

      # fallback to line based matching if no symbol is found
      missing_symbol <- is.na(matched_symbol)
      nodes[missing_symbol] <- lapply(which(missing_symbol), function(i) {
        line_based_match <- xml_find_first(
          fun_assignment,
          glue::glue_data(res[i, ], "descendant::expr[@line1 = {line1} and @line2 = {line2}]")
        )
        if (is.na(line_based_match)) fun_assignment else line_based_match
      })

      c(
        outer_env$library_lints,
        xml_nodes_to_lints(nodes, source_expression = source_expression, lint_message = res$message, type = "warning")
      )
    })
  })
}

make_check_env <- function(pkg_name, xml, library_lint_hook) {
  if (!is.null(pkg_name)) {
    parent_env <- try_silently(getNamespace(pkg_name))
  }
  if (is.null(pkg_name) || inherits(parent_env, "try-error")) {
    parent_env <- globalenv()
  }
  env <- new.env(parent = parent_env)

  symbols <- c(
    get_assignment_symbols(xml),
    get_imported_symbols(xml, library_lint_hook)
  )

  # Just assign them an empty function
  for (symbol in symbols) {
    assign(symbol, function(...) invisible(), envir = env)
  }
  env
}

get_assignment_symbols <- function(xml) {
  get_r_string(xml_find_all(
    xml,
    "
      expr[LEFT_ASSIGN or EQ_ASSIGN]/expr[1]/SYMBOL[1] |
      expr[RIGHT_ASSIGN]/expr[2]/SYMBOL[1] |
      equal_assign/expr[1]/SYMBOL[1] |
      expr_or_assign_or_help/expr[1]/SYMBOL[1] |
      expr[expr[1][SYMBOL_FUNCTION_CALL/text()='assign']]/expr[2]/* |
      expr[expr[1][SYMBOL_FUNCTION_CALL/text()='setMethod']]/expr[2]/*
    "
  ))
}

get_check_usage_results <- function(expression, known_used_symbols, declared_globals, skip_with) {
  report_env <- new.env(parent = emptyenv())
  report_env$vals <- character()
  report <- function(x) report_env$vals <- c(report_env$vals, x)
  old <- options(useFancyQuotes = FALSE)
  on.exit(options(old))
  try(
    codetools::checkUsage(
      expression,
      report = report,
      suppressLocalUnused = known_used_symbols,
      suppressUndefined = declared_globals,
      skipWith = skip_with
    )
  )
  report_env$vals
}

parse_check_usage <- function(expression,
                              known_used_symbols = character(),
                              declared_globals = character(),
                              start_line = 1L,
                              end_line = 1L,
                              skip_with = TRUE) {
  vals <- get_check_usage_results(expression, known_used_symbols, declared_globals, skip_with)

  function_name <- rex(anything, ": ")
  line_info <- rex(
    " ", "(", capture(name = "path", non_spaces), ":",
    capture(name = "line1", digits), maybe("-", capture(name = "line2", digits)), ")"
  )

  res <- re_matches(
    vals,
    rex(
      function_name,
      capture(
        name = "message",
        zero_or_more(any, type = "lazy"),
        maybe(
          "'",
          capture(name = "name", anything),
          "'",
          zero_or_more(any, type = "lazy")
        )
      ),
      or(line_info, end)
    )
  )

  # nocov start
  is_missing <- is.na(res$message)
  if (any(is_missing)) {
    # TODO(#2474): Remove this.
    missing_msg <- vals[is_missing][[1L]] # nolint: object_usage_linter. TODO(#2252).
    cli_warn(c(
      x = "Couldn't parse usage message {.str {missing_msg}}. Ignoring {.val {sum(is_missing)}} usage warnings.",
      i = "Please report a possible bug at {.url https://github.com/r-lib/lintr/issues}."
    ))
  }
  # nocov end
  res <- res[!is_missing, ]

  res$line1 <- ifelse(
    nzchar(res$line1),
    as.integer(res$line1) + start_line - 1L,
    NA_integer_
  )
  res$line2 <- ifelse(
    nzchar(res$line2),
    as.integer(res$line2) + start_line - 1L,
    res$line1
  )

  res$line1[is.na(res$line1)] <- start_line
  res$line2[is.na(res$line2)] <- end_line

  res
}

get_imported_symbols <- function(xml, library_lint_hook) {
  import_exprs_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']
    /parent::expr
    /parent::expr[
      not(SYMBOL_SUB[
        text() = 'character.only' and
        following-sibling::expr[1][NUM_CONST[text() = 'TRUE'] or SYMBOL[text() = 'T']]
      ])
      or expr[2][STR_CONST]
    ]
    /expr[STR_CONST or SYMBOL][1]
  "
  import_exprs <- xml_find_all(xml, import_exprs_xpath)
  imported_pkgs <- get_r_string(import_exprs)

  unlist(Map(pkg = imported_pkgs, expr = import_exprs, function(pkg, expr) {
    tryCatch(
      getNamespaceExports(pkg),
      error = function(e) {
        lint_node <- xml2::xml_parent(expr)
        lib_paths <- .libPaths() # nolint: undesirable_function_name. .libPaths() is necessary here.
        lib_noun <- if (length(lib_paths) == 1L) "library" else "libraries"
        lint_msg <- paste0(
          "Could not find exported symbols for package \"", pkg, "\" in ", lib_noun, " ",
          toString(shQuote(lib_paths)), " (", conditionMessage(e), "). This may lead to false positives."
        )
        library_lint_hook(lint_node, lint_msg)
        character()
      }
    )
  }))
}

known_used_symbols <- function(fun_assignment, interpret_extensions) {
  unique(c(
    if ("rlang" %in% interpret_extensions) extract_env_symbols(fun_assignment),
    if ("glue" %in% interpret_extensions) extract_glued_symbols(fun_assignment)
  ))
}

extract_env_symbols <- function(fun_assignment) {
  env_names <- xml_find_all(
    fun_assignment,
    "
    .//SYMBOL[text() = '.env']
      /parent::expr
      /following-sibling::OP-DOLLAR
      /following-sibling::SYMBOL
    | .//SYMBOL[text() = '.env']
      /parent::expr
      /following-sibling::LBB
      /following-sibling::expr[STR_CONST]
    "
  )
  get_r_string(env_names)
}
