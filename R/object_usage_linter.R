#' Object usage linter
#'
#' Check that closures have the proper usage using [codetools::checkUsage()].
#' Note that this runs [base::eval()] on the code, so **do not use with untrusted code**.
#'
#' @param interpret_glue If `TRUE`, interpret [glue::glue()] calls to avoid false positives caused by local variables
#' which are only used in a glue expression.
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
object_usage_linter <- function(interpret_glue = TRUE, skip_with = TRUE) {
  # NB: difference across R versions in how EQ_ASSIGN is represented in the AST
  #   (under <expr_or_assign_or_help> or <equal_assign>)
  # NB: the repeated expr[2][FUNCTION] XPath has no performance impact, so the different direct assignment XPaths are
  #   split for better readability, see PR#1197
  # TODO(#1106): use //[...] to capture assignments in more scopes
  xpath_function_assignment <- paste(
    # direct assignments
    "expr[LEFT_ASSIGN or EQ_ASSIGN]/expr[2][FUNCTION or OP-LAMBDA]",
    "expr_or_assign_or_help[EQ_ASSIGN]/expr[2][FUNCTION or OP-LAMBDA]",
    "equal_assign[EQ_ASSIGN]/expr[2][FUNCTION or OP-LAMBDA]",
    # assign() and setMethod() assignments
    "//SYMBOL_FUNCTION_CALL[text() = 'assign']/parent::expr/following-sibling::expr[2][FUNCTION or OP-LAMBDA]",
    "//SYMBOL_FUNCTION_CALL[text() = 'setMethod']/parent::expr/following-sibling::expr[3][FUNCTION or OP-LAMBDA]",
    sep = " | "
  )

  # not all instances of linted symbols are potential sources for the observed violations -- see #1914
  symbol_exclude_cond <- "preceding-sibling::OP-DOLLAR or preceding-sibling::OP-AT or ancestor::expr[OP-TILDE]"
  xpath_culprit_symbol <- glue::glue("
    descendant::SYMBOL[not( {symbol_exclude_cond} )]
    | descendant::SYMBOL_FUNCTION_CALL[not( {symbol_exclude_cond} )]
    | descendant::SPECIAL
    | descendant::LEFT_ASSIGN[text() = ':=']
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    pkg_name <- pkg_name(find_package(dirname(source_expression$filename)))
    # run the following at run-time, not "compile" time to allow package structure to change
    env <- make_check_env(pkg_name)

    declared_globals <- try_silently(utils::globalVariables(package = pkg_name %||% globalenv()))

    xml <- source_expression$full_xml_parsed_content

    symbols <- c(
      get_assignment_symbols(xml),
      get_imported_symbols(xml)
    )

    # Just assign them an empty function
    for (symbol in symbols) {
      assign(symbol, function(...) invisible(), envir = env)
    }

    fun_assignments <- xml2::xml_find_all(xml, xpath_function_assignment)

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
      known_used_symbols <- get_used_symbols(fun_assignment, interpret_glue = interpret_glue)
      res <- parse_check_usage(
        fun,
        known_used_symbols = known_used_symbols,
        declared_globals = declared_globals,
        start_line = as.integer(xml2::xml_attr(fun_assignment, "line1")),
        end_line = as.integer(xml2::xml_attr(fun_assignment, "line2")),
        skip_with = skip_with
      )

      # TODO handle assignment functions properly
      # e.g. `not_existing<-`(a, b)
      res$name <- rex::re_substitutes(res$name, rex::rex("<-"), "")

      lintable_symbols <- xml2::xml_find_all(fun_assignment, xpath_culprit_symbol)

      lintable_symbol_names <- gsub("^`|`$", "", xml2::xml_text(lintable_symbols))
      lintable_symbol_lines <- as.integer(xml2::xml_attr(lintable_symbols, "line1"))

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
        line_based_match <- xml2::xml_find_first(
          fun_assignment,
          glue::glue_data(res[i, ], "descendant::expr[@line1 = {line1} and @line2 = {line2}]")
        )
        if (is.na(line_based_match)) fun_assignment else line_based_match
      })

      xml_nodes_to_lints(nodes, source_expression = source_expression, lint_message = res$message, type = "warning")
    })
  })
}

make_check_env <- function(pkg_name) {
  if (!is.null(pkg_name)) {
    parent_env <- try_silently(getNamespace(pkg_name))
  }
  if (is.null(pkg_name) || inherits(parent_env, "try-error")) {
    parent_env <- globalenv()
  }
  env <- new.env(parent = parent_env)
  return(env)
}


extract_glued_symbols <- function(expr) {
  # TODO support more glue functions
  # Package glue:
  #  - glue_sql
  #  - glue_safe
  #  - glue_col
  #  - glue_data
  #  - glue_data_sql
  #  - glue_data_safe
  #  - glue_data_col
  #
  # Package stringr:
  #  - str_interp
  glue_calls <- xml2::xml_find_all(
    expr,
    xpath = paste0(
      "descendant::SYMBOL_FUNCTION_CALL[text() = 'glue']/", # a glue() call
      "preceding-sibling::NS_GET/preceding-sibling::SYMBOL_PACKAGE[text() = 'glue']/", # qualified with glue::
      "parent::expr[",
      # without .envir or .transform arguments
      "not(following-sibling::SYMBOL_SUB[text() = '.envir' or text() = '.transform']) and",
      # argument that is not a string constant
      "not(following-sibling::expr[not(STR_CONST)])",
      "]/",
      # get the complete call
      "parent::expr"
    )
  )

  if (length(glue_calls) == 0L) {
    return(character())
  }

  unexpected_error <- function(cond) {
    stop("Unexpected failure to parse glue call, please report: ", conditionMessage(cond)) # nocov
  }
  glued_symbols <- new.env(parent = emptyenv())
  for (glue_call in glue_calls) {
    # TODO(michaelchirico): consider dropping tryCatch() here if we're more confident in our logic
    parsed_call <- tryCatch(xml2lang(glue_call), error = unexpected_error, warning = unexpected_error)
    parsed_call[[".envir"]] <- glued_symbols
    parsed_call[[".transformer"]] <- symbol_extractor
    # #1459: syntax errors in glue'd code are ignored with warning, rather than crashing lint
    tryCatch(
      eval(parsed_call),
      error = function(cond) {
        warning(
          "Evaluating glue expression while testing for local variable usage failed: ",
          conditionMessage(cond), "\nPlease ensure correct glue syntax, e.g., matched delimiters.",
          call. = FALSE
        )
        NULL
      }
    )
  }
  names(glued_symbols)
}

symbol_extractor <- function(text, envir, data) {
  parsed_text <- tryCatch(
    parse(text = text, keep.source = TRUE),
    error = function(...) NULL,
    warning = function(...) NULL
  )
  if (length(parsed_text) == 0L) {
    return("")
  }
  parse_data <- utils::getParseData(parsed_text)

  # strip backticked symbols; `x` is the same as x.
  symbols <- gsub("^`(.*)`$", "\\1", parse_data$text[parse_data$token == "SYMBOL"])
  for (sym in symbols) {
    assign(sym, NULL, envir = envir)
  }
  ""
}

get_assignment_symbols <- function(xml) {
  get_r_string(xml2::xml_find_all(
    xml,
    "
      expr[LEFT_ASSIGN]/expr[1]/SYMBOL[1] |
      equal_assign/expr[1]/SYMBOL[1] |
      expr[expr[1][SYMBOL_FUNCTION_CALL/text()='assign']]/expr[2]/* |
      expr[expr[1][SYMBOL_FUNCTION_CALL/text()='setMethod']]/expr[2]/*
    "
  ))
}

parse_check_usage <- function(expression,
                              known_used_symbols = character(),
                              declared_globals = character(),
                              start_line = 1L,
                              end_line = 1L,
                              skip_with = TRUE) {
  vals <- list()

  report <- function(x) {
    vals[[length(vals) + 1L]] <<- x
  }

  withr::with_options(
    list(useFancyQuotes = FALSE),
    code = {
      try(codetools::checkUsage(
        expression,
        report = report,
        suppressLocalUnused = known_used_symbols,
        suppressUndefined = declared_globals,
        skipWith = skip_with
      ))
    }
  )

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
  missing <- is.na(res$message)
  if (any(missing)) {
    # TODO (AshesITR): Remove this in the future, if no bugs arise from this safeguard
    warning(
      "Possible bug in lintr: Couldn't parse usage message ", sQuote(vals[missing][[1L]]), ". ",
      "Ignoring ", sum(missing), " usage warnings. Please report an issue at https://github.com/r-lib/lintr/issues."
    )
  }
  # nocov end
  res <- res[!missing, ]

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

get_imported_symbols <- function(xml) {
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
  import_exprs <- xml2::xml_find_all(xml, import_exprs_xpath)
  if (length(import_exprs) == 0L) {
    return(character())
  }
  imported_pkgs <- get_r_string(import_exprs)

  unlist(lapply(imported_pkgs, function(pkg) {
    tryCatch(
      getNamespaceExports(pkg),
      error = function(e) {
        character()
      }
    )
  }))
}

get_used_symbols <- function(expr, interpret_glue) {
  if (!isTRUE(interpret_glue)) {
    return(character())
  }
  extract_glued_symbols(expr)
}
