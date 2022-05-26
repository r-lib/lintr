#' Object usage linter
#'
#' Check that closures have the proper usage using [codetools::checkUsage()].
#' Note that this runs [base::eval()] on the code, so do not use with untrusted code.
#'
#' @param interpret_glue If TRUE, interpret [glue::glue()] calls to avoid false positives caused by local variables
#' which are only used in a glue expression.
#'
#' @evalRd rd_tags("object_usage_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
object_usage_linter <- function(interpret_glue = TRUE) {
  # NB: difference across R versions in how EQ_ASSIGN is represented in the AST
  #   (under <expr_or_assign_or_help> or <equal_assign>)
  # NB: the repeated expr[2][FUNCTION] XPath has no performance impact, so the different direct assignment XPaths are
  #   split for better readability, see PR#1197
  # TODO(#1106): use //[...] to capture assignments in more scopes
  xpath_function_assignment <- paste(
    # direct assignments
    "expr[LEFT_ASSIGN or EQ_ASSIGN]/expr[2][FUNCTION]",
    "expr_or_assign_or_help[EQ_ASSIGN]/expr[2][FUNCTION]",
    "equal_assign[EQ_ASSIGN]/expr[2][FUNCTION]",
    # assign() and setMethod() assignments
    "//expr[expr[SYMBOL_FUNCTION_CALL[text() = 'assign' or text() = 'setMethod']]]/expr[3]",
    sep = " | "
  )

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
        start_line = as.integer(xml2::xml_attr(fun_assignment, "line1"))
      )

      # TODO handle assignment functions properly
      # e.g. `not_existing<-`(a, b)
      res$name <- rex::re_substitutes(res$name, rex::rex("<-"), "")

      lintable_symbols <- xml2::xml_find_all(
        fun_assignment,
        "descendant::SYMBOL | descendant::SYMBOL_FUNCTION_CALL"
      )

      lintable_symbol_names <- get_r_string(lintable_symbols)
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
      nodes[is.na(matched_symbol)] <- list(fun_assignment)

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

  if (length(glue_calls) == 0L) return(character())
  glued_symbols <- new.env(parent = emptyenv())
  for (cl in glue_calls) {
    parsed_cl <- tryCatch(
      parse(text = xml2::xml_text(cl)),
      error = function(...) NULL,
      warning = function(...) NULL
    )[[1L]]
    if (is.null(parsed_cl)) next
    parsed_cl[[".transformer"]] <- function(text, envir) {
      parsed_text <- tryCatch(
        parse(text = text, keep.source = TRUE),
        error = function(...) NULL,
        warning = function(...) NULL
      )
      parsed_xml <- safe_parse_to_xml(parsed_text)
      # covers NULL & NA cases
      if (length(parsed_xml) == 0L) {
        return("")
      }
      symbols <- xml2::xml_text(xml2::xml_find_all(parsed_xml, "//SYMBOL"))
      for (sym in symbols) {
        assign(sym, NULL, envir = glued_symbols)
      }
      ""
    }
    eval(parsed_cl)
  }
  ls(envir = glued_symbols, all.names = TRUE)
}

get_assignment_symbols <- function(xml) {
  get_r_string(xml2::xml_find_all(
    xml,
    "
      expr[LEFT_ASSIGN]/expr[1]/SYMBOL[1] |
      equal_assign/expr[1]/SYMBOL[1] |
      expr[expr[SYMBOL_FUNCTION_CALL/text()='assign']]/expr[2]/* |
      expr[expr[SYMBOL_FUNCTION_CALL/text()='setMethod']]/expr[2]/*
    "
  ))
}

parse_check_usage <- function(expression, known_used_symbols = character(), declared_globals = character(),
                              start_line = 1L) {

  vals <- list()

  report <- function(x) {
    vals[[length(vals) + 1L]] <<- x
  }

  try(codetools::checkUsage(
    expression,
    report = report,
    suppressLocalUnused = known_used_symbols,
    suppressUndefined = declared_globals
  ))

  function_name <- rex(anything, ": ")
  line_info <- rex(" ", "(", capture(name = "path", non_spaces), ":",
                   capture(name = "line1", digits), maybe("-", capture(name = "line2", digits)), ")")

  res <- re_matches(
    vals,
    rex(
      function_name,
      capture(
        name = "message",
        anything,
        one_of(quote, "\u2018"),
        capture(name = "name", anything),
        one_of(quote, "\u2019"),
        anything
      ),
      line_info
    )
  )

  missing <- is.na(res$message)
  if (any(missing)) {
    res[missing, ] <- re_matches(
      vals[missing],
      rex(
        function_name,
        capture(
          name = "message",
          "possible error in ", capture(name = "name", anything), ": ", anything
        ),
        line_info
      )
    )
  }

  res <- res[!is.na(res$message), ]

  res$line1 <- as.integer(res$line1) + start_line - 1L
  res$line2 <- ifelse(
    nzchar(res$line2),
    as.integer(res$line2) + start_line - 1L,
    res$line1
  )

  res
}

get_imported_symbols <- function(xml) {
  import_exprs <- xml2::xml_find_all(
    xml,
    "//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']]
      and
      (
        not(SYMBOL_SUB[
          text() = 'character.only' and
          following-sibling::expr[1][NUM_CONST[text() = 'TRUE'] or SYMBOL[text() = 'T']]
        ]) or
        expr[2][STR_CONST]
      )
    ]/expr[STR_CONST|SYMBOL][1]"
  )
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
