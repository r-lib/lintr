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
      fun_start_line <- as.integer(xml2::xml_attr(fun_assignment, "line1"))
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
      res <- parse_check_usage(fun, known_used_symbols = known_used_symbols, declared_globals = declared_globals)

      lapply(
        which(!is.na(res$message)),
        function(row_num) {
          row <- res[row_num, ]
          org_line_start <- as.integer(row$line1) + fun_start_line - 1L
          if (!nzchar(row$line2) || row$line2 == row$line1) {
            org_line_end <- org_line_start
          } else {
            org_line_end <- as.integer(row$line2) + fun_start_line - 1L
          }
          line <- source_expression$content[org_line_start]

          row$name <- re_substitutes(row$name, rex("<-"), "")

          linted_node <- xml2::xml_find_first(
            xml,
            sprintf(
              "
              //SYMBOL[(%1$s) and @line1 >= %2$d and @line1 <= %3$d] |
              //SYMBOL_FUNCTION_CALL[(%1$s) and @line1 >= %2$d and @line1 <= %3$d]
              ",
              xp_text_in_table(paste0(c("", "`", "'", '"'), row$name, c("", "`", "'", '"'))),
              org_line_start,
              org_line_end
            )
          )
          if (!is.na(linted_node)) {
            return(
              xml_nodes_to_lints(
                linted_node,
                source_expression = source_expression,
                lint_message = row$message,
                type = "warning"
              )
            )
          }

          location <- re_matches(line, rex(boundary, row$name, boundary), locations = TRUE)

          # Handle multi-line lints where name occurs on subsequent lines (#507)
          if (is.na(location$start) && nzchar(row$line2) && row$line2 != row$line1) {
            lines <- source_expression$content[org_line_start:org_line_end]
            locations <- re_matches(lines, rex(boundary, row$name, boundary), locations = TRUE)

            matching_row <- (which(!is.na(locations$start)) %||% 1L)[[1L]] # first matching row or 1 (as a fallback)

            org_line_start <- org_line_start + matching_row - 1L
            location <- locations[matching_row, ]
            line <- lines[matching_row]
          }

          # Fallback if name isn't found anywhere: lint the first line
          if (is.na(location$start)) {
            location$start <- 1L
            location$end <- nchar(line)
          }

          Lint(
            filename = source_expression$filename,
            line_number = org_line_start,
            column_number = location$start,
            type = "warning",
            message = row$message,
            line = line,
            ranges = list(c(location$start, location$end))
          )
        }
      )
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
  left_assignment_symbols <-
    xml2::xml_text(xml2::xml_find_all(xml, "expr[LEFT_ASSIGN]/expr[1]/SYMBOL[1]"))
  equal_assignment_symbols <-
    xml2::xml_text(xml2::xml_find_all(xml, "equal_assign/expr[1]/SYMBOL[1]"))
  assign_fun_symbols <-
    xml2::xml_text(xml2::xml_find_all(xml, "expr[expr[SYMBOL_FUNCTION_CALL/text()='assign']]/expr[2]/*"))
  set_method_fun_symbols <-
    xml2::xml_text(xml2::xml_find_all(xml, "expr[expr[SYMBOL_FUNCTION_CALL/text()='setMethod']]/expr[2]/*"))

  symbols <- c(
    left_assignment_symbols,
    equal_assignment_symbols,
    assign_fun_symbols,
    set_method_fun_symbols
  )

  # remove quotes or backticks from the beginning or the end
  symbols <- gsub("^[`'\"]|['\"`]$", "", symbols)

  symbols
}

parse_check_usage <- function(expression, known_used_symbols = character(), declared_globals = character()) {

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
