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
  Linter(function(source_file) {
    # If there is no xml data just return
    if (is.null(source_file$full_xml_parsed_content)) return(list())

    source_file$parsed_content <- source_file$full_parsed_content

    pkg_name <- pkg_name(find_package(dirname(source_file$filename)))
    if (!is.null(pkg_name)) {
      parent_env <- try_silently(getNamespace(pkg_name))
    }
    if (is.null(pkg_name) || inherits(parent_env, "try-error")) {
      parent_env <- globalenv()
    }
    env <- new.env(parent = parent_env)

    declared_globals <- try_silently(utils::globalVariables(package = pkg_name %||% globalenv()))

    symbols <- get_assignment_symbols(source_file$full_xml_parsed_content)

    # Just assign them an empty function
    for (symbol in symbols) {
      assign(symbol, function(...) invisible(), envir = env)
    }

    fun_info <- get_function_assignments(source_file$full_xml_parsed_content)

    lapply(seq_len(NROW(fun_info)), function(i) {
      info <- fun_info[i, ]

      code <- get_content(lines = source_file$content[seq(info$line1, info$line2)], info)
      fun <- try_silently(eval(envir = env,
                               parse(
                                 text = code,
                                 keep.source = TRUE
                               )
      ))

      if (inherits(fun, "try-error")) {
        return()
      }
      if (isTRUE(interpret_glue)) {
        known_used_symbols <- extract_glued_symbols(info$expr[[1L]])
      } else {
        known_used_symbols <- character()
      }

      res <- parse_check_usage(fun, known_used_symbols = known_used_symbols)

      lapply(
        which(!is.na(res$message)),
        function(row_num) {
          row <- res[row_num, ]

          if (row$name %in% declared_globals) {
            return()
          }

          org_line_num <- as.integer(row$line1) + info$line1 - 1L
          line <- source_file$content[as.integer(org_line_num)]

          row$name <- re_substitutes(row$name, rex("<-"), "")

          location <- re_matches(line, rex(boundary, row$name, boundary), locations = TRUE)

          # Handle multi-line lints where name occurs on subsequent lines (#507)
          if (is.na(location$start) && nzchar(row$line2) && row$line2 != row$line1) {
            lines <- source_file$content[org_line_num:(as.integer(row$line2) + info$line1 - 1L)]
            locations <- re_matches(lines, rex(boundary, row$name, boundary), locations = TRUE)

            matching_row <- (which(!is.na(locations$start)) %||% 1L)[[1L]] # first matching row or 1 (as a fallback)

            org_line_num <- org_line_num + matching_row - 1L
            location <- locations[matching_row, ]
            line <- lines[matching_row]
          }

          # Fallback if name isn't found anywhere: lint the first line
          if (is.na(location$start)) {
            location$start <- 1L
            location$end <- nchar(line)
          }

          Lint(
            filename = source_file$filename,
            line_number = org_line_num,
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
      if (is.null(parsed_xml)) return("")
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

get_function_assignments <- function(xml) {
  # NB: difference across R versions in how EQ_ASSIGN is represented in the AST
  #   (under <expr_or_assign_or_help> or <equal_assign>)
  # TODO(#1106): use //*[...] to capture assignments in more scopes
  funs <- xml2::xml_find_all(
    xml,
    paste(
      # direct assignments
      "*[
        (
          (self::expr and (LEFT_ASSIGN or EQ_ASSIGN))
          or ((self::expr_or_assign_or_help or self::equal_assign) and EQ_ASSIGN)
        )
        and expr[2][FUNCTION]
      ]
      /expr[2]
      ",
      # assign() and setMethod() assignments
      "//expr[expr[SYMBOL_FUNCTION_CALL[text() = 'assign' or text() = 'setMethod']]]/expr[3]",
      sep = " | "
    )
  )

   if (length(funs) == 0L) {
    res <- data.frame(line1 = integer(), line2 = integer(), col1 = integer(), col2 = integer())
  } else {
    res <- data.frame(
      line1 = as.integer(vapply(funs, xml2::xml_attr, attr = "line1", "")),
      line2 = as.integer(vapply(funs, xml2::xml_attr, attr = "line2", "")),
      col1 = as.integer(vapply(funs, xml2::xml_attr, attr = "col1", "")),
      col2 = as.integer(vapply(funs, xml2::xml_attr, attr = "col2", ""))
    )
  }
  res[["expr"]] <- funs
  res
}

parse_check_usage <- function(expression, known_used_symbols = character()) {

  vals <- list()

  report <- function(x) {
    vals[[length(vals) + 1L]] <<- x
  }

  try(codetools::checkUsage(
    expression,
    report = report,
    suppressLocalUnused = known_used_symbols
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
