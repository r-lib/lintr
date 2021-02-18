#' @describeIn linters Check that closures have the proper usage using
#' \code{\link[codetools]{checkUsage}}. Note that this runs
#' \code{\link[base]{eval}} on the code, so do not use with untrusted code.
#' @export
object_usage_linter <- function() {
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
      res <- parse_check_usage(fun)

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
  left_assignment_functions <-
    xml2::xml_find_all(xml, "expr[LEFT_ASSIGN][expr[2][FUNCTION]]/expr[2]")
  equal_assignment_functions <-
    xml2::xml_find_all(xml, "equal_assign[expr[2]][expr[FUNCTION]]/expr[2]")
  assign_assignment_functions <-
    xml2::xml_find_all(xml, "expr[expr[SYMBOL_FUNCTION_CALL/text()='assign']]/expr[3]")
  setmethod_assignment_functions <-
    xml2::xml_find_all(xml, "expr[expr[SYMBOL_FUNCTION_CALL/text()='setMethod']]/expr[3]")

  funs <- c(
    left_assignment_functions,
    equal_assignment_functions,
    assign_assignment_functions,
    setmethod_assignment_functions
  )

  get_attr <- function(x, attr) as.integer(xml2::xml_attr(x, attr))

  data.frame(
    line1 = viapply(funs, get_attr, "line1"),
    line2 = viapply(funs, get_attr, "line2"),
    col1 = viapply(funs, get_attr, "col1"),
    col2 = viapply(funs, get_attr, "col2"),
    stringsAsFactors = FALSE
  )
}

parse_check_usage <- function(expression) {

  vals <- list()

  report <- function(x) {
    vals[[length(vals) + 1L]] <<- x
  }

  try(codetools::checkUsage(expression, report = report))

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
