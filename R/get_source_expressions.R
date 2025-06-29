#' Parsed sourced file from a filename
#'
#' This object is given as input to each linter.
#'
#' @details
#' The file is read using the `encoding` setting.
#' This setting is found by taking the first valid result from the following locations
#'
#' 1. The `encoding` key from the usual lintr configuration settings.
#' 2. The `Encoding` field from a Package `DESCRIPTION` file in a parent directory.
#' 3. The `Encoding` field from an R Project `.Rproj` file in a parent directory.
#' 4. `"UTF-8"` as a fallback.
#'
#' @param filename the file to be parsed.
#' @param lines a character vector of lines.
#'   If `NULL`, then `filename` will be read.
#' @return A `list` with three components:
#' \describe{
#'   \item{expressions}{a `list` of
#'   `n+1` objects. The first `n` elements correspond to each expression in
#'   `filename`, and consist of a list of 8 elements:
#'   \itemize{
#'     \item{`filename` (`character`) the name of the file.}
#'     \item{`line` (`integer`) the line in the file where this expression begins.}
#'     \item{`column` (`integer`) the column in the file where this expression begins.}
#'     \item{`lines` (named `character`) vector of all lines spanned by this
#'           expression, named with the corresponding line numbers.}
#'     \item{`parsed_content` (`data.frame`) as given by [utils::getParseData()] for this expression.}
#'     \item{`xml_parsed_content` (`xml_document`) the XML parse tree of this expression as given by
#'           [xmlparsedata::xml_parse_data()].}
#'     \item{`content` (`character`) the same as `lines` as a single string (not split across lines).}
#'     \item{`xml_find_function_calls(function_names)` (`function`) a function that returns all `SYMBOL_FUNCTION_CALL`
#'           XML nodes from `xml_parsed_content` with specified function names.}
#'   }
#'
#'   The final element of `expressions` is a list corresponding to the full file
#'   consisting of 7 elements:
#'   \itemize{
#'     \item{`filename` (`character`) the name of this file.}
#'     \item{`file_lines` (`character`) the [readLines()] output for this file.}
#'     \item{`content` (`character`) for .R files, the same as `file_lines`;
#'           for .Rmd or .qmd scripts, this is the extracted R source code (as text).}
#'     \item{`full_parsed_content` (`data.frame`) as given by
#'           [utils::getParseData()] for the full content.}
#'     \item{`full_xml_parsed_content` (`xml_document`) the XML parse tree of all
#'           expressions as given by [xmlparsedata::xml_parse_data()].}
#'     \item{`terminal_newline` (`logical`) records whether `filename` has a terminal
#'           newline (as determined by [readLines()] producing a corresponding warning).}
#'     \item{`xml_find_function_calls(function_names)` (`function`) a function that returns all `SYMBOL_FUNCTION_CALL`
#'           XML nodes from `full_xml_parsed_content` with specified function names.}
#'   }
#'   }
#'   \item{error}{A `Lint` object describing any parsing error.}
#'   \item{warning}{A `lints` object describing any parsing warning.}
#'   \item{lines}{The [readLines()] output for this file.}
#' }
#'
#' @examples
#' tmp <- tempfile()
#' writeLines(c("x <- 1", "y <- x + 1"), tmp)
#' get_source_expressions(tmp)
#' unlink(tmp)
#' @export
get_source_expressions <- function(filename, lines = NULL) {
  source_expression <- srcfile(filename, encoding = settings$encoding)

  # Ensure English locale for terminal newline and zero-length variable warning messages
  old_lang <- set_lang("en")
  on.exit(reset_lang(old_lang))

  source_expression$lines <- lines %||% read_lines(filename)

  # Only regard explicit attribute terminal_newline=FALSE as FALSE and all other cases (e.g. NULL or TRUE) as TRUE.
  terminal_newline <- !isFALSE(attr(source_expression$lines, "terminal_newline", exact = TRUE))

  e <- w <- NULL
  source_expression$lines <- extract_r_source(
    filename = source_expression$filename,
    lines = source_expression$lines,
    error = function(e) lint_rmd_error(e, source_expression)
  )
  names(source_expression$lines) <- seq_along(source_expression$lines)
  source_expression$content <- get_content(source_expression$lines)
  parsed_content <- get_source_expression(source_expression, error = function(e) lint_parse_error(e, source_expression))
  is_unreliable_lint <- is_lint(e) && (is.na(e$line) || !nzchar(e$line) || e$message == "unexpected end of input")

  # Currently no way to distinguish the source of the warning
  #   from the message itself, so we just grep the source for the
  #   exact string generating the warning; de-dupe in case of
  #   multiple exact matches like '1e-3L; 1e-3L'.
  # See https://bugs.r-project.org/show_bug.cgi?id=18863.
  w <- lint_parse_warnings(w, parsed_content, source_expression)

  if (is_unreliable_lint) {
    # Don't create expression list if it's unreliable (invalid encoding or unhandled parse error)
    return(list(expressions = list(), error = e, warning = w, lines = source_expression$lines))
  }

  top_level_map <- generate_top_level_map(parsed_content)
  xml_parsed_content <- safe_parse_to_xml(parsed_content)

  expressions <- lapply(
    X = top_level_expressions(parsed_content),
    FUN = get_single_source_expression,
    parsed_content,
    source_expression,
    filename,
    top_level_map
  )

  expressions <- maybe_append_expression_xml(expressions, xml_parsed_content)

  # add global expression
  expressions[[length(expressions) + 1L]] <- list(
    filename = filename,
    file_lines = source_expression$lines,
    content = source_expression$lines,
    full_parsed_content = parsed_content,
    full_xml_parsed_content = xml_parsed_content,
    xml_find_function_calls = build_xml_find_function_calls(xml_parsed_content),
    terminal_newline = terminal_newline
  )

  list(expressions = expressions, error = e, warning = w, lines = source_expression$lines)
}

lint_parse_error <- function(e, source_expression) {
  # R >= 4.3.0
  if (inherits(e, "parseError")) {
    return(lint_parse_error_r43(e, source_expression))
  }

  message_info <- re_matches(
    e$message,
    rex(
      except_some_of(":"),
      ":",
      capture(name = "line", digits),
      ":",
      capture(name = "column", digits),
      ":",
      space,
      capture(name = "message", anything),
      "\n"
    )
  )

  if (!is.na(message_info$line)) {
    return(lint_parse_error_r42(message_info, source_expression))
  }

  # an error that does not use R_ParseErrorMsg
  lint_parse_error_nonstandard(e, source_expression)
}

#' Currently no way to distinguish the source of the warning
#'   from the message itself, so we just grep the source for the
#'   exact string generating the warning; de-dupe in case of
#'   multiple exact matches like '1e-3L; 1e-3L'
#' @noRd
lint_parse_warnings <- function(w, parsed_content, source_expression) {
  if (!length(w)) {
    return(w)
  }
  flatten_lints(lapply(unique(w), lint_parse_warning, parsed_content, source_expression))
}

#' The set of parser warnings seems pretty stable, but as
#'   long as they don't generate sourceref hints, we're
#'   stuck with this somewhat-manual approach.
#' @noRd
lint_parse_warning <- function(w, parsed_content, source_expression) {
  for (lint_re in parser_warning_regexes) {
    bad_txt <- re_matches(w, lint_re)$txt[1L]
    # at most one regex matches the warning
    if (is.na(bad_txt)) {
      next
    } else {
      break
    }
  }
  # use the parse tree to avoid baroque matches to comments, strings
  hits <- parsed_content[with(parsed_content, token == "NUM_CONST" & text == bad_txt), ]
  lapply(seq_len(nrow(hits)), function(ii) {
    Lint(
      filename = source_expression$filename,
      line_number = hits$line1[ii],
      column_number = hits$col1[ii],
      type = "warning",
      message = w,
      line = source_expression$lines[[as.character(hits$line1[ii])]],
      ranges = list(c(hits$col1[ii], hits$col2[ii]))
    )
  })
}

parser_warning_regexes <- list(
  int_with_decimal =
    rex("integer literal ", capture(anything, name = "txt"), " contains decimal; using numeric value"),
  nonint_with_l =
    rex("non-integer value ", capture(anything, name = "txt"), " qualified with L; using numeric value"),
  unneeded_decimal =
    rex("integer literal ", capture(anything, name = "txt"), " contains unnecessary decimal point")
)

#' Ensure a string is valid for printing
#'
#' Helper to ensure a valid string is provided as line where necessary.
#' Handles two cases:
#'
#'  1. NA lines, as generated by running outside of code blocks in Rmd documents
#'  2. invalid string lines, as generated by invalid encoding settings
#'
#' [nchar()] can detect both cases: 1. returns `NA_integer_`, 2. errors
#'
#' @param line Possibly misencoded or NA line
#'
#' @return `line` if it's a valid non-NA string, otherwise an empty string.
#'
#' @noRd
fixup_line <- function(line) {
  nchars <- tryCatch(nchar(line, type = "chars"), error = function(e) NA_integer_)
  if (is.na(nchars)) {
    ""
  } else {
    line
  }
}

#' Convert an R >= 4.3.0 classed parseError with metadata into a lint
#'
#' @param e A parse error of class `parseError` generated by R >= 4.3.0
#' @param source_expression The source expression that generated the parse error
#'
#' @return A [Lint()] based on the metadata attached to `e`.
#'
#' @noRd
lint_parse_error_r43 <- function(e, source_expression) {
  msg <- re_substitutes(e$message, rex(" (", except_some_of(")"), ")", end), "")
  line_number <- e$lineno
  column <- e$colno
  substr(msg, 1L, 1L) <- toupper(substr(msg, 1L, 1L))
  msg <- paste0(msg, ".")

  if (inherits(e, "invalidMBCS")) {
    msg <- paste(msg, "Is the encoding correct?")
  }

  if (column == 0L) {
    line_number <- line_number - 1L
  }

  # Safely handle invalid location info
  if (line_number < 1L || line_number > length(source_expression$lines)) {
    # nocov start
    line_number <- 1L
    column <- 1L
    # nocov end
  }

  line <- fixup_line(source_expression$lines[[line_number]])
  if (column == 0L || column > nchar(line) + 1L) {
    column <- nchar(line) + 1L
  }

  Lint(
    filename = source_expression$filename,
    line_number = line_number,
    column_number = column,
    type = "error",
    message = msg,
    line = line
  )
}

#' Convert a R < 4.3.0 standard parse error message into a lint
#'
#' @param message_info Match of the structured parse error message regex, matched in [lint_parse_error()]
#' @param source_expression The source expression that generated the parse error
#'
#' @return A [Lint()] based on text mining of the error message captured by `message_info`,
#'
#' @noRd
lint_parse_error_r42 <- function(message_info, source_expression) {
  line_number <- as.integer(message_info$line)
  column_number <- as.integer(message_info$column)

  # If the column number is zero it means the error really occurred at the
  # end of the previous line
  if (column_number %==% 0L) {
    line_number <- line_number - 1L
    line <- fixup_line(source_expression$lines[[line_number]])
    column_number <- nchar(line) + 1L
  } else {
    line <- fixup_line(source_expression$lines[[line_number]])
  }

  Lint(
    filename = source_expression$filename,
    line_number = line_number,
    column_number = column_number,
    type = "error",
    message = message_info$message,
    line = line
  )
}

#' Convert a R < 4.3.0 non-standard parse error message into a lint
#'
#' Uses a hand-crafted regex and some heuristics to find location information, falling back to Line 1, Column 1 if all
#' attempts fail.
#'
#' @param e A parse error that doesn't fit the standard `message_info` regex used for most parse errors in R < 4.3.0
#' @param source_expression The source expression that generated the parse error
#'
#' @return A [Lint()] based on trying to extract information from the error message of `e`.
#'
#' @noRd
lint_parse_error_nonstandard <- function(e, source_expression) {
  if (grepl("invalid multibyte character in parser at line", e$message, fixed = TRUE)) {
    # nocov start: platform-specific
    l <- as.integer(re_matches(
      e$message,
      rex("invalid multibyte character in parser at line ", capture(name = "line", digits))
    )$line)
    # Invalid encoding in source code
    return(
      Lint(
        filename = source_expression$filename,
        line_number = l,
        column_number = 1L,
        type = "error",
        message = "Invalid multibyte character in parser. Is the encoding correct?",
        line = fixup_line(source_expression$lines[[l]])
      )
    )
    # nocov end
  } else if (grepl("invalid multibyte string, element", e$message, fixed = TRUE)) {
    # Invalid encoding, will break even re_matches() below, so we need to handle this first.
    return(
      Lint(
        filename = source_expression$filename,
        line_number = 1L,
        column_number = 1L,
        type = "error",
        message = "Invalid multibyte string. Is the encoding correct?",
        line = ""
      )
    )
  } else if (grepl("repeated formal argument", e$message, fixed = TRUE)) {
    # nocov start: only throws on certain versions; tested on L389
    matches <- re_matches(
      e$message,
      rex(
        "repeated formal argument '",
        capture(name = "symbol", anything),
        "' on line ",
        capture(name = "line", digits)
      )
    )
    sym <- matches$symbol
    l <- as.integer(matches$line)
    # Repeated formal argument 'sym' on line l
    return(
      Lint(
        filename = source_expression$filename,
        line_number = l,
        column_number = 1L,
        type = "error",
        message = sprintf("Repeated formal argument '%s'.", sym),
        line = fixup_line(source_expression$lines[[l]])
      )
    )
    # nocov end
  }

  # Hand-crafted regex to parse all error messages generated by the R parser code for R < 4.3.
  # The error messages can be found in src/main/gram.c and src/main/character.c in the R code.
  # This code produces a list of all possible error messages:
  #
  # nolint start: commented_code_linter.
  # parser_files <- c("src/main/gram.c", "src/main/character.c")
  #
  # lines <- unlist(lapply(
  #   file.path("https://raw.githubusercontent.com/wch/r-source/trunk", parser_files),
  #   readLines
  # ))
  # error_calls <- grep("error(_(", lines, fixed = TRUE, value = TRUE)
  # error_formats <- trimws(gsub("^.*error\\(_\\(\"(.+)\".+", "\\1", error_calls))
  # error_formats <- unique(error_formats)
  # nolint end
  parse_error_rx <- rex(
    start,
    capture(anything, name = "msg_1"),
    or(" at ", " on ", " ("),
    "line ",
    capture(digits, name = "line"),
    maybe(")"),
    capture(anything, name = "msg_2"),
    end
  )

  if (grepl(parse_error_rx, e$message, perl = TRUE)) {
    # nocov start: only throws on certain versions. Tested on L396: Nul character not allowed
    rx_match <- re_matches(
      e$message,
      parse_error_rx
    )
    l <- as.integer(rx_match$line)
    # Sometimes the parser "line" runs one past the last line
    l <- pmin(l, length(source_expression$lines))

    msg <- paste0(rx_match$msg_1, if (nzchar(rx_match$msg_2)) " ", rx_match$msg_2, ".")
    substr(msg, 1L, 1L) <- toupper(substr(msg, 1L, 1L))

    return(
      Lint(
        filename = source_expression$filename,
        line_number = l,
        column_number = 1L,
        type = "error",
        message = msg,
        line = source_expression$lines[[l]]
      )
    )
    # nocov end
  }

  message_info <- re_matches(
    e$message,
    rex(
      single_quotes, capture(name = "name", anything), single_quotes,
      anything,
      double_quotes, capture(name = "starting", anything), double_quotes
    )
  )

  loc <- re_matches(source_expression$content, rex(message_info$starting), locations = TRUE)
  line_location <- loc[!is.na(loc$start) & !is.na(loc$end), ]

  if (nrow(line_location) == 0L) {
    if (grepl("attempt to use zero-length variable name", e$message, fixed = TRUE)) {
      # empty symbol: ``, ``(), ''(), ""(), fun(''=42), fun(""=42), fun(a=1,""=42)
      loc <- re_matches(
        source_expression$content,
        rex(
          "``" %or%
            list(or("''", '""'), any_spaces, "(") %or%
            list(or("(", ","), any_spaces, or("''", '""'), any_spaces, "=")
        ),
        options = "multi-line",
        locations = TRUE
      )
      loc <- loc[!is.na(loc$start) & !is.na(loc$end), ]
      if (nrow(loc) > 0L) {
        line_location <- loc[1L, ]
      }
    } else {
      # nocov start
      return(
        Lint(
          filename = source_expression$filename,
          line_number = 1L,
          column_number = 1L,
          type = "error",
          message = e$message,
          line = ""
        )
      )
      # nocov end
    }
  }

  newline_locs <- get_newline_locs(source_expression$content)
  line_number <- which(newline_locs >= line_location$start)[1L] - 1L
  column_number <- line_location$start - newline_locs[line_number]

  Lint(
    filename = source_expression$filename,
    line_number = line_number,
    column_number = column_number,
    type = "error",
    message = e$message,
    line = source_expression$lines[[line_number]]
  )
}

lint_rmd_error <- function(e, source_expression) {
  message_info <- re_matches(
    e$message,
    rex(
      except_some_of(":"),
      ":",
      capture(name = "line", digits),
      ":",
      capture(name = "column", digits),
      ":",
      space,
      capture(name = "message", anything),
      "\n"
    )
  )

  line_number <- as.integer(message_info$line)
  column_number <- as.integer(message_info$column)

  Lint(
    filename = source_expression$filename,
    line_number = line_number,
    column_number = column_number,
    type = "error",
    message = message_info$message,
    line = source_expression$lines[line_number]
  )
}

get_single_source_expression <- function(loc,
                                         parsed_content,
                                         source_expression,
                                         filename,
                                         top_level_map) {
  line_nums <- parsed_content$line1[loc]:parsed_content$line2[loc]
  expr_lines <- source_expression$lines[line_nums]
  names(expr_lines) <- line_nums
  content <- get_content(source_expression$lines, parsed_content[loc, ])
  id <- parsed_content$id[loc]
  pc <- parsed_content[which(top_level_map == id), ]
  list(
    filename = filename,
    line = parsed_content[loc, "line1"],
    column = parsed_content[loc, "col1"],
    lines = expr_lines,
    parsed_content = pc,
    xml_parsed_content = xml2::xml_missing(),
    # Placeholder for xml_find_function_calls, if needed (e.g. on R <= 4.0.5 with input source "\\")
    xml_find_function_calls = build_xml_find_function_calls(xml2::xml_missing()),
    content = content
  )
}

get_source_expression <- function(source_expression, error = identity) {
  parse_error <- FALSE

  env <- parent.frame() # nolint: object_usage_linter. Used below.
  # https://adv-r.hadley.nz/conditions.html
  parsed_content <- withCallingHandlers(
    tryCatch(
      parse(
        text = source_expression$content,
        srcfile = source_expression,
        keep.source = TRUE
      ),
      error = error
    ),
    warning = function(w) {
      env$w <- c(env$w, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (is_error(parsed_content) || is_lint(parsed_content)) {
    assign("e", parsed_content, envir = parent.frame())
    parse_error <- TRUE
  }

  # Triggers an error if the lines contain invalid characters.
  parsed_content <- tryCatch(
    nchar(source_expression$content, type = "chars"),
    error = error
  )

  if (is_error(parsed_content) || is_lint(parsed_content)) {
    # Let parse errors take precedence over encoding problems
    if (!parse_error) assign("e", parsed_content, envir = parent.frame())
    return() # parsed_content is unreliable if encoding is invalid
  }

  source_expression$parsed_content <- parsed_content
  fix_octal_escapes(fix_eq_assigns(fix_tab_indentations(source_expression)), source_expression$lines)
}

maybe_append_expression_xml <- function(expressions, xml_parsed_content) {
  if (is.null(xml_parsed_content) || is.na(xml_parsed_content)) {
    return(expressions)
  }
  expression_xmls <- lapply(
    xml_find_all(xml_parsed_content, "/exprlist/*"),
    function(top_level_expr) xml2::xml_add_parent(xml2::xml_new_root(top_level_expr), "exprlist")
  )
  for (i in seq_along(expressions)) {
    expressions[[i]]$xml_parsed_content <- expression_xmls[[i]]
    expressions[[i]]$xml_find_function_calls <- build_xml_find_function_calls(expression_xmls[[i]])
  }
  expressions
}

get_newline_locs <- function(x) {
  newline_search <- re_matches(x, rex("\n"), locations = TRUE, global = TRUE)[[1L]]$start
  c(
    0L,
    if (!is.na(newline_search[1L])) newline_search,
    nchar(x) + 1L
  )
}

# Fix column numbers when there are tabs
# getParseData() counts 1 tab as a variable number of spaces instead of one:
# https://github.com/wch/r-source/blame/e7401b68ab0e032fce3e376aaca9a5431619b2b4/src/main/gram.y#L512
# The number of spaces is so that the code is brought to the next 8-character indentation level e.g.:
#   "1\t;"          --> "1       ;"
#   "12\t;"         --> "12      ;"
#   "123\t;"        --> "123     ;"
#   "1234\t;"       --> "1234    ;"
#   "12345\t;"      --> "12345   ;"
#   "123456\t;"     --> "123456  ;"
#   "1234567\t;"    --> "1234567 ;"
#   "12345678\t;"   --> "12345678        ;"
#   "123456789\t;"  --> "123456789       ;"
#   "1234567890\t;" --> "1234567890      ;"
fix_tab_indentations <- function(source_expression) {
  parse_data <- getParseData(source_expression)

  if (is.null(parse_data)) {
    return(NULL)
  }

  tab_cols <- gregexpr("\t", source_expression[["lines"]], fixed = TRUE)
  names(tab_cols) <- seq_along(tab_cols)
  matched_lines <- vapply(tab_cols, function(line_match) !is.na(line_match[1L]) && line_match[1L] > 0L, logical(1L))
  if (!any(matched_lines)) {
    return(parse_data)
  }
  tab_cols <- tab_cols[matched_lines]

  fix_columns <- c("line1", "line2", "col1", "col2")
  parse_data[, fix_columns] <- fix_tab_columns(parse_data[, fix_columns], tab_cols)
  parse_data
}

fix_tab_columns <- function(parse_content, tab_cols) {
  dat <- cbind(
    c(parse_content$line1, parse_content$line2),
    c(parse_content$col1, parse_content$col2)
  )
  lines <- as.integer(names(tab_cols))
  for (i in seq_along(tab_cols)) {
    is_curr_line <- dat[, 1L] == lines[[i]]
    if (!any(is_curr_line)) {
      next
    }
    tab_col <- tab_cols[[i]]
    line_tab_offsets <- tab_offsets(tab_col)
    for (j in seq_along(tab_col)) {
      is_line_to_change <- is_curr_line & dat[, 2L] > tab_col[[j]]
      if (any(is_line_to_change)) {
        dat[is_line_to_change, 2L] <- dat[is_line_to_change, 2L] - line_tab_offsets[[j]]
      }
    }
  }
  dat
}

tab_offsets <- function(tab_columns) {
  cum_offset <- 0L
  vapply(
    tab_columns - 1L,
    function(tab_idx) {
      # nolint next: object_overwrite_linter. 'offset' is a perfect name here.
      offset <- 7L - (tab_idx + cum_offset) %% 8L # using a tab width of 8 characters
      cum_offset <<- cum_offset + offset
      offset
    },
    integer(1L),
    USE.NAMES = FALSE
  )
}

# This function wraps equal assign expressions in a parent expression so they
# are the same as the corresponding <- expression
fix_eq_assigns <- function(pc) {
  if (is.null(pc) || any(c("equal_assign", "expr_or_assign_or_help") %in% pc$token)) {
    return(pc)
  }

  eq_assign_locs <- which(pc$token == "EQ_ASSIGN")
  # check whether the equal-assignment is the final entry
  if (length(eq_assign_locs) == 0L || tail(eq_assign_locs, 1L) == nrow(pc)) {
    return(pc)
  }

  prev_locs <- vapply(eq_assign_locs, prev_with_parent, pc = pc, integer(1L))
  next_locs <- vapply(eq_assign_locs, next_with_parent, pc = pc, integer(1L))
  expr_locs <- prev_locs != lag(next_locs)
  expr_locs[is.na(expr_locs)] <- TRUE

  id_itr <- max(pc$id)

  true_locs <- which(expr_locs)
  n_expr <- length(true_locs)

  supplemental_content <- data.frame(
    line1 = integer(n_expr),
    col1 = integer(n_expr),
    line2 = integer(n_expr),
    col2 = integer(n_expr),
    id = integer(n_expr),
    parent = integer(n_expr),
    token = character(n_expr),
    terminal = logical(n_expr),
    text = character(n_expr)
  )

  for (i in seq_len(n_expr)) {
    start_loc <- true_locs[i]
    end_loc <- true_locs[i]

    prev_loc <- prev_locs[start_loc]
    next_loc <- next_locs[end_loc]

    id_itr <- id_itr + 1L
    supplemental_content[i, ] <- list(
      pc[prev_loc, "line1"],
      pc[prev_loc, "col1"],
      pc[next_loc, "line2"],
      pc[next_loc, "col2"],
      id_itr,
      pc[eq_assign_locs[true_locs[i]], "parent"],
      "expr", # R now uses "equal_assign"
      FALSE,
      ""
    )

    new_parent_locs <- c(
      prev_locs[start_loc:end_loc],
      eq_assign_locs[start_loc:end_loc],
      next_locs[start_loc:end_loc],
      next_loc
    )
    pc[new_parent_locs, "parent"] <- id_itr
  }
  rownames(supplemental_content) <- supplemental_content$id
  res <- rbind(pc, supplemental_content)
  res[order(res$line1, res$col1, res$line2, res$col2, res$id), ]
}

step_with_parent <- function(pc, loc, offset) {
  id <- pc$id[loc]
  parent_id <- pc$parent[loc]

  with_parent <- pc[pc$parent == parent_id, ]
  with_parent <- with_parent[order(with_parent$line1, with_parent$col1, with_parent$line2, with_parent$col2), ]

  loc <- which(with_parent$id == id)

  which(pc$id == with_parent$id[loc + offset])
}

prev_with_parent <- function(pc, loc) step_with_parent(pc, loc, offset = -1L)
next_with_parent <- function(pc, loc) step_with_parent(pc, loc, offset = 1L)

top_level_expressions <- function(pc) {
  if (is.null(pc)) {
    return(integer(0L))
  }
  which(pc$parent <= 0L)
}

# workaround for bad parse data bug for octal escapes
#   https://bugs.r-project.org/show_bug.cgi?id=18323
fix_octal_escapes <- function(pc, lines) {
  # subset first to prevent using nchar() on MBCS input
  is_str_const <- pc$token == "STR_CONST"
  str_const <- pc[is_str_const, ]
  str_const_mismatch <- str_const$col2 - str_const$col1 != nchar(str_const$text) - 1L
  if (!any(str_const_mismatch)) {
    return(pc)
  }
  str_const <- str_const[str_const_mismatch, ]
  out <- character(nrow(str_const))
  single_line <- str_const$line1 == str_const$line2
  out[single_line] <- substr(
    lines[str_const$line1[single_line]],
    str_const$col1[single_line],
    str_const$col2[single_line]
  )
  for (ii in which(!single_line)) {
    out[ii] <- paste(
      c(
        substring(lines[str_const$line1[ii]], str_const$col1[ii]),
        if (str_const$line1[ii] < str_const$line2[ii] - 1L) {
          lines[(str_const$line1[ii] + 1L):(str_const$line2[ii] - 1L)]
        },
        substr(lines[str_const$line2[ii]], 1L, str_const$col2[ii])
      ),
      collapse = "\n"
    )
  }
  pc$text[is_str_const][str_const_mismatch] <- out
  pc
}
