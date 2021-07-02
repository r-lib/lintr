#' Parsed sourced file from a filename
#'
#' This object is given as input to each linter
#'
#' @details
#' The file is read in using the `encoding` setting.
#' This setting found by taking the first valid result from the following locations
#'
#' 1. The `encoding` key from the usual lintr configuration settings.
#' 2. The `Encoding` field from a Package `DESCRIPTION` file in a parent directory.
#' 3. The `Encoding` field from an R Project `.Rproj` file in a parent directory.
#' 4. `"UTF-8"` as a fallback.
#'
#' @param filename the file to be parsed.
#' @param lines a character vector of lines.
#'   If \code{NULL}, then \code{filename} will be read.
#' @return A `list` with three components:
#'   \item{expressions}{a `list` of
#'   `n+1` objects. The first `n` elements correspond to each expression in
#'   `filename`, and consist of a list of 9 elements:
#'   \itemize{
#'     \item{`filename` (`character`)}
#'     \item{`line` (`integer`) the line in `filename` where this expression begins}
#'     \item{`column` (`integer`) the column in `filename` where this expression begins}
#'     \item{`lines` (named `character`) vector of all lines spanned by this
#'           expression, named with the line number corresponding to `filename`}
#'     \item{`parsed_content` (`data.frame`) as given by [utils::getParseData()] for this expression}
#'     \item{`xml_parsed_content` (`xml_document`) the XML parse tree of this
#'          expression as given by [xmlparsedata::xml_parse_data()]}
#'     \item{`content` (`character`) the same as `lines` as a single string (not split across lines)}
#'     \item{`find_line` (`function`) a function for returning lines in this expression}
#'     \item{`find_column` (`function`) a similar function for columns}
#'   }
#'
#'   The final element of `expressions` is a list corresponding to the full file
#'   consisting of 6 elements:
#'   \itemize{
#'     \item{`filename` (`character`)}
#'     \item{`file_lines` (`character`) the [readLines()] output for this file}
#'     \item{`content` (`character`) for .R files, the same as `file_lines`;
#'           for .Rmd scripts, this is the extracted R source code (as text)}
#'     \item{`full_parsed_content` (`data.frame`) as given by
#'           [utils::getParseData()] for the full content}
#'     \item{`full_xml_parsed_content` (`xml_document`) the XML parse tree of all
#'           expressions as given by [xmlparsedata::xml_parse_data()]}
#'     \item{`terminal_newline` (`logical`) records whether `filename` has a terminal
#'           newline (as determined by [readLines()] producing a corresponding warning)}
#'   }
#'   }
#'   \item{error}{A `Lint` object describing any parsing error.}
#'   \item{lines}{The [readLines()] output for this file.}
#' @export
#' @md
get_source_expressions <- function(filename, lines = NULL) {
  source_file <- srcfile(filename, encoding = settings$encoding)

  # Ensure English locale for terminal newline and zero-length variable warning messages
  old_lang <- set_lang("en")
  on.exit(reset_lang(old_lang))

  source_file$lines <- if (is.null(lines)) {
    read_lines(filename)
  } else {
    lines
  }

  # Only regard explict attribute terminal_newline=FALSE as FALSE and all other cases (e.g. NULL or TRUE) as TRUE.
  # We don't use isFALSE since it is introduced in R 3.5.0.
  terminal_newline <- !identical(attr(source_file$lines, "terminal_newline", exact = TRUE), FALSE)

  lint_error <- function(e) {
    message_info <- re_matches(e$message,
      rex(except_some_of(":"),
        ":",
        capture(name = "line",
          digits),
        ":",
        capture(name = "column",
          digits),
        ":",
        space,
        capture(name = "message",
          anything),
        "\n")
      )

    # an error that does not use R_ParseErrorMsg
    if (is.na(message_info$line)) {

      if (grepl("invalid multibyte character in parser at line", e$message, fixed = TRUE)) {
        l <- as.integer(re_matches(
          e$message,
          rex("invalid multibyte character in parser at line ", capture(name = "line", digits))
        )$line)
        # Invalid encoding in source code
        return(
          Lint(
            filename = source_file$filename,
            line_number = l,
            column_number = 1L,
            type = "error",
            message = "Invalid multibyte character in parser. Is the encoding correct?",
            line = source_file$lines[[l]]
          )
        )
      } else if (grepl("invalid multibyte string, element", e$message, fixed = TRUE)) {
        # Invalid encoding, will break even re_matches() below, so we need to handle this first.
        return(
          Lint(
            filename = source_file$filename,
            line_number = 1L,
            column_number = 1L,
            type = "error",
            message = "Invalid multibyte string. Is the encoding correct?",
            line = ""
          )
        )
      }

      message_info <- re_matches(e$message,
        rex(single_quotes, capture(name = "name", anything), single_quotes,
          anything,
          double_quotes, capture(name = "starting", anything), double_quotes))

      loc <- re_matches(source_file$content, rex(message_info$starting), locations = TRUE)
      line_location <- loc[!is.na(loc$start) & !is.na(loc$end), ]

      if (nrow(line_location) == 0L) {
        if (grepl("attempt to use zero-length variable name", e$message, fixed = TRUE)) {
          # empty symbol: ``, ``(), ''(), ""(), fun(''=42), fun(""=42), fun(a=1,""=42)
          loc <- re_matches(source_file$content,
            rex("``" %or% list(or("''", '""'), any_spaces, "(") %or%
              list(or("(", ","), any_spaces, or("''", '""'), any_spaces, "=")),
            options = "multi-line",
            locations = TRUE)
          loc <- loc[!is.na(loc$start) & !is.na(loc$end), ]
          if (nrow(loc) > 0) {
            line_location <- loc[1, ]
          }
        } else {
          # nocov start
          return(
            Lint(
              filename = source_file$filename,
              line_number = 1,
              column_number = 1,
              type = "error",
              message = e$message,
              line = ""
            )
          )
          # nocov end
        }
      }

      line_number <- find_line_fun(source_file$content)(line_location$start)
      column_number <- find_column_fun(source_file$content)(line_location$start)
      return(
        Lint(
          filename = source_file$filename,
          line_number = line_number,
          column_number = column_number,
          type = "error",
          message = e$message,
          line = source_file$lines[[line_number]]
        )
      )
    }

    line_number <- as.integer(message_info$line)
    column_number <- as.integer(message_info$column)

    # If the column number is zero it means the error really occurred at the
    # end of the previous line
    if (column_number %==% 0L) {
      line_number <- line_number - 1L
      line <- source_file$lines[[line_number]]
      column_number <- nchar(line)
    } else {
      line <- source_file$lines[[line_number]]
    }

    Lint(
      filename = source_file$filename,
      line_number = line_number,
      column_number = column_number,
      type = "error",
      message = message_info$message,
      line = line
    )
  }

  rmd_error <- function(e) {
    message_info <- re_matches(e$message,
      rex(except_some_of(":"),
        ":",
        capture(name = "line",
          digits),
        ":",
        capture(name = "column",
          digits),
        ":",
        space,
        capture(name = "message",
          anything),
        "\n")
      )

    line_number <- as.integer(message_info$line)
    column_number <- as.integer(message_info$column)

    Lint(
      filename = source_file$filename,
      line_number = line_number,
      column_number = column_number,
      type = "error",
      message = message_info$message,
      line = source_file$lines[line_number]
    )
  }

  e <- NULL
  source_file$lines <- extract_r_source(source_file$filename, source_file$lines, error = rmd_error)
  source_file$content <- get_content(source_file$lines)
  parsed_content <- get_source_file(source_file, error = lint_error)
  tree <- generate_tree(parsed_content)

  if (inherits(e, "lint") && !nzchar(e$line)) {
    # Don't create expression list if it's unreliable (invalid encoding or unhandled parse error)
    expressions <- list()
  } else {
    expressions <- lapply(
      X = top_level_expressions(parsed_content),
      FUN = get_single_source_expression,
      parsed_content,
      source_file,
      filename,
      tree
    )

    # add global expression
    expressions[[length(expressions) + 1L]] <-
      list(
        filename = filename,
        file_lines = source_file$lines,
        content = source_file$lines,
        full_parsed_content = parsed_content,
        full_xml_parsed_content = safe_parse_to_xml(parsed_content),
        terminal_newline = terminal_newline
      )
  }

  list(expressions = expressions, error = e, lines = source_file$lines)
}

get_single_source_expression <- function(loc,
                                         parsed_content,
                                         source_file,
                                         filename,
                                         tree) {
  line_nums <- parsed_content$line1[loc]:parsed_content$line2[loc]
  expr_lines <- source_file$lines[line_nums]
  names(expr_lines) <- line_nums
  content <- get_content(expr_lines, parsed_content[loc, ])

  id <- as.character(parsed_content$id[loc])
  edges <- component_edges(tree, id)
  pc <- parsed_content[c(loc, edges), ]
  list(
    filename = filename,
    line = parsed_content[loc, "line1"],
    column = parsed_content[loc, "col1"],
    lines = expr_lines,
    parsed_content = pc,
    xml_parsed_content = safe_parse_to_xml(pc),
    content = content,
    find_line = find_line_fun(content),
    find_column = find_column_fun(content)
  )
}

get_source_file <- function(source_file, error = identity) {
  parse_error <- FALSE

  e <- tryCatch(
    source_file$parsed_content <- parse(text = source_file$content, srcfile = source_file, keep.source = TRUE),
    error = error
  )

  # This needs to be done twice to avoid
  #   https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16041
  e <- tryCatch(
    source_file$parsed_content <- parse(text = source_file$content, srcfile = source_file, keep.source = TRUE),
    error = error
  )

  if (inherits(e, "error") || inherits(e, "lint")) {
    assign("e", e,  envir = parent.frame())
    parse_error <- TRUE
  }

  # Triggers an error if the lines contain invalid characters.
  e <- tryCatch(
    nchar(source_file$content, type = "chars"),
    error = error
  )

  if (inherits(e, "error") || inherits(e, "lint")) {
    # Let parse errors take precedence over encoding problems
    if (!parse_error) assign("e", e,  envir = parent.frame())
    return() # parsed_content is unreliable if encoding is invalid
  }

  fix_eq_assigns(fix_tab_indentations(source_file))
}

find_line_fun <- function(content) {
  newline_search <-
    re_matches(content,
      rex("\n"),
      locations = TRUE,
      global = TRUE)[[1]]$start

  newline_locs <- c(0L,
    if (!is.na(newline_search[1])) newline_search,
    nchar(content) + 1L)

  function(x) {
    which(newline_locs >= x)[1L] - 1L
  }
}

find_column_fun <- function(content) {
  newline_search <-
    re_matches(content,
      rex("\n"),
      locations = TRUE,
      global = TRUE)[[1]]$start

  newline_locs <- c(0L,
    if (!is.na(newline_search[1])) newline_search,
    nchar(content) + 1L)

  function(x) {
    line_number <- which(newline_locs >= x)[1L] - 1L
    x - newline_locs[line_number]
  }
}

# Fix column numbers when there are tabs
# getParseData() counts 1 tab as a variable number of spaces instead of one:
# https://github.com/wch/r-source/blame/e7401b68ab0e032fce3e376aaca9a5431619b2b4/src/main/gram.y#L512
# The number of spaces is so that the code is brought to the next 8-character indentation level e.g:
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
fix_tab_indentations <- function(source_file) {
  pc <- getParseData(source_file)

  if (is.null(pc)) {
    return(NULL)
  }

  tab_cols <- gregexpr("\t", source_file[["lines"]], fixed = TRUE)
  names(tab_cols) <- seq_along(tab_cols)
  tab_cols <- tab_cols[!is.na(tab_cols)]  # source lines from .Rmd and other files are NA
  tab_cols <- lapply(tab_cols, function(x) if (x[[1L]] < 0L) NA else x)
  tab_cols <- tab_cols[!is.na(tab_cols)]

  if (!length(tab_cols)) {
    return(pc)
  }

  pc_cols <- c("line1", "line2", "col1", "col2")
  dat <- matrix(data = unlist(pc[, pc_cols], use.names = FALSE), ncol = 2)
  lines <- as.integer(names(tab_cols))
  for (i in seq_along(tab_cols)) {
    is_curr_line <- dat[, 1L] == lines[[i]]
    if (any(is_curr_line)) {
      line_tab_offsets <- tab_offsets(tab_cols[[i]])
      for (j in seq_along(tab_cols[[i]])) {
        is_line_to_change <- is_curr_line & dat[, 2L] > tab_cols[[i]][[j]]
        if (any(is_line_to_change)) {
          dat[is_line_to_change, 2L] <- dat[is_line_to_change, 2L] - line_tab_offsets[[j]]
        }
      }
    }
  }
  pc[, pc_cols] <- dat
  pc
}


tab_offsets <- function(tab_columns) {
  cum_offset <- 0L
  vapply(
    tab_columns - 1L,
    function(tab_idx) {
      offset <- 7L - (tab_idx + cum_offset) %% 8L  # using a tab width of 8 characters
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
  if (is.null(pc)) {
    return(NULL)
  }

  eq_assign_locs <- which(pc$token == "EQ_ASSIGN")
  if (length(eq_assign_locs) == 0L ||
    nrow(pc) %in% eq_assign_locs || # check whether the equal-assignment is the final entry
    any(c("equal_assign", "expr_or_assign_or_help") %in% pc$token)) {
    return(pc)
  }

  prev_locs <- vapply(eq_assign_locs, prev_with_parent, pc = pc, integer(1))
  next_locs <- vapply(eq_assign_locs, next_with_parent, pc = pc, integer(1))
  expr_locs <- (function(x) {
    x[is.na(x)] <- FALSE
    !x
    })(prev_locs == lag(next_locs))

  id_itr <- max(pc$id)

  n_expr <- sum(expr_locs)

  line1 <- integer(n_expr)
  col1 <- integer(n_expr)

  line2 <- integer(n_expr)
  col2 <- integer(n_expr)

  id <- integer(n_expr)

  parent <- integer(n_expr)

  token <- character(n_expr)

  terminal <- logical(n_expr)

  text <- character(n_expr)

  true_locs <- which(expr_locs == TRUE)
  for (i in seq_along(true_locs)) {
    start <- true_locs[i]

    end <- true_locs[i]
    j <- end + 1L
    while (j <= length(expr_locs) && expr_locs[j] == FALSE) {
      end <- j
      j <- j + 1L
    }

    prev_loc <- prev_locs[start]
    next_loc <- next_locs[end]

    line1[i] <- pc[prev_loc, "line1"]
    col1[i] <- pc[prev_loc, "col1"]

    line2[i] <- pc[next_loc, "line2"]
    col2[i] <- pc[next_loc, "col2"]

    id[i] <- id_itr <- id_itr + 1L

    parent[i] <- pc[eq_assign_locs[true_locs[i]], "parent"]

    token[i] <- "expr" # R now uses "equal_assign"

    terminal[i] <- FALSE

    text[i] <- ""

    pc[eq_assign_locs[true_locs[i]], "parent"] <- id[i]
    for (j in start:end) {
      pc[prev_locs[j], "parent"] <- id[i]
      pc[eq_assign_locs[j], "parent"] <- id[i]
      pc[next_locs[j], "parent"] <- id[i]
    }
    pc[next_loc, "parent"] <- id[i]
  }
  res <- rbind(pc, data.frame(line1, col1, line2, col2, id, parent, token, terminal, text, row.names = id))
  res[order(res$line1, res$col1, res$line2, res$col2, res$id), ]
}

prev_with_parent <- function(pc, loc) {

  id <- pc$id[loc]
  parent_id <- pc$parent[loc]

  with_parent <- pc[pc$parent == parent_id, ]
  with_parent <- with_parent[order(with_parent$line1, with_parent$col1, with_parent$line2, with_parent$col2), ]

  loc <- which(with_parent$id == id)

  which(pc$id == with_parent$id[loc - 1L])
}

next_with_parent <- function(pc, loc) {

  id <- pc$id[loc]
  parent_id <- pc$parent[loc]

  with_parent <- pc[pc$parent == parent_id, ]
  with_parent <- with_parent[order(with_parent$line1, with_parent$col1, with_parent$line2, with_parent$col2), ]

  loc <- which(with_parent$id == id)

  which(pc$id == with_parent$id[loc + 1L])
}

top_level_expressions <- function(pc) {
  if (is.null(pc)) {
    return(integer(0))
  }
  which(pc$parent <= 0L)
}
