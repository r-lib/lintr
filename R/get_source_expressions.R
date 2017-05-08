#' Parsed sourced file from a filename
#'
#' This object is given as input to each linter
#' @param filename the file to be parsed.
#' @export
get_source_expressions <- function(filename) {
  source_file <- srcfile(filename)
  source_file$lines <- readLines(filename)
  source_file$lines <- extract_r_source(source_file$filename, source_file$lines)
  source_file$content <- get_content(source_file$lines)

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

      message_info <- re_matches(e$message,
        rex(single_quotes, capture(name = "name", anything), single_quotes,
          anything,
          double_quotes, capture(name = "starting", anything), double_quotes))

      line_location <- re_matches(source_file$content, rex(message_info$starting), locations = TRUE)

      line_number <- find_line_fun(source_file$content)(line_location$start)
      column_number <- find_column_fun(source_file$content)(line_location$start)
      return(
        Lint(
          filename = source_file$filename,
          line_number = line_number,
          column_number = column_number,
          type = "error",
          message = e$message,
          line = source_file$lines[[line_number]],
          linter = "error"
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
      line = line,
      linter = "error"
      )
  }

  e <- NULL

  parsed_content <- get_source_file(source_file, error = lint_error)
  tree <- generate_tree(parsed_content)

  expressions <- lapply(top_level_expressions(parsed_content), function(loc) {
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
      xml_parsed_content = xml2::read_xml(xmlparsedata::xml_parse_data(pc)),
      content = content,

      find_line = find_line_fun(content),

      find_column = find_column_fun(content)
      )
    })

  # add global expression
  expressions[[length(expressions) + 1L]] <-
    list(
      filename = filename,
      file_lines = source_file$lines,
      content = source_file$lines,
      xml_parsed_content = if (!is.null(parsed_content)) xml2::read_xml(xmlparsedata::xml_parse_data(parsed_content))
      )

  list(expressions = expressions, error = e, lines = source_file$lines)
}

get_source_file <- function(source_file, error = identity) {

  e <- tryCatch(
    source_file$parsed_content <- parse(text = source_file$content, srcfile = source_file, keep.source = TRUE),
    error = error)

  # This needs to be done twice to avoid
  #   https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16041
  e <- tryCatch(
    source_file$parsed_content <- parse(text = source_file$content, srcfile = source_file, keep.source = TRUE),
    error = error)

  if (!inherits(e, "expression")) {
    assign("e", e,  envir = parent.frame())
  }

  fix_eq_assigns(fix_column_numbers(fix_tab_indentations(source_file)))
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

# Adjust the columns that getParseData reports from bytes to characters.
fix_column_numbers <- function(content) {
  if (is.null(pc)) {
    return(NULL)
  }

  text_lengths <- nchar(content$text, "chars")
  byte_lengths <- nchar(content$text, "bytes")
  differences <- byte_lengths - text_lengths

  to_change <- which(differences > 0L)

  adjusted_col1 <- content$col1
  adjusted_col2 <- content$col2

  for (i in to_change) {
    needs_adjustment <- which(content$line1 == content$line1[i] & content$col1 >= content$col1[i])

    for (j in needs_adjustment) {
      adjusted_col1[j] <-
        content$col1[j] -
          sum(differences[
            content$line1 == content$line1[j] &
            content$col1 < content$col1[j]])

      adjusted_col2[j] <-
        content$col2[j] -
          sum(differences[
            content$line1 == content$line1[j] &
            content$col2 < content$col2[j]])
    }
  }
  content$col1 <- adjusted_col1
  content$col2 <- adjusted_col2
  content
}


# Fix column numbers when there are tabs
# getParseData() counts 1 tab as a variable number of spaces instead of one:
# https://github.com/wch/r-source/blame/e7401b68ab0e032fce3e376aaca9a5431619b2b4/src/main/gram.y#L512
# The number of spaces is so that the code is brought to the next 8-character indentation level e.g:
#   "1\t;"          -> "1       ;"
#   "12\t;"         -> "12      ;"
#   "123\t;"        -> "123     ;"
#   "1234\t;"       -> "1234    ;"
#   "12345\t;"      -> "12345   ;"
#   "123456\t;"     -> "123456  ;"
#   "1234567\t;"    -> "1234567 ;"
#   "12345678\t;"   -> "12345678        ;"
#   "123456789\t;"  -> "123456789       ;"
#   "1234567890\t;" -> "1234567890      ;"
fix_tab_indentations <- function(source_file) {
  pc <- getParseData(source_file)
  if (is.null(pc)) {
    return(NULL)
  }

  tab_cols <- re_matches(source_file[["lines"]], "\t", global = TRUE, locations = TRUE)
  tab_cols <- lapply(
    tab_cols,
    function(cols) {
      start_cols <- cols[["start"]]
      if (!is.na(start_cols[[1L]])) {
        start_cols
      } else {
        NA
      }
    }
  )
  names(tab_cols) <- seq_along(tab_cols)
  tab_cols <- tab_cols[!is.na(tab_cols)]

  for (line in names(tab_cols)) {
    tab_widths <- tab_widths(tab_cols[[line]])
    which_lines <- pc[["line1"]] == as.integer(line)
    cols <- pc[which_lines, c("col1", "col2")]
    if (nrow(cols)) {
      for (tab_col in tab_cols[[line]]) {
        which_cols <- cols > tab_col
        cols[which_cols] <- cols[which_cols] - tab_widths[[as.character(tab_col)]] + 1L
        pc[which_lines, c("col1", "col2")] <- cols
      }
    }
  }

  pc
}


tab_widths <- function(tab_columns, indent_width = 8L) {
  nms <- as.character(tab_columns)
  widths <- vapply(
    seq_along(tab_columns),
    function(i) {
      tab_col <- tab_columns[[i]]
      tab_width <- indent_width - (tab_col - 1L) %% indent_width
      which_cols <- tab_columns > tab_col
      tab_columns[which_cols] <<- tab_columns[which_cols] + tab_width - 1L
      tab_width
    },
    integer(1L)
  )
  names(widths) <- nms
  widths
}

# This function wraps equal assign expressions in a parent expression so they
# are the same as the corresponding <- expression
fix_eq_assigns <- function(pc) {
  if (is.null(pc)) {
    return(NULL)
  }

  eq_assign_locs <- which(pc$token == "EQ_ASSIGN")
  if (length(eq_assign_locs) == 0L) {
    return(pc)
  }

  prev_locs <- vapply(eq_assign_locs, prev_with_parent, pc = pc, integer(1))
  next_locs <- vapply(eq_assign_locs, next_with_parent, pc = pc, integer(1))
  expr_locs <- (function(x){
    x[is.na(x)] <- FALSE
    !x
    })(prev_locs == lag(next_locs)) # nolint

  id_itr <- max(pc$id)

  line1 <- integer(sum(expr_locs))
  col1 <- integer(sum(expr_locs))

  line2 <- integer(sum(expr_locs))
  col2 <- integer(sum(expr_locs))

  id <- integer(sum(expr_locs))

  parent <- integer(sum(expr_locs))

  token <- character(sum(expr_locs))

  terminal <- logical(sum(expr_locs))

  text <- character(sum(expr_locs))

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

    token[i] <- "expr"

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
