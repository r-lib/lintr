#' Line length linter
#'
#' Check that the line length of both comments and code is less than `length`.
#'
#' @param length Maximum line length allowed. Default is `80L` (Hollerith limit).
#' @param ignore_string_bodies Logical, default `FALSE`. If `TRUE`, the contents
#'   of string literals are ignored. The quotes themselves are included, so this
#'   mainly affects wide multiline strings, e.g. SQL queries.
#' @param allow_alignment_calls Character vector of function names whose
#'   calls are allowed to exceed `length` for tabular alignment (e.g.
#'   `tribble()`, `rowwiseDT()`).
#' @param allow_long_test_names Logical, default `TRUE`. If `TRUE`, string
#'   literal test descriptions in `testthat::test_that()` calls are allowed
#'   to exceed `length`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = strrep("x", 23L),
#'   linters = line_length_linter(length = 20L)
#' )
#'
#' # the trailing ' is counted towards line length, so this still lints
#' lint(
#'   text = "'a long single-line string'",
#'   linters = line_length_linter(length = 15L, ignore_string_bodies = TRUE)
#' )
#'
#' lines <- paste(
#'   "query <- '",
#'   "  SELECT *",
#'   "  FROM MyTable",
#'   "  WHERE profit > 0",
#'   "'",
#'   sep = "\n"
#' )
#' writeLines(lines)
#' lint(
#'   text = lines,
#'   linters = line_length_linter(length = 10L)
#' )
#'
#' code_lines <- "tibble::tribble(\n  ~col_one, ~col_two,\n  'long_val_1', 'long_val_2'\n)"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = line_length_linter(length = 20L, allow_alignment_calls = character())
#' )
#'
#' code_lines <- "test_that('a very long test description', {\n  expect_true(TRUE)\n})"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = line_length_linter(length = 20L, allow_long_test_names = FALSE)
#' )
#'
#' # okay
#' lint(
#'   text = strrep("x", 21L),
#'   linters = line_length_linter(length = 40L)
#' )
#'
#' lines <- paste(
#'   "paste(",
#'   "  'a long',",
#'   "  'single-line',",
#'   "  'string'",
#'   ")",
#'   sep = "\n"
#' )
#' writeLines(lines)
#' lint(
#'   text = lines,
#'   linters = line_length_linter(length = 15L, ignore_string_bodies = TRUE)
#' )
#'
#' lines <- paste(
#'   "query <- '",
#'   "  SELECT *",
#'   "  FROM MyTable",
#'   "  WHERE profit > 0",
#'   "'",
#'   sep = "\n"
#' )
#' writeLines(lines)
#' lint(
#'   text = lines,
#'   linters = line_length_linter(length = 10L, ignore_string_bodies = TRUE)
#' )
#'
#' code_lines <- "tibble::tribble(\n  ~col_one, ~col_two,\n  'long_val_1', 'long_val_2'\n)"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = line_length_linter(length = 20L)
#' )
#'
#' code_lines <- "test_that('a very long test description', {\n  expect_true(TRUE)\n})"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = line_length_linter(length = 20L)
#' )
#'
#' @evalRd rd_tags("line_length_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#long-lines>
#' @export
line_length_linter <- function(length = 80L,
                               ignore_string_bodies = FALSE,
                               allow_alignment_calls = c("tribble", "rowwiseDT"),
                               allow_long_test_names = TRUE) {
  general_msg <- paste("Lines should not be more than", length, "characters.")

  Linter(linter_level = "file", function(source_expression) {
    # Only go over complete file
    line_lengths <- nchar(source_expression$file_lines)
    long_lines <- which(line_lengths > length)

    if (ignore_string_bodies) {
      in_string_body_idx <-
        is_in_string_body(source_expression$full_parsed_content, length, long_lines)
      long_lines <- long_lines[!in_string_body_idx]
    }

    if (length(allow_alignment_calls) > 0L && length(long_lines) > 0L) {
      in_align_call_idx <- is_in_alignment_call(
        source_expression$full_parsed_content,
        long_lines,
        allow_alignment_calls
      )
      long_lines <- long_lines[!in_align_call_idx]
    }

    if (allow_long_test_names && length(long_lines) > 0L) {
      in_test_name_idx <- is_in_long_test_name(source_expression, length, long_lines)
      long_lines <- long_lines[!in_test_name_idx]
    }

    Map(
      function(long_line, line_length) {
        Lint(
          filename = source_expression$filename,
          line_number = long_line,
          column_number = length + 1L,
          type = "style",
          message = paste(general_msg, "This line is", line_length, "characters."),
          line = source_expression$file_lines[long_line],
          ranges = list(c(1L, line_length))
        )
      },
      long_lines,
      line_lengths[long_lines]
    )
  })
}

is_in_long_test_name <- function(source_expression, max_length, long_idx) {
  test_calls <- source_expression$xml_find_function_calls("test_that")
  if (length(test_calls) == 0L) {
    return(rep(FALSE, length(long_idx)))
  }
  desc_xpath <- "
    following-sibling::expr[
      STR_CONST
      and not(parent::expr/expr[1]/SYMBOL_PACKAGE[text() != 'testthat'])
      and (
        (
          position() = 1
          and not(preceding-sibling::SYMBOL_SUB[text() != 'desc'] or following-sibling::SYMBOL_SUB[text() = 'desc'])
          and not(
            following-sibling::expr[1]/*[not(self::OP-LEFT-BRACE or self::COMMENT)][1]/@line1 = STR_CONST/@line2
          )
        ) or (
          position() = 2
          and preceding-sibling::SYMBOL_SUB[
            (following-sibling::OP-COMMA and text() = 'code')
            or (preceding-sibling::OP-COMMA and text() = 'desc')
          ]
          and not(
            preceding-sibling::expr[1]/*[not(self::OP-RIGHT-BRACE or self::COMMENT)][last()]/@line2 = STR_CONST/@line1
          )
        )
      )
    ]
  "
  desc_nodes <- xml_find_all_(test_calls, desc_xpath)
  if (length(desc_nodes) == 0L) {
    return(rep(FALSE, length(long_idx)))
  }
  end_col_xpath <- "
    number(
      (
        STR_CONST
        | following-sibling::*[
          not(self::COMMENT or self::expr)
          and @line2 = preceding-sibling::expr[1][STR_CONST]/@line2
        ]
        | following-sibling::expr[1]/OP-LEFT-BRACE[@line2 = parent::expr/preceding-sibling::expr[1]/@line2]
      )[last()]/@col2
    )
  "
  line1 <- as.integer(xml_attr_(desc_nodes, "line1"))
  line2 <- as.integer(xml_attr_(desc_nodes, "line2"))
  line2_end_col <- as.integer(xml_find_num_(desc_nodes, end_col_xpath))
  line2[line2_end_col <= max_length] <- line2[line2_end_col <= max_length] - 1L
  vapply(
    long_idx,
    \(line) any(line1 <= line & line2 >= line),
    logical(1L)
  )
}

is_in_alignment_call <- function(parse_data, long_idx, allow_alignment_calls) {
  call_idx <- parse_data$token == "SYMBOL_FUNCTION_CALL" &
    parse_data$text %in% allow_alignment_calls
  if (!any(call_idx)) {
    return(rep(FALSE, length(long_idx)))
  }
  fn_expr_ids <- parse_data$parent[call_idx]
  call_expr_ids <- parse_data$parent[match(fn_expr_ids, parse_data$id)]
  call_data <- parse_data[match(call_expr_ids, parse_data$id), , drop = FALSE]
  vapply(
    long_idx,
    \(line) any(call_data$line1 <= line & call_data$line2 >= line),
    logical(1L)
  )
}

is_in_string_body <- function(parse_data, max_length, long_idx) {
  str_idx <- parse_data$token == "STR_CONST"
  if (!any(str_idx)) {
    return(rep(FALSE, length(long_idx)))
  }
  str_data <- parse_data[str_idx, ]
  if (all(str_data$line1 == str_data$line2)) {
    return(rep(FALSE, length(long_idx)))
  }
  # right delimiter just ends at 'col2', but 'col1' takes some sleuthing
  str_data$line1_width <- nchar(vapply(
    strsplit(str_data$text, "\n", fixed = TRUE),
    \(x) x[1L],
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  ))
  str_data$col1_end <- str_data$col1 + str_data$line1_width
  vapply(
    long_idx,
    function(line) {
      # strictly inside a multi-line string body
      if (any(str_data$line1 < line & str_data$line2 > line)) {
        return(TRUE)
      }
      on_line1_idx <- str_data$line1 == line
      if (any(on_line1_idx)) {
        return(max(str_data$col1_end[on_line1_idx]) <= max_length)
      }
      # use parse data to capture possible trailing expressions on this line
      on_line2_idx <- parse_data$line2 == line
      any(on_line2_idx) && max(parse_data$col2[on_line2_idx]) <= max_length
    },
    logical(1L)
  )
}
