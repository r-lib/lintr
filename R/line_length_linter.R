#' Line length linter
#'
#' Check that the line length of both comments and code is less than `length`.
#'
#' @param length Maximum line length allowed. Default is `80L` (Hollerith limit).
#' @param ignore_string_bodies Logical, default `FALSE`. If `TRUE`, the contents
#'   of string literals are ignored. The quotes themselves are included, so this
#'   mainly affects wide multiline strings, e.g. SQL queries.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = strrep("x", 23L),
#'   linters = line_length_linter(length = 20L)
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
#' # okay
#' lint(
#'   text = strrep("x", 21L),
#'   linters = line_length_linter(length = 40L)
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
#' @evalRd rd_tags("line_length_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#long-lines>
#' @export
line_length_linter <- function(length = 80L, ignore_string_bodies = FALSE) {
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
    function(x) x[1L],
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
