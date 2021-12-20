# This script crawls the R source code from GitHub to create a list of possible parser errors with corresponding
# regexes.
# It is used by the error_linter to extract location information from know parser errors.
requireNamespace("rex")
requireNamespace("usethis")
requireNamespace("stringi")

parser_files <- c("src/main/gram.c", "src/main/character.c")

lines <- unlist(lapply(
  parser_files,
  function(f) readLines(paste0("https://raw.githubusercontent.com/wch/r-source/trunk/", f))
))
error_calls <- grep("error(_(", lines, fixed = TRUE, value = TRUE)
error_formats <- trimws(gsub("^.*error\\(_\\(\"(.+)\".+", "\\1", error_calls))
error_formats <- unique(error_formats)
error_formats <- stringi::stri_unescape_unicode(error_formats)

# Only errors with line information need handling
error_formats <- grep("line %d", error_formats, fixed = TRUE, value = TRUE)

# rex::rex() does all escaping
.parse_error_regexes <- unname(vapply(error_formats, function(fmt) rex::rex(start, fmt, end), character(1L)))

# Convert line %d to named capture group
.parse_error_regexes <- gsub("line %d", "line (?<line>[[:digit:]]+)", .parse_error_regexes, fixed = TRUE)

.parse_error_regexes <- gsub("%s", ".*?", .parse_error_regexes, fixed = TRUE)
.parse_error_regexes <- gsub("%6x", "[[:xdigit:]]{6}", .parse_error_regexes, fixed = TRUE)

# Add message text capture groups
.parse_error_regexes <- gsub(
  "^\\^(.*?)( on | at | \\\\\\()line",
  "^(?<msg_1>\\1)\\2line",
  .parse_error_regexes
)
.parse_error_regexes <- gsub(
  "(?s)(\\(\\?<line>\\[\\[:digit:\\]\\]\\+\\)(?:\\\\\\)| ))([^$]+?)\\$$",
  "\\1(?<msg_2>\\2)$",
  .parse_error_regexes,
  perl = TRUE
)

# Check that no format specifiers remain
stopifnot(!any(grepl("%", .parse_error_regexes)))

usethis::use_data(.parse_error_regexes, internal = TRUE, overwrite = TRUE)
