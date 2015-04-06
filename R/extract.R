# content is the file content from readLines
extract_r_source <- function(content, start_re, end_re) {
  starts <- grep(start_re, content, perl = TRUE)
  ends <- grep(end_re, content, perl = TRUE)

  if (length(starts) != length(ends)) {
    stop("Malformed file!", call. = FALSE)
  }

  str(max(ends))
  # there is no need to worry about the content after the last chunk end
  output <- rep.int(NA_character_, max(ends - 1))
  Map(
    function(start, end) {
      if (

        # block contains at least one line of code
        start + 1 < end &&

        # block does not set an engine (so is r code)
        !rex::re_matches(content[start],
          rex::rex(boundary, "engine", any_spaces, "="))) {
        output[seq(start + 1, end - 1)] <<- content[seq(start + 1, end - 1)]
      }
    },
    starts, ends)
  output
}

get_knitr_pattern <- function(source_file) {
  pattern <- knitr:::detect_pattern(source_file$lines, tolower(knitr:::file_ext(source_file$filename)))
  if (!is.null(pattern)) {
    knitr::all_patterns[[pattern]]
  } else {
    NULL
  }
}
