# content is the file content from readLines
extract_r_source <- function(filename, lines) {

  pattern <- get_knitr_pattern(filename, lines)
  if (is.null(pattern)) {
    return(lines)
  }

  starts <- grep(pattern$chunk.begin, lines, perl = TRUE)
  ends <- grep(pattern$chunk.end, lines, perl = TRUE)

  if (length(starts) != length(ends)) {
    stop("Malformed file!", call. = FALSE)
  }

  # there is no need to worry about the lines after the last chunk end
  output <- rep.int(NA_character_, max(ends - 1))
  Map(
    function(start, end) {
      if (

        # block contains at least one line of code
        start + 1 < end &&

        # block does not set an engine (so is r code)
        !rex::re_matches(lines[start],
          rex::rex(boundary, "engine", any_spaces, "="))) {
        output[seq(start + 1, end - 1)] <<- lines[seq(start + 1, end - 1)]
      }
    },
    starts, ends)
  output
  replace_prefix(output, pattern$chunk.code)
}

detect_pattern <- get("detect_pattern", asNamespace("knitr"))
file_ext <- get("file_ext", asNamespace("knitr"))

get_knitr_pattern <- function(filename, lines) {
  pattern <- detect_pattern(lines, tolower(file_ext(filename)))
  if (!is.null(pattern)) {
    knitr::all_patterns[[pattern]]
  } else {
    NULL
  }
}

replace_prefix <- function(lines, prefix_pattern) {
  if (is.null(prefix_pattern)) {
    return(lines)
  }

  m <- gregexpr(prefix_pattern, lines)
  non_na <- !is.na(m)

  blanks <- function(n) {
    vapply(Map(rep.int, rep.int(" ", length(n)), n, USE.NAMES = FALSE),
      paste, "", collapse = "")
  }

  regmatches(lines[non_na], m[non_na]) <-
    Map(blanks, lapply(regmatches(lines[non_na], m[non_na]), nchar))

  lines
}
