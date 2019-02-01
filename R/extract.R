# content is the file content from readLines
extract_r_source <- function(filename, lines) {

  pattern <- get_knitr_pattern(filename, lines)
  if (is.null(pattern$chunk.begin) || is.null(pattern$chunk.end)) {
    return(lines)
  }

  starts <- grep(pattern$chunk.begin, lines, perl = TRUE)
  ends <- filter_chunk_end_positions(
    starts = starts,
    ends = grep(pattern$chunk.end, lines, perl = TRUE)
  )

  # no chunks found, so just return the lines
  if (length(starts) == 0 || length(ends) == 0) {
    return(character(0))
  }

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
        !defines_knitr_engine(lines[start])
      ) {
        output[seq(start + 1, end - 1)] <<- lines[seq(start + 1, end - 1)]
      }
    },
    starts, ends)
  replace_prefix(output, pattern$chunk.code)
}

get_knitr_pattern <- function(filename, lines) {
  pattern <- ("knitr" %:::% "detect_pattern")(lines, tolower( ("knitr" %:::% "file_ext")(filename)))
  if (!is.null(pattern)) {
    knitr::all_patterns[[pattern]]
  } else {
    NULL
  }
}

filter_chunk_end_positions <- function(starts, ends) {
  # In a valid file, possibly with plain-code-blocks,
  # - there should be at least as many ends as starts,
  #   and there should be an even-number of extra ends (possibly zero)
  #   since each plain-code-block should open & close, and the open/close
  #   tags of a plain-code-block both match the chunk.end pattern

  # starts (1, 3, 5, 7,        11)  --> (1, 3, 5, 7, 11)
  # ends   (2, 4, 6, 8, 9, 10, 12)  --> (2, 4, 6, 8, 12) # return this
  length_difference <- length(ends) - length(starts)

  if(length_difference < 0 | length_difference %% 2 != 0) {
    stop("Malformed file!", call. = FALSE)
  }
  if (length_difference == 0 && all(ends > starts)) {
    return(ends)
  }

  positions <- sort(c(starts = starts, ends = ends))
  code_start_indexes <- grep("starts", names(positions))
  code_ends <- positions[1 + code_start_indexes]

  stopifnot(all(grepl("ends", names(code_ends))))
  code_ends
}

defines_knitr_engine <- function(line) {
  engines <- names(knitr::knit_engines$get())

  # {some_engine}, {some_engine label, ...} or {some_engine, ...}
  bare_engine_pattern <- rex::rex(
    "{", or(engines), one_of("}", " ", ",")
  )
  # {... engine = "some_engine" ...}
  explicit_engine_pattern <- rex::rex(
    boundary, "engine", any_spaces, "="
  )

  rex::re_matches(line, explicit_engine_pattern) ||
  rex::re_matches(line, bare_engine_pattern)
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
