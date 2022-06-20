# content is the file content from readLines
extract_r_source <- function(filename, lines, error = identity) {
  pattern <- get_knitr_pattern(filename, lines)
  if (is.null(pattern$chunk.begin) || is.null(pattern$chunk.end)) {
    return(lines)
  }

  # mask non-source lines by NA, but keep total line count identical so the line number for EOF is correct, see #1400
  output <- rep.int(NA_character_, length(lines))

  chunks <- tryCatch(get_chunk_positions(pattern = pattern, lines = lines), error = error)
  if (inherits(chunks, "error") || inherits(chunks, "lint")) {
    assign("e", chunks,  envir = parent.frame())
    # error, so return empty code
    return(output)
  }

  # no chunks found, so just return the lines
  if (length(chunks[["starts"]]) == 0L || length(chunks[["ends"]]) == 0L) {
    return(output)
  }

  Map(
    function(start, end) {
      output[seq(start + 1L, end - 1L)] <<- lines[seq(start + 1L, end - 1L)]
    },
    chunks[["starts"]],
    chunks[["ends"]]
  )
  replace_prefix(output, pattern$chunk.code)
}

get_knitr_pattern <- function(filename, lines) {
  # Early return if the source code is parseable as plain R code.
  # Otherwise, R code containing a line which matches any knitr pattern will be treated as a knitr file.
  # See #1406 for details.
  if (parsable(lines)) return(NULL)
  pattern <- ("knitr" %:::% "detect_pattern")(lines, tolower(("knitr" %:::% "file_ext")(filename)))
  if (!is.null(pattern)) {
    knitr::all_patterns[[pattern]]
  } else {
    NULL
  }
}

get_chunk_positions <- function(pattern, lines) {
  starts <- filter_chunk_start_positions(
    starts = grep(pattern$chunk.begin, lines, perl = TRUE),
    lines = lines
  )
  ends <- filter_chunk_end_positions(
    starts = starts,
    ends = grep(pattern$chunk.end, lines, perl = TRUE)
  )
  # only keep those blocks that contain at least one line of code
  keep <- which(ends - starts > 1L)

  list(starts = starts[keep], ends = ends[keep])
}

filter_chunk_start_positions <- function(starts, lines) {
  # keep blocks that don't set a knitr engine (and so contain evaluated R code)
  drop <- defines_knitr_engine(lines[starts])
  starts[!drop]
}

filter_chunk_end_positions <- function(starts, ends) {
  # In a valid file, possibly with plain-code-blocks,
  # - there should be at least as many ends as starts
  # In Rmarkdown, unevaluated blocks may open & close with the same ``` pattern
  # that defines the end-pattern for an evaluated block

  # This returns the first end-position that succeeds each start-position
  # starts (1, 3, 5, 7,        11)  --> (1, 3, 5, 7, 11)
  # ends   (2, 4, 6, 8, 9, 10, 12)  --> (2, 4, 6, 8, 12) # return this
  length_difference <- length(ends) - length(starts)
  if (length_difference == 0L && all(ends > starts)) {
    return(ends)
  }

  positions <- sort(c(starts = starts, ends = ends))
  code_start_indexes <- grep("starts", names(positions), fixed = TRUE)

  code_ends <- positions[pmin(1L + code_start_indexes, length(positions))]

  bad_end_indexes <- grep("starts", names(code_ends), fixed = TRUE)
  if (length(bad_end_indexes)) {
    bad_start_positions <- positions[code_start_indexes[bad_end_indexes]]
    # This error message is formatted like a parse error
    stop(sprintf(
      "<rmd>:%1$d:1: Missing chunk end for chunk (maybe starting at line %1$d).\n",
      bad_start_positions[1L]
    ), call. = FALSE)
  }

  code_ends
}

defines_knitr_engine <- function(start_lines) {
  engines <- names(knitr::knit_engines$get())

  # {some_engine}, {some_engine label, ...} or {some_engine, ...}
  bare_engine_pattern <- rex::rex(
    "{", or(engines), one_of("}", " ", ",")
  )
  # {... engine = "some_engine" ...}
  explicit_engine_pattern <- rex::rex(
    boundary, "engine", any_spaces, "="
  )

  rex::re_matches(start_lines, explicit_engine_pattern) |
    rex::re_matches(start_lines, bare_engine_pattern)
}

replace_prefix <- function(lines, prefix_pattern) {
  if (is.null(prefix_pattern)) {
    return(lines)
  }

  m <- gregexpr(prefix_pattern, lines)
  non_na <- !is.na(m)

  blanks <- function(n) {
    vapply(Map(rep.int, rep.int(" ", length(n)), n, USE.NAMES = FALSE),
      paste, "",
      collapse = ""
    )
  }

  regmatches(lines[non_na], m[non_na]) <-
    Map(blanks, lapply(regmatches(lines[non_na], m[non_na]), nchar))

  lines
}
