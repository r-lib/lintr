# content is the file content from readLines
extract_r_source <- function(filename, lines, error = identity) {
  pattern <- get_knitr_pattern(filename, lines)
  if (is.null(pattern$chunk.begin) || is.null(pattern$chunk.end)) {
    return(lines)
  }

  # mask non-source lines by NA, but keep total line count identical so the line number for EOF is correct, see #1400
  output_env <- new.env(parent = emptyenv())
  output_env$output <- rep.int(NA_character_, length(lines))

  chunks <- tryCatch(get_chunk_positions(pattern = pattern, lines = lines), error = error)
  if (is_error(chunks) || is_lint(chunks)) {
    assign("e", chunks, envir = parent.frame())
    # error, so return empty code
    return(output_env$output)
  }

  # no chunks found, so just return the lines
  if (length(chunks[["starts"]]) == 0L || length(chunks[["ends"]]) == 0L) {
    return(output_env$output)
  }

  Map(
    function(start, end, indent) {
      line_seq <- seq(start + 1L, end - 1L)
      chunk_code <- lines[line_seq]
      output_env$output[line_seq] <- if (indent > 0L) substr(chunk_code, indent + 1L, nchar(chunk_code)) else chunk_code
    },
    chunks[["starts"]],
    chunks[["ends"]],
    chunks[["indents"]]
  )
  # drop <<chunk>> references, too
  is.na(output_env$output) <- grep(pattern$ref.chunk, output_env$output)
  replace_prefix(output_env$output, pattern$chunk.code)
}

get_knitr_pattern <- function(filename, lines) {
  # Early return if the source code is parseable as plain R code.
  # Otherwise, R code containing a line which matches any knitr pattern will be treated as a knitr file.
  # See #1406 for details.
  if (parsable(lines)) {
    return(NULL)
  }
  # suppressWarnings for #1920. TODO(michaelchirico): this is a bit sloppy -- we ignore
  #   warnings here because encoding issues are caught later and that code path handles them
  #   correctly by converting to a lint. It would require some refactoring to get that
  #   right here as well, but it would avoid the duplication.
  pattern <- withCallingHandlers(
    ("knitr" %:::% "detect_pattern")(lines, tolower(("knitr" %:::% "file_ext")(filename))),
    warning = function(cond) {
      if (!grepl("invalid UTF-8", conditionMessage(cond), fixed = TRUE)) {
        cli_warn(cond) # nocov. No known way to reach here.
      }
      invokeRestart("muffleWarning")
    }
  )
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

  starts <- starts[keep]
  ends <- ends[keep]

  # Check indent on all lines in the chunk to allow for staggered indentation within a chunk;
  #   set the initial column to the leftmost one within each chunk (including the start+end gates). See tests.
  # use 'ws_re' to make clear that we're matching knitr's definition of initial whitespace.
  ws_re <- sub("```.*", "", pattern$chunk.begin)
  extract_min_chunk_indent <- function(start, end) {
    indents <- attr(regexpr(ws_re, lines[start:end], perl = TRUE), "match.length")
    min(indents)
  }
  # NB: min() guarantees length(indents) == length(starts)
  indents <- unlist(Map(extract_min_chunk_indent, starts, ends))
  list(starts = starts, ends = ends, indents = indents)
}

filter_chunk_start_positions <- function(starts, lines) {
  # keep blocks that don't set a knitr engine (and so contain evaluated R code)
  drop_idx <- defines_knitr_engine(lines[starts])
  starts[!drop_idx]
}

filter_chunk_end_positions <- function(starts, ends) {
  # In a valid file, possibly with plain-code-blocks,
  # - there should be at least as many ends as starts
  # In Rmarkdown and Quarto, unevaluated blocks may open & close with the same ``` pattern
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
  if (length(bad_end_indexes) > 0L) {
    bad_start_positions <- positions[code_start_indexes[bad_end_indexes]]
    # This error message is formatted like a parse error; don't use {cli}
    stop(sprintf( # nolint: undesirable_function_call_linter.
      "<rmd>:%1$d:1: Missing chunk end for chunk (maybe starting at line %1$d).\n",
      bad_start_positions[1L]
    ), call. = FALSE)
  }

  code_ends
}

defines_knitr_engine <- function(start_lines) {
  # Other packages defining custom engines should have them loaded and thus visible
  #   via knitr_engines$get() below. It seems the simplest way to accomplish this is
  #   for those packages to set some code in their .onLoad() hook, but that's not
  #   always done (nor quite recommended as a "best practice" by knitr).
  #   See the discussion on #1552.
  # TODO(#1617): explore running loadNamespace() automatically.
  engines <- names(knitr::knit_engines$get())

  # {some_engine}, {some_engine label, ...} or {some_engine, ...}
  bare_engine_pattern <- rex(
    "{", or(engines), one_of("}", " ", ",")
  )
  # {... engine = "some_engine" ...}
  explicit_engine_pattern <- rex(
    boundary, "engine", any_spaces, "="
  )

  re_matches(start_lines, explicit_engine_pattern) |
    re_matches(start_lines, bare_engine_pattern)
}

replace_prefix <- function(lines, prefix_pattern) {
  if (is.null(prefix_pattern)) {
    return(lines)
  }

  m <- gregexpr(prefix_pattern, lines)
  non_na <- !is.na(m)

  prefix_lengths <- lapply(regmatches(lines[non_na], m[non_na]), nchar)
  regmatches(lines[non_na], m[non_na]) <- lapply(prefix_lengths, strrep, x = " ")

  lines
}
