maybe_fuzz_content <- function(file, lines) {
  if (is.null(file)) {
    new_file <- tempfile()
    con <- file(new_file, encoding = "UTF-8")
    writeLines(lines, con = con, sep = "\n")
    close(con)
  } else {
    new_file <- tempfile(fileext = paste0(".", tools::file_ext(file)))
    file.copy(file, new_file, copy.mode = FALSE)
  }

  apply_fuzzers(new_file)

  new_file
}

# skip errors for e.g. Rmd files, and ignore warnings.
#   We could use get_source_expressions(), but with little benefit & much slower.
# also avoid over-use of 'nofuzz' induced by some incompatible swaps, e.g. not all '%>%' can be
#   swapped to '|>' (if '.' is used, or if RHS is not an allowed simple call)
error_or_parse_data <- function(f) {
  tryCatch(getParseData(suppressWarnings(parse(f, keep.source = TRUE))), error = identity)
}

simple_swap_fuzzer <- function(pd_filter, replacements) {
  function(pd, lines) {
    idx <- which(pd_filter(pd))
    n <- length(idx)

    if (n == 0L) {
      return(invisible())
    }

    pd$new_text <- NA_character_
    pd$new_text[idx] <- sample(replacements, n, replace = TRUE)

    for (ii in rev(idx)) {
      if (pd$text[ii] == pd$new_text[ii]) next
      # Tried, with all rex(), hit a bug: https://github.com/r-lib/rex/issues/96
      ptn = paste0("^(.{", pd$col1[ii] - 1L, "})", rex::rex(pd$text[ii]))
      lines[pd$line1[ii]] <- rex::re_substitutes(lines[pd$line1[ii]], ptn, paste0("\\1", rex::rex(pd$new_text[ii])))
    }
    lines
  }
}

function_lambda_fuzzer <- simple_swap_fuzzer(
  \(pd) pd$token %in% c("'\\\\'", "FUNCTION"),
  replacements = c("\\", "function")
)

pipe_fuzzer <- simple_swap_fuzzer(
  \(pd) (pd$token == "SPECIAL" & pd$text == "%>%") | pd$token == "PIPE",
  replacements = c("%>%", "|>")
)

# we could also consider just passing any test where no fuzzing takes place,
#   i.e. letting the other GHA handle whether unfuzzed tests pass as expected.
apply_fuzzers <- function(f) {
  pd <- error_or_parse_data(f)
  if (inherits(pd, "error")) {
    return(invisible())
  }

  unedited <- lines <- readLines(f)
  for (fuzzer in list(function_lambda_fuzzer, pipe_fuzzer)) {
    updated_lines <- fuzzer(pd, lines)
    if (is.null(updated_lines) || identical(unedited, updated_lines)) next # skip some I/O if we can
    writeLines(updated_lines, f)
    # check if our attempted edit introduced some error; skip applying this fuzzer only if so
    pd <- error_or_parse_data(f)
    if (inherits(pd, "error")) {
      writeLines(lines, f)
      next
    }
    lines <- readLines(f)
  }

  invisible()
}
