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
  # skip errors for e.g. Rmd files, and ignore warnings.
  #   We could use get_source_expressions(), but with little benefit & much slower.
  pd <- tryCatch(getParseData(suppressWarnings(parse(f, keep.source = TRUE))), error = identity)
  if (inherits(pd, "error")) {
    return(invisible())
  }

  reparse <- FALSE
  lines <- readLines(f)
  for (fuzzer in list(function_lambda_fuzzer, pipe_fuzzer)) {
    if (reparse) {
      pd <- getParseData(parse(f, keep.source = TRUE))
      lines <- readLines(f)
    }
    updated_lines <- fuzzer(pd, lines)
    reparse <- !is.null(updated_lines)
    if (!reparse) next # skip some I/O if we can
    writeLines(updated_lines, f)
  }

  invisible()
}
