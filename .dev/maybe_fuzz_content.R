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

  fuzz_contents(new_file)

  new_file
}

# we could also consider just passing any test where no fuzzing takes place,
#   i.e. letting the other GHA handle whether unfuzzed tests pass as expected.
fuzz_contents <- function(f) {
  # skip errors for e.g. Rmd files, and ignore warnings.
  #   We could use get_source_expressions(), but with little benefit & much slower.
  pd <- tryCatch(getParseData(suppressWarnings(parse(f, keep.source = TRUE))), error = identity)
  if (inherits(pd, "error")) {
    return(invisible())
  }

  fun_tokens <- c(`'\\\\'` = "\\", `FUNCTION` = "function")
  fun_idx <- which(pd$token %in% names(fun_tokens))
  n_fun <- length(fun_idx)

  if (n_fun == 0L) {
    return(invisible())
  }

  pd$new_text <- NA_character_
  pd$new_text[fun_idx] <- sample(fun_tokens, n_fun, replace = TRUE)

  l <- readLines(f)

  for (ii in rev(fun_idx)) {
    if (pd$text[ii] == pd$new_text[ii]) next
    # Tried, with all rex(), hit a bug: https://github.com/r-lib/rex/issues/96
    ptn = paste0("^(.{", pd$col1[ii] - 1L, "})", rex::rex(pd$text[ii]))
    l[pd$line1[ii]] <- rex::re_substitutes(l[pd$line1[ii]], ptn, paste0("\\1", rex::rex(pd$new_text[ii])))
  }

  writeLines(l, f)

  invisible()
}
