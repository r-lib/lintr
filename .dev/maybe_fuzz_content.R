maybe_fuzz_content <- function(file, lines) {
  new_file <- tempfile()
  if (is.null(file)) {
    con <- file(new_file, encoding = "UTF-8")
    writeLines(lines, con = con, sep = "\n")
    close(con)
  } else {
    file.copy(file, new_file, copy.mode = FALSE)
  }

  fuzz_contents(new_file)

  new_file
}

fuzz_contents <- function(f) {
  pd <- tryCatch(getParseData(parse(f, keep.source = TRUE)), error = identity)
  # e.g. Rmd files. We could use get_source_expressions(), but with little benefit & much slower.
  if (inherits(pd, "error")) {
    return(invisible())
  }

  fun_tokens <- c("'\\\\'", "FUNCTION")
  fun_idx <- which(pd$token %in% fun_tokens)
  n_fun <- length(fun_idx)

  if (n_fun == 0L) {
    return(invisible())
  }

  pd$new_token <- NA_character_
  pd$new_token[fun_idx] <- sample(fun_tokens, n_fun, replace = TRUE)

  l <- readLines(f)

  replacement_map <- c(FUNCTION = "\\", `'\\\\'` = "function")
  for (ii in rev(fun_idx)) {
    if (pd$token[ii] == pd$new_token[ii]) next
    ptn = rex::rex(
      start,
      capture(n_times(anything, pd$col1[ii] - 1L), name = "prefix"),
      pd$text[ii]
    )
    l[pd$line1[ii]] <- rex::re_substitutes(l[pd$line1[ii]], ptn, paste0("\\1", rex::rex(replacement_map[pd$token[ii]])))
  }

  writeLines(l, f)

  invisible()
}
