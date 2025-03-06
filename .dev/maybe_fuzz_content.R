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
  pd <- getParseData(parse(f, keep.source = TRUE))

  fun_tokens <- c("'\\\\'", "FUNCTION")
  fun_idx <- which(pd$token %in% fun_tokens)
  n_fun <- length(fun_idx)

  if (n_fun == 0L) {
    return(invisible())
  }

  pd$new_token[fun_idx] <- sample(fun_tokens, n_fun, replace = TRUE)

  l <- readLines(f)

  replacement_map <- c(FUNCTION = "       \\", `'\\\\'` = "function")
  for (ii in rev(fun_idx)) {
    if (pd$token[ii] == pd$new_token[ii]) next
    browser()
    ptn = rex::rex(
      start,
      capture(n_times(anything, pd$col1[ii] - 1L), name = "prefix"),
      pd$text[ii]
    )
    l[pd$line1[ii]] <- rex::re_substitutes(l[pd$line1[ii]], ptn, replacement_map[pd$token[ii]])
  }


  start <- pd$col1[fun_idx]
  substr(l[pd$line1[fun_idx]], start, start + nchar("function") - 1L) <- replacement_map[pd$token[fun_idx]]

  writeLines(l, f)

  invisible()
}
