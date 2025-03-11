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

  apply_fuzzers(new_file, fuzzers = .fuzzers$active)

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

dollar_at_fuzzer <- simple_swap_fuzzer(
  \(pd) pd$token %in% c("'$'", "'@'"),
  replacements = c("$", "@")
)

assignment_fuzzer <- simple_swap_fuzzer(
  \(pd) (pd$token == "LEFT_ASSIGN" & pd$text == "<-") | pd$token == "EQ_ASSIGN",
  replacements = c("<-", "=")
)

comment_injection_fuzzer <- function(pd, lines) {
  # injecting comment before a call often structurally breaks parsing
  #   (SYMBOL_FUNCTION_CALL-->SYMBOL), so avoid
  terminal_token_idx <- which(pd$terminal & !pd$token %in% c("COMMENT", "SYMBOL_FUNCTION_CALL", "SLOT"))
  # formula is messy because it's very easy to break parsing, but not easy to exclude the right
  #   elements from the pd data.frame (easier with XPath ancestor axis). Just skip for now.
  if (any(pd$token == "'~'")) {
    return(invisible())
  }
  injection_count <- sample(0:length(terminal_token_idx), 1L)

  if (injection_count == 0L) {
    return(invisible())
  }

  terminal_token_idx <- sort(sample(terminal_token_idx, injection_count))

  for (ii in rev(terminal_token_idx)) {
    line <- lines[pd$line2[ii]]
    lines[pd$line2[ii]] <- paste0(
      substr(line, 1L, pd$col2[ii]),
      " # INJECTED COMMENT\n",
      substr(line, pd$col2[ii] + 1L, nchar(line))
    )
  }
  lines
}

# we could also consider just passing any test where no fuzzing takes place,
#   i.e. letting the other GHA handle whether unfuzzed tests pass as expected.
apply_fuzzers <- function(f, fuzzers) {
  pd <- error_or_parse_data(f)
  if (inherits(pd, "error")) {
    return(invisible())
  }

  unedited <- lines <- readLines(f)
  for (fuzzer in fuzzers) {
    updated_lines <- fuzzer(pd, lines)
    if (is.null(updated_lines)) next # skip some I/O if we can
    writeLines(updated_lines, f)
    # check if our attempted edit introduced some error
    pd <- error_or_parse_data(f)
    if (inherits(pd, "error")) {
      writeLines(unedited, f)
      return(invisible())
    }
    lines <- readLines(f)
  }

  invisible()
}

.fuzzers <- new.env()
.fuzzers$active <- list(
  function_lambda = function_lambda_fuzzer,
  pipe = pipe_fuzzer,
  dollar_at = dollar_at_fuzzer,
  comment_injection = comment_injection_fuzzer,
  assignment = assignment_fuzzer
)
.fuzzers$inactive <- list()

deactivate_fuzzers <- function(names_str) {
  req <- unlist(strsplit(names_str, " ", fixed = TRUE))
  if (!all(req %in% names(.fuzzers$active))) {
    stop(sprintf(
      "Invalid attempt to deactivate fuzzers: '%s'\n  Currently active fuzzers: %s\n  Currently inactive fuzzers: %s",
      names_str, toString(names(.fuzzers$active)), toString(names(.fuzzers$inactive))
    ))
  }
  .fuzzers$inactive[req] <- .fuzzers$active[req]
  .fuzzers$active[req] <- NULL
  invisible()
}

activate_fuzzers <- function(names_str) {
  req <- unlist(strsplit(names_str, " ", fixed = TRUE))
  if (!all(req %in% names(.fuzzers$inactive))) {
    stop(sprintf(
      "Invalid attempt to activate fuzzers: '%s'\n  Currently active fuzzers: %s\n  Currently inactive fuzzers: %s",
      names_str, toString(names(.fuzzers$active)), toString(names(.fuzzers$inactive))
    ))
  }
  .fuzzers$active[req] <- .fuzzers$inactive[req]
  .fuzzers$inactive[req] <- NULL
  invisible()
}
