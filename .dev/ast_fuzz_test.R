# Fuzz testing for lint consistency
#
# We have often encountered issues where we handle
#   equivalent R constructs inconsistently, e.g.,
#   function(...) should almost always match the same
#   rules as \(...), and '<-' assignment should almost
#   always be equivalent to '='.
#
# Here, we seek to enforce that (under eventual consistency)
#   by randomly altering the contents of files encountered
#   under expect_lint() to swap known equivalencies.

library(xml2)

expect_lint_file <- "R/expect_lint.R"

original <- readLines(expect_lint_file)
expected_line <- "file <- maybe_write_content(file, content)"
expected_line_idx <- grep(expected_line, original, fixed = TRUE)
if (length(expected_line_idx) != 1L) {
  stop(sprintf(
    "Please update this workflow -- need exactly one hit for line '%s' in file '%s'.",
    expected_line, expect_lint_file
  ))
}
writeLines(
  c(
    head(original, expected_line_idx-1L),
    # overwrite original exit hook to always delete the fuzzed file
    "  on.exit({reset_lang(old_lang); unlink(file)})",
    "  file <- maybe_fuzz_content(file, content)",
    tail(original, -expected_line_idx),
    readLines(".dev/maybe_fuzz_content.R")
  ),
  expect_lint_file
)

# Ensure the fuzzed contents are always visible to facilitate backing out which fuzzed content is at issue
contents <- readLines(expect_lint_file)
wrong_number_def_idx <- grep('wrong_number_fmt <- "got %d lints instead of %d%s"', contents, fixed = TRUE)
wrong_number_use_idx <- grep("sprintf(wrong_number_fmt,", contents, fixed = TRUE)
if (
  length(wrong_number_def_idx) != 1L ||
    length(wrong_number_use_idx) == 0L ||
    # these lines should be self-contained, have no comments, no nested calls
    !all(grepl("[^)][)]$", contents[wrong_number_use_idx])) ||
    inherits(tryCatch(parse(text = contents[wrong_number_use_idx]), error = identity), "error")
) {
  stop(sprintf(
    "Please update this workflow -- need wrong_number_fmt to be easily replaced in file '%s'.",
    expect_lint_file
  ))
}

contents[wrong_number_def_idx] <-
  '  wrong_number_fmt <- "got %d lints instead of %d%s\\nFile contents:\\n%s"'
contents[wrong_number_use_idx] <-
  gsub("\\)$", ", readChar(file, file.size(file)))", contents[wrong_number_use_idx])
writeLines(contents, expect_lint_file)

# Not useful in CI but good when running locally.
withr::defer({
  writeLines(original, expect_lint_file)
  suppressMessages(pkgload::load_all())
})

suppressMessages(pkgload::load_all())

can_parse <- \(lines) !inherits(tryCatch(parse(text = lines), error = identity), "error")
get_str <- \(x) tail(unlist(strsplit(x, ": ", fixed = TRUE)), 1L)

# beware lazy eval: originally tried adding a withr::defer() in each iteration, but
#   this effectively only runs the last 'defer' expression as the names are only
#   evaluated at run-time. So instead keep track of all edits in this object.
# these have to be enabled/disabled at runtime as it's not possible to disentagle which
#   fuzzer caused the error ex-post (and it might be the interaction of >1 at issue).
#   an earlier approach was like the current 'nofuzz' -- just comment out the troublesome
#   tests from being run at all. But that led to a very quickly growing set of tests being
#   skipped totally, which also hid some issues that are surfaced by the current approach.
#   Another idea would be to just leave the enable/disable calls as code in the test suite,
#   but I prefer the current approach of leaving them as comments: (1) it's more consistent
#   with the 'nolint' exclusion system and (2) it doesn't distract the casual reader as much.
test_restorations <- list()
for (test_file in list.files("tests/testthat", pattern = "^test-", full.names = TRUE)) {
  test_lines <- readLines(test_file)
  one_expr_idx <- grep("# nofuzz", test_lines, fixed = TRUE)
  range_start_idx <- grep("^\\s*# fuzzer disable:", test_lines)
  if (length(one_expr_idx) == 0L && length(range_start_idx) == 0L) next

  test_original <- test_lines
  pd <- getParseData(parse(test_file, keep.source = TRUE))

  for (start_line in rev(one_expr_idx)) {
    end_line <- start_line
    while (end_line <= length(test_lines) && !can_parse(test_lines[start_line:end_line])) {
      end_line <- end_line + 1L
    }
    if (end_line > length(test_lines)) {
      stop("Unable to parse any expression starting from line ", start_line)
    }
    comment_txt <- subset(pd, line1 == start_line & token == "COMMENT", select = "text", drop = TRUE)
    # blanket disable means the test cannot be run. this happens e.g. for tests of encoding
    #   that are too complicated to deal with in this GHA.
    if (comment_txt == "# nofuzz") {
      test_lines[start_line:end_line] <- ""
    } else {
      deactivated <- get_str(comment_txt)
      test_lines <- c(
        head(test_lines, start_line - 1L),
        sprintf("deactivate_fuzzers('%s')", deactivated),
        test_lines[start_line:end_line],
        sprintf("activate_fuzzers('%s')", deactivated),
        tail(test_lines, -end_line)
      )
    }
  }

  if (length(one_expr_idx)) {
    writeLines(test_lines, test_file)
    pd <- getParseData(parse(test_file, keep.source = TRUE))
    range_start_idx <- grep("^\\s*# fuzzer disable:", test_lines)
  }

  range_end_idx <- grep("^\\s*# fuzzer enable:", test_lines)

  if (length(range_start_idx) != length(range_end_idx) || any(range_end_idx < range_start_idx)) {
    stop(sprintf(
      "Mismatched '# fuzzer disable' (%s), '# fuzzer enable' (%s) in %s",
      toString(range_start_idx), toString(range_end_idx), test_file
    ))
  }

  for (ii in seq_along(range_start_idx)) {
    start_line <- test_lines[range_start_idx[ii]]
    test_lines[range_start_idx[ii]] <-
      gsub("#.*", sprintf("deactivate_fuzzers('%s')", get_str(start_line)), start_line)
    end_line <- test_lines[range_end_idx[ii]]
    test_lines[range_end_idx[ii]] <-
      gsub("#.*", sprintf("activate_fuzzers('%s')", get_str(end_line)), end_line)
  }

  if (length(range_start_idx)) writeLines(test_lines, test_file)

  test_restorations <- c(test_restorations, list(list(file = test_file, lines = test_original)))
}
withr::defer(for (restoration in test_restorations) writeLines(restoration$lines, restoration$file))

# ListReporter pretty essential here -- many other reporters fail to record the information we want,
#   and/or are overly verbose & reporting all the known-false-positive failures, unsuppressably.
reporter <- testthat::ListReporter$new()
testthat::test_local(reporter = reporter, stop_on_failure = FALSE)

all_classes <- unlist(lapply(
  reporter$get_results(),
  \(test) lapply(test$results, \(x) class(x)[1L])
))
print(table(`Summary of test statuses:` = all_classes))

# ignore any test that failed for expected reasons, e.g. some known lint metadata changes
#   about line numbers or the contents of the line. this saves us having to pepper tons of
#   'nofuzz' comments throughout the suite, as well as getting around the difficulty of injecting
#   'expect_lint()' with new code to ignore these attributes (this latter we might explore later).
invalid_failures <- list()
for (test in reporter$get_results()) {
  current_file <- test$file
  for (res in test$results) {
    if (!inherits(res, "expectation_failure")) next

    # line_number is for the comment injection fuzzer, which adds newlines.
    if (grepl("(column_number|ranges|line|line_number) .* did not match", res$message)) next
    res$file <- current_file
    invalid_failures <- c(invalid_failures, list(res))
  }
}

if (length(invalid_failures) > 0L) {
  names(invalid_failures) <- vapply(
    invalid_failures,
    \(x) sprintf("%s:%s", x$file, x$test),
    character(1L)
  )
  cat(sprintf("%d fuzzed tests failed unexpectedly!\n", length(invalid_failures)))
  print(invalid_failures)
  stop("Fix any bugs, or use '# nofuzz'/'# fuzzer [dis|en]able' to mark false positives.")
}
