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
# Not useful in CI but good when running locally.
withr::defer({
  writeLines(original, expect_lint_file)
  pkgload::load_all()
})

pkgload::load_all()

# beware lazy eval: originally tried adding a withr::defer() in each iteration, but
#   this effectively only runs the last 'defer' expression as the names are only
#   evaluated at run-time. So instead keep track of all edits in this object.
# this approach to implementing 'nofuzz' feels painfully manual, but I couldn't
#   figure out how else to get 'testthat' to give us what we need -- the failures
#   object in the reporter is frustratingly inconsistent in whether the trace
#   exists, and even if it does, we'd have to text-mangle to get the corresponding
#   file names out. Also, the trace 'srcref' happens under keep.source=FALSE,
#   so we lose any associated comments anyway. even that would not solve the issue
#   of getting top-level exclusions done for 'nofuzz start|end' ranges, except
#   maybe if it enabled us to reuse lintr's own exclude() system.
# therefore we take this approach: pass over the test suite first and comment out
#   any tests/units that have been marked 'nofuzz'. restore later. one consequence
#   is there's no support for fuzzer-specific exclusion, e.g. we fully disable
#   the unnecessary_placeholder_linter() tests because |> and _ placeholders differ.
test_restorations <- list()
for (test_file in list.files("tests/testthat", pattern = "^test-", full.names = TRUE)) {
  xml <- read_xml(xmlparsedata::xml_parse_data(parse(test_file, keep.source = TRUE)))
  # parent::* to catch top-level comments (exprlist). matches one-line nofuzz and start/end ranges.
  nofuzz_lines <- xml_find_all(xml, "//COMMENT[contains(text(), 'nofuzz')]/parent::*")
  if (length(nofuzz_lines) == 0L) next

  test_original <- test_lines <- readLines(test_file)

  for (nofuzz_line in nofuzz_lines) {
    comments <- xml_find_all(nofuzz_line, "COMMENT[contains(text(), 'nofuzz')]")
    comment_text <- xml_text(comments)
    # handle start/end ranges first.
    start_idx <- grep("nofuzz start", comment_text, fixed = TRUE)
    end_idx <- grep("nofuzz end", comment_text, fixed = TRUE)
    if (length(start_idx) != length(end_idx) || any(end_idx < start_idx)) {
      stop(sprintf(
        "Mismatched '# nofuzz start' (%s), '# nofuzz end' (%s) in %s",
        toString(start_idx), toString(end_idx), test_file
      ))
    }

    comment_ranges <- Map(`:`,
      as.integer(xml_attr(comments[start_idx], "line1")),
      as.integer(xml_attr(comments[end_idx], "line1"))
    )
    for (comment_range in comment_ranges) {
      test_lines[comment_range] <- paste("#", test_lines[comment_range])
    }

    if (length(start_idx) > 0L && !any(!start_idx & !end_idx)) next

    # NB: one-line tests line expect_lint(...) # nofuzz are not supported,
    #   since the comment will attach to the parent test_that() & thus comment
    #   out the whole unit. Easiest solution is just to spread out those few tests for now.
    comment_range <- as.integer(xml_attr(nofuzz_line, "line1")):as.integer(xml_attr(nofuzz_line, "line2"))
    test_lines[comment_range] <- paste("#", test_lines[comment_range])
  }

  writeLines(test_lines, test_file)
  test_restorations <- c(test_restorations, list(list(file = test_file, lines = test_original)))
}
withr::defer(for (restoration in test_restorations) writeLines(restoration$lines, restoration$file))

# for some reason, 'report <- test_dir(...)' did not work -- the resulting object is ~empty.
#   even 'report <- test_local(...)', which does return an object, lacks any information about
#   which tests failed (all reports are about successful or skipped tests). probably this is not
#   the best approach but documentation was not very helpful.
reporter <- testthat::SummaryReporter$new()
testthat::test_local(reporter = reporter, stop_on_failure = FALSE)

failures <- reporter$failures$as_list()
# ignore any test that failed for expected reasons, e.g. some known lint metadata changes
#   about line numbers or the contents of the line. this saves us having to pepper tons of
#   'nofuzz' comments throughout the suite, as well as getting around the difficulty of injecting
#   'expect_lint()' with new code to ignore these attributes (this latter we might explore later).
valid_failure <- vapply(
  failures,
  function(failure) {
    if (grepl("(column_number|ranges|line) .* did not match", failure$message)) {
      return(TRUE)
    }
    FALSE
  },
  logical(1L)
)
if (!all(valid_failure)) {
  failures <- failures[!valid_failure]
  names(failures) <- vapply(failures, `[[`, "test", FUN.VALUE = character(1L))
  cat("Some fuzzed tests failed unexpectedly!\n")
  print(failures)
  stop("Use # nofuzz [start|end] to mark false positives or fix any bugs.")
}
