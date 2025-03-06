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

testthat::test_dir("tests")
