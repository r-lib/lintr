test_that("is_lint_level works", {
  pc <- list(parsed_content = 1L)
  expect_true(is_lint_level(pc, "expression"))
  expect_false(is_lint_level(pc, "file"))

  names(pc) <- "full_parsed_content"
  expect_true(is_lint_level(pc, "file"))
  expect_false(is_lint_level(pc, "expression"))
})
