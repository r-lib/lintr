test_that("is_lint_level works", {
  pc <- list(parsed_content = 1)
  expect_true(is_lint_level(pc, "expression"))
  expect_false(is_lint_level(pc, "file"))

  names(pc) <- "full_parsed_content"
  expect_true(is_lint_level(pc, "file"))
  expect_false(is_lint_level(pc, "expression"))

  names(pc) <- "xml_parsed_content"
  expect_true(is_lint_level(pc, "expression", require_xml = TRUE))
  expect_false(is_lint_level(pc, "file", require_xml = TRUE))

  names(pc) <- "full_xml_parsed_content"
  expect_true(is_lint_level(pc, "file", require_xml = TRUE))
  expect_false(is_lint_level(pc, "expression", require_xml = TRUE))
})
