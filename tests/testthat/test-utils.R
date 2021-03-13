# TODO: delete this test when we have any lints using xml_nodes_to_lint where the
#   target node can span >1 line (e.g. an <expr>); until then, this test is here
#   for coverage
test_that("xml_nodes_to_lint handles >1-line nodes correctly", {
  xml <- xml2::read_xml('<x line1="1" col1="1" line2="2" col2="10" start="8" end="25"></x>')
  source_file <- list(filename = "test", lines = c(`1` = "abcdefg", `2` = "hijklmnopq"))
  expect_equal(
    lintr:::xml_nodes_to_lint(xml, source_file, "foo"),
    Lint(
      filename = "test",
      line_number = 1L, column_number = 1L,
      type = "style", message = "foo",
      line = source_file$lines[1],
      ranges = list(c(1L, 7L))
    )
  )
})
