test_that("xml_nodes_to_lints handles >1-line nodes correctly", {
  xml <- xml2::read_xml('<x line1="1" col1="1" line2="2" col2="10" start="8" end="25"></x>')
  source_expression <- list(filename = "test", lines = c(`1` = "abcdefg", `2` = "hijklmnopq"))
  expect_equal(
    xml_nodes_to_lints(xml, source_expression, "foo"),
    Lint(
      filename = "test",
      line_number = 1L, column_number = 1L,
      type = "style", message = "foo",
      line = source_expression$lines[1L],
      ranges = list(c(1L, 7L))
    )
  )
})
