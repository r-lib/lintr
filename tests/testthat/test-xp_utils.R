test_that("xp_call_name works", {
  xml_from_code <- function(str) {
    xml2::read_xml(xmlparsedata::xml_parse_data(parse(text = str, keep.source = TRUE)))
  }
  xml <- xml_from_code("sum(1:10)")
  expect_identical(xp_call_name(xml, depth = 2L), "sum")

  expect_identical(xp_call_name(xml2::xml_find_first(xml, "expr")), "sum")

  xml <- xml_from_code(c("sum(1:10)", "sd(1:10)"))
  expect_identical(xp_call_name(xml, depth = 2L, condition = "text() = 'sum'"), "sum")
})

test_that("xp_call_name input validation works", {
  expect_error(xp_call_name(2L), "`expr` must be an <xml_nodeset>", fixed = TRUE)

  xml <- xml2::read_xml("<a></a>")
  expect_error(xp_call_name(xml, depth = -1L), "depth >= 0", fixed = TRUE)
  expect_error(xp_call_name(xml, depth = "1"), "is.numeric(depth)", fixed = TRUE)
  expect_error(xp_call_name(xml, condition = 1L), "is.character(condition)", fixed = TRUE)
})
