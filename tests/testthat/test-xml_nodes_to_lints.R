test_that("it creates basic lints", {
  code <- "before   %+%   after"
  tmpfile <- withr::local_tempfile(lines = code)
  expr <- get_source_expressions(tmpfile)$expressions[[2L]]
  xml <- expr$full_xml_parsed_content
  node <- xml2::xml_find_first(xml, "//SPECIAL")

  l <- xml_nodes_to_lints(
    xml = node,
    source_expression = expr,
    lint_message = "msg",
    type = "warning"
  )

  expect_s3_class(l, "lint")
  expect_identical(l$filename, tmpfile)
  expect_identical(l$line_number, 1L)
  expect_identical(l$column_number, as.integer(xml2::xml_attr(node, "col1")))
  expect_identical(l$type, "warning")
  expect_identical(l$message, "msg")
  expect_identical(l$line, code)
  expect_identical(l$ranges, list(as.integer(c(xml2::xml_attr(node, "col1"), xml2::xml_attr(node, "col2")))))

  # location XPaths work
  l_before_col <- xml_nodes_to_lints(
    xml = node,
    source_expression = expr,
    lint_message = "msg",
    column_number_xpath = "number(./preceding-sibling::*[1]/@col2 + 1)",
    range_start_xpath = "number(./preceding-sibling::*[1]/@col1)",
    range_end_xpath = "number(./following-sibling::*[1]/@col2)"
  )
  expect_identical(l_before_col$column_number, nchar("before") + 1L)
  expect_identical(l_before_col$ranges, list(c(1L, nchar(code))))

  # Vectorization works
  ll <- xml_nodes_to_lints(
    xml = xml2::xml_find_all(xml, "//expr"),
    source_expression = expr,
    lint_message = "msg"
  )
  expect_s3_class(ll, "lints")
  expect_length(ll, 3L)
  expect_s3_class(ll[[1L]], "lint")

  # Also for plain lists of xml_nodes, usually obtained by c(nodeset_a, nodeset_b)
  ll_unclassed <- xml_nodes_to_lints(
    xml = unclass(xml2::xml_find_all(xml, "//expr")),
    source_expression = expr,
    lint_message = "msg"
  )
  expect_s3_class(ll_unclassed, "lints")
  expect_length(ll_unclassed, 3L)
  expect_s3_class(ll_unclassed[[1L]], "lint")

  # lint_message as a vector
  ll_msgvec <- xml_nodes_to_lints(
    xml = xml2::xml_find_all(xml, "//SYMBOL"),
    source_expression = expr,
    lint_message = letters[1L:2L]
  )
  expect_identical(ll_msgvec[[1L]]$message, "a")
  expect_identical(ll_msgvec[[2L]]$message, "b")
})

test_that("it handles multi-line lints correctly", {
  code <- c("before %+%", "  after")
  tmpfile <- withr::local_tempfile(lines = code)
  expr <- get_source_expressions(tmpfile)$expressions[[2L]]
  xml <- expr$full_xml_parsed_content
  node <- xml2::xml_find_first(xml, "/exprlist/expr")

  l <- xml_nodes_to_lints(
    xml = node,
    source_expression = expr,
    lint_message = "msg"
  )

  expect_s3_class(l, "lint")
  expect_identical(l$filename, tmpfile)
  expect_identical(l$line_number, 1L)
  expect_identical(l$column_number, as.integer(xml2::xml_attr(node, "col1")))
  expect_identical(l$type, "style")
  expect_identical(l$message, "msg")
  expect_identical(l$line, code[[1L]])
  expect_identical(l$ranges, list(as.integer(c(xml2::xml_attr(node, "col1"), nchar(code[1L])))))
})
