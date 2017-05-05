context("get_source_expression")

test_that("tab positions have been corrected", {
  f <- tempfile()

  writeLines("1\n\t", f)
  expect_error(lintr:::get_source_expressions(f), NA, label="empty parsed_content line")

  writeLines("TRUE", f)
  pc <- lintr:::get_source_expressions(f)[["expressions"]][[1L]][["parsed_content"]]
  expect_equivalent(pc[pc[["text"]] == "TRUE", c("col1", "col2")], c(1L, 4L))

  writeLines("\tTRUE", f)
  pc <- lintr:::get_source_expressions(f)[["expressions"]][[1L]][["parsed_content"]]
  expect_equivalent(pc[pc[["text"]] == "TRUE", c("col1", "col2")], c(2L, 5L))

  writeLines("\t\tTRUE", f)
  pc <- lintr:::get_source_expressions(f)[["expressions"]][[1L]][["parsed_content"]]
  expect_equivalent(pc[pc[["text"]] == "TRUE", c("col1", "col2")], c(3L, 6L))

  writeLines("n\t<=\tTRUE", f)
  pc <- lintr:::get_source_expressions(f)[["expressions"]][[1L]][["parsed_content"]]
  expect_equivalent(pc[pc[["text"]] == "n", c("col1", "col2")], c(1L, 1L))
  expect_equivalent(pc[pc[["text"]] == "<=", c("col1", "col2")], c(3L, 4L))
  expect_equivalent(pc[pc[["text"]] == "TRUE", c("col1", "col2")], c(6L, 9L))

  writeLines("\tfunction\t(x)\t{\tprint(pc[\t,1])\t;\t}", f)
  pc <- lintr:::get_source_expressions(f)[["expressions"]][[1L]][["parsed_content"]]
  expect_equivalent(pc[pc[["text"]] == "function", c("col1", "col2")], c(2L, 9L))
  expect_equivalent(pc[pc[["text"]] == "x", c("col1", "col2")], c(12L, 12L))
  expect_equivalent(pc[pc[["text"]] == "print", c("col1", "col2")], c(17L, 21L))
  expect_equivalent(pc[pc[["text"]] == ";", c("col1", "col2")], c(32L, 32L))
  expect_equivalent(pc[pc[["text"]] == "}", c("col1", "col2")], c(34L, 34L))

  writeLines("# test tab\n\ns <- 'I have \\t a dog'\nrep(\ts, \t3)", f)
  y <- lintr:::get_source_expressions(f)[["expressions"]]
  pc <- y[[2]][["parsed_content"]]
  expect_equivalent(pc[pc[["token"]] == "STR_CONST", c("line1", "col1", "col2")], c(3L, 6L, 22L))
  pc <- y[[3]][["parsed_content"]]
  expect_equivalent(pc[pc[["token"]] == "NUM_CONST", c("line1", "col1", "col2")], c(4L, 10L, 10L))

})
