test_that("ids_with_token works as expected", {
  source_expression <- get_source_expressions("tmp.R", "a <- 42L")$expressions[[1L]]
  ref <- ids_with_token(source_expression = source_expression, value = "expr")
  expect_identical(ref, c(1L, 3L, 6L))
  expect_identical(source_expression$parsed_content$token[ref], rep_len("expr", length(ref)))
})

test_that("ids_with_token returns empty integer vector when source_expression is not at expression level", {
  source_expressions <- get_source_expressions("tmp.R", "a <- 42L")$expressions
  file_level_expr <- source_expressions[[length(source_expressions)]]
  expect_identical(ids_with_token(file_level_expr, value = "expr"), integer())
})
