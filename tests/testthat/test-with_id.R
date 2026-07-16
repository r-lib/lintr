test_that("with_id works as expected", {
  source_expression <- get_source_expressions("tmp.R", "a <- 42L")$expressions[[1L]]
  ref <- with_id(
    source_expression = source_expression,
    ids_with_token(source_expression = source_expression, value = "expr")
  )
  expect_identical(ref, source_expression$parsed_content[c(1L, 3L, 6L), ])
  expect_identical(ref$token, rep_len("expr", nrow(ref)))
})

test_that("with_id returns empty data frame when source_expression is not at expression level", {
  expect_identical(with_id(list(full_parsed_content = 1L), id = 1L), data.frame())
})
