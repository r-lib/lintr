test_that("ids_with_token works as expected", {
  source_expression <- get_source_expressions("tmp.R", "a <- 42L")$expressions[[1L]]
  ref <- ids_with_token(source_expression = source_expression, value = "expr")
  expect_identical(ref, c(1L, 3L, 6L))
  expect_identical(source_expression$parsed_content$token[ref], rep_len("expr", length(ref)))

  # deprecated argument
  expect_error(
    ids_with_token(source_file = source_expression, value = "expr"),
    "Argument source_file was deprecated"
  )
})
