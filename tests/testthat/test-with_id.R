test_that("with_id works as expected", {
  source_expression <- get_source_expressions("tmp.R", "a <- 42L")$expressions[[1L]]
  ref <- with_id(
    source_expression = source_expression,
    ids_with_token(source_expression = source_expression, value = "expr")
  )
  expect_equal(ref, source_expression$parsed_content[c(1L, 3L, 6L), ])
  expect_equal(ref$token, rep_len("expr", nrow(ref)))

  # deprecated argument
  expect_warning(
    old_arg <- with_id(
      source_file = source_expression,
      id = ids_with_token(source_expression = source_expression, value = "expr")
    ),
    "Argument source_file was deprecated"
  )
  expect_equal(old_arg, ref)
})
