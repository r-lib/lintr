test_that("linters", {
  # non-empty named list of functions
  x <- default_linters
  expect_is(x, "list")
  expect_gt(length(x), 0L)
  expect_true(all(names(x) != ""))
  expect_true(all(vapply(x, inherits, logical(1L), "linter")))
  expect_true(all(vapply(x, is.function, logical(1L))))
})

test_that("undesirable functions and operators", {
  # non-empty named list of NAs and character strings
  vars <- list(all_undesirable_functions, default_undesirable_functions,
               all_undesirable_operators, default_undesirable_operators)

  for (x in vars) {
    expect_is(x, "list")
    expect_gt(length(x), 0L)
    expect_true(all(names(x) != ""))
    expect_true(all(vapply(x, function(x) is.na(x) || is.character(x), logical(1L))))
    expect_true(all(vapply(x, length, integer(1L)) == 1L))
  }
})

test_that("settings", {
  # non-empty named list
  x <- default_settings
  expect_is(x, "list")
  expect_gt(length(x), 0L)
  expect_true(all(names(x) != ""))
})

test_that("linter_names", {
  # If they throw a Lint() call, the name of the caught linter should match
  # the name of the linting function
  test_file <- "default_linter_testcode.R"
  x <- default_linters
  for (linter_name in names(x)) {
    lints <- lint(test_file, linters = x[linter_name], parse_settings = FALSE)
    lint_df <- as.data.frame(lints)
    expect_true(
      nrow(lint_df) > 0,
      info = paste(
        "each default linter should throw a lint on",
        "'default_linter_testcode.R'"
      )
    )
    expect_equal(
      lint_df[["linter"]][1], linter_name,
      info = paste(
        "the 'linter' name reported by lint() / Lint() should match the",
        "name of the corresponding linting function"
      )
    )
  }
})
