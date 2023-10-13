test_that("`sarif_output` produces expected error", {
  skip_if_not_installed("jsonlite")

  l <- lint(text = "x = 1", linters = assignment_linter())
  expect_error(sarif_output(l), "Package path needs to be a relative path", fixed = TRUE)
})

test_that("`sarif_output` writes expected files", {
  skip_if_not_installed("jsonlite")

  l <- lint_package(
    test_path("dummy_packages", "missing_dep"),
    linters = object_length_linter(),
    parse_settings = FALSE
  )

  withr::with_tempdir({
    sarif_output(l)
    expect_true(file.exists("lintr_results.sarif"))
  })

  withr::with_tempdir({
    sarif_output(l, filename = "myfile.sarif")
    expect_true(file.exists("myfile.sarif"))
  })
})

test_that("`sarif_output` produces valid files", {
  skip_if_not_installed("jsonlite")

  l <- lint_package(
    test_path("dummy_packages", "clean"),
    linters = default_linters,
    parse_settings = FALSE
  )

  withr::with_tempdir({
    sarif <- sarif_output(l)
    sarif <- jsonlite::fromJSON(
      "lintr_results.sarif",
      simplifyVector = TRUE,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    )

    expect_false(is.null(sarif$runs))
    expect_false(is.null(sarif$runs[[1L]]$results))
  })
})
