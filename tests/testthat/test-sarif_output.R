test_that("`sarif_output` produces expected warning", {
  l <- lint(text = "x = 1", linters = assignment_linter())
  expect_error(sarif_output(l), "Package path needs to be a relative path")
})

test_that("`sarif_output` writes expected files", {
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
