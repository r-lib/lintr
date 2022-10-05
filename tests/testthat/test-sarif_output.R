test_that("`sarif_output` writes lints to JSON", {
  l <- lint_package(
    test_path("dummy_packages", "missing_dep"),
    linters = object_length_linter(),
    parse_settings = FALSE
  )
  attributes(l) <- list(path = "./missing_dep")

  withr::with_tempdir({
    sarif_output(l)
    expect_true(file.exists("lintr_results.sarif"))
  })

  withr::with_tempdir({
    sarif_output(l, filename = "myfile.sarif")
    expect_true(file.exists("myfile.sarif"))
  })
})
