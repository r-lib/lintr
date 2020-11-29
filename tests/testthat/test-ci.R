test_that("GitHub Actions functionality works", {
  # imitate being on GHA whether or not we are
  withr::with_envvar(c(GITHUB_ACTIONS = "true"), {
    writeLines("x <- 1:nrow(y)", tmp <- tempfile())
    on.exit(unlink(tmp))

    old = options(lintr.rstudio_source_markers = FALSE)
    on.exit(options(old), add = TRUE)

    l <- lint(tmp)
    expect_output(print(l), "::warning file", fixed = TRUE)
  })
})
