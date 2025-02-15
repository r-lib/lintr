test_that("marginfigure engine from tufte package doesn't cause problems", {
  skip_if_not_installed("tufte", minimum_version = "0.12.4") # for rstudio/tufte#117
  loadNamespace("tufte") # to register additional engines

  expect_lint(
    file = test_path("knitr_extended_formats", "tufte.Rmd"),
    checks = list(rex::rex("Use one of <-, <<- for assignment, not =."), line_number = 11L),
    default_linters,
    parse_settings = FALSE
  )
})

test_that("engines from bookdown package cause no problems", {
  skip_if_not_installed("bookdown")
  loadNamespace("bookdown") # to register additional engines

  expect_lint(
    file = test_path("knitr_extended_formats", "bookdown.Rmd"),
    checks = list(rex::rex("Use one of <-, <<- for assignment, not =."), line_number = 14L),
    default_linters,
    parse_settings = FALSE
  )
})
