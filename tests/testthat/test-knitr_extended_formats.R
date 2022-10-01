test_that("marginfigure engine from tufte package doesn't cause problems", {
  expect_lint(
    file = "knitr_extended_formats/tufte.Rmd",
    checks = list(rex("Use <-, not =, for assignment."), line_number = 11L),
    default_linters,
    parse_settings = FALSE
  )
})

test_that("engines from bookdown package cause no problems", {
  skip_if_not_installed("bookdown")
  loadNamespace("bookdown") # to register additional engines

  expect_lint(
    file = "knitr_extended_formats/bookdown.Rmd",
    checks = list(rex("Use <-, not =, for assignment."), line_number = 14L),
    default_linters,
    parse_settings = FALSE
  )
})
