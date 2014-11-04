context("assignment_linter")
test_that("returns the correct linting", {
  expect_lint("blah", NULL, assignment_linter)

  expect_lint("blah <- 1", NULL, assignment_linter)

  expect_lint("blah<-1", NULL, assignment_linter)

  expect_lint("fun(blah=1)", NULL, assignment_linter)

  expect_lint("blah=1",
    rex("Use <-, not =, for assignment."),
      assignment_linter)

    expect_lint("blah = 1",
      rex("Use <-, not =, for assignment."),
        assignment_linter)

  expect_lint("blah = fun(1)",
    rex("Use <-, not =, for assignment.")
    , assignment_linter)

  expect_lint("blah = fun(1) {",
    list(
      rex("Use <-, not =, for assignment."),
      c(type = "error", "unexpected")
      ),
      assignment_linter)

  expect_lint("fun((blah = fun(1)))",
    rex("Use <-, not =, for assignment."),
    assignment_linter)

  test_that("handles unicode characters correctly", {
    expect_lint("Ħ = 1",
      list(c(column_number = 3L)),
      assignment_linter)

    expect_lint("Ħ = 1;aèn = 2",
      list(c(column_number = 3L),
        c(column_number = 11L)),
      assignment_linter)

    })
})
