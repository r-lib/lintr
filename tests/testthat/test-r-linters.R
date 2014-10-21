context("r-linter-single_quotes")
test_that("returns the correct linting", {
  expect_lint("blah", NULL, single_quotes_linter)

  expect_lint("\"blah\"", NULL, single_quotes_linter)

  expect_lint("\"'blah\"", NULL, single_quotes_linter)

  expect_lint("\"blah'\"", NULL, single_quotes_linter)

  expect_lint("\"blah'\"", NULL, single_quotes_linter)

  expect_lint("\"'blah'\"", NULL, single_quotes_linter)

  expect_lint("'blah'", rex("Only use double-quotes."), single_quotes_linter)

  expect_lint("fun('blah')", rex("Only use double-quotes."), single_quotes_linter)

  expect_lint("{'blah'}", rex("Only use double-quotes."), single_quotes_linter)
})

context("r-linter-assignment")
test_that("returns the correct linting", {
  expect_lint("blah", NULL, assignment_linter)

  expect_lint("blah <- 1", NULL, assignment_linter)

  expect_lint("blah<-1", NULL, assignment_linter)

  expect_lint("fun(blah=1)", NULL, assignment_linter)

  expect_lint("blah=1", rex("Use <-, not =, for assignment."), assignment_linter)

  expect_lint("blah = 1", rex("Use <-, not =, for assignment."), assignment_linter)

  expect_lint("blah = fun(1)", rex("Use <-, not =, for assignment."), assignment_linter)

  expect_lint("blah = fun(1) {",
    list(
      rex("Use <-, not =, for assignment."),
      c(type = "error", "unexpected")
      ),
      assignment_linter)

  expect_lint("fun((blah = fun(1)))", rex("Use <-, not =, for assignment."), assignment_linter)
})

context("r-linter-absolute_paths")
test_that("returns the correct linting", {
  expect_lint("blah", NULL, absolute_paths_linter)

  expect_lint("'blah'", NULL, absolute_paths_linter)

  expect_lint("'blah/file.txt'", NULL, absolute_paths_linter)

  expect_lint("'blah\\file.txt'", NULL, absolute_paths_linter)

  expect_lint("'../blah/file.txt'", NULL, absolute_paths_linter)

  expect_lint("'//blah/file.txt'", NULL, absolute_paths_linter)

  expect_lint("'~'", NULL, absolute_paths_linter)

  expect_lint("# 'C:/blah/file.txt'", NULL, absolute_paths_linter)

  expect_lint("'/blah/file.txt'", rex("Do not use absolute paths."), absolute_paths_linter)

  expect_lint("\"/blah/file.txt\"", rex("Do not use absolute paths."), absolute_paths_linter)

  expect_lint("'c:/blah/file.txt'", rex("Do not use absolute paths."), absolute_paths_linter)

  expect_lint("'C:/blah/file.txt'", rex("Do not use absolute paths."), absolute_paths_linter)

  expect_lint("'E:/blah/file.txt'", rex("Do not use absolute paths."), absolute_paths_linter)

  expect_lint("'E:\\blah\\file.txt'", rex("Do not use absolute paths."), absolute_paths_linter)

  expect_lint("'~/blah/file.txt'", rex("Do not use absolute paths."), absolute_paths_linter)

  expect_lint("'~james.hester/blah/file.txt'", rex("Do not use absolute paths."), absolute_paths_linter)

  expect_lint("'~/'", rex("Do not use absolute paths."), absolute_paths_linter)
})

context("r-linter-no_tabs")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("#\tblah", NULL, no_tab_linter)

  expect_lint("\tblah", rex("Use two spaces to indent, never tabs."), no_tab_linter)

  expect_lint("\t\tblah", rex("Use two spaces to indent, never tabs."), no_tab_linter)

})

context("r-linter-line_length")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, line_length_linter(80))

  expect_lint("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", NULL, line_length_linter(80))

  expect_lint("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    "lines should not be more than 80 characters" , line_length_linter(80))

  expect_lint(
    paste0("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n",
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),

    list(
      "lines should not be more than 80 characters",
      "lines should not be more than 80 characters"),

    line_length_linter(80))

  expect_lint("aaaaaaaaaaaaaaaaaaaa", NULL,
    line_length_linter(20))

  expect_lint("aaaaaaaaaaaaaaaaaaaab",
    "lines should not be more than 20 characters",
    line_length_linter(20))

})
