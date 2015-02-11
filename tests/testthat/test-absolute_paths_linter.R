context("absolute_paths_linter")
test_that("returns the correct linting", {
  expect_lint("blah", NULL, absolute_paths_linter)

  expect_lint("'blah'", NULL, absolute_paths_linter)

  expect_lint("'blah/file.txt'", NULL, absolute_paths_linter)

  expect_lint("'blah\\file.txt'", NULL, absolute_paths_linter)

  expect_lint("'../blah/file.txt'", NULL, absolute_paths_linter)

  expect_lint("'//blah/file.txt'", NULL, absolute_paths_linter)

  expect_lint("'~'", NULL, absolute_paths_linter)

  expect_lint("# 'C:/blah/file.txt'", NULL, absolute_paths_linter) # nolint

  expect_lint("'/blah/file.txt'", # nolint
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("\"/blah/file.txt\"", # nolint
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'c:/blah/file.txt'", # nolint
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'C:/blah/file.txt'", # nolint
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'E:/blah/file.txt'", # nolint
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'E:\\blah\\file.txt'", # nolint
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'~/blah/file.txt'", # nolint
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'~james.hester/blah/file.txt'", # nolint
    rex("Do not use absolute paths."),
    absolute_paths_linter)

  expect_lint("'~/'", # nolint
    rex("Do not use absolute paths."),
    absolute_paths_linter)
})
