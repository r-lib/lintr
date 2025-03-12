regexes <- list(
  assign = rex::rex("Use one of <-, <<- for assignment, not =."),
  local_var = rex::rex("local variable"),
  quotes = rex::rex("Only use double-quotes."),
  trailing = rex::rex("Remove trailing blank lines."),
  trailws = rex::rex("Remove trailing whitespace."),
  indent = rex::rex("Indentation should be")
)

test_that("it handles dir", {
  file_pattern <- rex::rex(".R", one_of("html", "md", "nw", "rst", "tex", "txt"))

  lints <- lint_dir(path = "knitr_formats", pattern = file_pattern, parse_settings = FALSE)

  # For every file there should be at least 1 lint
  expect_identical(
    sort(unique(names(lints))),
    sort(list.files(test_path("knitr_formats"), pattern = file_pattern))
  )
})

test_that("it handles markdown", {
  expect_lint(
    file = test_path("knitr_formats", "test.Rmd"),
    checks = list(
      list(regexes[["assign"]], line_number = 9L),
      list(regexes[["local_var"]], line_number = 22L),
      list(regexes[["assign"]], line_number = 22L),
      list(regexes[["trailing"]], line_number = 24L)
    ),
    linters = default_linters,
    parse_settings = FALSE
  )
})

test_that("it handles quarto", {
  expect_lint(
    file = test_path("knitr_formats", "test.qmd"),
    checks = list(
      list(regexes[["assign"]], line_number = 9L),
      list(regexes[["local_var"]], line_number = 22L),
      list(regexes[["assign"]], line_number = 22L),
      list(regexes[["trailing"]], line_number = 24L)
    ),
    linters = default_linters,
    parse_settings = FALSE
  )
})

test_that("it handles Sweave", {
  expect_lint(
    file = test_path("knitr_formats", "test.Rnw"),
    checks = list(
      list(regexes[["assign"]], line_number = 12L),
      list(regexes[["local_var"]], line_number = 24L),
      list(regexes[["assign"]], line_number = 24L),
      list(regexes[["trailing"]], line_number = 26L)
    ),
    linters = default_linters,
    parse_settings = FALSE
  )
})

test_that("it handles reStructuredText", {
  expect_lint(
    file = test_path("knitr_formats", "test.Rrst"),
    checks = list(
      list(regexes[["assign"]], line_number = 10L),
      list(regexes[["local_var"]], line_number = 23L),
      list(regexes[["assign"]], line_number = 23L),
      list(regexes[["trailing"]], line_number = 25L)
    ),
    linters = default_linters,
    parse_settings = FALSE
  )
})

test_that("it handles HTML", {
  expect_lint(
    file = test_path("knitr_formats", "test.Rhtml"),
    checks = list(
      list(regexes[["assign"]], line_number = 15L),
      list(regexes[["local_var"]], line_number = 27L),
      list(regexes[["assign"]], line_number = 27L),
      list(regexes[["trailing"]], line_number = 29L)
    ),
    linters = default_linters,
    parse_settings = FALSE
  )
})

test_that("it handles tex", {
  expect_lint(
    file = test_path("knitr_formats", "test.Rtex"),
    checks = list(
      list(regexes[["indent"]], line_number = 11L),
      list(regexes[["assign"]], line_number = 11L),
      list(regexes[["indent"]], line_number = 22L),
      list(regexes[["local_var"]], line_number = 23L),
      list(regexes[["assign"]], line_number = 23L),
      list(regexes[["trailing"]], line_number = 25L),
      list(regexes[["trailws"]], line_number = 25L)
    ),
    linters = default_linters,
    parse_settings = FALSE
  )
})

test_that("it handles asciidoc", {
  expect_lint(
    file = test_path("knitr_formats", "test.Rtxt"),
    checks = list(
      list(regexes[["assign"]], line_number = 9L),
      list(regexes[["local_var"]], line_number = 22L),
      list(regexes[["assign"]], line_number = 22L),
      list(regexes[["trailing"]], line_number = 24L)
    ),
    linters = default_linters,
    parse_settings = FALSE
  )
})

test_that("it does _not_ handle brew", { # nofuzz: comment_injection
  expect_lint("'<% a %>'\n",
    checks = list(
      regexes[["quotes"]],
      regexes[["trailing"]]
    ),
    default_linters
  )
})

test_that("it does _not_ error with inline \\Sexpr", {
  expect_no_lint(
    "#' text \\Sexpr{1 + 1} more text",
    default_linters
  )
})

test_that("it does lint .Rmd or .qmd file with malformed input", {
  expect_lint(
    file = test_path("knitr_malformed", "incomplete_r_block.Rmd"),
    checks = "Missing chunk end",
    linters = default_linters,
    parse_settings = FALSE
  )

  expect_lint(
    file = test_path("knitr_malformed", "incomplete_r_block.qmd"),
    checks = "Missing chunk end",
    linters = default_linters,
    parse_settings = FALSE
  )

  contents <- c(
    trim_some("
      ```{r chunk}
      lm(x ~ y)


      # some text

      ```
      bash some_script.sh
      ```
    "),
    trim_some("
      ```{r chunk-1}
      code <- 42

      # A heading
      Some text

      ```{r chunk-2}
      some_more_code <- 42
      ```
    "),
    trim_some("
      ```{r chunk-1}
      code <- 42
      ```

      # A heading
      Some text

      ```{r chunk-2}
      some_more_code <- 42
    ")
  )

  expected <- list(
    NULL, # This test case would require parsing all chunk fences, not just r chunks.
    "maybe starting at line 1",
    "maybe starting at line 8"
  )

  for (i in seq_along(contents)) {
    expect_lint(contents[[i]], expected[[i]], linters = list())
  }
})
