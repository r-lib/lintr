regexes <- list(
  assign = rex("Use <-, not =, for assignment."),
  local_var = rex("local variable"),
  quotes = rex("Only use double-quotes."),
  trailing = rex("Trailing blank lines are superfluous."),
  trailws = rex("Trailing whitespace is superfluous.")
)

test_that("it handles dir", {
  file_pattern <- rex::rex(".R", one_of("html", "md", "nw", "rst", "tex", "txt"))

  lints <- lint_dir(path = "knitr_formats", pattern = file_pattern, parse_settings = FALSE)

  # For every file there should be at least 1 lint
  expect_identical(sort(unique(names(lints))), sort(list.files("knitr_formats", pattern = file_pattern)))
})

test_that("it handles markdown", {
  expect_lint(file = "knitr_formats/test.Rmd",
    checks = list(
      list(regexes[["assign"]], line_number = 9L),
      list(regexes[["local_var"]], line_number = 22L),
      list(regexes[["assign"]], line_number = 22L),
      list(regexes[["trailing"]], line_number = 24L)
    ),
    default_linters,
    parse_settings = FALSE
  )
})

test_that("it handles Sweave", {
  expect_lint(file = "knitr_formats/test.Rnw",
    checks = list(
      list(regexes[["assign"]], line_number = 12L),
      list(regexes[["local_var"]], line_number = 24L),
      list(regexes[["assign"]], line_number = 24L),
      list(regexes[["trailing"]], line_number = 26L)
    ),
    default_linters,
    parse_settings = FALSE
  )
})

test_that("it handles reStructuredText", {
  expect_lint(file = "knitr_formats/test.Rrst",
    checks = list(
      list(regexes[["assign"]], line_number = 10L),
      list(regexes[["local_var"]], line_number = 23L),
      list(regexes[["assign"]], line_number = 23L),
      list(regexes[["trailing"]], line_number = 25L)
    ),
    default_linters,
    parse_settings = FALSE
  )
})

test_that("it handles HTML", {
  expect_lint(file = "knitr_formats/test.Rhtml",
    checks = list(
      list(regexes[["assign"]], line_number = 15L),
      list(regexes[["local_var"]], line_number = 27L),
      list(regexes[["assign"]], line_number = 27L),
      list(regexes[["trailing"]], line_number = 29L)
    ),
    default_linters,
    parse_settings = FALSE
  )
})

test_that("it handles tex", {
  expect_lint(file = "knitr_formats/test.Rtex",
    checks = list(
      list(regexes[["assign"]], line_number = 11L),
      list(regexes[["local_var"]], line_number = 23L),
      list(regexes[["assign"]], line_number = 23L),
      list(regexes[["trailing"]], line_number = 25L),
      list(regexes[["trailws"]], line_number = 25L)
      # FIXME(AshesITR): #1043
      # file_lines contains a whitespace on the final line for Rtex, because that is used to mark the Rtex escape char
      # "%" as well.
      # cf. get_source_expressions("tests/testthat/knitr_formats/test.Rtex")$lines[[25]]
    ),
    default_linters,
    parse_settings = FALSE
  )
})

test_that("it handles asciidoc", {
  expect_lint(file = "knitr_formats/test.Rtxt",
    checks = list(
      list(regexes[["assign"]], line_number = 9L),
      list(regexes[["local_var"]], line_number = 22L),
      list(regexes[["assign"]], line_number = 22L),
      list(regexes[["trailing"]], line_number = 24L)
    ),
    default_linters,
    parse_settings = FALSE
  )
})

test_that("it does _not_ handle brew", {
  expect_lint("'<% a %>'\n",
    checks = list(
      regexes[["quotes"]],
      regexes[["trailing"]]
    ),
    default_linters
  )
})

test_that("it does _not_ error with inline \\Sexpr", {
  expect_lint("#' text \\Sexpr{1 + 1} more text",
    NULL,
    default_linters
  )
})

test_that("it does lint with malformed input", {
  expect_lint(
    file = "knitr_malformed/incomplete_r_block.Rmd",
    checks = "Missing chunk end",
    default_linters,
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
