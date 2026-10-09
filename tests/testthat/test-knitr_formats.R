regexes <- list(
  assign = rex::rex("Use <- for assignment, not =."),
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

  # Adjacent code chunks without intervening '@' and multiple '@' doc chunks (#2619)
  expect_lint(
    trim_some("
      <<chunk-1>>=
      bad_code = 1
      <<chunk-2>>=
      another_bad = 2
      <<py-chunk, engine='python'>>=
      a = [1, 2]
      <<empty-chunk>>=
      <<eval=FALSE>>=
      skipped_bad = 3
      @ % comment on doc chunk
      @
      <<chunk-3>>=
      final_bad = 4
      @
    "),
    list(
      list(regexes[["assign"]], line_number = 2L, column_number = 10L),
      list(regexes[["assign"]], line_number = 4L, column_number = 13L),
      list(regexes[["assign"]], line_number = 13L, column_number = 11L)
    ),
    assignment_linter()
  )

  sweave_test_rnw <- system.file("Sweave", "Sweave-test-1.Rnw", package = "utils")
  expect_no_lint(file = sweave_test_rnw, linters = assignment_linter())
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

  # Prefixed code lines in .Rrst
  expect_lint(
    trim_some("
      .. {r}
      .. b <- function(x) {
      ..   d = 1
      .. }
      ..
      .. ..
    "),
    list(
      list(regexes[["local_var"]], line_number = 3L, column_number = 6L),
      list(regexes[["assign"]], line_number = 3L, column_number = 8L),
      list(regexes[["trailing"]], line_number = 5L, column_number = 3L)
    ),
    default_linters
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
      list(regexes[["assign"]], line_number = 11L, column_number = 5L),
      list(regexes[["local_var"]], line_number = 23L, column_number = 5L),
      list(regexes[["assign"]], line_number = 23L, column_number = 7L),
      list(regexes[["trailing"]], line_number = 25L, column_number = 2L)
    ),
    linters = default_linters,
    parse_settings = FALSE
  )

  # Genuine whitespace lints, #| eval: false, and <<ref>> chunks in .Rtex (#1043)
  expect_lint(
    c(
      "%% begin.rcode",
      "unprefixed = 1",
      "%% end.rcode",
      "  %% begin.rcode",
      "  % <<indented_ref_chunk>>",
      "  % z <- 1",
      "  %% end.rcode",
      "%% begin.rcode",
      "% <<other_chunk>>",
      "%   y <- 1",
      "% ",
      "%% end.rcode",
      "%% begin.rcode",
      "% #| eval: false",
      "% bad = 1",
      "%% end.rcode",
      "%% begin.rcode",
      "% b <- function(x) {",
      "%    x + 1",
      "% }",
      "%   ",
      "%% end.rcode"
    ),
    list(
      list(regexes[["assign"]], line_number = 2L, column_number = 12L),
      list(regexes[["indent"]], line_number = 10L, column_number = 4L),
      list(regexes[["indent"]], line_number = 19L, column_number = 5L),
      list(regexes[["trailing"]], line_number = 21L, column_number = 3L),
      list(regexes[["trailws"]], line_number = 21L, column_number = 3L)
    ),
    default_linters
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

  # Prefixed code lines in .Rtxt
  expect_lint(
    trim_some("
      //begin.rcode
      // b <- function(x) {
      //   d = 1
      // }
      //
      //end.rcode
    "),
    list(
      list(regexes[["local_var"]], line_number = 3L, column_number = 6L),
      list(regexes[["assign"]], line_number = 3L, column_number = 8L),
      list(regexes[["trailing"]], line_number = 5L, column_number = 3L)
    ),
    default_linters
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

test_that("it does lint .Rmd, .qmd, or .Rnw file with malformed input", {
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

      ```{r chunk-2}
      some_more_code <- 42
      ```
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
    "),
    trim_some("
      <<chunk-1>>=
      code <- 42
      <<chunk-2>>=
      some_more_code <- 42
    ")
  )

  expected <- list(
    NULL, # This test case would require parsing all chunk fences, not just r chunks.
    list("maybe starting at line 1", line_number = 1L, type = "error"),
    list("maybe starting at line 1", line_number = 1L, type = "error"),
    list("maybe starting at line 8", line_number = 8L, type = "error"),
    list("maybe starting at line 3", line_number = 3L, type = "error")
  )

  for (i in seq_along(contents)) {
    expect_lint(contents[[i]], expected[[i]], linters = list())
  }
})

test_that("chunkless files are fine", {
  tmp <- withr::local_tempfile(fileext = ".Rmd", lines = c(
    "---",
    "some_option: true",
    "---",
    "Some text!"
  ))
  expect_no_lint(file = tmp, linters = assignment_linter())
})

test_that("it skips eval=FALSE chunks (#1964)", {
  linter <- assignment_linter()

  expect_lint(
    trim_some("
      ```{r label, eval=FALSE}
      bad_code = 1
      ```

      ```{r}
      good_code = 2
      ```

      ```{r, eval=F}
      bad_code = 1
      ```

      ```{r, eval=TRUE}
      good_code = 3
      ```
    "),
    list(
      list(regexes[["assign"]], line_number = 6L),
      list(regexes[["assign"]], line_number = 14L)
    ),
    linter
  )

  # .qmd with #| eval: false in chunk body
  qmd_file <- withr::local_tempfile(fileext = ".qmd", lines = c(
    "```{r}",
    "#| eval: false",
    "bad_code = 1",
    "```",
    "```{r}",
    "good_code = 2",
    "```"
  ))
  expect_lint(
    file = qmd_file,
    checks = list(regexes[["assign"]], line_number = 6L),
    linter
  )

  # .Rnw with eval=FALSE
  expect_lint(
    trim_some("
      <<chunk-1, eval=FALSE>>=
      bad_code = 1
      @

      <<chunk-2, eval=TRUE>>=
      good_code = 2
      @
    "),
    list(regexes[["assign"]], line_number = 6L),
    linter
  )
})

test_that("malformed chunk options don't crash linting and fallback to evaluated", {
  linter <- assignment_linter()

  # syntax error in header options
  tmp_csv_err <- withr::local_tempfile(fileext = ".Rmd", lines = c(
    "```{r, eval=1+}",
    "bad_code = 1",
    "```"
  ))
  expect_silent(
    expect_lint(
      file = tmp_csv_err,
      checks = list(regexes[["assign"]], line_number = 2L),
      linters = linter
    )
  )

  # divide_chunk error (YAML syntax error in body options)
  tmp_yaml_err <- withr::local_tempfile(fileext = ".qmd", lines = c(
    "```{r}",
    "#| eval: {",
    "bad_code = 1",
    "```"
  ))
  expect_silent(
    expect_lint(
      file = tmp_yaml_err,
      checks = list(regexes[["assign"]], line_number = 3L),
      linters = linter
    )
  )

  # divide_chunk warning (YAML warning - not a list)
  tmp_yaml_warn <- withr::local_tempfile(fileext = ".qmd", lines = c(
    "```{r}",
    '#| "key: 1"',
    "bad_code = 1",
    "```"
  ))
  expect_silent(
    expect_lint(
      file = tmp_yaml_warn,
      checks = list(regexes[["assign"]], line_number = 3L),
      linters = linter
    )
  )
})

test_that("non-R code blocks are ignored (#1896)", {
  linter <- assignment_linter()

  # {extendr} and {ojs} chunks in .Rmd / .qmd
  expect_lint(
    trim_some(R'[
      ```{extendr}
      fn hello() -> &'static str {
        let x = 1;
        "hello"
      }
      ```

      ```{r}
      bad_code = 1
      ```

      ```{ojs}
      data = FileAttachment("seattle-weather.csv")
        .csv({typed: true})
      ```

      ```{r}
      good_code <- 2
      another_bad = 3
      ```
    ]'),
    list(
      list(regexes[["assign"]], line_number = 9L),
      list(regexes[["assign"]], line_number = 19L)
    ),
    linter
  )

  # Various other non-R chunk engines ({mermaid}, {dot}, {python}, {rust}, {sql})
  expect_lint(
    trim_some("
      ```{mermaid}
      graph TD;
        A-->B;
      ```

      ```{dot}
      digraph G {
        a -> b;
      }
      ```

      ```{python}
      a = [1, 2, 3]
      b = {'x': 1}
      ```

      ```{rust}
      let mut v = vec![1, 2, 3];
      ```

      ```{sql}
      SELECT * FROM table WHERE x = 1;
      ```

      ```{r}
      bad_code = 1
      ```
    "),
    list(regexes[["assign"]], line_number = 26L),
    linter
  )

  # Explicit engine option in chunk header
  expect_lint(
    trim_some('
      ```{r, engine = "python"}
      a = [1, 2]
      ```

      ```{r, engine = "R"}
      bad_code = 1
      ```

      ```{r, engine = "bash"}
      echo "hello"
      ```

      ```{r, engine = "r"}
      another_bad = 2
      ```
    '),
    list(
      list(regexes[["assign"]], line_number = 6L),
      list(regexes[["assign"]], line_number = 14L)
    ),
    linter
  )

  # .qmd with #| engine in chunk body
  qmd_file <- withr::local_tempfile(fileext = ".qmd", lines = c(
    "```{r}",
    "#| engine: python",
    "a = [1, 2]",
    "```",
    "```{r}",
    "#| engine: r",
    "bad_code = 1",
    "```"
  ))
  expect_lint(
    file = qmd_file,
    checks = list(regexes[["assign"]], line_number = 7L),
    linters = linter
  )

  # .Rnw with default R engine and explicit engine="python" vs engine="R"
  expect_lint(
    trim_some('
      <<chunk-0>>=
      initial_bad = 0
      @

      <<chunk-1, engine = "python">>=
      a = [1, 2]
      @

      <<chunk-2, engine = "R">>=
      bad_code = 1
      @

      <<chunk-3, engine = "r">>=
      another_bad = 2
      @
    '),
    list(
      list(regexes[["assign"]], line_number = 2L),
      list(regexes[["assign"]], line_number = 10L),
      list(regexes[["assign"]], line_number = 14L)
    ),
    linter
  )

  # Document containing only non-R code blocks extracts cleanly with 0 lints
  expect_no_lint(
    trim_some("
      ```{python}
      a = 1
      b = 2
      ```
    "),
    linters = linter
  )

  # Uppercase {R} fence
  expect_lint(
    trim_some("
      ```{R}
      bad_code = 1
      ```
    "),
    list(regexes[["assign"]], line_number = 2L),
    linter
  )
})
