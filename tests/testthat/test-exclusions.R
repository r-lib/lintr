test_that("it excludes properly", {
  withr::local_options(
    lintr.exclude = "#TeSt_NoLiNt",
    lintr.exclude_start = "#TeSt_NoLiNt_StArT",
    lintr.exclude_end = "#TeSt_NoLiNt_EnD"
  )

  lintr:::read_settings(NULL)

  t1 <- lint("exclusions-test", parse_settings = FALSE)
  expect_length(t1, 8L)

  t2 <- lint("exclusions-test", exclusions = list("exclusions-test" = 4L), parse_settings = FALSE)
  expect_length(t2, 7L)

  t3 <- lint("exclusions-test", exclusions = list("exclusions-test"), parse_settings = FALSE)
  expect_length(t3, 0L)

  cache_path <- file.path(tempdir(), "lintr_cache")
  clear_cache("exclusions-test", cache_path)
  for (info in sprintf("caching: pass %s", 1L:4L)) {
    t4 <- lint("exclusions-test", cache = cache_path, parse_settings = FALSE)

    expect_identical(length(t4), 8L, info = info)
  }
})

test_that("it doesn't fail when encountering misspecified encodings", {
  withr::local_options(
    lintr.exclude = "#TeSt_NoLiNt",
    lintr.exclude_start = "#TeSt_NoLiNt_StArT",
    lintr.exclude_end = "#TeSt_NoLiNt_EnD"
  )
  lintr:::read_settings(NULL)

  expect_length(lintr:::parse_exclusions("dummy_projects/project/cp1252.R"), 0L)
})

test_that("it gives the expected error message when there is only one start but no end", {
  lintr:::read_settings(NULL)

  expect_error(
    lintr:::parse_exclusions("dummy_projects/project/one_start_no_end.R"),
    "has 1 range start (line 3) and 0 range ends",
    fixed = TRUE
  )
})

test_that("it gives the expected error message when there is mismatch between multiple starts and ends", {
  lintr:::read_settings(NULL)

  expect_error(
    lintr:::parse_exclusions("dummy_projects/project/mismatched_starts_ends.R"),
    "has 3 range starts (lines 3, 7, 11) and 2 range ends (lines 1, 9)",
    fixed = TRUE
  )
})

test_that("partial matching works for exclusions but warns if no linter found", { # nofuzz: assignment
  lintr:::read_settings(NULL)

  expect_warning(
    expect_warning(
      expect_warning(
        expect_lint(
          file = "dummy_projects/project/partially_matched_exclusions.R",
          checks = rex::rex("semicolons"),
          parse_settings = FALSE
        ),
        rex::rex("Could not find linter named ", anything, "s")
      ),
      rex::rex("Could not find linter named ", anything, "bogus_linter")
    ),
    rex::rex("Could not find linters named ", anything, "hocus_pocus", anything, "bogus")
  )
})

test_that("#1413: lint_dir properly excludes files", {
  withr::local_options(lintr.linter_file = "lintr_test_config")
  tmp <- withr::local_tempdir()
  writeLines(
    trim_some("
      linters: linters_with_defaults(
          line_length_linter(10)
        )
      exclusions: list(
          'bad.R' = list(
            1, # global exclusions are unnamed
            line_length_linter = 4:6
          )
        )
    "),
    file.path(tmp, "lintr_test_config")
  )

  writeLines(
    trim_some("
      tmp = 'value'

      # comment
      # long comment
      # long comment
      # long comment
      # comment
    "),
    file.path(tmp, "bad.R")
  )

  expect_length(lint(file.path(tmp, "bad.R")), 0L)
  expect_length(lint_dir(tmp), 0L)
})

test_that("#1442: is_excluded_files works if no global exclusions are specified", { # nofuzz
  withr::local_options(lintr.linter_file = "lintr_test_config")
  withr::local_dir(withr::local_tempdir())

  writeLines(
    trim_some("
      linters: linters_with_defaults(
          line_length_linter(10)
        )
      exclusions: list(
          'bad.R' = list(
            line_length_linter = 4:6
          )
        )
    "),
    "lintr_test_config"
  )

  writeLines(
    trim_some("
      tmp = 'value'

      # comment
      # long comment
      # long comment
      # long comment
      # comment
    "),
    "bad.R"
  )

  # 3 lints: assignment_linter(), quotes_linter() and line_length_linter()
  expect_lint(
    file = "bad.R",
    checks = list(
      list(linter = "assignment_linter", line_number = 1L),
      list(linter = "quotes_linter", line_number = 1L),
      list(linter = "line_length_linter", line_number = 1L)
    )
  )
  expect_length(lint_dir(), 3L)
})

test_that("next-line exclusion works", { # nofuzz: assignment
  withr::local_options(
    lintr.exclude = "# NL",
    lintr.exclude_next = "# NLN",
    lintr.exclude_linter = default_settings$exclude_linter
  )

  linter <- assignment_linter()

  # blanket exclusion works
  expect_no_lint(
    trim_some("
      # NLN
      x = 1
    "),
    linter
  )

  # specific exclusion works
  expect_no_lint(
    trim_some("
      # NLN: assignment_linter.
      x = 1
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      # NLN: assignment.
      x = 1
    "),
    linter
  )
  expect_lint(
    trim_some("
      # NLN: line_length_linter.
      x = 1
    "),
    rex::rex("Use one of <-, <<- for assignment, not =."),
    list(linter, line_length_linter())
  )

  # interaction with plain nolint
  expect_lint(
    trim_some("
      x = 1 # NLN: assignment_linter.
      x = 2
    "),
    list(rex::rex("Use one of <-, <<- for assignment, not =."), line_number = 1L),
    linter
  )
})
