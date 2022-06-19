test_that("line_info works as expected", {
  expect_identical(
    line_info(integer(), type = "end"),
    "0 range ends"
  )
  expect_identical(
    line_info(2L, type = "start"),
    "1 range start (line 2)"
  )
  expect_identical(
    line_info(c(2L, 5L), type = "end"),
    "2 range ends (lines 2, 5)"
  )
})

test_that("it excludes properly", {
  withr::local_options(
    lintr.exclude = "#TeSt_NoLiNt",
    lintr.exclude_start = "#TeSt_NoLiNt_StArT",
    lintr.exclude_end = "#TeSt_NoLiNt_EnD"
  )

  read_settings(NULL)

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

    expect_equal(length(t4), 8L, info = info)
  }
})

test_that("it doesn't fail when encountering misspecified encodings", {
  withr::local_options(
    lintr.exclude = "#TeSt_NoLiNt",
    lintr.exclude_start = "#TeSt_NoLiNt_StArT",
    lintr.exclude_end = "#TeSt_NoLiNt_EnD"
  )
  read_settings(NULL)

  expect_length(parse_exclusions("dummy_projects/project/cp1252.R"), 0L)
})

test_that("it gives the expected error message when there is only one start but no end", {
  read_settings(NULL)

  expect_error(
    parse_exclusions("dummy_projects/project/one_start_no_end.R"),
    "has 1 range start (line 3) but only 0 range ends for exclusion from linting",
    fixed = TRUE
  )
})

test_that("it gives the expected error message when there is mismatch between multiple starts and ends", {
  read_settings(NULL)

  expect_error(
    parse_exclusions("dummy_projects/project/mismatched_starts_ends.R"),
    "has 3 range starts (lines 3, 7, 11) but only 2 range ends (lines 1, 9) for exclusion from linting",
    fixed = TRUE
  )
})

test_that("partial matching works for exclusions but warns if no linter found", {
  read_settings(NULL)

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
