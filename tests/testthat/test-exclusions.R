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
    line_info(c(2, 5), type = "end"),
    "2 range ends (lines 2, 5)"
  )
})

old_ops <- options(lintr.exclude = "#TeSt_NoLiNt",
                   lintr.exclude_start = "#TeSt_NoLiNt_StArT",
                   lintr.exclude_end = "#TeSt_NoLiNt_EnD")

test_that("it excludes properly", {
  read_settings(NULL)

  t1 <- lint("exclusions-test", parse_settings = FALSE)

  expect_length(t1, 8L)

  t2 <- lint("exclusions-test", exclusions = list("exclusions-test" = 4), parse_settings = FALSE)

  expect_length(t2, 7L)

  t3 <- lint("exclusions-test", exclusions = list("exclusions-test"), parse_settings = FALSE)

  expect_length(t3, 0L)

  cache_path <- file.path(tempdir(), "lintr_cache")
  clear_cache("exclusions-test", cache_path)
  for (info in sprintf("caching: pass %s", 1:4)) {
    t4 <- lint("exclusions-test", cache = cache_path, parse_settings = FALSE)

    expect_equal(length(t4), 8, info = info)
  }
})

test_that("it doesn't fail when encountering misspecified encodings", {
  read_settings(NULL)

  expect_length(parse_exclusions("dummy_projects/project/cp1252.R"), 0L)
})

options(old_ops)

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
