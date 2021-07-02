old_ops <- options(lintr.exclude = "#TeSt_NoLiNt",
                   lintr.exclude_start = "#TeSt_NoLiNt_StArT",
                   lintr.exclude_end = "#TeSt_NoLiNt_EnD")

test_that("it excludes properly", {
  read_settings(NULL)

  t1 <- lint("exclusions-test", parse_settings = FALSE)

  expect_equal(length(t1), 8)

  t2 <- lint("exclusions-test", exclusions = list("exclusions-test" = 4), parse_settings = FALSE)

  expect_equal(length(t2), 7)

  t3 <- lint("exclusions-test", exclusions = list("exclusions-test"), parse_settings = FALSE)

  expect_equal(length(t3), 0)

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
