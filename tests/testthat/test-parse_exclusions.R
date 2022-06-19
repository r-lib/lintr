old_ops <- options(lintr.exclude = "#TeSt_NoLiNt",
                   lintr.exclude_start = "#TeSt_NoLiNt_StArT",
                   lintr.exclude_end = "#TeSt_NoLiNt_EnD")

test_that("it returns an empty list if there are no exclusions", {
  read_settings(NULL)
  t1 <- withr::local_tempfile(lines = trim_some("
    this
    is
    a
    test
   "))
  expect_equal(parse_exclusions(t1), list())
})

test_that("it returns the line if one line is excluded", {
  read_settings(NULL)

  t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(trim_some("
    this
    is #TeSt_NoLiNt
    a
    test
  "), t1)
  expect_equal(parse_exclusions(t1), list(2L))

  t2 <- tempfile()
  on.exit(unlink(t2))
  writeLines(trim_some("
    this
    is #TeSt_NoLiNt
    a
    test #TeSt_NoLiNt
  "), t2)
  expect_equal(parse_exclusions(t2), list(c(2L, 4L)))
})

test_that("it supports specific linter exclusions", {
  read_settings(NULL)

  t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(trim_some("
    this
    is #TeSt_NoLiNt: my_linter.
    a
    test
  "), t1)
  expect_equal(parse_exclusions(t1), list(my_linter = 2L))

  t2 <- tempfile()
  on.exit(unlink(t2))
  writeLines(trim_some("
    this
    is #TeSt_NoLiNt: my_linter.
    a
    test #TeSt_NoLiNt: my_linter2.
  "), t2)
  expect_equal(parse_exclusions(t2), list(my_linter = 2L, my_linter2 = 4L))

  t3 <- tempfile()
  on.exit(unlink(t3))
  writeLines(trim_some("
    this
    is #TeSt_NoLiNt: my_linter.
    another
    useful #TeSt_NoLiNt: my_linter.
    test
    testing #TeSt_NoLiNt: my_linter2.
  "), t2)
  expect_equal(parse_exclusions(t2), list(my_linter = c(2L, 4L), my_linter2 = 6L))
})

test_that("it supports multiple linter exclusions", {
  t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(trim_some("
    this #TeSt_NoLiNt
    is #TeSt_NoLiNt: a, b.
    a
    thorough #TeSt_NoLiNt_StArT: a, b, c.
    test #TeSt_NoLiNt
    of #TeSt_NoLiNt_EnD
    all
    features #TeSt_NoLiNt_StArT a, b, c
    interleaved #TeSt_NoLiNt a, b
    with
    each #TeSt_NoLiNt_EnD
    other
  "), t1)
  expect_equal(parse_exclusions(t1), list(
    a = c(2L, 4L:6L),
    b = c(2L, 4L:6L),
    c = 4L:6L,
    c(1L, 5L, 8L:11L)
  ))
})

test_that("it supports overlapping exclusion ranges", {
  t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(trim_some("
    this #TeSt_NoLiNt_StArT: a.
    is
    a #TeSt_NoLiNt_StArT: b.
    test
    with #TeSt_NoLiNt_EnD
    overlapping #TeSt_NoLiNt_EnD
    ranges
  "), t1)
  expect_equal(parse_exclusions(t1), list(
    a = 1L:5L,
    b = 3L:6L
  ))
})

test_that("it returns all lines between start and end", {
  read_settings(NULL)

  t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(trim_some("
    this #TeSt_NoLiNt_StArT
    is
    a #TeSt_NoLiNt_EnD
    test
  "), t1)
  expect_equal(parse_exclusions(t1), list(c(1L, 2L, 3L)))

  t2 <- tempfile()
  on.exit(unlink(t2))
  writeLines(trim_some("
    this #TeSt_NoLiNt_StArT
    is
    a #TeSt_NoLiNt_EnD
    test
    of
    the #TeSt_NoLiNt_StArT
    emergency #TeSt_NoLiNt_EnD
    broadcast
    system
  "), t2)
  expect_equal(parse_exclusions(t2), list(c(1L, 2L, 3L, 6L, 7L)))
})

test_that("it ignores exclude coverage lines within start and end", {
  read_settings(NULL)

    t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(
    c("this #TeSt_NoLiNt_StArT",
      "is #TeSt_NoLiNt",
      "a #TeSt_NoLiNt_EnD",
      "test"), t1)
  expect_equal(parse_exclusions(t1), list(c(1L, 2L, 3L)))
})

test_that("it throws an error if start and end are unpaired", {
  read_settings(NULL)

  t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(trim_some("
    this #TeSt_NoLiNt_StArT
    is #TeSt_NoLiNt
    a
    test
  "), t1)
  expect_error(parse_exclusions(t1), "but only")


  t2 <- tempfile()
  on.exit(unlink(t2))
  writeLines(trim_some("
    this #TeSt_NoLiNt_StArT
    is #TeSt_NoLiNt_EnD
    a  #TeSt_NoLiNt_EnD
    test
  "), t2)
  expect_error(parse_exclusions(t2), "but only")
})

options(old_ops)
