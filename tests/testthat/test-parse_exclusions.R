old_ops <- options(lintr.exclude = "#TeSt_NoLiNt",
                   lintr.exclude_start = "#TeSt_NoLiNt_StArT",
                   lintr.exclude_end = "#TeSt_NoLiNt_EnD")

test_that("it returns an empty list if there are no exclusions", {
  read_settings(NULL)
  t1 <- tempfile()
  on.exit(unlink(t1))
  writeLines(trim_some("
    this
    is
    a
    test
  "), t1)
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
  expect_equal(parse_exclusions(t1), list(2))

  t2 <- tempfile()
  on.exit(unlink(t2))
  writeLines(trim_some("
    this
    is #TeSt_NoLiNt
    a
    test #TeSt_NoLiNt
  "), t2)
  expect_equal(parse_exclusions(t2), list(c(2, 4)))
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
  expect_equal(parse_exclusions(t1), list(my_linter = 2))

  t2 <- tempfile()
  on.exit(unlink(t2))
  writeLines(trim_some("
    this
    is #TeSt_NoLiNt: my_linter.
    a
    test #TeSt_NoLiNt: my_linter2.
  "), t2)
  expect_equal(parse_exclusions(t2), list(my_linter = 2, my_linter2 = 4))

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
  expect_equal(parse_exclusions(t2), list(my_linter = c(2, 4), my_linter2 = 6))
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
    a = c(2, 4:6),
    b = c(2, 4:6),
    c = 4:6,
    c(1, 5, 8:11)
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
    a = 1:5,
    b = 3:6
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
  expect_equal(parse_exclusions(t1), list(c(1, 2, 3)))

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
  expect_equal(parse_exclusions(t2), list(c(1, 2, 3, 6, 7)))
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
  expect_equal(parse_exclusions(t1), list(c(1, 2, 3)))
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
