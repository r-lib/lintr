test_that("inner_combine_linter lints a false positive-ish usage", {
  # By default as.POSIXct.character picks up the format to apply from
  #   the first element and, since it succeeds, applies that to the remaining
  #   timestamps. Whereas when run individually, it won't succeed until
  #   the correct format is matched for each input. Nevertheless, it is
  #   still preferable to vectorize the call, while being sure to use a
  #   consistent format for the inputs. In this case, the correct equivalent
  #   call is as.POSIXct(c("2021-01-01 00:00:00", "2021-01-01 01:00:00")).
  # See http://google3/analysis/economics/r/izeitgeist/tests/testthat/test-izeitgeist.R;l=1058-1062;rcl=394984377.
  expect_lint(
    "c(as.POSIXct('2021-01-01'), as.POSIXct('2021-01-01 01:00:00'))",
    rex::rex("Combine inputs to vectorized functions first"),
    inner_combine_linter()
  )
})

skip_if_not_installed("patrick")
local({
  vector_funs <- c(
    "as.Date", "as.POSIXct", "as.POSIXlt",
    "sin", "cos", "tan", "sinpi", "cospi", "tanpi", "asin", "acos", "atan",
    "log", "logb", "log2", "log10", "log1p", "exp", "expm1",
    "sqrt", "abs",
    "ymd", "ydm", "mdy", "myd", "dmy", "dym",
    "yq", "ym", "my",
    "ymd_hms", "ymd_hm", "ymd_h", "dmy_hms", "dmy_hm", "dmy_h",
    "mdy_hms", "mdy_hm", "mdy_h", "ydm_hms", "ydm_hm", "ydm_h",
    NULL
  )
  patrick::with_parameters_test_that(
    "inner_combine_linter blocks simple vectorized calls:",
    expect_lint(
      sprintf("c(%1$s(x), %1$s(y))", vector_fun),
      rex::rex("Combine inputs to vectorized functions first"),
      inner_combine_linter()
    ),
    .test_name = vector_funs,
    vector_fun = vector_funs
  )
})

patrick::with_parameters_test_that(
  "inner_combine_linter blocks as.Date with identical passed arguments:",
  expect_lint(
    sprintf("c(as.Date(x, %1$s), as.Date(y, %1$s))", arg),
    rex::rex("Combine inputs to vectorized functions first"),
    inner_combine_linter()
  ),
  .test_name = c("format", "origin", "tz", "tryFormats", "non-literal"),
  arg = c("format = '%F'", "origin = '1900-01-01'", "tz = 'Asia/Jakarta'", "tryFormats = '%F'", "tz = tz")
)

patrick::with_parameters_test_that(
  "inner_combine_linter blocks as.POSIXct with identical passed arguments:",
  expect_lint(
    sprintf("c(as.POSIXct(x, %1$s), as.POSIXct(y, %1$s))", arg),
    rex::rex("Combine inputs to vectorized functions first"),
    inner_combine_linter()
  ),
  .test_name = c("format", "origin", "tz", "non-literal"),
  arg = c("format = '%F'", "origin = '1900-01-01'", "tz = 'UTC'", "tz = tz")
)

test_that("inner_combine_linter is order-agnostic for matching arguments", {
  expect_lint(
    "c(as.Date(x, format = f, tz = t), as.Date(y, tz = t, format = f))",
    rex::rex("Combine inputs to vectorized functions first"),
    inner_combine_linter()
  )
})

skip_if_not_installed("tibble")
patrick::with_parameters_test_that(
  "inner_combine_linter skips allowed usages:",
  expect_lint(expr, NULL, inner_combine_linter()),
  .cases = tibble::tribble(
    ~.test_name,                      ~expr,
    "simple sin()",                   "x <- sin(1:10)",
    "mixed sin()+cos()",              "y <- c(sin(1:10), cos(2:20))",
    "present/absent vector function", "c(log(x), 0.5)",
    "absent/present vector function", "c(0.5, log(x))",
    "mismatched arg (Date)",          "c(as.Date(x, format = '%y'), as.Date(y, format = '%m'))",
    "present/absent arg (Date)",      "c(as.Date(x, format = '%y'), as.Date(y))",
    "absent/present arg (Date)",      "c(as.Date(x), as.Date(y, format = '%y'))",
    "matched value, not arg (Date)",  "c(as.Date(x, format = '%y'), as.Date(y, tz = '%y'))",
    "mismatched arg (POSIXct)",       "c(as.POSIXct(x, format = '%y'), as.POSIXct(y, format = '%m'))",
    "present/absent arg (POSIXct)",   "c(as.POSIXct(x, format = '%y'), as.POSIXct(y))",
    "mismatched arg (log)",           "c(log(x, base = 4), log(y, base = 5))",
    "present/absent arg (log)",       "c(log(x, base = 4), log(y))"
    # TODO(michaelchirico): fix the code so these edge cases are covered
    # "unknown Date method argument",    "c(as.Date(x, zoo = zzz), as.Date(y, zoo = zzz))",
    # "known+unknown Date argument", "c(as.Date(x, format = '%y', zoo = zzz), as.Date(y, format = '%y', zoo = zzz))",
    # "unknown POSIXct method argument", "c(as.POSIXct(x, zoo = zzz), as.POSIXct(y, zoo = zzz))",
  )
)
