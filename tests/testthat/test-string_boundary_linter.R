test_that("StringBoundaryLinter skips allowed grepl() usages", {
  # no known start/end anchor --> no lint
  expect_lint("grepl(p1, x)", NULL, string_boundary_linter())
  # no start/end anchor --> no lint
  expect_lint("grepl('abc', x)", NULL, string_boundary_linter())
  # regex pattern --> no lint
  expect_lint("grepl('^[a-z]', x)", NULL, string_boundary_linter())
  expect_lint("grepl('[a-z]$', x)", NULL, string_boundary_linter())

  # ignore.case --> no lint
  expect_lint("grepl('^abc', x, ignore.case = TRUE)", NULL, string_boundary_linter())
  expect_lint("grepl('^abc', x, ignore.case = ignore.case)", NULL, string_boundary_linter())

  # fixed --> no lint
  expect_lint("grepl('^abc', x, fixed = TRUE)", NULL, string_boundary_linter())
  expect_lint("grepl('^abc', x, fixed = fixed)", NULL, string_boundary_linter())
})

test_that("string_boundary_linter skips allowed str_detect() usages", {
  # no known start/end anchor --> no lint
  expect_lint("str_detect(x, p1)", NULL, string_boundary_linter())
  # no start/end anchor --> no lint
  expect_lint("str_detect(x, 'abc')", NULL, string_boundary_linter())
  # regex pattern --> no lint
  expect_lint("str_detect(x, '^[a-z]')", NULL, string_boundary_linter())
  expect_lint("str_detect(x, '[a-z]$')", NULL, string_boundary_linter())
})

test_that("string_boundary_linter skips allowed substr()/substring() usages", {
  # no comparison operator --> no lint
  expect_lint("substr(x, start, end)", NULL, string_boundary_linter())
  # unknown indices --> no lint
  expect_lint("substr(x, start, end) == 'a'", NULL, string_boundary_linter())
  expect_lint("substring(x, start, end) == 'a'", NULL, string_boundary_linter())
  # using foo(nchar(.))
  expect_lint("substring(x, nchar(x) - 4, nchar(x) - 1) == 'abc'", NULL, string_boundary_linter())
  # using nchar(), but not of the input
  expect_lint("substring(x, nchar(y) - 4, nchar(y)) == 'abcd'", NULL, string_boundary_linter())
  # using x in nchar(), but on foo(input)
  expect_lint("substring(x, nchar(foo(x)) - 4, nchar(foo(x))) == 'abcd'", NULL, string_boundary_linter())

  # _close_ to equivalent, but not so in general -- e.g.
  #   substring(s <- "abcdefg", 2L) == "efg" is not TRUE, but endsWith(s, "efg")
  #   is. And if `s` contains strings of varying lengths, there's no equivalent.
  expect_lint("substring(x, 2L)", NULL, string_boundary_linter())
})

test_that("string_boundary_linter blocks simple disallowed grepl() usages", {
  starts_message <- rex::rex("Use !is.na(x) & startsWith(x, string) to detect a fixed initial substring,")
  ends_message <- rex::rex("Use !is.na(x) & endsWith(x, string) to detect a fixed terminal substring,")

  expect_lint(
    "grepl('^a', x)",
    starts_message,
    string_boundary_linter()
  )
  # non-trivially equivalent (but still same as startsWith())
  expect_lint(
    "grepl('^[.]', x)",
    starts_message,
    string_boundary_linter()
  )
  expect_lint(
    "grepl('a$', x)",
    ends_message,
    string_boundary_linter()
  )
  # also get negation for free
  expect_lint(
    "!grepl('a$', x)",
    ends_message,
    string_boundary_linter()
  )

  # perl = TRUE doesn't matter
  expect_lint(
    "grepl('^a', x, perl = TRUE)",
    starts_message,
    string_boundary_linter()
  )
  # explicit FALSE (i.e., an explicit default) is ignored
  expect_lint(
    "grepl('^a', x, fixed = FALSE)",
    starts_message,
    string_boundary_linter()
  )
  expect_lint(
    "grepl('^a', x, fixed = F)",
    starts_message,
    string_boundary_linter()
  )
})

test_that("string_boundary_linter blocks simple disallowed str_detect() usages", {
  expect_lint(
    "str_detect(x, '^a')",
    rex::rex("Use startsWith() to detect a fixed initial substring."),
    string_boundary_linter()
  )
  expect_lint(
    "str_detect(x, 'a$')",
    rex::rex("Use endsWith() to detect a fixed terminal substring."),
    string_boundary_linter()
  )
})

test_that("string_boundary_linter blocks disallowed substr()/substring() usage", {
  expect_lint(
    "substr(x, 1L, 2L) == 'ab'",
    rex::rex("Use startsWith() to detect an initial substring."),
    string_boundary_linter()
  )
  # end doesn't matter, just anchoring to 1L
  expect_lint(
    "substr(x, 1L, end) == 'ab'",
    rex::rex("Use startsWith() to detect an initial substring."),
    string_boundary_linter()
  )
  expect_lint(
    "substring(x, nchar(x) - 4L, nchar(x)) == 'abcde'",
    rex::rex("Use endsWith() to detect a terminal substring."),
    string_boundary_linter()
  )
  # start doesn't matter, just anchoring to nchar(x)
  expect_lint(
    "substring(x, start, nchar(x)) == 'abcde'",
    rex::rex("Use endsWith() to detect a terminal substring."),
    string_boundary_linter()
  )
  # more complicated expressions
  expect_lint(
    "substring(colnames(x), start, nchar(colnames(x))) == 'abc'",
    rex::rex("Use endsWith() to detect a terminal substring."),
    string_boundary_linter()
  )
})

test_that("plain ^ or $ are skipped", {
  expect_lint('grepl("^", x)', NULL, string_boundary_linter())
  expect_lint('grepl("$", x)', NULL, string_boundary_linter())
})

test_that("substr inverted tests are caught as well", {
  expect_lint(
    "substr(x, 1L, 2L) != 'ab'",
    rex::rex("Use startsWith() to detect an initial substring."),
    string_boundary_linter()
  )
  expect_lint(
    "substring(x, nchar(x) - 4L, nchar(x)) != 'abcde'",
    rex::rex("Use endsWith() to detect a terminal substring."),
    string_boundary_linter()
  )
})

test_that("R>=4 raw strings are detected", {
  skip_if_not_r_version("4.0.0")
  expect_lint('grepl(R"(^.{3})", x)', NULL, string_boundary_linter())
  expect_lint(
    'grepl(R"(^abc)", x)',
    rex::rex("Use !is.na(x) & startsWith(x, string) to detect a fixed initial substring,"),
    string_boundary_linter()
  )
})

test_that("grepl() can optionally be ignored", {
  expect_lint("grepl('^abc', x)", NULL, string_boundary_linter(allow_grepl = TRUE))
  expect_lint("grepl('xyz$', x)", NULL, string_boundary_linter(allow_grepl = TRUE))
})
