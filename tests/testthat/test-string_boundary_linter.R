test_that("StringBoundaryLinter skips allowed grepl() usages", {
  # no known start/end anchor --> no lint
  lintr::expect_lint("grepl(p1, x)", NULL, string_boundary_linter())
  # no start/end anchor --> no lint
  lintr::expect_lint("grepl('abc', x)", NULL, string_boundary_linter())
  # regex pattern --> no lint
  lintr::expect_lint("grepl('^[a-z]', x)", NULL, string_boundary_linter())
  lintr::expect_lint("grepl('[a-z]$', x)", NULL, string_boundary_linter())

  # ignore.case --> no lint
  lintr::expect_lint("grepl('^abc', x, ignore.case = TRUE)", NULL, string_boundary_linter())
  lintr::expect_lint("grepl('^abc', x, ignore.case = ignore.case)", NULL, string_boundary_linter())
})

test_that("string_boundary_linter skips allowed str_detect() usages", {
  # no known start/end anchor --> no lint
  lintr::expect_lint("str_detect(x, p1)", NULL, string_boundary_linter())
  # no start/end anchor --> no lint
  lintr::expect_lint("str_detect(x, 'abc')", NULL, string_boundary_linter())
  # regex pattern --> no lint
  lintr::expect_lint("str_detect(x, '^[a-z]')", NULL, string_boundary_linter())
  lintr::expect_lint("str_detect(x, '[a-z]$')", NULL, string_boundary_linter())
})

test_that("string_boundary_linter skips allowed substr()/substring() usages", {
  # no comparison operator --> no lint
  lintr::expect_lint("substr(x, start, end)", NULL, string_boundary_linter())
  # unknown indices --> no lint
  lintr::expect_lint("substr(x, start, end) == 'a'", NULL, string_boundary_linter())
  lintr::expect_lint("substring(x, start, end) == 'a'", NULL, string_boundary_linter())
  # using foo(nchar(.))
  lintr::expect_lint("substring(x, nchar(x) - 4, nchar(x) - 1) == 'abc'", NULL, string_boundary_linter())
  # using nchar(), but not of the input
  lintr::expect_lint("substring(x, nchar(y) - 4, nchar(y)) == 'abcd'", NULL, string_boundary_linter())
  # using x in nchar(), but on foo(input)
  lintr::expect_lint("substring(x, nchar(foo(x)) - 4, nchar(foo(x))) == 'abcd'", NULL, string_boundary_linter())

  # _close_ to equivalent, but not so in general -- e.g.
  #   substring(s <- "abcdefg", 2L) == "efg" is not TRUE, but endsWith(s, "efg")
  #   is. And if `s` contains strings of varying lengths, there's no equivalent.
  lintr::expect_lint("substring(x, 2L)", NULL, string_boundary_linter())
})

test_that("string_boundary_linter blocks simple disallowed grepl() usages", {
  lintr::expect_lint(
    "grepl('^a', x)",
    rex::rex("Use startsWith() to detect a fixed initial substring."),
    string_boundary_linter()
  )
  lintr::expect_lint(
    "grepl('a$', x)",
    rex::rex("Use endsWith() to detect a fixed terminal substring."),
    string_boundary_linter()
  )

  # options besides ignore.case also don't matter
  lintr::expect_lint(
    "grepl('^a', x, perl = TRUE, fixed = TRUE)",
    rex::rex("Use startsWith() to detect a fixed initial substring."),
    string_boundary_linter()
  )
  # explicit FALSE (i.e., an explicit default) is ignored
  lintr::expect_lint(
    "grepl('^a', x, ignore.case = FALSE)",
    rex::rex("Use startsWith() to detect a fixed initial substring."),
    string_boundary_linter()
  )
})

test_that("string_boundary_linter blocks simple disallowed str_detect() usages", {
  lintr::expect_lint(
    "str_detect(x, '^a')",
    rex::rex("Use startsWith() to detect a fixed initial substring."),
    string_boundary_linter()
  )
  lintr::expect_lint(
    "str_detect(x, 'a$')",
    rex::rex("Use endsWith() to detect a fixed terminal substring."),
    string_boundary_linter()
  )
})

test_that("string_boundary_linter blocks disallowed substr()/substring() usage", {
  lintr::expect_lint(
    "substr(x, 1L, 2L) == 'ab'",
    rex::rex("Use startsWith() to detect an initial substring."),
    string_boundary_linter()
  )
  # end doesn't matter, just anchoring to 1L
  lintr::expect_lint(
    "substr(x, 1L, end) == 'ab'",
    rex::rex("Use startsWith() to detect an initial substring."),
    string_boundary_linter()
  )
  lintr::expect_lint(
    "substring(x, nchar(x) - 4L, nchar(x)) == 'abcde'",
    rex::rex("Use endsWith() to detect a terminal substring."),
    string_boundary_linter()
  )
  # start doesn't matter, just anchoring to nchar(x)
  lintr::expect_lint(
    "substring(x, start, nchar(x)) == 'abcde'",
    rex::rex("Use endsWith() to detect a terminal substring."),
    string_boundary_linter()
  )
  # more complicated expressions
  lintr::expect_lint(
    "substring(colnames(x), start, nchar(colnames(x))) == 'abc'",
    rex::rex("Use endsWith() to detect a terminal substring."),
    string_boundary_linter()
  )
})

test_that("plain ^ or $ are skipped", {
  lintr::expect_lint('grepl("^", x)', NULL, string_boundary_linter())
  lintr::expect_lint('grepl("$", x)', NULL, string_boundary_linter())
})

test_that("substr inverted tests are caught as well", {
  lintr::expect_lint(
    "substr(x, 1L, 2L) != 'ab'",
    rex::rex("Use startsWith() to detect an initial substring."),
    string_boundary_linter()
  )
  lintr::expect_lint(
    "substring(x, nchar(x) - 4L, nchar(x)) != 'abcde'",
    rex::rex("Use endsWith() to detect a terminal substring."),
    string_boundary_linter()
  )
})

# TODO(chiricom): extend to inversion cases too:
#  !grepl("^abc", x) --> !startsWith(x, "abc")

# TODO(chiricom): fixed=TRUE should be skipped.
#  grepl("^a", x, fixed = TRUE) is not equivalent to startsWith(x, "a")
