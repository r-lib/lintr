test_that("string_boundary_linter skips allowed grepl() usages", {
  linter <- string_boundary_linter()

  # no known start/end anchor --> no lint
  expect_no_lint("grepl(p1, x)", linter)
  # no start/end anchor --> no lint
  expect_no_lint("grepl('abc', x)", linter)
  # regex pattern --> no lint
  expect_no_lint("grepl('^[a-z]', x)", linter)
  expect_no_lint("grepl('[a-z]$', x)", linter)
  # anchor-ish but not a regex --> no lint (#2636)
  expect_no_lint(R"[grepl("\\^", x)]", linter)
  expect_no_lint(R"[grepl("\\\\^", x)]", linter)
  expect_no_lint(R"[grepl("$\\\\", x)]", linter)

  # ignore.case --> no lint
  expect_no_lint("grepl('^abc', x, ignore.case = TRUE)", linter)
  expect_no_lint("grepl('^abc', x, ignore.case = ignore.case)", linter)

  # fixed --> no lint
  expect_no_lint("grepl('^abc', x, fixed = TRUE)", linter)
  expect_no_lint("grepl('^abc', x, fixed = fixed)", linter)
})

test_that("string_boundary_linter skips allowed str_detect() usages", {
  linter <- string_boundary_linter()

  # no known start/end anchor --> no lint
  expect_no_lint("str_detect(x, p1)", linter)
  # no start/end anchor --> no lint
  expect_no_lint("str_detect(x, 'abc')", linter)
  # regex pattern --> no lint
  expect_no_lint("str_detect(x, '^[a-z]')", linter)
  expect_no_lint("str_detect(x, '[a-z]$')", linter)
})

test_that("string_boundary_linter skips allowed substr()/substring() usages", {
  linter <- string_boundary_linter()

  # no comparison operator --> no lint
  expect_no_lint("substr(x, start, end)", linter)
  # unknown indices --> no lint
  expect_no_lint("substr(x, start, end) == 'a'", linter)
  expect_no_lint("substring(x, start, end) == 'a'", linter)
  # using foo(nchar(.))
  expect_no_lint("substring(x, nchar(x) - 4, nchar(x) - 1) == 'abc'", linter)
  # using nchar(), but not of the input
  expect_no_lint("substring(x, nchar(y) - 4, nchar(y)) == 'abcd'", linter)
  # using x in nchar(), but on foo(input)
  expect_no_lint("substring(x, nchar(foo(x)) - 4, nchar(foo(x))) == 'abcd'", linter)

  # _close_ to equivalent, but not so in general -- e.g.
  #   substring(s <- "abcdefg", 2L) == "efg" is not TRUE, but endsWith(s, "efg")
  #   is. And if `s` contains strings of varying lengths, there's no equivalent.
  expect_no_lint("substring(x, 2L)", linter)
})

test_that("string_boundary_linter blocks simple disallowed grepl() usages", {
  linter <- string_boundary_linter()
  starts_message <- rex::rex('Use !is.na(x) & startsWith(x, "a") to detect a fixed initial substring,')
  ends_message <- rex::rex('Use !is.na(x) & endsWith(x, "a") to detect a fixed terminal substring,')

  expect_lint("grepl('^a', x)", starts_message, linter)
  # non-trivially equivalent (but still same as startsWith())
  expect_lint(
    "grepl('^[.]', x)",
    rex::rex('Use !is.na(x) & startsWith(x, ".") to detect a fixed initial substring,'),
    linter
  )
  expect_lint("grepl('a$', x)", ends_message, linter)
  # also get negation for free
  expect_lint("!grepl('a$', x)", ends_message, linter)

  # perl = TRUE doesn't matter
  expect_lint("grepl('^a', x, perl = TRUE)", starts_message, linter)
  # explicit FALSE (i.e., an explicit default) is ignored
  expect_lint("grepl('^a', x, fixed = FALSE)", starts_message, linter)
  expect_lint("grepl('^a', x, fixed = F)", starts_message, linter)
  expect_lint("grepl('^a', x, ignore.case = FALSE)", starts_message, linter)
  expect_lint("grepl('^a', x, ignore.case = F)", starts_message, linter)
})

test_that("string_boundary_linter blocks simple disallowed str_detect() usages", {
  linter <- string_boundary_linter()

  expect_lint(
    "str_detect(x, '^a')",
    rex::rex('Use startsWith(x, "a") or str_starts(x, fixed("a")) to detect a fixed initial substring.'),
    linter
  )
  expect_lint(
    "str_detect(x, 'a$')",
    rex::rex('Use endsWith(x, "a") or str_ends(x, fixed("a")) to detect a fixed terminal substring.'),
    linter
  )
})

test_that("string_boundary_linter blocks disallowed substr()/substring() usage", {
  linter <- string_boundary_linter()
  starts_message <- rex::rex("Use startsWith() to detect an initial substring.")
  ends_message <- rex::rex("Use endsWith() to detect a terminal substring.")

  expect_lint("substr(x, 0, 1) == 'a'", starts_message, linter)
  expect_lint("substring(x, 0L, 1L) == 'a'", starts_message, linter)
  expect_lint("substr(x, 1L, 2L) == 'ab'", starts_message, linter)
  # end doesn't matter, just anchoring to 1L
  expect_lint("substr(x, 1L, end) == 'ab'", starts_message, linter)
  expect_lint("substring(x, nchar(x) - 4L, nchar(x)) == 'abcde'", ends_message, linter)
  # start doesn't matter, just anchoring to nchar(x)
  expect_lint("substring(x, start, nchar(x)) == 'abcde'", ends_message, linter)
  # more complicated expressions
  expect_lint("substring(colnames(x), start, nchar(colnames(x))) == 'abc'", ends_message, linter)

  # adversarial comments
  expect_lint(
    trim_some("
      substring(colnames(x), start, nchar(colnames( # comment
      x))) == 'abc'
    "),
    ends_message,
    linter
  )
})

test_that("plain ^ or $ are skipped across all quoting formats", {
  linter <- string_boundary_linter()

  expect_no_lint('grepl("^", x)', linter)
  expect_no_lint('grepl("$", x)', linter)
  expect_no_lint('grepl(R"(^)", x)', linter)
  expect_no_lint('grepl(R"($)", x)', linter)
  expect_no_lint('str_detect(x, "^")', linter)
  expect_no_lint('str_detect(x, "$")', linter)
  expect_no_lint('str_detect(x, R"(^)")', linter)
  expect_no_lint('str_detect(x, R"($)")', linter)
})

test_that("substr inverted tests are caught as well", {
  linter <- string_boundary_linter()

  expect_lint(
    "substr(x, 1L, 2L) != 'ab'",
    rex::rex("Use startsWith() to detect an initial substring."),
    linter
  )
  expect_lint(
    "substring(x, nchar(x) - 4L, nchar(x)) != 'abcde'",
    rex::rex("Use endsWith() to detect a terminal substring."),
    linter
  )
})

test_that("raw strings are detected", {
  linter <- string_boundary_linter()

  expect_no_lint('grepl(R"(^.{3})", x)', linter)
  expect_lint(
    'grepl(R"(^abc)", x)',
    rex::rex('Use !is.na(x) & startsWith(x, "abc") to detect a fixed initial substring,'),
    linter
  )
})

test_that("grepl() can optionally be ignored while retaining other checks", {
  linter <- string_boundary_linter(allow_grepl = TRUE)

  # grepl usages ignored
  expect_no_lint("grepl('^abc', x)", linter)
  expect_no_lint("grepl('xyz$', x)", linter)
  expect_no_lint("grepl('^abc$', x)", linter)

  # other behavior of the linter continues WAI
  expect_lint("str_detect(x, '^abc')", rex::rex('str_starts(x, fixed("abc"))'), linter)
  expect_lint("str_detect(x, 'xyz$')", rex::rex('str_ends(x, fixed("xyz"))'), linter)
  expect_lint("str_detect(x, '^abc$')", rex::rex('x == "abc"'), linter)
  expect_lint("substr(x, 1L, 3L) == 'abc'", "startsWith", linter)
  expect_lint("substring(x, nchar(x) - 2L, nchar(x)) == 'xyz'", "endsWith", linter)
})

test_that("whole-string regex recommends ==, not {starts,ends}With()", {
  linter <- string_boundary_linter()

  expect_lint(
    "grepl('^abc$', x)",
    rex::rex('Use !is.na(x) & x == "abc" to check for an exact string match,'),
    linter
  )
  expect_lint(
    "grepl('^a\\\\.b$', x)",
    rex::rex('Use !is.na(x) & x == "a.b" to check for an exact string match,'),
    linter
  )
  expect_lint("str_detect(x, '^abc$')", rex::rex('Use x == "abc" to check for an exact string match.'), linter)
  expect_lint("str_detect(x, '^a[.]b$')", rex::rex('Use x == "a.b" to check for an exact string match.'), linter)
  expect_lint('grepl("^$", x)', rex::rex('Use !is.na(x) & x == "" to check for an exact string match,'), linter)
  expect_lint('str_detect(x, "^$")', rex::rex('Use x == "" to check for an exact string match.'), linter)
  expect_lint('grepl(R"(^$)", x)', rex::rex('Use !is.na(x) & x == "" to check for an exact string match,'), linter)
})

test_that("string_boundary_linter messages explain specific inefficiency reasons", {
  linter <- string_boundary_linter()
  regex_reason <- "avoids regular expression overhead"
  substr_reason <- "avoids extracting intermediate substrings before comparison"

  expect_lint("grepl('^abc', x)", regex_reason, linter)
  expect_lint("grepl('abc$', x)", regex_reason, linter)
  expect_lint("grepl('^abc$', x)", regex_reason, linter)
  expect_lint("str_detect(x, '^abc')", regex_reason, linter)
  expect_lint("str_detect(x, 'abc$')", regex_reason, linter)
  expect_lint("str_detect(x, '^abc$')", regex_reason, linter)
  expect_lint("substr(x, 1L, 3L) == 'abc'", substr_reason, linter)
  expect_lint("substring(x, nchar(x) - 2L, nchar(x)) == 'abc'", substr_reason, linter)
  expect_lint("substr(x, 1L, 3L) != 'abc'", substr_reason, linter)
})

test_that("quotes, backslashes, and escaped anchors are correctly handled", {
  linter <- string_boundary_linter()

  # double and single quotes
  expect_lint('grepl(\'^"abc"\', x)', rex::rex('startsWith(x, "\\"abc\\"")'), linter)
  expect_lint("grepl(\"^'abc'\", x)", rex::rex('startsWith(x, "\'abc\'")'), linter)
  expect_lint('grepl(\'"abc"$\', x)', rex::rex('endsWith(x, "\\"abc\\"")'), linter)
  expect_lint("grepl(\"'abc'$\", x)", rex::rex('endsWith(x, "\'abc\'")'), linter)
  expect_lint('str_detect(x, \'^"abc"\')', rex::rex('str_starts(x, fixed("\\"abc\\""))'), linter)
  expect_lint('str_detect(x, \'"abc"$\')', rex::rex('str_ends(x, fixed("\\"abc\\""))'), linter)
  expect_lint('grepl(\'^"abc"$\', x)', rex::rex('x == "\\"abc\\""'), linter)

  # literal backslashes
  expect_lint(R"[grepl(R"(^a\\b)", x)]", rex::rex(R"[startsWith(x, "a\\b")]"), linter)
  expect_lint(R"[str_detect(x, R"(a\\b$)")]", rex::rex(R"[str_ends(x, fixed("a\\b"))]"), linter)

  # trivial character groups & escaped metacharacters
  expect_lint("grepl('[.]$', x)", rex::rex('endsWith(x, ".")'), linter)
  expect_lint("str_detect(x, '^[.]')", rex::rex('str_starts(x, fixed("."))'), linter)
  expect_lint("str_detect(x, '[.]$')", rex::rex('str_ends(x, fixed("."))'), linter)

  # internal and boundary escaped anchors
  expect_lint(R"[grepl('^a\\^b', x)]", rex::rex('startsWith(x, "a^b")'), linter)
  expect_lint(R"[grepl('a\\$b$', x)]", rex::rex('endsWith(x, "a$b")'), linter)
  expect_lint(R"[grepl('^a\\$', x)]", rex::rex('startsWith(x, "a$")'), linter)
  expect_lint(R"[grepl('^\\^', x)]", rex::rex('startsWith(x, "^")'), linter)
  expect_lint(R"[grepl('\\$$', x)]", rex::rex('endsWith(x, "$")'), linter)

  # exact symbol matches
  expect_lint("grepl('^[$]$', x)", rex::rex('x == "$"'), linter)
  expect_lint(R"[grepl('^[\\^]$', x)]", rex::rex('x == "^"'), linter)
})

test_that("vectorization + metadata work as intended", {
  linter <- string_boundary_linter()

  expect_lint(
    trim_some("{
      substring(a, 1, 3) == 'abc'
      substring(b, nchar(b) - 3, nchar(b)) == 'defg'
      substr(c, 1, 3) == 'hij'
      substr(d, nchar(d) - 3, nchar(d)) == 'klmn'
      grepl('^start_g', e)
      grepl('end_g$', f)
      grepl('^exact_g$', g)
      str_detect(h, '^start_s')
      str_detect(i, 'end_s$')
      str_detect(j, '^exact_s$')
    }"),
    list(
      list("startsWith", line_number = 2L),
      list("endsWith", line_number = 3L),
      list("startsWith", line_number = 4L),
      list("endsWith", line_number = 5L),
      list(rex::rex('startsWith(x, "start_g")'), line_number = 6L),
      list(rex::rex('endsWith(x, "end_g")'), line_number = 7L),
      list(rex::rex('x == "exact_g"'), line_number = 8L),
      list(rex::rex('str_starts(x, fixed("start_s"))'), line_number = 9L),
      list(rex::rex('str_ends(x, fixed("end_s"))'), line_number = 10L),
      list(rex::rex('x == "exact_s"'), line_number = 11L)
    ),
    linter
  )

  # some but not all anchor-esque as in #2636
  expect_lint(
    trim_some(R"[{
      grepl("\\^", x)
      grepl("^abc", x)
      str_detect(x, "\\^")
      str_detect(x, "^def")
    }]"),
    list(
      list(rex::rex('startsWith(x, "abc")'), line_number = 3L),
      list(rex::rex('str_starts(x, fixed("def"))'), line_number = 5L)
    ),
    linter
  )
})
