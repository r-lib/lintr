# NB: escaping is confusing. We have to double-escape everything -- the first
#   escape creates a string that will be parse()d, the second escape is normal
#   escaping that would be done in R code. E.g. in "\\\\.", the R code would
#   read like "\\.", but in order to create those two slashes, we need to write
#   "\\\\." in the string here.

test_that("fixed_regex_linter skips allowed usages", {
  expect_lint("gsub('^x', '', y)", NULL, fixed_regex_linter())
  expect_lint("grep('x$', '', y)", NULL, fixed_regex_linter())
  expect_lint("sub('[a-zA-Z]', '', y)", NULL, fixed_regex_linter())
  expect_lint("grepl(fmt, y)", NULL, fixed_regex_linter())
  expect_lint("regexec('\\\\s', '', y)", NULL, fixed_regex_linter())
  expect_lint("grep('a(?=b)', x, perl = TRUE)", NULL, fixed_regex_linter())
  expect_lint("grep('0+1', x, perl = TRUE)", NULL, fixed_regex_linter())
  expect_lint("grep('1*2', x)", NULL, fixed_regex_linter())
  expect_lint("grep('a|b', x)", NULL, fixed_regex_linter())
  expect_lint("grep('\\\\[|\\\\]', x)", NULL, fixed_regex_linter())

  # if fixed=TRUE is already set, regex patterns don't matter
  expect_lint("gsub('\\\\.', '', y, fixed = TRUE)", NULL, fixed_regex_linter())

  # ignore.case=TRUE implies regex interpretation
  expect_lint("gsub('abcdefg', '', y, ignore.case = TRUE)", NULL, fixed_regex_linter())
})

test_that("fixed_regex_linter blocks simple disallowed usages", {
  expect_lint(
    "gsub('\\\\.', '', x)",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "grepl('abcdefg', x)",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "gregexpr('a-z', y)",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "regexec('\\\\$', x)",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "grep('\n', x)",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  # naming the argument doesn't matter (if it's still used positionally)
  expect_lint(
    "gregexpr(pattern = 'a-z', y)",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )
})

test_that("fixed_regex_linter catches regex like [.] or [$]", {
  expect_lint(
    "grep('[.]', x)",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "grepl('a[*]b', x)",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )
})

test_that("fixed_regex_linter catches null calls to strsplit as well", {
  expect_lint("strsplit(x, '^x')", NULL, fixed_regex_linter())
  expect_lint("tstrsplit(x, '[a-zA-Z]')", NULL, fixed_regex_linter())
  expect_lint("tstrsplit(x, fmt)", NULL, fixed_regex_linter())
  expect_lint("strsplit(x, '\\\\s')", NULL, fixed_regex_linter())
  expect_lint("strsplit(x, 'a(?=b)', perl = TRUE)", NULL, fixed_regex_linter())
  expect_lint("strsplit(x, '0+1', perl = TRUE)", NULL, fixed_regex_linter())
  expect_lint("tstrsplit(x, '1*2')", NULL, fixed_regex_linter())
  expect_lint("strsplit(x, 'a|b')", NULL, fixed_regex_linter())

  # if fixed=TRUE is already set, regex patterns don't matter
  expect_lint("strsplit(x, '\\\\.', fixed = TRUE)", NULL, fixed_regex_linter())
  expect_lint("strsplit(x, '\\\\.', fixed = T)", NULL, fixed_regex_linter())
})

test_that("fixed_regex_linter catches calls to strsplit as well", {
  expect_lint(
    "strsplit(x, '\\\\.')",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "tstrsplit(x, 'abcdefg')",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "strsplit(x, '[.]')",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )
})

test_that("fixed_regex_linter is more exact about distinguishing \\s from \\:", {
  expect_lint("grep('\\\\s', '', x)", NULL, fixed_regex_linter())
  expect_lint(
    "grep('\\\\:', '', x)",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )
})

## tests for stringr functions
test_that("fixed_regex_linter skips allowed usages", {
  expect_lint("str_replace(y, '[a-zA-Z]', '')", NULL, fixed_regex_linter())
  expect_lint("str_replace_all(y, '^x', '')", NULL, fixed_regex_linter())
  expect_lint("str_detect(y, fmt)", NULL, fixed_regex_linter())
  expect_lint("str_extract(y, '\\\\s')", NULL, fixed_regex_linter())
  expect_lint("str_extract_all(y, '\\\\s')", NULL, fixed_regex_linter())
  expect_lint("str_which(x, '1*2')", NULL, fixed_regex_linter())

  # if fixed() is already set, regex patterns don't matter
  expect_lint("str_replace(y, fixed('\\\\.'), '')", NULL, fixed_regex_linter())

  # namespace qualification doesn't matter
  expect_lint("stringr::str_replace(y, stringr::fixed('abcdefg'), '')", NULL, fixed_regex_linter())
})

test_that("fixed_regex_linter blocks simple disallowed usages of stringr functions", {
  expect_lint(
    "str_replace_all(x, '\\\\.', '')",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "str_detect(x, 'abcdefg')",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "str_locate(y, 'a-z')",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "str_subset(x, '\\\\$')",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "str_which(x, '\n')",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  # named, positional arguments are still caught
  expect_lint(
    "str_locate(y, pattern = 'a-z')",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )
  # nor do other named arguments throw things off
  expect_lint(
    "str_starts(x, '\\\\.', negate = TRUE)",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )
})

test_that("fixed_regex_linter catches calls to str_split as well", {
  expect_lint("str_split(x, '^x')", NULL, fixed_regex_linter())
  expect_lint("str_split(x, fmt)", NULL, fixed_regex_linter())

  # if fixed() is already set, regex patterns don't matter
  expect_lint("str_split(x, fixed('\\\\.'))", NULL, fixed_regex_linter())

  expect_lint(
    "str_split(x, '\\\\.')",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )

  expect_lint(
    "str_split(x, '[.]')",
    rex::rex("For static regular expression patterns, set `fixed = TRUE`."),
    fixed_regex_linter()
  )
})

test_that("str_replace_all's multi-replacement version is handled", {
  # While each of the replacements is fixed, and this _could_ in principle be replaced by
  #   a pipeline where each step does one of the replacements and fixed() is used, this is overkill.
  #   Instead, ensure that no lint is returned for this case
  expect_lint('str_replace_all(x, c("one" = "1", "two" = "2", "three" = "3"))', NULL, fixed_regex_linter())
  expect_lint('grepl(c("a" = "b"), x)', NULL, fixed_regex_linter())
})

test_that("1- or 2-width octal escape sequences are handled", {
  expect_lint('strsplit(x, "\\1")', rex::rex("For static regular expression patterns, set `fixed = TRUE`."), fixed_regex_linter())
})

# TODO(michaelchirico): one difference for stringr functions vs. base is that
#   stringr is much friendlier to piping, so that
#   str %>% str_replace_all("x$", "y")
#   actually doesn't need fixed(), but the logic now is only looking at "y"
#   since it's the second argument and a non-regex string. Similarly,
#   str %>% str_detect("x")
#   is a false negative. thankfully there appear to be few false positives here
