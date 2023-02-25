# NB: escaping is confusing. We have to double-escape everything -- the first
#   escape creates a string that will be parse()d, the second escape is normal
#   escaping that would be done in R code. E.g. in "\\\\.", the R code would
#   read like "\\.", but in order to create those two slashes, we need to write
#   "\\\\." in the string here.

test_that("fixed_regex_linter skips allowed usages", {
  linter <- fixed_regex_linter()

  expect_lint("gsub('^x', '', y)", NULL, linter)
  expect_lint("grep('x$', '', y)", NULL, linter)
  expect_lint("sub('[a-zA-Z]', '', y)", NULL, linter)
  expect_lint("grepl(fmt, y)", NULL, linter)
  expect_lint("regexec('\\\\s', '', y)", NULL, linter)
  expect_lint("grep('a(?=b)', x, perl = TRUE)", NULL, linter)
  expect_lint("grep('0+1', x, perl = TRUE)", NULL, linter)
  expect_lint("grep('1*2', x)", NULL, linter)
  expect_lint("grep('a|b', x)", NULL, linter)
  expect_lint("grep('\\\\[|\\\\]', x)", NULL, linter)

  # if fixed=TRUE is already set, regex patterns don't matter
  expect_lint("gsub('\\\\.', '', y, fixed = TRUE)", NULL, linter)

  # ignore.case=TRUE implies regex interpretation
  expect_lint("gsub('abcdefg', '', y, ignore.case = TRUE)", NULL, linter)

  # char classes starting with [] might contain other characters -> not fixed
  expect_lint("sub('[][]', '', y)", NULL, linter)
  expect_lint("sub('[][ ]', '', y)", NULL, linter)
  expect_lint("sub('[],[]', '', y)", NULL, linter)
})

test_that("fixed_regex_linter blocks simple disallowed usages", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint("gsub('\\\\.', '', x)", lint_msg, linter)
  expect_lint("grepl('abcdefg', x)", lint_msg, linter)
  expect_lint("gregexpr('a-z', y)", lint_msg, linter)
  expect_lint("regexec('\\\\$', x)", lint_msg, linter)
  expect_lint("grep('\n', x)", lint_msg, linter)

  # naming the argument doesn't matter (if it's still used positionally)
  expect_lint("gregexpr(pattern = 'a-z', y)", lint_msg, linter)
})

patrick::with_parameters_test_that(
  "fixed_regex_linter is robust to unrecognized escapes error",
  {
    expect_lint(
      sprintf("grep('\\\\%s', x)", char),
      rex::rex("This regular expression is static"),
      fixed_regex_linter()
    )

    expect_lint(
      sprintf("strsplit('a%sb', '\\\\%s')", char, char),
      rex::rex("This regular expression is static"),
      fixed_regex_linter()
    )
  },
  .cases = local({
    char <- c(
      "^", "$", "{", "}", "(", ")", ".", "*", "+", "?",
      "|", "[", "]", "\\\\", "<", ">", "=", ":", ";", "/",
      "_", "-", "!", "@", "#", "%", "&", "~"
    )
    data.frame(char = char, .test_name = char, stringsAsFactors = FALSE)
  })
)

test_that("fixed_regex_linter catches regex like [.] or [$]", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint("grep('[.]', x)", lint_msg, linter)
  expect_lint("grepl('a[*]b', x)", lint_msg, linter)

  # also catch char classes for [ and ]
  expect_lint("gregexpr('[]]', x)", lint_msg, linter)
})

test_that("fixed_regex_linter catches null calls to strsplit as well", {
  linter <- fixed_regex_linter()

  expect_lint("strsplit(x, '^x')", NULL, linter)
  expect_lint("strsplit(x, '\\\\s')", NULL, linter)
  expect_lint("strsplit(x, 'a(?=b)', perl = TRUE)", NULL, linter)
  expect_lint("strsplit(x, '0+1', perl = TRUE)", NULL, linter)
  expect_lint("strsplit(x, 'a|b')", NULL, linter)

  expect_lint("tstrsplit(x, '1*2')", NULL, linter)
  expect_lint("tstrsplit(x, '[a-zA-Z]')", NULL, linter)
  expect_lint("tstrsplit(x, fmt)", NULL, linter)

  # if fixed=TRUE is already set, regex patterns don't matter
  expect_lint("strsplit(x, '\\\\.', fixed = TRUE)", NULL, linter)
  expect_lint("strsplit(x, '\\\\.', fixed = T)", NULL, linter)
})

test_that("fixed_regex_linter catches calls to strsplit as well", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint("strsplit(x, '\\\\.')", lint_msg, linter)
  expect_lint("strsplit(x, '[.]')", lint_msg, linter)

  expect_lint("tstrsplit(x, 'abcdefg')", lint_msg, linter)
})

test_that("fixed_regex_linter is more exact about distinguishing \\s from \\:", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint("grep('\\\\s', '', x)", NULL, linter)
  expect_lint("grep('\\\\:', '', x)", lint_msg, linter)
})

## tests for stringr functions
test_that("fixed_regex_linter skips allowed stringr usages", {
  linter <- fixed_regex_linter()

  expect_lint("str_replace(y, '[a-zA-Z]', '')", NULL, linter)
  expect_lint("str_replace_all(y, '^x', '')", NULL, linter)
  expect_lint("str_detect(y, fmt)", NULL, linter)
  expect_lint("str_extract(y, '\\\\s')", NULL, linter)
  expect_lint("str_extract_all(y, '\\\\s')", NULL, linter)
  expect_lint("str_which(x, '1*2')", NULL, linter)

  # if fixed() is already set, regex patterns don't matter
  expect_lint("str_replace(y, fixed('\\\\.'), '')", NULL, linter)

  # namespace qualification doesn't matter
  expect_lint("stringr::str_replace(y, stringr::fixed('abcdefg'), '')", NULL, linter)
})

test_that("fixed_regex_linter blocks simple disallowed usages of stringr functions", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint("str_replace_all(x, '\\\\.', '')", lint_msg, linter)
  expect_lint("str_detect(x, 'abcdefg')", lint_msg, linter)
  expect_lint("str_locate(y, 'a-z')", lint_msg, linter)
  expect_lint("str_subset(x, '\\\\$')", lint_msg, linter)
  expect_lint("str_which(x, '\n')", lint_msg, linter)

  # named, positional arguments are still caught
  expect_lint("str_locate(y, pattern = 'a-z')", lint_msg, linter)
  # nor do other named arguments throw things off
  expect_lint("str_starts(x, '\\\\.', negate = TRUE)", lint_msg, linter)
})

test_that("fixed_regex_linter catches calls to str_split as well", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint("str_split(x, '^x')", NULL, linter)
  expect_lint("str_split(x, fmt)", NULL, linter)

  # if fixed() is already set, regex patterns don't matter
  expect_lint("str_split(x, fixed('\\\\.'))", NULL, linter)
  expect_lint("str_split(x, '\\\\.')", lint_msg, linter)
  expect_lint("str_split(x, '[.]')", lint_msg, linter)
})

test_that("str_replace_all's multi-replacement version is handled", {
  linter <- fixed_regex_linter()

  # While each of the replacements is fixed, and this _could_ in principle be replaced by
  #   a pipeline where each step does one of the replacements and fixed() is used, this is overkill.
  #   Instead, ensure that no lint is returned for this case
  expect_lint('str_replace_all(x, c("one" = "1", "two" = "2", "three" = "3"))', NULL, linter)
  expect_lint('grepl(c("a" = "b"), x)', NULL, linter)
})

test_that("1- or 2-width octal escape sequences are handled", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint('strsplit(x, "\\1")', lint_msg, linter)
})

test_that("one-character character classes with escaped characters are caught", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint("gsub('[\\n]', '', x)", lint_msg, linter)
  expect_lint("gsub('[\\\"]', '', x)", lint_msg, linter)
  expect_lint('gsub("\\\\<", "x", x, perl = TRUE)', lint_msg, linter)

  expect_lint("str_split(x, '[\\1]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\12]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\123]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\xa]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\x32]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\uF]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\u01]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\u012]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\u0123]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\U8]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\U1d4d7]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\u{1}]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\U{F7D5}]')", lint_msg, linter)
  expect_lint("str_split(x, '[\\U{1D4D7}]')", lint_msg, linter)
})

test_that("bracketed unicode escapes are caught", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint('gsub("\\u{A0}", " ", out, useBytes = TRUE)', lint_msg, linter)
  expect_lint('gsub("abc\\U{A0DEF}ghi", " ", out, useBytes = TRUE)', lint_msg, linter)
  expect_lint('gsub("\\u{A0}\\U{0001d4d7}", " ", out, useBytes = TRUE)', lint_msg, linter)
})

test_that("escaped characters are handled correctly", {
  linter <- fixed_regex_linter()
  expect_lint("gsub('\\n+', '', sql)", NULL, linter)
  expect_lint('gsub("\\n{2,}", "\n", D)', NULL, linter)
  expect_lint('gsub("[\\r\\n]", "", x)', NULL, linter)
  expect_lint('gsub("\\n $", "", y)', NULL, linter)
  expect_lint('gsub("```\\n*```r*\\n*", "", x)', NULL, linter)
  expect_lint('strsplit(x, "(;|\n)")', NULL, linter)
  expect_lint('strsplit(x, "(;|\\n)")', NULL, linter)
  expect_lint('grepl("[\\\\W]", x, perl = TRUE)', NULL, linter)
  expect_lint('grepl("[\\\\W]", x)', NULL, linter)
})

# make sure the logic is properly vectorized
test_that("single expression with multiple regexes is OK", {
  expect_lint('c(grep("^a", x), grep("b$", x))', NULL, fixed_regex_linter())
})

test_that("fixed replacements vectorize and recognize str_detect", {
  linter <- fixed_regex_linter()
  # properly vectorized
  expect_lint(
    trim_some("
      c(
        grepl('abcdefg', x),
        grepl('a[.]\\\\.b\\n', x)
      )
    "),
    list(
      rex::rex('Here, you can use "abcdefg" with fixed = TRUE'),
      rex::rex('Here, you can use "a..b\\n" with fixed = TRUE')
    ),
    linter
  )

  # stringr hint works
  expect_lint(
    "str_detect(x, 'abc')",
    rex::rex('Here, you can use stringr::fixed("abc") as the pattern'),
    linter
  )
})

test_that("fixed replacement is correct with UTF-8", {
  skip_if(
    .Platform$OS.type == "windows" && !hasName(R.Version(), "crt"),
    message = "UTF-8 support is required"
  )

  expect_lint(
    "grepl('[\\U{1D4D7}]', x)",
    rex::rex('Here, you can use "\U1D4D7" with fixed = TRUE'),
    fixed_regex_linter()
  )
})

# TODO(michaelchirico): one difference for stringr functions vs. base is that
#   stringr is much friendlier to piping, so that
#   > str %>% str_replace_all("x$", "y")
#   actually doesn't need fixed(), but the logic now is only looking at "y"
#   since it's the second argument and a non-regex string. Similarly,
#   > str %>% str_detect("x")
#   is a false negative. thankfully there appear to be few false positives here

# TODO(michaelchirico): we could in principle build in logic to detect whether
#   perl=TRUE and interpret "regex or not" accordingly. One place
#   up in practice is for '\<', which is a special character in default
#   regex but not in PCRE. Empirically relevant for HTML-related regex e.g. \\<li\\>

#' Generate a string with a non-printable Unicode entry robust to test environment
#'
#' Non-printable unicode behaves wildly different with `encodeString()`
#'   across R versions and platforms. Nonetheless, all of these expressions
#'   are valid replacements.
#' @noRd
robust_non_printable_unicode <- function() {
  if (getRversion() < "4.1.0") {
    "abc\\U000a0defghi"
  } else if (.Platform$OS.type == "windows") {
    "abc\U{0a0def}ghi"
  } else {
    "abc\\U{0a0def}ghi"
  }
}

# styler: off
patrick::with_parameters_test_that("fixed replacements are correct", {
  skip_if(
    regex_expr %in% c("abc\\U{A0DEF}ghi", "[\\U1d4d7]", "[\\U{1D4D7}]", "\\u{A0}\\U{0001d4d7}") &&
      .Platform$OS.type == "windows" &&
      !hasName(R.Version(), "crt"),
    message = "UTF-8 support is required"
  )
  expect_lint(
    sprintf("grepl('%s', x)", regex_expr),
    rex::rex(sprintf('Here, you can use "%s" with fixed = TRUE', fixed_expr)),
    fixed_regex_linter()
  )
}, .cases = tibble::tribble(
  ~.test_name,            ~regex_expr,            ~fixed_expr,
  "[.]",                  "[.]",                  ".",
  '[\\\"]',               '[\\\"]',               '\\"',
  "[]]",                  "[]]",                  "]",
  "\\\\.",                "\\\\.",                ".",
  "\\\\:",                "\\\\:",                ":",
  "\\\\<",                "\\\\<",                "<",
  "\\\\$",                "\\\\$",                "$",
  "[\\1]",                "[\\1]",                "\\001",
  "\\1",                  "\\1",                  "\\001",
  "[\\12]",               "[\\12]",               "\\n",
  "[\\123]",              "[\\123]",              "S",
  "a[*]b",                "a[*]b",                "a*b",
  "abcdefg",              "abcdefg",              "abcdefg",
  "abc\\U{A0DEF}ghi",     "abc\\U{A0DEF}ghi",     robust_non_printable_unicode(),
  "a-z",                  "a-z",                  "a-z",
  "[\\n]",                "[\\n]",                "\\n",
  "\\n",                  "\n",                   "\\n",
  "[\\u01]",              "[\\u01]",              "\\001",
  "[\\u012]",             "[\\u012]",             "\\022",
  "[\\u0123]",            "[\\u0123]",            "\u0123",
  "[\\u{1}]",             "[\\u{1}]",             "\\001",
  "[\\U1d4d7]",           "[\\U1d4d7]",           "\U1D4D7",
  "[\\U{1D4D7}]",         "[\\U{1D4D7}]",         "\U1D4D7",
  "[\\U8]",               "[\\U8]",               "\\b",
  "\\u{A0}",              "\\u{A0}",              "\uA0",
  "\\u{A0}\\U{0001d4d7}", "\\u{A0}\\U{0001d4d7}", "\uA0\U1D4D7",
  "[\\uF]",               "[\\uF]",               "\\017",
  "[\\U{F7D5}]",          "[\\U{F7D5}]",          "\UF7D5",
  "[\\x32]",              "[\\x32]",              "2",
  "[\\xa]",               "[\\xa]",               "\\n"
))
# styler: on
