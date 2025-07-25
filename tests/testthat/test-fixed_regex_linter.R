test_that("fixed_regex_linter skips allowed usages", {
  linter <- fixed_regex_linter()

  expect_no_lint("gsub('^x', '', y)", linter)
  expect_no_lint("grep('x$', y)", linter)
  expect_no_lint("grepv('x$', y)", linter)
  expect_no_lint("sub('[a-zA-Z]', '', y)", linter)
  expect_no_lint("grepl(fmt, y)", linter)
  expect_no_lint(R"{regexec('\\s', '', y)}", linter)
  expect_no_lint("grep('a(?=b)', x, perl = TRUE)", linter)
  expect_no_lint("grep('0+1', x, perl = TRUE)", linter)
  expect_no_lint("grep('1*2', x)", linter)
  expect_no_lint("grep('a|b', x)", linter)
  expect_no_lint(R"{grep('\\[|\\]', x)}", linter)

  # if fixed=TRUE is already set, regex patterns don't matter
  expect_no_lint(R"{gsub('\\.', '', y, fixed = TRUE)}", linter)

  # ignore.case=TRUE implies regex interpretation
  expect_no_lint("gsub('abcdefg', '', y, ignore.case = TRUE)", linter)

  # char classes starting with [] might contain other characters -> not fixed
  expect_no_lint("sub('[][]', '', y)", linter)
  expect_no_lint("sub('[][ ]', '', y)", linter)
  expect_no_lint("sub('[],[]', '', y)", linter)

  # wrapper functions don't throw
  expect_no_lint("gregexpr(pattern = pattern, data, perl = TRUE, ...)", linter)
})

test_that("fixed_regex_linter blocks simple disallowed usages", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint(R"{gsub('\\.', '', x)}", lint_msg, linter)
  expect_lint("grepl('abcdefg', x)", lint_msg, linter)
  expect_lint("gregexpr('a-z', y)", lint_msg, linter)
  expect_lint(R"{regexec('\\$', x)}", lint_msg, linter)
  expect_lint("grep('\n', x)", lint_msg, linter)
  expect_lint("grepv('\n', x)", lint_msg, linter)

  # naming the argument doesn't matter (if it's still used positionally)
  expect_lint("gregexpr(pattern = 'a-z', y)", lint_msg, linter)
})

patrick::with_parameters_test_that(
  "fixed_regex_linter is robust to unrecognized escapes error for {char}",
  {
    expect_lint(
      sprintf(R"{grep('\\%s', x)}", char),
      rex::rex("This regular expression is static"),
      fixed_regex_linter()
    )

    expect_lint(
      sprintf(R"{strsplit('a%sb', '\\%s')}", char, char),
      rex::rex("This regular expression is static"),
      fixed_regex_linter()
    )
  },
  char = c(
    "^", "$", "{", "}", "(", ")", ".", "*", "+", "?",
    "|", "[", "]", R"(\\)", "<", ">", "=", ":", ";", "/",
    "_", "-", "!", "@", "#", "%", "&", "~"
  )
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

  expect_no_lint("strsplit(x, '^x')", linter)
  expect_no_lint(R"{strsplit(x, '\\s')}", linter)
  expect_no_lint("strsplit(x, 'a(?=b)', perl = TRUE)", linter)
  expect_no_lint("strsplit(x, '0+1', perl = TRUE)", linter)
  expect_no_lint("strsplit(x, 'a|b')", linter)

  expect_no_lint("tstrsplit(x, '1*2')", linter)
  expect_no_lint("tstrsplit(x, '[a-zA-Z]')", linter)
  expect_no_lint("tstrsplit(x, fmt)", linter)

  # if fixed=TRUE is already set, regex patterns don't matter
  expect_no_lint(R"{strsplit(x, '\\.', fixed = TRUE)}", linter)
  expect_no_lint(R"{strsplit(x, '\\.', fixed = T)}", linter)
})

test_that("fixed_regex_linter catches calls to strsplit as well", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint(R"{strsplit(x, '\\.')}", lint_msg, linter)
  expect_lint("strsplit(x, '[.]')", lint_msg, linter)

  expect_lint("tstrsplit(x, 'abcdefg')", lint_msg, linter)
})

test_that("fixed_regex_linter is more exact about distinguishing \\s from \\:", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_no_lint(R"{grep('\\s', '', x)}", linter)
  expect_lint(R"{grep('\\:', '', x)}", lint_msg, linter)
})

## tests for stringr functions
test_that("fixed_regex_linter skips allowed stringr usages", {
  linter <- fixed_regex_linter()

  expect_no_lint("str_replace(y, '[a-zA-Z]', '')", linter)
  expect_no_lint("str_replace_all(y, '^x', '')", linter)
  expect_no_lint("str_detect(y, fmt)", linter)
  expect_no_lint(R"{str_extract(y, '\\s')}", linter)
  expect_no_lint(R"{str_extract_all(y, '\\s')}", linter)
  expect_no_lint("str_which(x, '1*2')", linter)

  # if fixed() is already set, regex patterns don't matter
  expect_no_lint(R"{str_replace(y, fixed('\\.'), '')}", linter)

  # namespace qualification doesn't matter
  expect_no_lint("stringr::str_replace(y, stringr::fixed('abcdefg'), '')", linter)
})

test_that("fixed_regex_linter blocks simple disallowed usages of stringr functions", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint(R"{str_replace_all(x, '\\.', '')}", lint_msg, linter)
  expect_lint("str_detect(x, 'abcdefg')", lint_msg, linter)
  expect_lint("str_locate(y, 'a-z')", lint_msg, linter)
  expect_lint(R"{str_subset(x, '\\$')}", lint_msg, linter)
  expect_lint("str_which(x, '\n')", lint_msg, linter)

  # named, positional arguments are still caught
  expect_lint("str_locate(y, pattern = 'a-z')", lint_msg, linter)
  # nor do other named arguments throw things off
  expect_lint(R"{str_starts(x, '\\.', negate = TRUE)}", lint_msg, linter)
})

test_that("fixed_regex_linter catches calls to str_split as well", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_no_lint("str_split(x, '^x')", linter)
  expect_no_lint("str_split(x, fmt)", linter)

  # if fixed() is already set, regex patterns don't matter
  expect_no_lint(R"{str_split(x, fixed('\\.'))}", linter)
  expect_lint(R"{str_split(x, '\\.')}", lint_msg, linter)
  expect_lint("str_split(x, '[.]')", lint_msg, linter)
})

test_that("str_replace_all's multi-replacement version is handled", {
  linter <- fixed_regex_linter()

  # While each of the replacements is fixed, and this _could_ in principle be replaced by
  #   a pipeline where each step does one of the replacements and fixed() is used, this is overkill.
  #   Instead, ensure that no lint is returned for this case
  expect_no_lint('str_replace_all(x, c("one" = "1", "two" = "2", "three" = "3"))', linter)
  expect_no_lint('grepl(c("a" = "b"), x)', linter)
})

test_that("1- or 2-width octal escape sequences are handled", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint('strsplit(x, "\\1")', lint_msg, linter)
})

test_that("one-character character classes with escaped characters are caught", {
  linter <- fixed_regex_linter()
  lint_msg <- rex::rex("This regular expression is static")

  expect_lint(R"{gsub('[\n]', '', x)}", lint_msg, linter)
  expect_lint(R"{gsub('[\"]', '', x)}", lint_msg, linter)
  expect_lint(R'{gsub("\\<", "x", x, perl = TRUE)}', lint_msg, linter)

  expect_lint(R"{str_split(x, '[\1]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\12]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\123]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\xa]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\x32]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\uF]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\u01]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\u012]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\u0123]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\U8]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\U1d4d7]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\u{1}]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\U{F7D5}]')}", lint_msg, linter)
  expect_lint(R"{str_split(x, '[\U{1D4D7}]')}", lint_msg, linter)
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
  expect_no_lint(R"{gsub('\n+', '', sql)}", linter)
  expect_no_lint('gsub("\\n{2,}", "\n", D)', linter)
  expect_no_lint(R'{gsub("[\r\n]", "", x)}', linter)
  expect_no_lint(R'{gsub("\n $", "", y)}', linter)
  expect_no_lint(R'{gsub("```\n*```r*\n*", "", x)}', linter)
  expect_no_lint('strsplit(x, "(;|\n)")', linter)
  expect_no_lint(R'{strsplit(x, "(;|\n)")}', linter)
  expect_no_lint(R'{grepl("[\\W]", x, perl = TRUE)}', linter)
  expect_no_lint(R'{grepl("[\\W]", x)}', linter)
})

# make sure the logic is properly vectorized
test_that("single expression with multiple regexes is OK", {
  expect_no_lint('c(grep("^a", x), grep("b$", x))', fixed_regex_linter())
})

test_that("fixed replacements vectorize and recognize str_detect", {
  linter <- fixed_regex_linter()
  # properly vectorized
  expect_lint(
    trim_some(R"({
      grepl('abcdefg', x)
      grepl('a[.]\\.b\n', x)
    })"),
    list(
      list(rex::rex('Use "abcdefg" with fixed = TRUE'), line_number = 2L),
      list(rex::rex('Use "a..b\\n" with fixed = TRUE'), line_number = 3L)
    ),
    linter
  )

  # stringr hint works
  expect_lint(
    "str_detect(x, 'abc')",
    rex::rex('Use stringr::fixed("abc") as the pattern'),
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
    rex::rex('Use "\U1D4D7" with fixed = TRUE'),
    fixed_regex_linter()
  )
})

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
local({
  .cases <- tibble::tribble(
    ~.test_name,             ~regex_expr,             ~fixed_expr,
    "[.]",                   "[.]",                   ".",
    '[\\\"]',                R'([\"])',               '\\"',
    "[]]",                   "[]]",                   "]",
    R"(\\.)",                R"(\\.)",                ".",
    R"(\\:)",                R"(\\:)",                ":",
    R"(\\<)",                R"(\\<)",                "<",
    R"(\\$)",                R"(\\$)",                "$",
    R"([\1])",               R"([\1])",               R"(\001)",
    R"(\1)",                 R"(\1)",                 R"(\001)",
    R"([\12])",              R"([\12])",              R"(\n)",
    R"([\123])",             R"([\123])",             "S",
    "a[*]b",                 "a[*]b",                 "a*b",
    "abcdefg",               "abcdefg",               "abcdefg",
    "abc\\U{A0DEF}ghi",      "abc\\U{A0DEF}ghi",      robust_non_printable_unicode(),
    "a-z",                   "a-z",                   "a-z",
    R"([\n])",               R"([\n])",               R"(\n)",
    R"(\n)",                 "\n",                    R"(\n)",
    R"([\u01])",             R"([\u01])",             R"(\001)",
    R"([\u012])",            R"([\u012])",            R"(\022)",
    R"([\u0123])",           R"([\u0123])",           "\u0123",
    R"([\u{1}])",            R"([\u{1}])",            R"(\001)",
    R"([\U1d4d7])",          R"([\U1d4d7])",          "\U1D4D7",
    R"([\U{1D4D7}])",        R"([\U{1D4D7}])",        "\U1D4D7",
    R"([\U8])",              R"([\U8])",              R"(\b)",
    R"(\u{A0})",             R"(\u{A0})",             "\uA0",
    R"(\u{A0}\U{0001d4d7})", R"(\u{A0}\U{0001d4d7})", "\uA0\U1D4D7",
    R"([\uF])",              R"([\uF])",              R"(\017)",
    R"([\U{F7D5}])",         R"([\U{F7D5}])",         "\UF7D5",
    R"([\x32])",             R"([\x32])",             "2",
    R"([\xa])",              R"([\xa])",              R"(\n)"
  )
  if (.Platform$OS.type == "windows" && !hasName(R.Version(), "crt")) {
    skip_cases <- c(
      # These require UTF-8 support
      "abc\\U{A0DEF}ghi", "[\\U1d4d7]", "[\\U{1D4D7}]", "\\u{A0}\\U{0001d4d7}",
      # R version-specific difference in output message on Windows (probably r80051)
      if (getRversion() == "4.0.4") "[\\U{F7D5}]"
    )
  } else {
    skip_cases <- character()
  }
  patrick::with_parameters_test_that(
    "fixed replacements of {regex_expr} with {fixed_expr} is correct",
    {
      # TODO(google/patrick#19): handle this more cleanly by skipping up-front
      skip_if(
        regex_expr %in% skip_cases,
        sprintf("regex '%s' is not supported on this system", regex_expr)
      )
      expect_lint(
        sprintf("grepl('%s', x)", regex_expr),
        rex::rex(sprintf('Use "%s" with fixed = TRUE', fixed_expr)),
        fixed_regex_linter()
      )
    },
    .cases = .cases
  )
})
# styler: on

test_that("'unescaped' regex can optionally be skipped", {
  linter <- fixed_regex_linter(allow_unescaped = TRUE)

  expect_no_lint("grepl('a', x)", linter)
  expect_no_lint("str_detect(x, 'a')", linter)
  expect_lint("grepl('[$]', x)", rex::rex('Use "$" with fixed = TRUE'), linter)
})

local({
  linter <- fixed_regex_linter()
  lint_msg <- "This regular expression is static"
  pipes <- pipes(exclude = c("%$%", "%T>%"))

  patrick::with_parameters_test_that(
    "linter is pipe-aware",
    {
      expect_lint(paste("x", pipe, "grepl(pattern = 'a')"), lint_msg, linter)
      expect_no_lint(paste("x", pipe, "grepl(pattern = '^a')"), linter)
      expect_no_lint(paste("x", pipe, "grepl(pattern = 'a', fixed = TRUE)"), linter)
      expect_lint(paste("x", pipe, "str_detect('a')"), lint_msg, linter)
      expect_no_lint(paste("x", pipe, "str_detect('^a')"), linter)
      expect_no_lint(paste("x", pipe, "str_detect(fixed('a'))"), linter)

      expect_lint(paste("x", pipe, "gsub(pattern = 'a', replacement = '')"), lint_msg, linter)
      expect_no_lint(paste("x", pipe, "gsub(pattern = '^a', replacement = '')"), linter)
      expect_no_lint(paste("x", pipe, "gsub(pattern = 'a', replacement = '', fixed = TRUE)"), linter)
      expect_lint(paste("x", pipe, "str_replace('a', '')"), lint_msg, linter)
      expect_no_lint(paste("x", pipe, "str_replace('^a', '')"), linter)
      expect_no_lint(paste("x", pipe, "str_replace(fixed('a'), '')"), linter)
    },
    pipe = pipes,
    .test_name = names(pipes)
  )
})

test_that("pipe-aware lint logic survives adversarial comments", {
  expect_lint(
    trim_some("
      x %>%   grepl(pattern = # comment
      'a')
    "),
    "This regular expression is static",
    fixed_regex_linter()
  )
})
