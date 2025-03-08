test_that("literal_coercion_linter skips allowed usages", {
  linter <- literal_coercion_linter()

  # naive xpath includes the "_f0" here as a literal
  expect_no_lint('as.numeric(x$"_f0")', linter)
  expect_no_lint('as.numeric(x@"_f0")', linter)
  # only examine the first method for as.<type> methods
  expect_no_lint("as.character(as.Date(x), '%Y%m%d')", linter)

  # we are as yet agnostic on whether to prefer literals over coerced vectors
  expect_no_lint("as.integer(c(1, 2, 3))", linter)
  # even more ambiguous for character vectors like here, where quotes are much
  #   more awkward to type than a sequence of numbers
  expect_no_lint("as.character(c(1, 2, 3))", linter)
  # not possible to declare raw literals
  expect_no_lint("as.raw(c(1, 2, 3))", linter)
  # also not taking a stand on as.complex(0) vs. 0 + 0i
  expect_no_lint("as.complex(0)", linter)
  # ditto for as.integer(1e6) vs. 1000000L
  expect_no_lint("as.integer(1e6)", linter)
  # ditto for as.numeric(1:3) vs. c(1, 2, 3)
  expect_no_lint("as.numeric(1:3)", linter)
})

test_that("literal_coercion_linter skips allowed rlang usages", {
  linter <- literal_coercion_linter()

  expect_no_lint("int(1, 2.0, 3)", linter)
  expect_no_lint("chr('e', 'ab', 'xyz')", linter)
  expect_no_lint("lgl(0, 1)", linter)
  expect_no_lint("lgl(0L, 1)", linter)
  expect_no_lint("dbl(1.2, 1e5, 3L, 2E4)", linter)
  # make sure using namespace (`rlang::`) doesn't create problems
  expect_no_lint("rlang::int(1, 2, 3)", linter)
  # even if scalar, carve out exceptions for the following
  expect_no_lint("int(1.0e6)", linter)
})

test_that("literal_coercion_linter skips quoted keyword arguments", {
  expect_no_lint("as.numeric(foo('a' = 1))", literal_coercion_linter())
})

test_that("no warnings surfaced by running coercion", {
  linter <- literal_coercion_linter()

  expect_no_warning(
    expect_lint("as.integer('a')", "Use NA_integer_", linter)
  )

  expect_no_warning(
    expect_lint("as.integer(2147483648)", "Use NA_integer_", linter)
  )

  expect_no_warning(
    expect_lint(
      trim_some("
        as.double(
          NA # comment
        )
      "),
      "Use NA_real_",
      linter
    )
  )
})

skip_if_not_installed("tibble")
patrick::with_parameters_test_that(
  "literal_coercion_linter blocks simple disallowed usages",
  expect_lint(
    sprintf("as.%s(%s)", out_type, input),
    lint_msg,
    literal_coercion_linter()
  ),
  .cases = tibble::tribble(
    ~.test_name,     ~out_type,   ~input,   ~lint_msg,
    "lgl, from int", "logical",   "1L",     rex::rex("Use TRUE instead of as.logical(1L)"),
    "lgl, from num", "logical",   "1",      rex::rex("Use TRUE instead of as.logical(1)"),
    "lgl, from chr", "logical",   '"true"', rex::rex('Use TRUE instead of as.logical("true")'),
    "int, from num", "integer",   "1",      rex::rex("Use 1L instead of as.integer(1)"),
    "num, from num", "numeric",   "1",      rex::rex("Use 1 instead of as.numeric(1)"),
    "dbl, from num", "double",    "1",      rex::rex("Use 1 instead of as.double(1)"),
    "chr, from num", "character", "1",      rex::rex('Use "1" instead of as.character(1)'),
    "chr, from chr", "character", '"e"',    rex::rex('Use "e" instead of as.character("e")'),
    "chr, from chr", "character", '"E"',    rex::rex('Use "E" instead of as.character("E")'),
    # affirmatively lint as.<type>(NA) should be NA_<type>_
    "int, from NA",  "integer",   "NA",     rex::rex("Use NA_integer_ instead of as.integer(NA)"),
    "num, from NA",  "numeric",   "NA",     rex::rex("Use NA_real_ instead of as.numeric(NA)"),
    "dbl, from NA",  "double",    "NA",     rex::rex("Use NA_real_ instead of as.double(NA)"),
    "chr, from NA",  "character", "NA",     rex::rex("Use NA_character_ instead of as.character(NA)")
  )
)

skip_if_not_installed("rlang")
test_that("multiple lints return custom messages", {
  linter <- literal_coercion_linter()
  expect_lint(
    trim_some("{
      as.integer(1)
      lgl(1L)
    }"),
    list(
      list(rex::rex("Use 1L instead of as.integer(1)"), line_number = 2L),
      list(rex::rex("Use TRUE instead of lgl(1L)"), line_number = 3L)
    ),
    linter
  )

  # also ensure comment remove logic works across several lints
  expect_lint(
    trim_some("{
      as.integer( # comment
      1           # comment
      )           # comment
      lgl(        # comment
      1L          # comment
      )           # comment
    }"),
    list(
      list(rex::rex("Use 1L instead of as.integer(1)"), line_number = 2L),
      list(rex::rex("Use TRUE instead of lgl(1L)"), line_number = 5L)
    ),
    linter
  )
})

patrick::with_parameters_test_that(
  "literal_coercion_linter blocks rlang disallowed usages",
  expect_lint(
    sprintf("%s(%s)", out_type, input),
    lint_msg,
    literal_coercion_linter()
  ),
  # even if `as.character(1)` works, `chr(1)` doesn't, so no corresponding test case
  .cases = tibble::tribble(
    ~.test_name,      ~out_type,    ~input, ~lint_msg,
    "rlang::lgl",     "lgl",        "1L",   rex::rex("Use TRUE instead of lgl(1L)"),
    "rlang::lgl[ns]", "rlang::lgl", "1L",   rex::rex("Use TRUE instead of rlang::lgl(1L)"),
    "rlang::int",     "int",        "1.0",  rex::rex("Use 1L instead of int(1.0)"),
    "rlang::dbl",     "dbl",        "1L",   rex::rex("Use 1 instead of dbl(1L)"),
    "rlang::chr",     "chr",        '"e"',  rex::rex('Use "e" instead of chr("e")'),
    "rlang::chr",     "chr",        '"E"',  rex::rex('Use "E" instead of chr("E")')
  )
)

test_that("literal_coercion_linter blocks scalar rlang list2 construction", {
  expect_lint(
    "int(1, )",
    rex::rex("Use 1L instead of int(1,)"),
    literal_coercion_linter()
  )
})
