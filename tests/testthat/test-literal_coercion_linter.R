test_that("literal_coercion_linter skips allowed usages", {
  # naive xpath includes the "_f0" here as a literal
  expect_lint('as.numeric(x$"_f0")', NULL, literal_coercion_linter())
  # only examine the first method for as.<type> methods
  expect_lint("as.character(as.Date(x), '%Y%m%d')", NULL, literal_coercion_linter())

  # we are as yet agnostic on whether to prefer literals over coerced vectors
  expect_lint("as.integer(c(1, 2, 3))", NULL, literal_coercion_linter())
  # even more ambiguous for character vectors like here, where quotes are much
  #   more awkward to type than a sequence of numbers
  expect_lint("as.character(c(1, 2, 3))", NULL, literal_coercion_linter())
  # not possible to declare raw literals
  expect_lint("as.raw(c(1, 2, 3))", NULL, literal_coercion_linter())
  # also not taking a stand on as.complex(0) vs. 0 + 0i
  expect_lint("as.complex(0)", NULL, literal_coercion_linter())
  # ditto for as.integer(1e6) vs. 1000000L
  expect_lint("as.integer(1e6)", NULL, literal_coercion_linter())
  # ditto for as.numeric(1:3) vs. c(1, 2, 3)
  expect_lint("as.numeric(1:3)", NULL, literal_coercion_linter())
})

skip_if_not_installed("patrick")
patrick::with_parameters_test_that(
  "literal_coercion_linter blocks simple disallowed usages",
  expect_lint(
    sprintf("as.%s(%s)", out_type, input),
    rex::rex("Use literals directly where possible, instead of coercion."),
    literal_coercion_linter()
  ),
  .cases = tibble::tribble(
        ~.test_name,   ~out_type,   ~input,
    "lgl, from int",   "logical",     "1L",
    "lgl, from num",   "logical",      "1",
    "lgl, from chr",   "logical", '"true"',
    "int, from num",   "integer",      "1",
    "num, from num",   "numeric",      "1",
    "dbl, from num",    "double",      "1",
    "chr, from num", "character",      "1",
    # affirmatively lint as.<type>(NA) should be NA_<type>_
    "int, from NA",    "integer",     "NA",
    "num, from NA",    "numeric",     "NA",
    "dbl, from NA",     "double",     "NA",
    "chr, from NA",  "character",     "NA",
  )
)

test_that("literal_coercion_linter skips quoted keyword arguments", {
  expect_lint("as.numeric(foo('a' = 1))", NULL, literal_coercion_linter())
})
