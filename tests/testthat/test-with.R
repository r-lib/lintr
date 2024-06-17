test_that("modify_defaults produces error with missing or incorrect defaults", {
  expect_error(
    modify_defaults(),
    "`defaults` is a required argument, but is missing",
    fixed = TRUE
  )
  expect_error(
    modify_defaults("assignment_linter"),
    "`defaults` must be a named list",
    fixed = TRUE
  )
})

test_that("linters_with_tags produces error with incorrect tags", {
  expect_error(linters_with_tags(1L:4L), "`tags` must be a character vector, or `NULL`", fixed = TRUE)
})

test_that("linters_with_defaults works as expected with unnamed args", {
  # assignment_linter is in defaults, so output doesn't change
  expect_named(linters_with_defaults(assignment_linter), names(linters_with_defaults()))
})

test_that("linters_with_defaults warns on unused NULLs", {
  expect_warning(linters_with_defaults(not_a_default = NULL), rex::rex("which is not in `defaults`."))
  expect_warning(
    linters_with_defaults(not_a_default = NULL, also_not_default = NULL),
    rex::rex("which are not in `defaults`.")
  )
})

test_that("linters_with_tags() verifies the output of available_linters()", {
  local_mocked_bindings(
    available_linters = function(...) {
      data.frame(linter = c("fake_linter", "very_fake_linter"), package = "lintr", tags = "")
    }
  )
  expect_error(
    linters_with_tags(NULL),
    "Can't find linters `fake_linter()` and `very_fake_linter()`",
    fixed = TRUE
  )
})

test_that("all default linters are tagged default", {
  expect_named(linters_with_defaults(), available_linters(tags = "default")$linter)

  # covr modifies package functions causing differing deparse() results even for identical anonymous functions.
  # This happens because default_linters is generated at build time and thus not modifiable by covr, whereas
  # linters_with_tags() constructs the linters at runtime.
  skip_on_covr()

  expect_identical(linters_with_tags("default"), linters_with_defaults())
  expect_length(linters_with_tags(NULL, exclude_tags = available_tags()), 0L)

  # Check that above test also trips on default arguments.
  skip_if_not_r_version("4.1.0") # Desired all.equal behavior only available in >= 4.1
  expect_identical(
    all.equal(linters_with_tags("default"), linters_with_defaults(line_length_linter(120L))),
    c(
      'Component "line_length_linter": Component "general_msg": 1 string mismatch',
      'Component "line_length_linter": Component "length": Mean relative difference: 0.5'
    )
  )
})

test_that("can instantiate all linters without arguments", {
  all_linters <- linters_with_tags(tags = NULL)

  expect_type(all_linters, "list")
  expect_length(all_linters, nrow(available_linters()))

  really_all_linters <- suppressWarnings(linters_with_tags(tags = NULL, exclude_tags = NULL))
  expect_type(really_all_linters, "list")
  expect_length(really_all_linters, nrow(available_linters(exclude_tags = NULL)))
})

test_that("with_defaults is fully deprecated", {
  expect_error(
    with_defaults(),
    rex::rex("Use linters_with_defaults or modify_defaults instead.")
  )
})

test_that("modify_defaults works", {
  my_default <- list(a = 1L, b = 2L, c = 3L)
  expect_identical(modify_defaults(defaults = my_default), my_default)
  expect_identical(modify_defaults(defaults = my_default, a = 2L), list(a = 2L, b = 2L, c = 3L))
  expect_identical(modify_defaults(defaults = my_default, c = NULL), list(a = 1L, b = 2L))

  # auto-sorts
  expect_identical(modify_defaults(defaults = list(b = 2L, a = 1L), c = 3L), my_default)
})

test_that("linters_with_defaults(default = .) is supported with a deprecation warning", {
  expect_warning(
    {
      linters <- linters_with_defaults(default = list(), whitespace_linter())
    },
    "`default` is not an argument"
  )
  expect_named(linters, "whitespace_linter")

  # the same warning is not triggered in modify_defaults
  expect_silent({
    linters <- modify_defaults(defaults = list(), default = list(), whitespace_linter())
  })
  expect_named(linters, c("default", "whitespace_linter"))

  # if default= is explicitly provided alongside defaults=, assume that was intentional
  default <- Linter(function(.) list())
  expect_silent({
    linters <- linters_with_defaults(defaults = list(), default = default)
  })
  expect_named(linters, "default")
})

test_that("all_linters contains all available linters", {
  all_linters <- all_linters(packages = "lintr")

  expect_identical(linters_with_tags(NULL, packages = "lintr"), all_linters)
  expect_length(all_linters, nrow(available_linters()))
})

test_that("all_linters respects ellipsis argument", {
  expect_identical(
    linters_with_tags(tags = NULL, implicit_integer_linter = NULL),
    all_linters(packages = "lintr", implicit_integer_linter = NULL)
  )
})
