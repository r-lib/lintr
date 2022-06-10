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

test_that("all default linters are tagged default", {
  expect_named(linters_with_defaults(), available_linters(tags = "default")$linter)

  # TODO(michaelchirico): use plain expect_equal after waldo#133 makes it into a CRAN release
  # Here, the environment()s are different because factories use them.
  skip_if_not_r_version("4.1.0") # Desired all.equal behaviour only available in >= 4.1
  # covr modifies package functions causing differing deparse() results even for identical anonymous functions.
  # This happens because default_linters is generated at build time and thus not modifiable by covr, whereas
  # linters_with_tags() constructs the linters at runtime.
  skip_if(covr::in_covr())

  expect_true(all.equal(linters_with_tags("default"), linters_with_defaults()))
  expect_length(linters_with_tags("default", exclude_tags = "default"), 0L)

  # Check that above test also trips on default arguments.
  expect_equal(
    all.equal(linters_with_tags("default"), linters_with_defaults(line_length_linter(120L))),
    'Component "line_length_linter": Component "length": Mean relative difference: 0.5'
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

test_that("with_defaults is supported with a deprecation warning", {
  defaults <- linters_with_defaults()
  expect_warning(
    old_defaults <- with_defaults(),
    rex::rex("Use linters_with_defaults instead.")
  )
  expect_identical(defaults, old_defaults)

  # linters_with_defaults only accepts `defaults = list()` to start from blank
  defaults <- linters_with_defaults(defaults = list(), no_tab_linter())
  expect_warning(
    old_defaults <- with_defaults(default = NULL, no_tab_linter()),
    rex::rex("Use linters_with_defaults instead.")
  )
  expect_identical(defaults, old_defaults)
})

test_that("modify_defaults works", {
  my_default <- list(a = 1L, b = 2L, c = 3L)
  expect_equal(modify_defaults(defaults = my_default), my_default)
  expect_equal(modify_defaults(defaults = my_default, a = 2L), list(a = 2L, b = 2L, c = 3L))
  expect_equal(modify_defaults(defaults = my_default, c = NULL), list(a = 1L, b = 2L))

  # auto-sorts
  expect_equal(modify_defaults(defaults = list(b = 2L, a = 1L), c = 3L), my_default)
})

test_that("linters_with_defaults(default = .) is supported with a deprecation warning", {
  expect_warning(linters <- linters_with_defaults(default = list(), no_tab_linter()), "'default'")
  expect_named(linters, "no_tab_linter")

  # the same warning is not triggered in modify_defaults
  expect_silent(linters <- modify_defaults(defaults = list(), default = list(), no_tab_linter()))
  expect_named(linters, c("default", "no_tab_linter"))

  # if default= is explicitly provided alongside defaults=, assume that was intentional
  default <- Linter(function(.) list())
  expect_silent(linters <- linters_with_defaults(defaults = list(), default = default))
  expect_named(linters, "default")
})
