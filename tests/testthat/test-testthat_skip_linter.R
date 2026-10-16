test_that("testthat_skip_linter skips allowed usages", {
  linter <- testthat_skip_linter()

  expect_no_lint("test_that('works', { skip_if(cond); expect_true(TRUE) })", linter)
  expect_no_lint("test_that('works', { skip_if_not(cond); expect_true(TRUE) })", linter)
  expect_no_lint("skip('unconditional skip')", linter)
  expect_no_lint("skip_if_not_installed('pkg')", linter)
  expect_no_lint("skip_if_not_installed('pkg', minimum_version = '1.0.0')", linter)
  expect_no_lint("skip_unless_r('>= 4.1.0')", linter)
  expect_no_lint("skip_on_os('windows')", linter)
  expect_no_lint("skip_on_cran()", linter)
  expect_no_lint("skip_on_ci()", linter)
  expect_no_lint("skip_on_covr()", linter)
  expect_no_lint("skip_on_bioc()", linter)

  # Compound OS + CRT check cannot be replaced by skip_on_os("windows") alone
  expect_no_lint(
    "skip_if(.Platform$OS.type == 'windows' && !hasName(R.Version(), 'crt'))",
    linter
  )

  # Early return inside a helper function defined inside test_that() is fine
  expect_no_lint(
    trim_some("
      test_that('helper return is allowed', {
        helper <- function(x) {
          if (is.null(x)) return(NULL)
          x + 1
        }
        expect_identical(helper(1), 2)
      })
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      test_that('lambda return is allowed', {
        res <- lapply(xs, \\(x) {
          if (is.na(x)) return(0)
          x
        })
        expect_length(res, 2L)
      })
    "),
    linter
  )

  # if (...) { skip(); ... } with multiple statements or an else branch is not a simple skip_if()
  expect_no_lint(
    trim_some("
      if (cond) {
        cleanup()
        skip('reason')
      }
    "),
    linter
  )
  expect_no_lint("if (cond) skip('a') else do_something()", linter)
})

test_that("testthat_skip_linter blocks wrapping test_that() in if () and early return() in test_that()", {
  linter <- testthat_skip_linter()

  expect_lint(
    trim_some("
      if (requireNamespace('pkg', quietly = TRUE)) {
        test_that('my test', {
          expect_true(TRUE)
        })
      }
    "),
    list(
      rex::rex("Use skip_if() or skip_if_not() inside test_that() instead of wrapping test_that() in an if () block."),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )

  expect_lint(
    trim_some("
      test_that('my test', {
        if (!has_feature()) return()
        expect_true(TRUE)
      })
    "),
    list(
      rex::rex("Use skip_if() or skip_if_not() instead of returning early with if (...) return() inside test_that()."),
      line_number = 2L,
      column_number = 3L
    ),
    linter
  )

  expect_lint(
    trim_some("
      test_that('my test', {
        if (!has_feature()) {
          return()
        }
        expect_true(TRUE)
      })
    "),
    list(
      rex::rex("Use skip_if() or skip_if_not() instead of returning early with if (...) return() inside test_that()."),
      line_number = 2L,
      column_number = 3L
    ),
    linter
  )
})

test_that("testthat_skip_linter blocks if (...) skip(...) and negated skip_if(!x) / skip_if_not(!x)", {
  linter <- testthat_skip_linter()

  expect_lint(
    "if (is_broken) skip('broken')",
    list(
      rex::rex("Use skip_if() instead of if (...) skip()."),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )
  expect_lint(
    "if (!is_ready) { skip('not ready') }",
    list(
      rex::rex("Use skip_if_not() instead of if (!...) skip()."),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )

  expect_lint(
    "skip_if(!is_ready)",
    list(
      rex::rex("Use skip_if_not(x) instead of skip_if(!x)."),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )
  expect_lint(
    "skip_if_not(!is_broken, message = 'broken')",
    list(
      rex::rex("Use skip_if(x) instead of skip_if_not(!x)."),
      line_number = 1L,
      column_number = 1L
    ),
    linter
  )
  expect_lint(
    "skip_if(condition = !is_ready)",
    rex::rex("Use skip_if_not(x) instead of skip_if(!x)."),
    linter
  )
})

test_that("testthat_skip_linter recommends dedicated skip_*() helpers", {
  linter <- testthat_skip_linter()

  # Dedicated skip_unless_r() helper
  expect_lint(
    "skip_if(getRversion() < '4.1.0')",
    rex::rex("Use skip_unless_r() instead of comparing getRversion() in skip_if()."),
    linter
  )
  expect_lint(
    "skip_if_not(getRversion() >= '4.1.0')",
    rex::rex("Use skip_unless_r() instead of comparing getRversion() in skip_if_not()."),
    linter
  )
  expect_lint(
    "if (getRversion() < '4.1.0') skip('needs R 4.1')",
    rex::rex("Use skip_unless_r() instead of comparing getRversion() in if (...) skip()."),
    linter
  )

  # Dedicated skip_if_not_installed() helper
  expect_lint(
    "skip_if(!requireNamespace('pkg', quietly = TRUE))",
    rex::rex("Use skip_if_not_installed() instead of checking requireNamespace() in skip_if()."),
    linter
  )
  expect_lint(
    "skip_if_not(requireNamespace('pkg', quietly = TRUE))",
    rex::rex("Use skip_if_not_installed() instead of checking requireNamespace() in skip_if_not()."),
    linter
  )
  expect_lint(
    "if (!requireNamespace('pkg', quietly = TRUE)) skip('no pkg')",
    rex::rex("Use skip_if_not_installed() instead of checking requireNamespace() in if (...) skip()."),
    linter
  )
  expect_lint(
    "skip_if(packageVersion('pkg') < '1.2.0')",
    rex::rex("Use skip_if_not_installed() instead of checking packageVersion() in skip_if()."),
    linter
  )
  expect_lint(
    "skip_if_not(requireNamespace('pkg', quietly = TRUE) && packageVersion('pkg') >= '1.2.0')",
    rex::rex("Use skip_if_not_installed() instead of checking requireNamespace() in skip_if_not()."),
    linter
  )

  # Dedicated skip_on_os() helper
  expect_lint(
    "skip_if(.Platform$OS.type == 'windows')",
    rex::rex("Use skip_on_os() instead of checking the operating system in skip_if()."),
    linter
  )
  expect_lint(
    "skip_if_not(.Platform$OS.type != 'windows')",
    rex::rex("Use skip_on_os() instead of checking the operating system in skip_if_not()."),
    linter
  )
  expect_lint(
    "skip_if(Sys.info()[['sysname']] == 'Darwin')",
    rex::rex("Use skip_on_os() instead of checking the operating system in skip_if()."),
    linter
  )
  expect_lint(
    "skip_if(tolower(Sys.info()['sysname']) %in% c('windows', 'darwin'))",
    rex::rex("Use skip_on_os() instead of checking the operating system in skip_if()."),
    linter
  )
  expect_lint(
    "if (.Platform$OS.type == 'windows') skip('not on windows')",
    rex::rex("Use skip_on_os() instead of checking the operating system in if (...) skip()."),
    linter
  )

  # skip_on_cran(), skip_on_ci(), skip_on_covr(), skip_on_bioc()
  expect_lint(
    "skip_if(!identical(Sys.getenv('NOT_CRAN'), 'true'))",
    rex::rex("Use skip_on_cran() instead of checking Sys.getenv('NOT_CRAN') in skip_if()."),
    linter
  )
  expect_lint(
    "skip_if(nzchar(Sys.getenv('CI')))",
    rex::rex("Use skip_on_ci() instead of checking Sys.getenv('CI') in skip_if()."),
    linter
  )
  expect_lint(
    "skip_if(Sys.getenv('R_COVR') == 'true')",
    rex::rex("Use skip_on_covr() instead of checking Sys.getenv('R_COVR') in skip_if()."),
    linter
  )
  expect_lint(
    "if (nzchar(Sys.getenv('IS_BIOC_BUILD_MACHINE'))) skip('on bioc')",
    rex::rex("Use skip_on_bioc() instead of checking Sys.getenv('IS_BIOC_BUILD_MACHINE') in if (...) skip()."),
    linter
  )

  # Superseded skip_on_travis() and skip_on_appveyor()
  expect_lint(
    "skip_on_travis()",
    rex::rex("Use skip_on_ci() instead of superseded skip_on_travis()."),
    linter
  )
  expect_lint(
    "testthat::skip_on_appveyor()",
    rex::rex("Use skip_on_ci() instead of superseded skip_on_appveyor()."),
    linter
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      if (cond) skip('a')
      skip_if(!ready)
      skip_if(getRversion() < '4.2.0')
      skip_on_travis()
    }"),
    list(
      list(rex::rex("Use skip_if() instead of if (...) skip()."), line_number = 2L, column_number = 3L),
      list(rex::rex("Use skip_if_not(x) instead of skip_if(!x)."), line_number = 3L, column_number = 3L),
      list(rex::rex("Use skip_unless_r()"), line_number = 4L, column_number = 3L),
      list(rex::rex("Use skip_on_ci() instead of superseded skip_on_travis()."), line_number = 5L, column_number = 3L)
    ),
    testthat_skip_linter()
  )
})
