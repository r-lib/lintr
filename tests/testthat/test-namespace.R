test_that("is_s3_generic", {
  func <- function(x) {
    print(x)
    UseMethod("func")
  }

  expect_true(lintr:::is_s3_generic(func))
})

test_that("is_s3_generic doesn't error for namespace-qualified calls", {
  func <- function(...) {
    pkg::call()
  }

  expect_warning(result <- lintr:::is_s3_generic(func), NA)
  expect_false(result)
})


test_that("namespace_imports respects except directives", {
  skip_if_not_installed("callr")
  skip_if_not_installed("withr")

  withr::with_dir(
    test_path("dummy_packages", "importsexcept"),
    {
      callr::rcmd("INSTALL", ".", echo = FALSE, show = FALSE)
      df <- namespace_imports(test_path("dummy_packages", "importsexcept"))

      expect_equal(unique(df$pkg), "glue")
      expect_true(any("glue_sql_collapse" == df$fun))

      # excluded functions from imports not present
      expect_false(any("glue_collapse" == df$fun))
      expect_false(any("glue_data" == df$fun))
    }
  )
})
