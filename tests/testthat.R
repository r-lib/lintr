library(testthat)
library(lintr)

# These packages are used heavily in the suite and are thus required
if (
  requireNamespace("patrick", quietly = TRUE) &&
    requireNamespace("withr", quietly = TRUE)
) {
  test_check("lintr")
}
