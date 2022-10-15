library(testthat)
library(lintr)
# suppress printing environment name (noisy)
invisible({
  loadNamespace("rex")
  loadNamespace("withr")
  loadNamespace("patrick")
})

test_check("lintr")
