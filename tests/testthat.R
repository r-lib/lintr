library(testthat)
library(lintr)
# suppress printing environment name (noisy)
invisible({
  loadNamespace("rex")
  loadNamespace("withr")
})

test_check("lintr")
