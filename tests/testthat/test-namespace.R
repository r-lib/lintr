test_that("namespace_imports() respects except= in import() directives", {
  pkg <- withr::local_tempdir()
  writeLines(c("Package: pkg", "Version: 1.0", "Title: Test", "Description: Test."), file.path(pkg, "DESCRIPTION"))
  writeLines(
    c("import(tools, except = c(toTitleCase, file_ext))", "importFrom(stats, lag)"),
    file.path(pkg, "NAMESPACE")
  )

  imports <- namespace_imports(pkg)
  tools_funs <- imports$fun[imports$pkg == "tools"]

  expect_identical(
    sort(tools_funs),
    sort(setdiff(getNamespaceExports("tools"), c("toTitleCase", "file_ext")))
  )
  expect_identical(imports$fun[imports$pkg == "stats"], "lag")
})

test_that("namespace_imports() handles an empty except= in import() directives", {
  pkg <- withr::local_tempdir()
  writeLines(c("Package: pkg", "Version: 1.0", "Title: Test", "Description: Test."), file.path(pkg, "DESCRIPTION"))
  writeLines("import(tools, except = c())", file.path(pkg, "NAMESPACE"))

  expect_identical(
    sort(namespace_imports(pkg)$fun),
    sort(getNamespaceExports("tools"))
  )
})
