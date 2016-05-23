library(XML)
context("checkstyle_output")
test_that("return lint report as checkstyle xml", {
  with_mock(`rstudioapi::callFun` = function(...) return(list(...)),
    lints <- structure(
      list(
        Lint(filename = "test_file",
             line_number = 1,
             column_number = 2,
             type = "error",
             line = "a line",
             message = "hi")
      ),
      class = "lints"
    ),
    checkstyle_output(lints, "test-checkstyle.xml"),
    expect_true(file.exists("results/test-checkstyle.xml"))
  )
  
  xmlfile <- xmlTreeParse("results/test-checkstyle.xml")
  topxml <- xmlRoot(xmlfile)
  topxmlStr <- toString.XMLNode(topxml)
  expect_equal(topxmlStr,
                  "<checkstyle version=\"4.3\">\n <file name=\"test_file\">\n  <error line=\"1\" column=\"2\" severity=\"error\" message=\"hi\"/>\n </file>\n</checkstyle>"
              )
  
  #clean up
  file.remove("results/test-checkstyle.xml")
  file.remove("results")
})