test_that("set_lang and reset_lang work as expected", {
  old_lang <- Sys.getenv("LANGUAGE")
  new_lang <- "en"

  lintr:::set_lang(new_lang)
  expect_identical(Sys.getenv("LANGUAGE"), new_lang)

  lintr:::reset_lang(old_lang)
  expect_identical(Sys.getenv("LANGUAGE"), old_lang)
})
