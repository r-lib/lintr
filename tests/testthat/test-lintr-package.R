test_that("All linter help files have examples", {
  help_db <- safe_load_help_db()
  linter_db <- help_db[endsWith(names(help_db), "_linter.Rd")]
  rd_has_examples <- function(rd) any(vapply(rd, attr, "Rd_tag", FUN.VALUE = character(1L)) == "\\examples")
  linter_has_examples <- vapply(linter_db, rd_has_examples, logical(1L))
  for (ii in seq_along(linter_has_examples)) {
    expect_true(linter_has_examples[ii], label = paste("Linter", names(linter_db)[ii], "has examples"))
  }
})
