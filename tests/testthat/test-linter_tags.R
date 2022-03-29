test_that("available_linters returns a data frame", {
  avail <- available_linters()
  avail2 <- available_linters(c("lintr", "not-a-package"))
  empty <- available_linters("not-a-package")

  expect_s3_class(avail, "data.frame")
  expect_identical(avail, avail2)
  expect_identical(nrow(empty), 0L)
  expect_named(avail, c("linter", "package", "tags"))
  expect_type(avail[["linter"]], "character")
  expect_type(avail[["package"]], "character")
  expect_type(avail[["tags"]], "list")
  expect_type(avail[["tags"]][[1L]], "character")
})

test_that("default_linters and default tag match up", {
  avail <- available_linters()
  tagged_default <- avail[["linter"]][vapply(avail[["tags"]], function(tags) "default" %in% tags, logical(1L))]
  expect_setequal(tagged_default, names(default_linters))
})

test_that("available_linters matches the set of linters available from lintr", {
  lintr_db <- available_linters()
  all_linters <- ls(asNamespace("lintr"), pattern = "_linter$")
  # ensure that the contents of inst/lintr/linters.csv covers all _linter objects in our namespace
  expect_setequal(lintr_db$linter, all_linters)
  # ensure that all _linter objects in our namespace are also exported
  expect_setequal(all_linters, grep("_linter$", getNamespaceExports("lintr"), value = TRUE))
})

test_that("lintr help files are up to date", {
  helper_db_dir <- system.file("help", package = "lintr")
  skip_if_not(dir.exists(helper_db_dir))
  skip_if_not(file.exists(file.path(helper_db_dir, "lintr.rdb")))
  skip_if_not(file.exists(file.path(helper_db_dir, "lintr.rdx")))

  withr::local_dir(helper_db_dir)
  help_env <- new.env(parent = topenv())
  lazyLoad("lintr", help_env)

  lintr_db <- available_linters()

  expect_true(exists("linters", envir = help_env))
  linter_help_text <- paste(as.character(help_env$linters), collapse = "")

  linter_item_regex <- "[\\]item[{][\\]code[{][\\]link[{][a-zA-Z0-9._]+[}][}] [(]tags: [a-z_, ]+[)][}]"
  expect_identical(
    length(gregexpr(linter_item_regex, linter_help_text)[[1L]]),
    nrow(lintr_db),
    info = "Count of linter entries in ?linters matches inst/lintr/linters.csv row count"
  )

  tag_table <- table(unlist(lintr_db$tags))
  all_tags <- names(tag_table)
  tag_help_text <- character(length(all_tags))
  names(tag_help_text) <- all_tags

  for (tag in all_tags) {
    expect_true(exists(paste0(tag, "_linters"), envir = help_env), info = paste(tag, "tag page"))

    tag_help <- help_env[[paste0(tag, "_linters")]]
    tag_help_text[[tag]] <- paste(as.character(tag_help), collapse = "")
    tag_item_regex <- "[\\]item[{][\\]code[{][\\]link[{][a-zA-Z0-9._]+[}][}][}]"
    expect_identical(
      length(gregexpr(tag_item_regex, tag_help_text[[tag]])[[1L]]),
      tag_table[[tag]],
      info = paste0("Count of linter entries in ?", tag, "_linters matches tags given in inst/lintr/linters.csv")
    )
  }

  for (ii in seq_len(nrow(lintr_db))) {
    # NB: matching tags requires reproducing the "platform-independent sort" we do; instead, rely on the other tests
    #   for matching the tags
    linter_name <- lintr_db$linter[[ii]]
    expected_entry <- sprintf("\\item{\\code{\\link{%s}} (tags:", linter_name)
    expect_match(linter_help_text, expected_entry, fixed = TRUE, info = paste(linter_name, "entry in ?linters"))

    for (linter_tag in lintr_db$tags[[ii]]) {
      expected_entry <- sprintf("\\item{\\code{\\link{%s}}}", linter_name)
      expect_match(
        tag_help_text[[linter_tag]],
        expected_entry,
        fixed = TRUE,
        info = paste0(linter_name, " entry in ?", linter_tag, "_linters")
      )
    }
  }
})
