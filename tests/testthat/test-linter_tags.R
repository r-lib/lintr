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
  expect_identical(tagged_default, names(default_linters))
})

test_that("available_linters matches the set of linters available from lintr", {
  lintr_db <- available_linters()
  linters_in_namespace <- ls(asNamespace("lintr"), pattern = "_linter$")
  # ensure that the contents of inst/lintr/linters.csv covers all _linter objects in our namespace
  expect_identical(sort(lintr_db$linter), sort(linters_in_namespace))
  # ensure that all _linter objects in our namespace are also exported
  exported_linters <- grep("_linter$", getNamespaceExports("lintr"), value = TRUE)
  expect_identical(sort(linters_in_namespace), sort(exported_linters))
})

# See the roxygen helpers in R/linter_tags.R for the code used to generate the docs.
#   This test helps ensure the documentation is up to date with the available_linters() database
test_that("lintr help files are up to date", {
  helper_db_dir <- system.file("help", package = "lintr")
  skip_if_not(dir.exists(helper_db_dir))
  skip_if_not(file.exists(file.path(helper_db_dir, "lintr.rdb")))
  skip_if_not(file.exists(file.path(helper_db_dir, "lintr.rdx")))

  help_env <- new.env(parent = topenv())
  lazyLoad(file.path(helper_db_dir, "lintr"), help_env)

  lintr_db <- available_linters()
  lintr_db$package <- NULL
  lintr_db$tags <- lapply(lintr_db$tags, sort)

  expect_true(exists("linters", envir = help_env), info = "?linters exists")
  # objects in help_env are class Rd, see ?as.character.Rd (part of 'tools')
  linter_help_text <- paste(as.character(help_env$linters), collapse = "")

  # Test two things about ?linters
  #   (1) the complete list of linters and tags matches that in available_linters()
  #   (2) the tabulation of tags & corresponding count of linters matches that in available_linters()

  # Rd markup for items looks like \item{\code{\link{...}} (tags: ...)}
  help_linters <- rex::re_matches(
    linter_help_text,
    rex::rex(
      "\\item{\\code{\\link{",
      capture(some_of(letter, number, "_", "."), name = "linter"),
      "}} (tags: ",
      capture(some_of(letter, "_", ",", " "), name = "tags"),
      ")}"
    ),
    global = TRUE
  )[[1L]]
  help_linters$tags <- lapply(strsplit(help_linters$tags, ", ", fixed = TRUE), sort)


  # (1) from above
  expect_identical(
    help_linters[order(help_linters$linter), ],
    lintr_db[order(lintr_db$linter), ],
    info = "Database implied by ?linters is the same as is available_linters()"
  )

  db_tag_table <- as.data.frame(
    table(tag = unlist(lintr_db$tags)),
    responseName = "n_linters",
    stringsAsFactors = FALSE
  )
  help_tag_table <- rex::re_matches(
    linter_help_text,
    rex::rex(
      "\\item{\\link[=",
      capture(some_of(letter, "_"), "_linters", name = "tag_page"),
      "]{",
      capture(some_of(letter, "_"), name = "tag"),
      "} (",
      capture(numbers, name = "n_linters"),
      " linters)}"
    ),
    global = TRUE
  )[[1L]]
  # consistency check
  expect_identical(help_tag_table$tag_page, paste0(help_tag_table$tag, "_linters"))
  help_tag_table$tag_page <- NULL
  help_tag_table$n_linters <- as.integer(help_tag_table$n_linters)

  # (2) from above
  expect_identical(
    help_tag_table[order(help_tag_table$tag), ],
    db_tag_table[order(db_tag_table$tag), ],
    info = "Tags and corresponding counts in ?linters is the same as in available_linters()"
  )

  # Now test an analogue to (1) from above for each tag's help page
  for (tag in db_tag_table$tag) {
    expect_true(exists(paste0(tag, "_linters"), envir = help_env), info = paste0("?", tag, "_linters exists"))

    tag_help <- help_env[[paste0(tag, "_linters")]]
    tag_help_text <- paste(as.character(tag_help), collapse = "")

    help_tag_linters <- rex::re_matches(
      tag_help_text,
      rex::rex("\\item{\\code{\\link{", capture(some_of(letter, number, "_", "."), name = "linter"), "}}}"),
      global = TRUE
    )[[1L]]

    db_linter_has_tag <- vapply(lintr_db$tags, function(linter_tag) any(tag %in% linter_tag), logical(1L))

    expect_identical(
      sort(help_tag_linters$linter),
      sort(lintr_db$linter[db_linter_has_tag]),
      info = paste0("?", tag, "_linters lists all linters with that tag in available_linters()")
    )
  }
})
