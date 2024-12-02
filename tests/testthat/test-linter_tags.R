test_that("input validation for available_linters works as expected", {
  expect_error(available_linters(1L), "`packages` must be a <character> vector.")
  expect_error(available_linters(tags = 1L), "`tags` must be a <character> vector.")
  expect_error(available_linters(exclude_tags = 1L), "`exclude_tags` must be a <character> vector.")
})

test_that("validate_linter_db works as expected", {
  df_empty <- data.frame()
  expect_warning(
    lintr:::validate_linter_db(df_empty, "mypkg"),
    'must contain the columns "linter" and "tags"',
    fixed = TRUE
  )
  expect_false(suppressWarnings(lintr:::validate_linter_db(df_empty, "mypkg")))

  df <- data.frame(linter = "absolute_path_linter", tags = "robustness")
  expect_true(lintr:::validate_linter_db(df, "mypkg"))
})

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

test_that("available_tags returns a character vector", {
  tags <- available_tags()
  tags2 <- available_tags(c("lintr", "not-a-package"))
  empty <- available_tags("not-a-package")

  expect_type(tags, "character")
  expect_identical(tags, tags2)
  expect_length(empty, 0L)
  expect_true(all(available_linters()$tags[[1L]] %in% tags))
  expect_true(all(unlist(available_linters()$tags) %in% tags))
  expect_identical(anyDuplicated(tags), 0L)
  expect_identical(lintr:::platform_independent_order(tags), seq_along(tags))
})

test_that("default_linters and default tag match up", {
  avail <- available_linters()
  tagged_default <- avail[["linter"]][vapply(avail[["tags"]], function(tags) "default" %in% tags, logical(1L))]
  expect_identical(tagged_default, names(default_linters))
})

test_that("warnings occur only for deprecated linters", {
  expect_silent(linters_with_tags(tags = NULL))
  num_deprecated_linters <- nrow(available_linters(tags = "deprecated", exclude_tags = NULL))
  deprecation_warns_seen <- 0L
  outer_env <- environment()
  expect_silent({
    withCallingHandlers(
      linters_with_tags(tags = "deprecated", exclude_tags = NULL),
      warning = function(w) {
        if (grepl("was deprecated", conditionMessage(w), fixed = TRUE)) {
          outer_env$deprecation_warns_seen <- outer_env$deprecation_warns_seen + 1L
          invokeRestart("muffleWarning")
        } else {
          w
        }
      }
    )
  })
  expect_identical(deprecation_warns_seen, num_deprecated_linters)
})

test_that("available_linters matches the set of linters available from lintr", {
  lintr_db <- available_linters(exclude_tags = NULL)
  defunct_linters <-
    subset(read.csv(system.file("lintr", "linters.csv", package = "lintr"), as.is = TRUE), tags == "defunct")$linter
  ignore <- c("is_linter", defunct_linters)
  linters_in_namespace <- setdiff(ls(asNamespace("lintr"), pattern = "_linter$"), ignore)
  # ensure that the contents of inst/lintr/linters.csv covers all _linter objects in our namespace
  expect_identical(sort(lintr_db$linter), sort(linters_in_namespace))
  # ensure that all _linter objects in our namespace are also exported
  exported_linters <- setdiff(grep("_linter$", getNamespaceExports("lintr"), value = TRUE), ignore)
  expect_identical(sort(linters_in_namespace), sort(exported_linters))
})

test_that("rownames for available_linters data frame doesn't have missing entries", {
  lintr_db <- available_linters()
  expect_identical(
    tail(rownames(lintr_db), 1L),
    as.character(nrow(lintr_db))
  )

  lintr_db2 <- available_linters(exclude_tags = NULL)
  expect_identical(
    tail(rownames(lintr_db2), 1L),
    as.character(nrow(lintr_db2))
  )
})

# See the roxygen helpers in R/linter_tags.R for the code used to generate the docs.
#   This test helps ensure the documentation is up to date with the available_linters() database
test_that("lintr help files are up to date", {
  help_db <- tools::Rd_db("lintr")
  # e.g. in dev under pkgload::load_all()
  if (length(help_db) == 0L) {
    help_db <- tools::Rd_db(dir = test_path("..", ".."))
    skip_if_not(length(help_db) > 0L, message = "Package help not installed or corrupted")
  }

  lintr_db <- available_linters(exclude_tags = NULL)
  lintr_db$package <- NULL
  lintr_db$tags <- lapply(lintr_db$tags, function(x) if ("deprecated" %in% x) "deprecated" else sort(x))
  lintr_db <- lintr_db[order(lintr_db$linter), ]

  expect_true("linters.Rd" %in% names(help_db), info = "?linters exists")

  # NB: objects in help_env are class Rd, see ?as.character.Rd (part of 'tools')
  rd_as_string <- function(rd) paste(as.character(rd), collapse = "")

  # Get a character string with the contents of ?linters
  linter_help_text <- rd_as_string(help_db$linters.Rd)

  # Test three things about ?linters
  #   (1) the complete list of linters and tags matches that in available_linters()
  #   (2) the tabulation of tags & corresponding count of linters matches that in available_linters()
  #   (3) the 'configurable' tag applies if and only if the linter has parameters

  # Extract linters & associated tags from ?linters text
  # Rd markup for items looks like \item{\code{\link{...}} (tags: ...)}
  #                                                  [1]          [2]
  # [1]: linter name; [2]: associated tags
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
  help_linters <- help_linters[order(help_linters$linter), ]

  # (1) the complete list of linters and tags matches that in available_linters()
  expect_identical(
    help_linters,
    # deprecated linters are excluded from this page
    lintr_db[!vapply(lintr_db$tags, identical, "deprecated", FUN.VALUE = NA), ],
    info = "Database implied by ?linters is the same as is available_linters()",
    ignore_attr = "row.names"
  )

  # Counts of tags from available_linters()
  #   NB: as.data.frame.table returns stringsAsFactors=TRUE default in R>4
  db_tag_table <- as.data.frame(
    table(tag = unlist(lintr_db$tags)),
    responseName = "n_linters",
    stringsAsFactors = FALSE
  )
  # In ?linters, entries in the enumeration of tags look like
  #   \item{\link[=${TAG}_linters]{${TAG}} (${N_LINTERS_WITH_TAG} linters)}
  help_tag_table <- rex::re_matches(
    linter_help_text,
    rex::rex(
      "\\item{\\link[=",
      capture(some_of(letter, "_"), name = "tag_page"),
      "_linters]{",
      capture(some_of(letter, "_"), name = "tag"),
      "} (",
      capture(numbers, name = "n_linters"),
      " linters)}"
    ),
    global = TRUE
  )[[1L]]
  # consistency/sanity check
  expect_identical(help_tag_table$tag_page, help_tag_table$tag)
  help_tag_table$tag_page <- NULL
  help_tag_table$n_linters <- as.integer(help_tag_table$n_linters)

  # (2) the tabulation of tags & corresponding count of linters matches that in available_linters()
  expect_identical(
    help_tag_table[order(help_tag_table$tag), ],
    db_tag_table[order(db_tag_table$tag), ],
    ignore_attr = "row.names",
    info = "Tags and corresponding counts in ?linters is the same as in available_linters()"
  )

  # Now test an analogue to (1) from above for each tag's help page
  for (tag in db_tag_table$tag) {
    expect_true(paste0(tag, "_linters.Rd") %in% names(help_db), info = paste0("?", tag, "_linters exists"))

    tag_help <- help_db[[paste0(tag, "_linters.Rd")]]
    tag_help_text <- rd_as_string(tag_help)

    # linters listed in ?${TAG}_linter
    help_tag_linters <- rex::re_matches(
      tag_help_text,
      rex::rex("\\item{\\code{\\link{", capture(some_of(letter, number, "_", "."), name = "linter"), "}}}"),
      global = TRUE
    )[[1L]]

    # those entries in available_linters() with the current tag
    db_linter_has_tag <- vapply(lintr_db$tags, function(linter_tag) any(tag %in% linter_tag), logical(1L))
    expected <- lintr_db$linter[db_linter_has_tag]

    expect_identical(
      sort(help_tag_linters$linter),
      sort(expected),
      info = paste0("?", tag, "_linters lists all linters with that tag in available_linters()")
    )
  }

  # (3) the 'configurable' tag applies if and only if the linter has parameters
  has_args <- 0L < lengths(Map(
    function(linter, tags) if ("deprecated" %in% tags) NULL else formals(match.fun(linter)),
    lintr_db$linter,
    lintr_db$tags
  ))
  has_configurable_tag <- vapply(lintr_db$tags, function(tags) "configurable" %in% tags, logical(1L))

  expect_identical(has_configurable_tag, unname(has_args))
})

test_that("available_linters gives precedence to included tags", {
  expect_true("style" %in% unlist(available_linters(tags = "style", exclude_tags = "style")$tags))
  # also for the default case
  expect_identical(
    available_linters(tags = "deprecated"),
    available_linters(tags = "deprecated", exclude_tags = NULL)
  )
})

test_that("all linters have at least one tag", {
  expect_true(all(lengths(available_linters()$tags) > 0L))
})
