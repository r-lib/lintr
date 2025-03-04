withr::local_options(list(
  lintr.exclude = "#TeSt_NoLiNt",
  lintr.exclude_start = "#TeSt_NoLiNt_StArT",
  lintr.exclude_end = "#TeSt_NoLiNt_EnD"
))

test_that("it merges two NULL or empty objects as an empty list", {
  expect_identical(lintr:::normalize_exclusions(c(NULL, NULL)), list())
  expect_identical(lintr:::normalize_exclusions(c(NULL, list())), list())
  expect_identical(lintr:::normalize_exclusions(c(list(), NULL)), list())
  expect_identical(lintr:::normalize_exclusions(c(list(), list())), list())
})

test_that("it returns the object if the other is NULL", {
  a <- withr::local_tempfile()
  file.create(a)
  a <- normalize_path(a)

  t1 <- list()
  t1[[a]] <- list(1L:10L)
  expect_identical(lintr:::normalize_exclusions(c(t1, NULL)), t1)
  expect_identical(lintr:::normalize_exclusions(c(NULL, t1)), t1)
})

test_that("it returns the union of two non-overlapping lists", {
  a <- withr::local_tempfile()
  file.create(a)
  a <- normalize_path(a)

  t1 <- list()
  t1[[a]] <- list(1L:10L)
  t2 <- list()
  t2[[a]] <- list(20L:30L)
  res <- list()
  res[[a]] <- list(c(1L:10L, 20L:30L))
  expect_identical(lintr:::normalize_exclusions(c(t1, t2)), res)
})

test_that("it works with named lists", {
  a <- withr::local_tempfile()
  file.create(a)
  a <- normalize_path(a)

  t1 <- list()
  t1[[a]] <- list(1L:10L, my_linter = 1L:20L)
  t2 <- list()
  t2[[a]] <- list(11L:15L, my_linter = 21L:25L)
  res <- list()
  res[[a]] <- list(1L:15L, my_linter = 1L:25L)
  expect_identical(lintr:::normalize_exclusions(c(t1, t2)), res)
})

test_that("it returns the union of two overlapping lists", {
  a <- withr::local_tempfile()
  file.create(a)
  a <- normalize_path(a)

  t1 <- list()
  t1[[a]] <- list(1L:10L)
  t2 <- list()
  t2[[a]] <- list(5L:15L)
  res <- list()
  res[[a]] <- list(1L:15L)
  expect_identical(lintr:::normalize_exclusions(c(t1, t2)), res)
})

test_that("it adds names if needed", {
  a <- withr::local_tempfile()
  b <- withr::local_tempfile()
  file.create(a, b)
  a <- normalize_path(a)
  b <- normalize_path(b)

  t1 <- list()
  t1[[a]] <- list(1L:10L)
  t2 <- list()
  t2[[b]] <- list(5L:15L)
  res <- list()
  res[[a]] <- list(1L:10L)
  res[[b]] <- list(5L:15L)
  expect_identical(lintr:::normalize_exclusions(c(t1, t2)), res)
})

test_that("it handles full file exclusions", {
  a <- withr::local_tempfile()
  b <- withr::local_tempfile()
  file.create(a, b)
  a <- normalize_path(a)
  b <- normalize_path(b)

  res <- list()
  res[[a]] <- list(Inf)
  expect_identical(lintr:::normalize_exclusions(list(a)), res)

  t1 <- list()
  t1[[1L]] <- a
  t1[[b]] <- 1L
  res <- list()
  res[[a]] <- list(Inf)
  res[[b]] <- list(1L)
  expect_identical(lintr:::normalize_exclusions(t1), res)
})

test_that("it handles redundant lines", {
  a <- withr::local_tempfile()
  b <- withr::local_tempfile()
  file.create(a, b)
  a <- normalize_path(a)
  b <- normalize_path(b)

  t1 <- list()
  t1[[a]] <- list(c(1L, 1L, 1L:10L))
  res <- list()
  res[[a]] <- list(1L:10L)
  expect_identical(lintr:::normalize_exclusions(t1), res)

  t1 <- list()
  t1[[a]] <- list(c(1L, 1L, 1L:10L))
  t1[[b]] <- list(1L:10L)
  res <- list()
  res[[a]] <- list(1L:10L)
  res[[b]] <- list(1L:10L)
  expect_identical(lintr:::normalize_exclusions(t1), res)
})

test_that("it handles redundant linters", {
  a <- withr::local_tempfile()
  b <- withr::local_tempfile()
  file.create(a, b)
  a <- normalize_path(a)
  b <- normalize_path(b)

  t1 <- list()
  # nolint next: duplicate_argument_linter.
  t1[[a]] <- list(c(1L, 1L, 1L:10L), my_linter = c(1L, 1L, 1L, 2L), my_linter = 3L)
  res <- list()
  res[[a]] <- list(1L:10L, my_linter = 1L:3L)
  expect_identical(lintr:::normalize_exclusions(t1), res)

  t1 <- list()
  t1[[a]] <- list(c(1L, 1L, 1L:10L), my_linter = c(1L, 1L, 1L, 2L))
  # nolint next: duplicate_argument_linter.
  t1[[b]] <- list(1L:10L, my_linter = 1L:10L, my_linter = 11L:20L)
  res <- list()
  res[[a]] <- list(1L:10L, my_linter = 1L:2L)
  res[[b]] <- list(1L:10L, my_linter = 1L:20L)
  expect_identical(lintr:::normalize_exclusions(t1), res)
})

test_that("it handles redundant files", {
  a <- withr::local_tempfile()
  file.create(a)
  a <- normalize_path(a)

  t1 <- list(1L:10L, 10L:20L)
  names(t1) <- c(a, a)
  res <- list()
  res[[a]] <- list(1L:20L)
  expect_identical(lintr:::normalize_exclusions(t1), res)
})

test_that("it normalizes file paths, removing non-existing files", {
  a <- withr::local_tempfile()
  c <- withr::local_tempfile(tmpdir = ".")
  file.create(a, c)
  a <- normalize_path(a)
  c <- normalize_path(c)

  t1 <- list()
  t1[[a]] <- 1L:10L
  t2 <- list()
  t2[["notafile"]] <- 5L:15L
  t3 <- list()
  t3[[c]] <- 5L:15L
  res <- list()
  res[[a]] <- list(1L:10L)
  res[[c]] <- list(5L:15L)
  expect_identical(lintr:::normalize_exclusions(c(t1, t2, t3)), res)

  res <- list()
  res[[a]] <- list(1L:10L)
  res[["notafile"]] <- list(5L:15L)
  res[[c]] <- list(5L:15L)
  expect_identical(lintr:::normalize_exclusions(c(t1, t2, t3), normalize_path = FALSE), res)
})

test_that("it errors for invalid specifications", {
  msg_full_files <- "Full file exclusions must be <character> vectors of length 1."
  expect_error(lintr:::normalize_exclusions(2L), msg_full_files)
  expect_error(lintr:::normalize_exclusions(list("a.R", 2L)), msg_full_files)
  expect_error(lintr:::normalize_exclusions(list(a.R = Inf, 2L)), msg_full_files)

  msg_full_lines <- "Full line exclusions must be <numeric> or <integer> vectors."
  expect_error(lintr:::normalize_exclusions(list(a.R = "Inf")), msg_full_lines)
})

test_that("globs are supported", {
  withr::local_dir(withr::local_tempdir("test-linting"))
  dirs <- c("a", "b")
  dir_files <- c("foo1.R", "foo2.R", "bar.R")
  for (dir in dirs) {
    dir.create(dir)
    for (file in dir_files) {
      writeLines("a = 1", file.path(dir, file))
    }
  }

  with_config(
    "linters: list(assignment_linter())",
    expect_length(unique(as.data.frame(lint_dir())$filename), 6L)
  )

  with_config(
    trim_some(R"(
    linters: list(assignment_linter())
    exclusions: list(
        "a/foo*",
        "b"
      )
    )"),
    expect_identical(as.data.frame(lint_dir())$filename, "a/bar.R")
  )

  # add a second lint to test linter- and line-specific exclusion
  for (dir in dirs) {
    for (file in dir_files) {
      cat("1+1\n", file = file.path(dir, file), append = TRUE)
    }
  }

  # exclude by linter
  with_config(
    trim_some(R"(
    linters: list(assignment_linter(), infix_spaces_linter())
    exclusions: list(
        "a/foo*" = list(infix_spaces_linter = Inf),
        "b/b*"
      )
    )"),
    {
      lint_df <- as.data.frame(lint_dir())
      # exclusion worked in the specified files, and only there
      expect_false(any(
        with(lint_df, startsWith(filename, "a/foo") & linter == "infix_spaces_linter")
      ))
      expect_true("a/foo1.R" %in% lint_df$filename)
      expect_true("infix_spaces_linter" %in% lint_df$linter)
    }
  )

  # exclude by line
  with_config(
    trim_some(R"(
    linters: list(assignment_linter(), infix_spaces_linter())
    exclusions: list(
        "a/foo*" = 2L,
        "b/b*"
      )
    )"),
    {
      lint_df <- as.data.frame(lint_dir())
      # exclusion worked in the specified files, and only there
      expect_false(any(
        with(lint_df, startsWith(filename, "a/foo") & linter == "infix_spaces_linter")
      ))
      expect_true("a/foo1.R" %in% lint_df$filename)
      expect_true("infix_spaces_linter" %in% lint_df$linter)
    }
  )

  # exclude by linter by line
  with_config(
    trim_some(R"(
    linters: list(assignment_linter(), infix_spaces_linter())
    exclusions: list(
        "a/foo*" = list(infix_spaces_linter = 2L),
        "b/b*"
      )
    )"),
    {
      lint_df <- as.data.frame(lint_dir())
      # exclusion worked in the specified files, and only there
      expect_false(any(
        with(lint_df, startsWith(filename, "a/foo") & linter == "infix_spaces_linter")
      ))
      expect_true("a/foo1.R" %in% lint_df$filename)
      expect_true("infix_spaces_linter" %in% lint_df$linter)
    }
  )
})
