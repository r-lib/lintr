with_content_to_parse <- function(content, code) {
  f <- tempfile()
  con <- file(f, open = "w", encoding = "UTF-8")
  on.exit(unlink(f))
  writeLines(content, con)
  close(con)
  source_expressions <- get_source_expressions(f)
  content_env <- new.env()
  content_env$pc <- lapply(source_expressions[["expressions"]], `[[`, "parsed_content")
  content_env$error <- source_expressions$error
  eval(substitute(code), envir = content_env)
}


test_that("tab positions have been corrected", {
  with_content_to_parse("1\n\t",
    expect_length(pc, 2L)
  )

  with_content_to_parse("TRUE",
    expect_equivalent(pc[[1L]][pc[[1L]][["text"]] == "TRUE", c("col1", "col2")], c(1L, 4L))
  )

  with_content_to_parse("\tTRUE",
    expect_equivalent(pc[[1L]][pc[[1L]][["text"]] == "TRUE", c("col1", "col2")], c(2L, 5L))
  )

  with_content_to_parse("\t\tTRUE",
    expect_equivalent(pc[[1]][pc[[1L]][["text"]] == "TRUE", c("col1", "col2")], c(3L, 6L))
  )

  with_content_to_parse("x\t<-\tTRUE", {
    expect_equivalent(pc[[1L]][pc[[1L]][["text"]] == "x", c("col1", "col2")], c(1L, 1L))
    expect_equivalent(pc[[1L]][pc[[1L]][["text"]] == "<-", c("col1", "col2")], c(3L, 4L))
    expect_equivalent(pc[[1L]][pc[[1L]][["text"]] == "TRUE", c("col1", "col2")], c(6L, 9L))
  })

  with_content_to_parse("\tfunction\t(x)\t{\tprint(pc[\t,1])\t;\t}", {
    expect_equivalent(pc[[1L]][pc[[1L]][["text"]] == "function", c("col1", "col2")], c(2L, 9L))
    expect_equivalent(pc[[1L]][pc[[1L]][["text"]] == "x", c("col1", "col2")], c(12L, 12L))
    expect_equivalent(pc[[1L]][pc[[1L]][["text"]] == "print", c("col1", "col2")], c(17L, 21L))
    expect_equivalent(pc[[1L]][pc[[1L]][["text"]] == ";", c("col1", "col2")], c(32L, 32L))
    expect_equivalent(pc[[1L]][pc[[1L]][["text"]] == "}", c("col1", "col2")], c(34L, 34L))
  })

  with_content_to_parse("# test tab\n\ns <- 'I have \\t a dog'\nrep(\ts, \t3)", {
    expect_equivalent(
      pc[[2L]][pc[[2L]][["text"]] == "'I have \\t a dog'", c("line1", "col1", "col2")],
      c(3L, 6L, 22L)
    )
    expect_equivalent(
      pc[[3L]][pc[[3L]][["text"]] == "3", c("line1", "col1", "col2")],
      c(4L, 10L, 10L)
    )
  })

  with_content_to_parse("function(){\nTRUE\n\t}", {
    expect_equivalent(
      pc[[1L]][1L, c("line1", "col1", "line2", "col2")],
      c(1L, 1L, 3L, 2L),
      info = "expression that spans several lines"
    )
  })
})

test_that("Terminal newlines are detected correctly", {
  writeLines("lm(y ~ x)", tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)
  writeBin(
    # strip the last (two) element(s) (\r\n or \n)
    head(readBin(tmp, raw(), file.size(tmp)), if (.Platform$OS.type == "windows") -2L else -1L),
    tmp2 <- tempfile()
  )
  on.exit(unlink(tmp2), add = TRUE)

  expect_true(get_source_expressions(tmp)$expressions[[2L]]$terminal_newline)
  expect_false(get_source_expressions(tmp2)$expressions[[2L]]$terminal_newline)
})

test_that("Multi-byte characters correct columns", {
  with_content_to_parse("`\U2020` <- 1", {
    # fix_column_numbers corrects the start of <-
    expect_equal(pc[[1L]]$col1[4L], pc[[1L]]$col1[2L] + 4L)
  })
})

test_that("Multi-byte character truncated by parser is ignored", {
  # \U2013 is the Unicode character 'en dash', which is
  # almost identical to a minus sign in monospaced fonts.
  with_content_to_parse("y <- x \U2013 42", {
    expect_equal(error$message, "unexpected input")
    expect_equal(error$column_number, 8L)
  })
})

test_that("Can read non UTF-8 file", {
  file <- "dummy_projects/project/cp1252.R"
  read_settings(file)
  expect_null(get_source_expressions(file)$error)
})

test_that("Warns if encoding is misspecified", {
  file <- "dummy_projects/project/cp1252.R"
  read_settings(NULL)
  the_lint <- get_source_expressions(file)$error
  expect_s3_class(the_lint, "lint")

  msg <- "Invalid multibyte character in parser. Is the encoding correct?"
  if (.Platform$OS.type == "windows") {
    # Windows parser throws a different error message because the source code is converted to native encoding
    # This results in line 4 becoming <fc> <- 42 before the parser sees it.
    msg <- "unexpected '<'"
  }

  expect_equal(the_lint$message, msg)
  expect_equal(the_lint$line_number, 4L)

  file <- "dummy_projects/project/cp1252_parseable.R"
  read_settings(NULL)
  the_lint <- get_source_expressions(file)$error
  expect_s3_class(the_lint, "lint")
  expect_equal(the_lint$message, "Invalid multibyte string. Is the encoding correct?")
  expect_equal(the_lint$line_number, 1L)
})
