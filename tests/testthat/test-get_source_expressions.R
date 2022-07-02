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
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == "TRUE", c("col1", "col2")], use.names = FALSE), c(1L, 4L))
  )
  with_content_to_parse("\tTRUE",
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == "TRUE", c("col1", "col2")], use.names = FALSE), c(2L, 5L))
  )

  with_content_to_parse("\t\tTRUE",
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == "TRUE", c("col1", "col2")], use.names = FALSE), c(3L, 6L))
  )

  with_content_to_parse("x\t<-\tTRUE", {
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == "x", c("col1", "col2")], use.names = FALSE), c(1L, 1L))
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == "<-", c("col1", "col2")], use.names = FALSE), c(3L, 4L))
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == "TRUE", c("col1", "col2")], use.names = FALSE), c(6L, 9L))
  })

  with_content_to_parse("\tfunction\t(x)\t{\tprint(pc[\t,1])\t;\t}", {
    expect_identical(
      unlist(pc[[1L]][pc[[1L]][["text"]] == "function", c("col1", "col2")], use.names = FALSE),
      c(2L, 9L)
    )
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == "x", c("col1", "col2")], use.names = FALSE), c(12L, 12L))
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == "print", c("col1", "col2")], use.names = FALSE), c(17L, 21L))
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == ";", c("col1", "col2")], use.names = FALSE), c(32L, 32L))
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == "}", c("col1", "col2")], use.names = FALSE), c(34L, 34L))
  })

  with_content_to_parse("# test tab\n\ns <- 'I have \\t a dog'\nrep(\ts, \t3)", {
    expect_identical(
      unlist(pc[[2L]][pc[[2L]][["text"]] == "'I have \\t a dog'", c("line1", "col1", "col2")], use.names = FALSE),
      c(3L, 6L, 22L)
    )
    expect_identical(
      unlist(pc[[3L]][pc[[3L]][["text"]] == "3", c("line1", "col1", "col2")], use.names = FALSE),
      c(4L, 10L, 10L)
    )
  })

  with_content_to_parse("function(){\nTRUE\n\t}", {
    expect_identical(
      unlist(pc[[1L]][1L, c("line1", "col1", "line2", "col2")], use.names = FALSE),
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
  file <- test_path("dummy_projects", "project", "cp1252.R")
  read_settings(file)
  expect_null(get_source_expressions(file)$error)
})

test_that("Warns if encoding is misspecified", {
  file <- test_path("dummy_projects", "project", "cp1252.R")
  read_settings(NULL)
  the_lint <- lint(filename = file, parse_settings = FALSE)[[1L]]
  expect_s3_class(the_lint, "lint")

  msg <- "Invalid multibyte character in parser. Is the encoding correct?"
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    # Prior to R 4.2.0, the Windows parser throws a different error message because the source code is converted to
    # native encoding.
    # This results in line 4 becoming <fc> <- 42 before the parser sees it.
    msg <- "unexpected '<'"
  }

  expect_equal(the_lint$linter, "error")
  expect_equal(the_lint$message, msg)
  expect_equal(the_lint$line_number, 4L)

  file <- test_path("dummy_projects", "project", "cp1252_parseable.R")
  read_settings(NULL)
  the_lint <- lint(filename = file, parse_settings = FALSE)[[1L]]
  expect_s3_class(the_lint, "lint")
  expect_equal(the_lint$linter, "error")
  expect_equal(the_lint$message, "Invalid multibyte string. Is the encoding correct?")
  expect_equal(the_lint$line_number, 1L)
})

test_that("Can extract line number from parser errors", {
  skip_if_not_r_version("4.0.0")

  # malformed raw string literal at line 2
  with_content_to_parse(
    trim_some('
      "ok"
      R"---a---"
    '), {
    expect_equal(error$message, "Malformed raw string literal.")
    expect_equal(error$line_number, 2L)
  })

  # invalid \u{xxxx} sequence (line 3)
  with_content_to_parse(
    trim_some('
      ok
      ok
      "\\u{9999"
    '), {
    expect_equal(error$message, "Invalid \\u{xxxx} sequence.")
    expect_equal(error$line_number, 3L)
  })

  # invalid \u{xxxx} sequence (line 4)
  with_content_to_parse(
    trim_some('
      ok
      ok
      "\\u{9999
    '), {
    # parser erroneously reports line 4
    expect_equal(error$message, "Invalid \\u{xxxx} sequence.")
    expect_equal(error$line_number, 3L)
  })

  # repeated formal argument 'a' on line 1
  with_content_to_parse("function(a, a) {}", {
    expect_equal(error$message, "Repeated formal argument 'a'.")
    expect_equal(error$line_number, 1L)
  })
})

test_that("1- or 2-width octal expressions give the right STR_CONST values", {
  with_content_to_parse("'\\1'", expect_identical(pc[[1L]][1L, "text"], "'\\1'"))
  with_content_to_parse('"\\1"', expect_identical(pc[[1L]][1L, "text"], '"\\1"'))

  # multiple literals
  with_content_to_parse("'\\1'\n'\\2'", {
    expect_identical(pc[[1L]][1L, "text"], "'\\1'")
    expect_identical(pc[[2L]][1L, "text"], "'\\2'")
  })

  # multiple escapes
  with_content_to_parse("'\\1\\2'", expect_identical(pc[[1L]][1L, "text"], "'\\1\\2'"))

  # multi-line strings
  with_content_to_parse("'\n\\1\n'", expect_identical(pc[[1L]][1L, "text"], "'\n\\1\n'"))
  with_content_to_parse("a <- '\\1\n\\2'", expect_identical(pc[[1L]][5L, "text"], "'\\1\n\\2'"))

  # mixed-length strings
  with_content_to_parse("foo('\\1',\n  '\n\\2\n')", {
    expect_identical(pc[[1L]][5L, "text"], "'\\1'")
    expect_identical(pc[[1L]][8L, "text"], "'\n\\2\n'")
  })
})

test_that("returned data structure is complete", {
  lines <- c("line_1", "line_2", "line_3")
  temp_file <- withr::local_tempfile(lines = lines)

  lines_with_attr <- setNames(lines, seq_along(lines))
  attr(lines_with_attr, "terminal_newline") <- TRUE

  exprs <- get_source_expressions(temp_file)
  expect_named(exprs, c("expressions", "error", "lines"))
  expect_length(exprs$expressions, length(lines) + 1L)

  for (i in seq_along(lines)) {
    expr <- exprs$expressions[[i]]
    expect_named(expr, c(
      "filename", "line", "column", "lines", "parsed_content", "xml_parsed_content", "content", "find_line",
      "find_column"
    ))
    expect_identical(expr$filename, temp_file)
    expect_identical(expr$line, i)
    expect_identical(expr$column, 1L)
    expect_identical(expr$lines, setNames(lines[i], i))
    expect_identical(nrow(expr$parsed_content), 2L)
    expect_true(xml2::xml_find_lgl(expr$xml_parsed_content, "count(//SYMBOL) > 0"))
    expect_identical(expr$content, lines[i])
    expect_type(expr$find_line, "closure")
    expect_type(expr$find_column, "closure")
    # find_line() and find_column() are deprecated
    expect_warning(expr$find_line(1L), "find_line.*deprecated")
    expect_warning(expr$find_column(1L), "find_column.*deprecated")
  }
  full_expr <- exprs$expressions[[length(lines) + 1L]]
  expect_named(full_expr, c(
    "filename", "file_lines", "content", "full_parsed_content", "full_xml_parsed_content", "terminal_newline"
  ))
  expect_identical(full_expr$filename, temp_file)
  expect_identical(full_expr$file_lines, lines_with_attr)
  expect_identical(full_expr$content, lines_with_attr)
  expect_identical(nrow(full_expr$full_parsed_content), 2L * length(lines))
  expect_identical(xml2::xml_find_num(full_expr$full_xml_parsed_content, "count(//SYMBOL)"),
                    as.numeric(length(lines)))
  expect_true(full_expr$terminal_newline)

  expect_null(exprs$error)
  expect_identical(exprs$lines, lines_with_attr)
})

test_that("#1262: xml_parsed_content gets returned as missing even if there's no parsed_content", {
  tempfile <- withr::local_tempfile(lines = '"\\R"')

  source_expressions <- get_source_expressions(tempfile)
  expect_null(source_expressions$expressions[[1L]]$full_parsed_content)
  expect_identical(source_expressions$expressions[[1L]]$full_xml_parsed_content, xml2::xml_missing())
})

test_that("#743, #879, #1406: get_source_expressions works on R files matching a knitr pattern", {
  # from #743
  tempfile <- withr::local_tempfile(
    lines = trim_some('
      create_template <- function(x) {
        sprintf("
      ```{r code}
      foo <- function(x) x+%d
      foo(5)
      ```", x)
      }
    ')
  )
  source_expressions <- get_source_expressions(tempfile)
  expect_null(source_expressions$error)

  # from #879
  tempfile <- withr::local_tempfile(
    lines = trim_some('
      # `r print("7")`
      function() 2<=3
    ')
  )
  source_expressions <- get_source_expressions(tempfile)
  expect_null(source_expressions$error)

  # from #1406
  tempfile <- withr::local_tempfile()
  writeLines(c("x <- '", "```{r}", "'"), con = tempfile)
  source_expressions <- get_source_expressions(tempfile)
  expect_null(source_expressions$error)
})

skip_if_not_installed("patrick")
# NB: this is just a cursory test for linters not to
#   fail on files where the XML content is xml_missing;
#   the main linter test files provide more thorough
#   evidence that things are working as intended.
bad_source <- withr::local_tempfile(lines = c("a <- 1L", "b <- 2L"))
expressions <- get_source_expressions(bad_source)$expressions

# "zap" the xml_parsed_content to be xml_missing -- this gets
#   around the issue of creating a file that fails to parse now,
#   but later fails in a different way -> xml not missing.
for (ii in seq_along(expressions)) {
  if ("xml_parsed_content" %in% names(expressions[[ii]])) {
    expressions[[ii]]$xml_parsed_content <- xml2::xml_missing()
  } else {
    expressions[[ii]]$full_xml_parsed_content <- xml2::xml_missing()
  }
}
param_df <- expand.grid(
  linter = available_linters(tags = NULL)$linter,
  expression_idx = seq_along(expressions),
  stringsAsFactors = FALSE
)
param_df$.test_name <-
 with(param_df, sprintf("%s on expression %d", linter, expression_idx))

patrick::with_parameters_test_that(
  "linters pass with xml_missing() content",
  {
    linter <- eval(call(linter))
    expression <- expressions[[expression_idx]]
    expect_warning(lints <- linter(expression), NA)
    expect_length(lints, 0L)
  },
  .test_name = param_df$.test_name,
  linter = param_df$linter,
  expression_idx = param_df$expression_idx
)
