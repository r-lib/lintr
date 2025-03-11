with_content_to_parse <- function(content, code) {
  f <- withr::local_tempfile()
  local({
    con <- file(f, open = "w", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(content, con)
  })
  source_expressions <- get_source_expressions(f)
  content_env <- new.env()
  content_env$pc <- lapply(source_expressions[["expressions"]], `[[`, "parsed_content")
  content_env$error <- source_expressions$error
  content_env$warning <- source_expressions$warning
  eval(substitute(code), envir = content_env)
}

test_that("tab positions have been corrected", {
  with_content_to_parse(
    "1\n\t",
    expect_length(pc, 2L)
  )

  with_content_to_parse(
    "TRUE",
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == "TRUE", c("col1", "col2")], use.names = FALSE), c(1L, 4L))
  )
  with_content_to_parse(
    "\tTRUE",
    expect_identical(unlist(pc[[1L]][pc[[1L]][["text"]] == "TRUE", c("col1", "col2")], use.names = FALSE), c(2L, 5L))
  )

  with_content_to_parse(
    "\t\tTRUE",
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
  content <- "lm(y ~ x)"
  # NB: need to specify terminal newline explicitly with cat, not writeLines()
  tmp <- withr::local_tempfile(lines = content)
  tmp2 <- withr::local_tempfile()
  cat(content, file = tmp2)

  expect_true(get_source_expressions(tmp)$expressions[[2L]]$terminal_newline)
  expect_false(get_source_expressions(tmp2)$expressions[[2L]]$terminal_newline)
})

test_that("Multi-byte characters correct columns", {
  skip_if_not_utf8_locale()

  with_content_to_parse("`\U2020` <- 1", {
    # fix_column_numbers corrects the start of <-
    expect_identical(pc[[1L]]$col1[4L], pc[[1L]]$col1[2L] + 4L)
  })
})

test_that("Multi-byte character truncated by parser is ignored", {
  skip_if_not_utf8_locale()
  # \U2013 is the Unicode character 'en dash', which is
  # almost identical to a minus sign in monospaced fonts.
  content <- "y <- x \U2013 42"
  # message is like '<text>:1:8: unexpected invalid token\n1: ...'
  with_content_to_parse(content, {
    base_msg <- conditionMessage(tryCatch(str2lang(content), error = identity))
    # Just ensure that the captured message is a substring of the parser error, #2527
    expect_true(grepl(error$message, base_msg, fixed = TRUE, useBytes = TRUE))
    expect_identical(error$column_number, 8L)
  })
})

test_that("Can read non UTF-8 file", {
  withr::local_options(list(lintr.linter_file = tempfile()))
  proj_dir <- test_path("dummy_projects", "project")
  withr::local_dir(proj_dir)
  expect_no_lint( # nofuzz: assignment
    file = "cp1252.R",
    linters = list()
  )
})

test_that("Warns if encoding is misspecified, Pt. 1", {
  proj_dir <- test_path("dummy_projects", "project")
  withr::local_dir(proj_dir)

  lint_msg <- "Invalid multibyte character in parser. Is the encoding correct?"
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    # Prior to R 4.2.0, the Windows parser throws a different error message because the source code is converted to
    # native encoding.
    # This results in line 4 becoming <fc> <- 42 before the parser sees it.
    lint_msg <- "unexpected '<'"
  }

  expect_lint(
    file = "cp1252.R",
    parse_settings = FALSE,
    checks = list(rex::rex(lint_msg), linter = "error", line_number = 4L)
  )

  expect_lint(
    file = "cp1252_parseable.R",
    parse_settings = FALSE,
    checks = list(
      rex::rex("Invalid multibyte string. Is the encoding correct?"),
      linter = "error",
      line_number = 1L
    )
  )
})

test_that("Can extract line number from parser errors", {
  with_content_to_parse(
    trim_some('
      "ok"
      R"---a---"
    '),
    {
      expect_identical(error$message, "Malformed raw string literal.")
      expect_identical(error$line_number, 2L)
    }
  )

  with_content_to_parse(
    trim_some('
      ok
      ok
      "\\u{9999"
    '),
    {
      expect_identical(error$message, "Invalid \\u{xxxx} sequence.")
      expect_identical(error$line_number, 3L)
    }
  )

  with_content_to_parse(
    trim_some('
      ok
      ok
      "\\u{9999
    '),
    {
      # parser erroneously reports line 4
      expect_identical(error$message, "Invalid \\u{xxxx} sequence.")
      expect_identical(error$line_number, 3L)
    }
  )

  with_content_to_parse("function(a, a) {}", {
    expect_identical(error$message, "Repeated formal argument 'a'.")
    expect_identical(error$line_number, 1L)
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
  expect_named(exprs, c("expressions", "error", "warning", "lines"))
  expect_length(exprs$expressions, length(lines) + 1L)

  for (i in seq_along(lines)) {
    expr <- exprs$expressions[[i]]
    expect_named(expr, c(
      "filename", "line", "column", "lines", "parsed_content", "xml_parsed_content", "xml_find_function_calls",
      "content"
    ))
    expect_identical(expr$filename, temp_file)
    expect_identical(expr$line, i)
    expect_identical(expr$column, 1L)
    expect_identical(expr$lines, setNames(lines[i], i))
    expect_identical(nrow(expr$parsed_content), 2L)
    expect_true(xml2::xml_find_lgl(expr$xml_parsed_content, "count(//SYMBOL) > 0"))
    expect_identical(expr$content, lines[i])
  }
  full_expr <- exprs$expressions[[length(lines) + 1L]]
  expect_named(full_expr, c(
    "filename", "file_lines", "content", "full_parsed_content", "full_xml_parsed_content", "xml_find_function_calls",
    "terminal_newline"
  ))
  expect_identical(full_expr$filename, temp_file)
  expect_identical(full_expr$file_lines, lines_with_attr)
  expect_identical(full_expr$content, lines_with_attr)
  expect_identical(nrow(full_expr$full_parsed_content), 2L * length(lines))
  expect_identical(
    xml2::xml_find_num(full_expr$full_xml_parsed_content, "count(//SYMBOL)"),
    as.numeric(length(lines))
  )
  expect_true(full_expr$terminal_newline)

  expect_null(exprs$error)
  expect_identical(exprs$lines, lines_with_attr)
})

test_that("xml_find_function_calls works as intended", {
  lines <- c(
    "foo()",
    "bar()",
    "foo()",
    "s4Obj@baz()",
    "{ foo(); foo(); bar(); s4Obj@baz() }",
    NULL
  )
  temp_file <- withr::local_tempfile(lines = lines)

  exprs <- get_source_expressions(temp_file)

  expect_length(exprs$expressions[[1L]]$xml_find_function_calls("foo"), 1L)
  expect_length(exprs$expressions[[1L]]$xml_find_function_calls("bar"), 0L)
  expect_identical(
    exprs$expressions[[1L]]$xml_find_function_calls("foo"),
    xml_find_all(exprs$expressions[[1L]]$xml_parsed_content, "//SYMBOL_FUNCTION_CALL[text() = 'foo']/parent::expr")
  )

  expect_length(exprs$expressions[[2L]]$xml_find_function_calls("foo"), 0L)
  expect_length(exprs$expressions[[2L]]$xml_find_function_calls("bar"), 1L)

  expect_length(exprs$expressions[[5L]]$xml_find_function_calls("foo"), 2L)
  expect_length(exprs$expressions[[5L]]$xml_find_function_calls("bar"), 1L)
  expect_length(exprs$expressions[[5L]]$xml_find_function_calls(c("foo", "bar")), 3L)

  # file-level source expression contains all function calls
  expect_length(exprs$expressions[[6L]]$xml_find_function_calls("foo"), 4L)
  expect_length(exprs$expressions[[6L]]$xml_find_function_calls("bar"), 2L)
  expect_length(exprs$expressions[[6L]]$xml_find_function_calls(c("foo", "bar")), 6L)

  # Also check order is retained:
  expect_identical(
    exprs$expressions[[6L]]$xml_find_function_calls(c("foo", "bar")),
    xml_find_all(exprs$expressions[[6L]]$full_xml_parsed_content, "//SYMBOL_FUNCTION_CALL/parent::expr")
  )

  # Check naming and full cache
  expect_identical(
    exprs$expressions[[6L]]$xml_find_function_calls(NULL),
    exprs$expressions[[6L]]$xml_find_function_calls(c("foo", "bar"))
  )
  expect_named(
    exprs$expressions[[5L]]$xml_find_function_calls(c("foo", "bar"), keep_names = TRUE),
    c("foo", "foo", "bar")
  )

  # include_s4_slots
  expect_identical(
    exprs$expressions[[6L]]$xml_find_function_calls(NULL, include_s4_slots = TRUE),
    exprs$expressions[[6L]]$xml_find_function_calls(c("foo", "bar", "baz"), include_s4_slots = TRUE)
  )
  expect_named(
    exprs$expressions[[5L]]$xml_find_function_calls(NULL, keep_names = TRUE, include_s4_slots = TRUE),
    c("foo", "foo", "bar", "baz")
  )
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

test_that("Syntax errors in Rmd or qmd don't choke lintr", {
  tmp <- withr::local_tempfile(lines = c(
    "```{r}",
    "if (TRUE) {",
    "  1",
    # missing `}` here
    "if (TRUE) {",
    "}",
    "```"
  ))
  expect_silent(get_source_expressions(tmp))
})

test_that("Indented Rmd chunks don't cause spurious whitespace lints", {
  tmp <- withr::local_tempfile(lines = c(
    "* An enumeration item with code:",
    "",
    "  ```{r}",
    '  "properly indented"',
    "  ```",
    "",
    "# New section",
    "",
    "```{r unindented_chunk}",
    '  "improperly indented"',
    "```",
    "",
    "# Third section",
    "",
    "   ```{r staggered}",
    ' "leftmost code"',
    '  "further right"',
    '   "aligned with code gate"',
    "   ```"
  ))

  parsed_lines <- get_source_expressions(tmp)$lines
  expect_identical(parsed_lines[4L], '"properly indented"', ignore_attr = "names")
  expect_identical(parsed_lines[10L], '  "improperly indented"', ignore_attr = "names")
  expect_identical(parsed_lines[16L], '"leftmost code"', ignore_attr = "names")
  expect_identical(parsed_lines[17L], ' "further right"', ignore_attr = "names")
  expect_identical(parsed_lines[18L], '  "aligned with code gate"', ignore_attr = "names")
})

test_that("Reference chunks in Sweave/Rmd are ignored", {
  example_rnw <- system.file("Sweave", "example-1.Rnw", package = "utils")
  # ensure such a chunk continues to exist upstream
  expect_true(any(grepl("^\\s*<<[^>]*>>\\s*$", readLines(example_rnw))))
  expect_silent(lint(example_rnw))
})

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
param_df$.test_name <- with(param_df, sprintf("%s on expression %d", linter, expression_idx))

patrick::with_parameters_test_that(
  "linters pass with xml_missing() content",
  {
    if (linter == "backport_linter") {
      # otherwise we test the trivial linter (#2339)
      linter <- backport_linter(r_version = "3.6.0")
    } else {
      if (linter == "cyclocomp_linter") {
        skip_if_not_installed("cyclocomp")
      }
      linter <- eval(call(linter))
    }
    expression <- expressions[[expression_idx]]
    is_valid_linter_level <-
      (is_linter_level(linter, "expression") && is_lint_level(expression, "expression")) ||
      (is_linter_level(linter, "file") && is_lint_level(expression, "file"))
    if (is_valid_linter_level) {
      expect_no_warning({
        lints <- linter(expression)
      })
      expect_length(lints, 0L)
    } else {
      # suppress "empty test" skips
      expect_true(TRUE)
    }
  },
  .test_name = param_df$.test_name,
  linter = param_df$linter,
  expression_idx = param_df$expression_idx
)

test_that("invalid function definition parser failure lints", {
  expect_lint(
    "function(a = 1, a = 1) NULL",
    rex::rex("Repeated formal argument 'a'."),
    linters = list()
  )
})

test_that("Disallowed embedded null gives parser failure lint", {
  expect_lint(
    "'\\0'",
    rex::rex("Nul character not allowed."),
    linters = list()
  )
})

test_that("parser warnings are captured in output", {
  with_content_to_parse("1e-3L", {
    expect_length(warning, 1L)
    expect_s3_class(warning, "lints")
  })
  with_content_to_parse("1e-3L; 1e-3L", {
    expect_length(warning, 2L)
  })
  with_content_to_parse("1e-3L; 1.0L; 1.1L", {
    expect_length(warning, 3L)
  })
  with_content_to_parse("1e-3L\n1.0L\n1.1L", {
    expect_length(warning, 3L)
  })
  with_content_to_parse("1e-3L\n1+1\n1.0L\n2+2\n1.1L", {
    expect_length(warning, 3L)
  })
  with_content_to_parse("1e-3L\nc(", {
    expect_length(warning, 1L)
    expect_length(error, 8L)
  })
})

test_that("parser warnings generate lints", {
  expect_lint(
    "1e-3L",
    "non-integer value 1e-3L",
    linters = list()
  )
  expect_lint(
    "1e-3L; 1e-3L",
    list(
      list("non-integer value 1e-3L", column_number = 1L),
      list("non-integer value 1e-3L", column_number = 8L)
    ),
    linters = list()
  )
  expect_lint(
    "1e-3L; 1.0L",
    list(
      list("non-integer value 1e-3L", column_number = 1L),
      list("integer literal 1\\.0L", column_number = 8L)
    ),
    linters = list()
  )
  expect_lint(
    trim_some("
      1e-3L
      1 + 1
      1.0L
      2 + 2
      1.1L
      3 + 3
      2.2L
      4 + 4
      2.0L
      5 + 5
      2e-3L
      # don't match strictly on regex, use parse tree
      # 3.0L
      # 3e-3L
      # 3.3L
      '4.0L'
      '4e-3L'
      '4.4L'
    "),
    list(
      list("non-integer value 1e-3L", line_number = 1L),
      list("integer literal 1\\.0L", line_number = 3L),
      list("integer literal 1.1L contains decimal", line_number = 5L),
      list("integer literal 2\\.2L contains decimal", line_number = 7L),
      list("integer literal 2\\.0L", line_number = 9L),
      list("non-integer value 2e-3L", line_number = 11L)
    ),
    linters = list()
  )
  # parser catches warning before erroring
  expect_lint(
    "1e-3L; c(",
    list(
      list("non-integer value 1e-3L", type = "warning"),
      list("unexpected end of input", type = "error")
    ),
    linters = list()
  )
})
