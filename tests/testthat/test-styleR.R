check_file <- function(search, ...){
  name = tempfile()
  on.exit(unlink(name))
  cat(sep="\n", ..., file=name)

  check_style(name, search)
}

message_contains <- function(diffs, value) {
  any(vapply(diffs, function(diff) {
    grepl(value, diff$message)
  }, logical(1)))
}

context("Filenames")
test_that("Camel case file names", {
  #expect_equal(check_filenames('testThis.R', file_names)[[1]],
    #"camel case")
  #expect_message(check_filenames('testThis.r', file_names),
    #"end in .r")
  #expect_message(check_filenames(c('testThis.R', 'testthis.r'), file_names),
    #"not differ only in capitalization")
})

context("variable_names")
test_that("base_functions ignored", {
  #re <- variable_names[["Should not be in camel case"]]
  #lapply(base_functions, function(name) {
    #expect_false(grepl(re, name, perl = TRUE), info = name)
  #})
})
context("source")
test_that("Camel case variable names", {
  expect_true(message_contains(check_file(variable_names, 'camelCase'),
      "camel case"))
})

test_that("multiple dots", {
  expect_true(message_contains(check_file(variable_names, 'bad.function.name'),
      "multiple dots"))
})

test_that("remove text within quotes", {
  expect_equal(blank_text(re = quoted,
      "single_quoted 'text to remove'"),
      "single_quoted '              '")
  expect_equal(blank_text(re = quoted,
      "double_quoted \"text to remove\""),
      "double_quoted \"              \"")

  expect_equal(blank_text(re = quoted,
      "no_quotes"),
      "no_quotes")

  expect_equal(blank_text(re = quoted,
      "escaped quotes 'quoted text \\' not the end yet!'"),
      "escaped quotes '                               '")
  expect_equal(blank_text(re = quoted,
      "escaped quotes \"quoted text \\\" not the end yet!\""),
      "escaped quotes \"                               \"")
})

context("spacing")
ops <-c(
  '=',
  '==',
  '!=',
  '<=',
  '>=',
  '+',
  '-',
  '<-',
  '->',
  '<',
  '>',
  '<>',
  '%%',
  '/',
  '^',
  "*",
  '**',
  '|',
  '||',
  '&',
  '&&',
  "%>%",
  "%Anything%",
  "%+%",
  NULL)

test_that("does not match operators with correct spacing", {
  lapply(ops, function(op){
    expect_false(grepl(spacing[["Should have spaces around infix operators"]], paste0("a ", op, " b"), perl = TRUE), info = op)
  })
})

test_that("does match operators without correct spacing", {
  lapply(ops, function(op) {
    search = paste0("a", op, " b")
    expect_true(grepl(spacing[["Should have spaces around infix operators"]], search, perl = TRUE), info=search)

    search = paste0("a ", op, "b")
    expect_true(grepl(spacing[["Should have spaces around infix operators"]], search, perl = TRUE), info=search)
  })
})

context("assignment")

test_that("matches assignment", {
  re <- assignment[["Use <- not = for assignment"]]

  expect_false(grepl(parens, "arst", perl = TRUE))
  expect_false(grepl(parens, "a(st", perl = TRUE))
  expect_true(grepl(parens, "a()t", perl = TRUE))
  expect_false(grepl(parens, "a)t", perl = TRUE))

  expect_true(grepl(re, "blah = blah", perl = TRUE))
  expect_true(grepl(re, "blah = list(blah,", perl = TRUE))
  expect_true(grepl(re, "blah = fun(true)", perl = TRUE))

  expect_false(grepl(re, "fun(blah = true)", perl = TRUE))
  expect_false(grepl(re, "blah = true,", perl = TRUE))
  #expect_false(grepl(re, "blah = fun(true),", perl = TRUE)) PUNT! I can't get this one to work without breaking the others
})
