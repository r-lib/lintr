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
  expect_equal(blank_quoted_text(
      "single_quoted 'text to remove'"),
      "single_quoted '              '")
  expect_equal(blank_quoted_text(
      "double_quoted \"text to remove\""),
      "double_quoted \"              \"")

  expect_equal(blank_quoted_text(
      "no_quotes"),
      "no_quotes")

  expect_equal(blank_quoted_text(
      "escaped quotes 'quoted text \\' not the end yet!'"),
      "escaped quotes '                               '")
  expect_equal(blank_quoted_text(
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
  '!',
  "%>%",
  "%Anything%",
  "%+%",
  NULL)

test_that("does not match operators with correct spacing", {
  with_spaces <- ops[grepl(spacing[["Should have spaces around operators"]], paste0("a ", ops, " b"), perl=TRUE)]
  expect_equal(with_spaces, character(0), info=paste("bad operators: ", paste0(collapse=", ", with_spaces)))
})

test_that("does match operators without correct spacing", {
  for(op in ops){
    search = paste0("a", op, " b")
    expect_true(grepl(spacing[["Should have spaces around operators"]], search, perl = TRUE), info=search)

    search = paste0("a ", op, "b")
    expect_true(grepl(spacing[["Should have spaces around operators"]], search, perl = TRUE), info=search)
  }
})
