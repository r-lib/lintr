test_that("linter returns correct linting", {
  linter <- undesirable_function_linter(fun = c(return = NA, log10 = "use log()"))
  msg_return <- rex::rex('Avoid undesirable function "return".', end)
  msg_log10 <- rex::rex('Avoid undesirable function "log10". As an alternative, use log().')

  expect_no_lint("x <- options()", linter)
  expect_no_lint("cat(\"Try to return\")", linter)
  expect_lint("lapply(x, log10)", list(message = msg_log10, line_number = 1L, column_number = 11L), linter)
  expect_lint("return()", list(message = msg_return, line_number = 1L, column_number = 1L), linter)
  expect_lint(
    trim_some("
      function(x) {
        print(options())
        y <- log10(x)
        return(y)
      }"),
    list(
      list(message = msg_log10, line_number = 3L, column_number = 8L),
      list(message = msg_return, line_number = 4L, column_number = 3L)
    ),
    linter
  )
  # regression test for #1050
  expect_no_lint("df$return <- 1", linter)
  expect_no_lint("df@return <- 1", linter)
})

test_that("it's possible to NOT lint symbols", {
  linter <- undesirable_function_linter(
    fun = c(dir = NA, log10 = "use log()"),
    symbol_is_undesirable = FALSE
  )
  expect_no_lint("dir <- 'path/to/a/directory'", linter)
  expect_no_lint("lapply(x, log10)", linter)
})

test_that("undesirable_function_linter doesn't lint library and require calls", {
  linter <- undesirable_function_linter(fun = c(foo = NA))
  expect_lint("test::foo()", "undesirable", linter)
  expect_no_lint("foo::test()", linter)
  expect_no_lint("library(foo)", linter)
  expect_no_lint("require(foo)", linter)

  linter <- undesirable_function_linter(fun = c(foo = NA, bar = NA))
  expect_no_lint("library(foo)", linter)

  linter <- undesirable_function_linter(fun = c(foo = NA, bar = NA), symbol_is_undesirable = FALSE)
  expect_no_lint("library(foo)", linter)
})

# regression test for #866
test_that("Line numbers are extracted correctly", {
  lines <- c(rep(letters, 10L), "tmp <- tempdir()")
  expect_lint(paste(lines, collapse = "\n"), "undesirable", undesirable_function_linter(c(tempdir = NA)))
})

test_that("invalid inputs fail correctly", {
  expect_error(
    undesirable_function_linter(fun = NULL),
    "`fun` must be a non-empty character vector",
    fixed = TRUE
  )
  expect_error(
    undesirable_function_linter(fun = character(0L)),
    "`fun` must be a non-empty character vector",
    fixed = TRUE
  )
  expect_error(
    undesirable_function_linter(c(NA, NA)),
    rex::rex("Unnamed elements of `fun` must not be missing", anything, "1", anything, "2")
  )

  expect_error(
    undesirable_function_linter(symbol_is_undesirable = 1.0),
    "symbol_is_undesirable` must be a logical",
    fixed = TRUE
  )
})

test_that("Default recommendations can be specified multiple ways", {
  linter_na <- undesirable_function_linter(c(foo = NA))
  linter_unnamed1 <- undesirable_function_linter("foo")
  linter_unnamed2 <- undesirable_function_linter(c("foo", "bar"))
  linter_mixed1 <- undesirable_function_linter(c("foo", bar = "no bar"))
  linter_mixed2 <- undesirable_function_linter(c("foo", bar = NA))

  lint_message <- rex::rex('Avoid undesirable function "foo"')

  lint_str <- "foo()"
  expect_lint(lint_str, lint_message, linter_na)
  expect_lint(lint_str, lint_message, linter_unnamed1)
  expect_lint(lint_str, lint_message, linter_unnamed2)
  expect_lint(lint_str, lint_message, linter_mixed1)
  expect_lint(lint_str, lint_message, linter_mixed2)
})

test_that("setter calls are distinguished from getter calls (#1912)", {
  getter_linter <- undesirable_function_linter(c(time = NA))
  setter_linter <- undesirable_function_linter(c(`time<-` = "use zoo::time<-()"))
  both_linter <- undesirable_function_linter(c(time = NA, `time<-` = "use zoo::time<-()"))
  msg_getter <- rex::rex('Avoid undesirable function "time".', end)
  msg_setter <- rex::rex('Avoid undesirable function "time<-". As an alternative, use zoo::time<-().')

  expect_lint("time(x)", list(msg_getter, column_number = 1L), getter_linter)
  expect_no_lint("time(x)", setter_linter)

  expect_no_lint("time(x) <- 1", getter_linter)
  expect_lint("time(x) <- 1", list(msg_setter, column_number = 1L), setter_linter)

  expect_no_lint("time(x) <<- 1", getter_linter)
  expect_lint("time(x) <<- 1", list(msg_setter, column_number = 1L), setter_linter)

  expect_no_lint("time(x) = 1", getter_linter)
  expect_lint("time(x) = 1", list(msg_setter, column_number = 1L), setter_linter)

  expect_no_lint("1 -> time(x)", getter_linter)
  expect_lint("1 -> time(x)", list(msg_setter, column_number = 6L), setter_linter)

  expect_no_lint("1 ->> time(x)", getter_linter)
  expect_lint("1 ->> time(x)", list(msg_setter, column_number = 7L), setter_linter)

  expect_no_lint("stats::time(x) <- 1", getter_linter)
  expect_lint("stats::time(x) <- 1", list(msg_setter, column_number = 8L), setter_linter)

  expect_no_lint("time(names(x)) <- 1", getter_linter)
  expect_lint("time(names(x)) <- 1", list(msg_setter, column_number = 1L), setter_linter)
  expect_lint("names(time(x)) <- 'a'", list(msg_getter, column_number = 7L), getter_linter)
  expect_no_lint("names(time(x)) <- 'a'", setter_linter)

  expect_lint("DT[, time(x) := 1]", list(msg_getter, column_number = 6L), getter_linter)
  expect_no_lint("DT[, time(x) := 1]", setter_linter)

  expect_lint("mutate(df, !!time(x) := 1)", list(msg_getter, column_number = 14L), getter_linter)
  expect_no_lint("mutate(df, !!time(x) := 1)", setter_linter)

  expect_lint("x <- time(y)", list(msg_getter, column_number = 6L), getter_linter)
  expect_no_lint("x <- time(y)", setter_linter)

  expect_lint("`time<-`(x, 1)", list(msg_setter, column_number = 1L), setter_linter)
  expect_lint("stats::`time<-`(x, 1)", list(msg_setter, column_number = 8L), setter_linter)
  expect_lint("lapply(x, `time<-`, 1)", list(msg_setter, column_number = 11L), setter_linter)
  expect_no_lint(
    "lapply(x, `time<-`, 1)",
    undesirable_function_linter(c(`time<-` = NA), symbol_is_undesirable = FALSE)
  )

  expect_lint("time(x) <- time(y)", list(msg_getter, column_number = 12L), getter_linter)
  expect_lint("time(x) <- time(y)", list(msg_setter, column_number = 1L), setter_linter)
  expect_lint(
    "time(x) <- time(y)",
    list(
      list(msg_setter, column_number = 1L),
      list(msg_getter, column_number = 12L)
    ),
    both_linter
  )
  expect_lint(
    "time(y) + (time(x) <- 1)",
    list(
      list(msg_getter, column_number = 1L),
      list(msg_setter, column_number = 12L)
    ),
    both_linter
  )

  # Non-syntactic and explicitly backticked names
  expect_lint(
    "`time<-`(x, 1)",
    rex::rex('Avoid undesirable function "time<-".', end),
    undesirable_function_linter("`time<-`")
  )
  expect_lint(
    "`%in%`(x, y)",
    rex::rex('Avoid undesirable function "%in%".', end),
    undesirable_function_linter("%in%")
  )
  expect_lint(
    "`foo bar`(x) <- 1",
    rex::rex('Avoid undesirable function "foo bar<-".', end),
    undesirable_function_linter("foo bar<-")
  )
})

test_that("multiple assignments are handle according to R precedence rules", { # nofuzz: assignment
  getter_linter <- undesirable_function_linter(c(time = NA))
  setter_linter <- undesirable_function_linter(c(`time<-` = "use zoo::time<-()"))
  msg_getter <- rex::rex('Avoid undesirable function "time".', end)
  msg_setter <- rex::rex('Avoid undesirable function "time<-". As an alternative, use zoo::time<-().')

  # `<-` has higher precedence than `=`, so `a <- time(x) = 1` parses as `(a <- time(x)) = 1`
  expect_no_lint("a <- time(x) <- 1", getter_linter)
  expect_lint("a <- time(x) <- 1", list(msg_setter, column_number = 6L), setter_linter)
  expect_no_lint("a = time(x) <- 1", getter_linter)
  expect_lint("a = time(x) <- 1", list(msg_setter, column_number = 5L), setter_linter)
  expect_lint("a <- time(x) = 1", list(msg_getter, column_number = 6L), getter_linter)
  expect_no_lint("a <- time(x) = 1", setter_linter)
  expect_no_lint("1 -> a -> time(x)", getter_linter)
  expect_lint("1 -> a -> time(x)", list(msg_setter, column_number = 11L), setter_linter)
})

test_that("namespace-qualified functions are matched selectively (#1770)", {
  linter <- undesirable_function_linter(c(
    "bar::run_model" = "use foo::run_model()",
    "bar:::internal_helper" = NA,
    "bar::`%in%`" = NA,
    "bar::`time<-`" = NA
  ))
  msg_bar <- rex::rex('Avoid undesirable function "bar::run_model". As an alternative, use foo::run_model().')
  msg_int <- rex::rex('Avoid undesirable function "bar:::internal_helper".', end)
  msg_in <- rex::rex('Avoid undesirable function "bar::%in%".', end)
  msg_set <- rex::rex('Avoid undesirable function "bar::time<-".', end)

  expect_no_lint("run_model()", linter)
  expect_no_lint("foo::run_model()", linter)
  expect_no_lint("bar:::run_model()", linter)
  expect_no_lint("lapply(x, run_model)", linter)
  expect_no_lint("lapply(x, foo::run_model)", linter)
  expect_no_lint("internal_helper()", linter)
  expect_no_lint("bar::internal_helper()", linter)
  expect_no_lint("`%in%`(x, y)", linter)
  expect_no_lint("foo::`%in%`(x, y)", linter)
  expect_no_lint("time(x) <- 1", linter)
  expect_no_lint("foo::time(x) <- 1", linter)

  expect_lint("bar::run_model()", list(msg_bar, line_number = 1L, column_number = 6L), linter)
  expect_lint("lapply(x, bar::run_model)", list(msg_bar, line_number = 1L, column_number = 16L), linter)
  expect_no_lint(
    "lapply(x, bar::run_model)",
    undesirable_function_linter(c("bar::run_model" = NA), symbol_is_undesirable = FALSE)
  )
  expect_lint("bar:::internal_helper()", list(msg_int, line_number = 1L, column_number = 7L), linter)
  expect_lint("bar::`%in%`(x, y)", list(msg_in, line_number = 1L, column_number = 6L), linter)
  expect_lint("bar::time(x) <- 1", list(msg_set, line_number = 1L, column_number = 6L), linter)

  # Specific namespace message overrides generic unqualified message when both are supplied
  combined_linter <- undesirable_function_linter(c(
    run_model = "use model()",
    "bar::run_model" = "use foo::run_model()"
  ))
  msg_generic <- rex::rex('Avoid undesirable function "run_model". As an alternative, use model().')
  expect_lint("run_model()", list(msg_generic, column_number = 1L), combined_linter)
  expect_lint("foo::run_model()", list(msg_generic, column_number = 6L), combined_linter)
  expect_lint("bar::run_model()", list(msg_bar, column_number = 6L), combined_linter)
})
