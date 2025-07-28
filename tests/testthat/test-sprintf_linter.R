patrick::with_parameters_test_that(
  "sprintf_linter skips allowed usages",
  {
    linter <- sprintf_linter()

    # NB: using paste0, not sprintf, to avoid escaping '%d' in sprint fmt=
    expect_no_lint(paste0(call_name, "('hello %d', 1)"), linter)
    expect_no_lint(paste0(call_name, "('hello %d', x)"), linter)
    expect_no_lint(paste0(call_name, "('hello %d', x + 1)"), linter)
    expect_no_lint(paste0(call_name, "('hello %d', f(x))"), linter)
    expect_no_lint(paste0(call_name, "('hello %1$s %1$s', x)"), linter)
    expect_no_lint(paste0(call_name, "('hello %1$s %1$s %2$d', x, y)"), linter)
    expect_no_lint(paste0(call_name, "('hello %1$s %1$s %2$d %3$s', x, y, 1.5)"), linter)
  },
  .test_name = c("sprintf", "gettextf"),
  call_name = c("sprintf", "gettextf")
)

patrick::with_parameters_test_that(
  "sprintf_linter blocks disallowed usages",
  {
    linter <- sprintf_linter()
    unused_arg_msg <- if (getRversion() >= "4.1.0") "one argument not used by format" else NULL

    expect_lint(paste0(call_name, "('hello', 1)"), "constant", linter)

    expect_lint(paste0(call_name, "('hello')"), "constant", linter)
    expect_lint(paste0(call_name, "('100%% automated')"), "constant", linter)
    expect_lint(paste0(call_name, "('100%%%% automated')"), "constant", linter)
    expect_lint(paste0(call_name, "('100%%%s')"), "too few", linter)
    expect_lint(paste0(call_name, "('100%%%%s', x)"), "constant", linter)

    expect_lint(
      paste0(call_name, "('hello %d', 'a')"),
      rex::rex("invalid format '%d'; use format %s for character objects"),
      linter
    )

    expect_lint(
      paste0(call_name, "('hello %d', 1.5)"),
      rex::rex("invalid format '%d'; use format %f, %e, %g or %a for numeric objects"),
      linter
    )

    expect_lint(
      paste0(call_name, "('hello %d',)"),
      rex::rex("argument is missing, with no default"),
      linter
    )

    expect_lint(paste0(call_name, "('hello %1$s %s', 'a', 'b')"), unused_arg_msg, linter)
    expect_lint(paste0(call_name, "('hello %1$s %1$s', x, y)"), unused_arg_msg, linter)

    expect_lint(
      paste0(call_name, "('hello %1$s %1$s %3$d', x, y)"),
      rex::rex("reference to non-existent argument 3"),
      linter
    )

    expect_lint(
      paste0(call_name, "('hello %1$s %1$s %2$d %3$d', x, y, 1.5)"),
      rex::rex("invalid format '%d'; use format %f, %e, %g or %a for numeric objects"),
      linter
    )
  },
  .test_name = c("sprintf", "gettextf"),
  call_name = c("sprintf", "gettextf")
)

test_that("edge cases are detected correctly", {
  linter <- sprintf_linter()

  # works with multi-line sprintf and comments
  expect_no_lint(
    trim_some("
      sprintf(
        'test fmt %s', # this is a comment
        2
      )
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      'test fmt %s' |>   # this is a pipe comment
        sprintf(         # this is an opening comment
          2              # this is a mid-call comment
        )
    "),
    linter
  )

  expect_lint(
    trim_some("
      'test fmt' |>   # this is a pipe comment
        sprintf()
    "),
    "constant",
    linter
  )

  # dots
  expect_no_lint("sprintf('%d %d, %d', id, ...)", linter)

  # TODO(#1265) extend ... detection to at least test for too many arguments.

  # named argument fmt
  expect_no_lint("sprintf(x, fmt = 'hello %1$s %1$s')", linter)

  expect_lint(
    "sprintf(x, fmt = 'hello %1$s %1$s %3$d', y)",
    list(message = rex::rex("reference to non-existent argument 3")),
    linter
  )

  # #2131: xml2lang stripped necessary whitespace
  expect_no_lint("sprintf('%s', if (A) '' else y)", linter)

  expect_no_lint("sprintf('100%%%s', x)", linter)
})

local({
  linter <- sprintf_linter()
  unused_fmt_msg <- "too few arguments"
  unused_arg_msg <- "one argument not used by format"
  pipes <- pipes(exclude = "%$%")
  patrick::with_parameters_test_that(
    "piping into sprintf works",
    {
      expect_no_lint(paste("x", pipe, "sprintf(fmt = '%s')"), linter)
      # no fmt= specified -> this is just 'sprintf("%s", "%s%s")', which won't lint
      expect_no_lint(paste('"%s"', pipe, 'sprintf("%s%s")'), linter)
      expect_lint(paste("x", pipe, "sprintf(fmt = '%s%s')"), unused_fmt_msg, linter)

      # Cannot evaluate statically --> skip
      expect_no_lint(paste("x", pipe, 'sprintf("a")'), linter)
      # Nested pipes
      expect_lint(
        paste("'%%sb'", pipe, "sprintf('%s')", pipe, "sprintf('a')"),
        if (getRversion() >= "4.1.0") list(column_number = nchar(paste("'%%sb'", pipe, "x")), message = "constant"),
        linter
      )
      expect_lint(
        paste("x", pipe, 'sprintf(fmt = "%s")', pipe, 'sprintf(fmt = "%s%s")'),
        list(column_number = nchar(paste("x", pipe, 'sprintf(fmt = "%s")', pipe, "x")), message = unused_fmt_msg),
        linter
      )
      expect_lint(
        paste("x", pipe, 'sprintf(fmt = "%s%s")', pipe, 'sprintf(fmt = "%s")'),
        list(column_number = nchar(paste("x", pipe, "x")), message = unused_fmt_msg),
        linter
      )
    },
    pipe = pipes,
    .test_name = names(pipes)
  )
})

test_that("pipe logic survives adversarial comments", {
  linter <- sprintf_linter()

  expect_no_lint(
    trim_some("
      x |> # comment
      sprintf(fmt = '%s')
    "),
    linter
  )

  expect_no_lint(
    trim_some('
      "%s" %>% # comment
      sprintf("%s%s")
    '),
    linter
  )
})

test_that("lints vectorize", {
  skip_if_not_r_version("4.1.0")

  expect_lint(
    trim_some("{
      sprintf('%s', a, b)
      sprintf('%s%s', a)
    }"),
    list(
      list("one argument not used by format", line_number = 2L),
      list("too few arguments", line_number = 3L)
    ),
    sprintf_linter()
  )
})
