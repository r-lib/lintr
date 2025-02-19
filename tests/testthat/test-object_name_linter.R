# styler: off
test_that("styles are correctly identified", {
  do_style_check <- function(nms) lapply(unname(style_regexes), lintr:::check_style, nms = nms)

  #                                            symbl   UpC   lowC   snake  SNAKE  dot    alllow  ALLUP
  expect_identical(do_style_check("x"),   list(FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,   TRUE,  FALSE))
  expect_identical(do_style_check(".x"),  list(FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,   TRUE,  FALSE))
  expect_identical(do_style_check("X"),   list(FALSE, TRUE,  FALSE, FALSE, TRUE,  FALSE,  FALSE,  TRUE))
  expect_identical(do_style_check("x."),  list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_identical(do_style_check("X."),  list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_identical(do_style_check("x_"),  list(FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE,  FALSE, FALSE))
  expect_identical(do_style_check("X_"),  list(FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE,  FALSE, FALSE))
  expect_identical(do_style_check("xy"),  list(FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,   TRUE,  FALSE))
  expect_identical(do_style_check("xY"),  list(FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_identical(do_style_check("Xy"),  list(FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_identical(do_style_check("XY"),  list(FALSE, TRUE,  FALSE, FALSE, TRUE,  FALSE,  FALSE,  TRUE))
  expect_identical(do_style_check("x1"),  list(FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,   TRUE,  FALSE))
  expect_identical(do_style_check("X1"),  list(FALSE, TRUE,  FALSE, FALSE, TRUE,  FALSE,  FALSE,  TRUE))
  expect_identical(do_style_check("x_y"), list(FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE,  FALSE, FALSE))
  expect_identical(do_style_check("X_Y"), list(FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE,  FALSE, FALSE))
  expect_identical(do_style_check("X.Y"), list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_identical(do_style_check("x_2"), list(FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE,  FALSE, FALSE))
  expect_identical(do_style_check("X_2"), list(FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE,  FALSE, FALSE))
  expect_identical(do_style_check("x.2"), list(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,   FALSE, FALSE))
  expect_identical(do_style_check("X.2"), list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))

  #                                                     symbl   UpC   lowC   snake  SNAKE  dot    alllow  ALLUP
  expect_identical(do_style_check("IHave1Cat"),     list(FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(do_style_check("iHave1Cat"),     list(FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(do_style_check("i_have_1_cat"),  list(FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE))
  expect_identical(do_style_check("I_HAVE_1_CAT"),  list(FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE))
  expect_identical(do_style_check("i.have.1.cat"),  list(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE))
  expect_identical(do_style_check("ihave1cat"),     list(FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,  TRUE,  FALSE))
  expect_identical(do_style_check("IHAVE1CAT"),     list(FALSE, TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE, TRUE))
  expect_identical(do_style_check("I.HAVE_ONECAT"), list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(do_style_check("."),             list(TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(do_style_check("%^%"),           list(TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
})
# styler: on

test_that("linter ignores some objects", {
  # names for which style check is ignored
  expect_no_lint("`%X%` <- t", object_name_linter("SNAKE_CASE")) # operator
  expect_no_lint("`%x%` <- t", object_name_linter("snake_case")) # operator
  expect_no_lint("`t.test` <- t", object_name_linter("UPPERCASE")) # std pkg
  expect_no_lint(".Deprecated('x')", object_name_linter("lowercase")) # std pkg
  expect_no_lint("print.foo <- t", object_name_linter("CamelCase")) # S3 generic
  expect_no_lint("names.foo <- t", object_name_linter("CamelCase")) # int generic
  expect_no_lint("sapply(x,f,USE.NAMES=T)", object_name_linter("snake_case")) # defined elsewhere
  expect_no_lint(".onLoad <- function(...) TRUE", object_name_linter("snake_case")) # namespace hooks, #500
  expect_no_lint(".First <- function(...) TRUE", object_name_linter("snake_case")) # namespace hooks
  expect_no_lint("`%++%` <- `+`", object_name_linter("symbols")) # all-symbol operator
  expect_no_lint("`%<-%` <- `+`", object_name_linter("symbols")) # all-symbol operator #495
  # S3 group generic, #1841
  expect_no_lint(
    "`==.snake_case` <- function(a, b) unclass(a) == unclass(b)",
    object_name_linter("snake_case")
  )
})

test_that("linter returns correct linting", {
  lint_msg <- "Variable and function name style should match camelCase."
  linter <- object_name_linter("camelCase")

  expect_no_lint("myObject <- 123", linter)
  expect_no_lint("`myObject` <- 123", linter)
  expect_lint("my.confused_NAME <- 1;", list(message = lint_msg, line_number = 1L, column_number = 1L), linter)
  expect_lint("1 ->> read.data.frame;", list(message = lint_msg, line_number = 1L, column_number = 7L), linter)
  expect_lint(
    "object_name_linter <- function(...) {}",
    list(message = lint_msg, line_number = 1L, column_number = 1L), linter
  )

  expect_lint(
    "Z = sapply('function', function(x=function(x){1}, b.a.z=F, ...){identity(b.a.z)}, USE.NAMES=TRUE)",
    list(
      list(message = lint_msg, line_number = 1L, column_number = 1L),
      list(message = lint_msg, line_number = 1L, column_number = 51L)
    ),
    linter
  )

  expect_no_lint("blah", linter)
  expect_no_lint("invokeRestartInteractively", linter)
  expect_no_lint("camelCase", linter)
  expect_no_lint("camelCase()", linter)
  expect_no_lint("pack::camelCase", linter)
  expect_no_lint("pack:::camelCase", linter)
  expect_no_lint("a(camelCase = 1)", linter)
  expect_no_lint("a$b <- 1", linter)
})

test_that("linter accepts vector of styles", {
  lint_msg <- "Variable and function name style should match camelCase or dotted.case."
  linter <- object_name_linter(styles = c("camelCase", "dotted.case"))

  expect_lint(
    "var.one <- 1\nvarTwo <- 2\nvar_three <- 3",
    list(message = lint_msg, line_number = 3L, column_number = 1L),
    linter
  )
})

test_that("dollar subsetting only lints the first expression", {
  # Regression test for #582
  linter <- object_name_linter()
  lint_msg <- rex::rex("Variable and function name style should match snake_case or symbols.")

  expect_no_lint("my_var$MY_COL <- 42L", linter)
  expect_lint("MY_VAR$MY_COL <- 42L", lint_msg, linter)
  expect_no_lint("my_var@MY_SUB <- 42L", linter)
  expect_lint("MY_VAR@MY_SUB <- 42L", lint_msg, linter)
})

patrick::with_parameters_test_that(
  "nested extraction only lints on the first symbol",
  expect_lint(
    sprintf("%s%sMY_SUB%sMY_COL <- 42L", if (should_lint) "MY_VAR" else "my_var", op1, op2),
    if (should_lint) rex::rex("Variable and function name style should match snake_case or symbols."),
    object_name_linter()
  ),
  .cases = within(
    expand.grid(should_lint = c(TRUE, FALSE), op1 = c("$", "@"), op2 = c("$", "@"), stringsAsFactors = FALSE),
    {
      .test_name <- sprintf("(should lint? %s, op1=%s, op2=%s)", should_lint, op1, op2)
    }
  )
)

test_that("assignment targets of compound lhs are correctly identified", {
  linter <- object_name_linter()
  lint_msg <- "Variable and function name style should match snake_case or symbols."

  # (recursive) [, $, and [[ subsetting
  expect_no_lint("good_name[badName] <- badName2", linter)
  expect_no_lint("good_name[1L][badName] <- badName2", linter)
  expect_no_lint("good_name[[badName]] <- badName2", linter)
  expect_no_lint("good_name[[1L]][[badName]] <- badName2", linter)
  expect_no_lint("good_name[[fun(badName)]] <- badName2", linter)
  expect_no_lint("good_name[[badName]]$badName2 <- badName3", linter)
  expect_no_lint("good_name$badName[[badName2]][badName3]$badName4 <- badName5", linter)

  expect_lint("badName[badName] <- badName2", lint_msg, linter)
  expect_lint("badName[1L][badName] <- badName2", lint_msg, linter)
  expect_lint("badName[[badName]] <- badName2", lint_msg, linter)
  expect_lint("badName[[1L]][[badName]] <- badName2", lint_msg, linter)
  expect_lint("badName[[fun(badName)]] <- badName2", lint_msg, linter)
  expect_lint("badName[[badName]]$badName2 <- badName3", lint_msg, linter)
  expect_lint("badName$badName[[badName2]][badName3]$badName4 <- badName5", lint_msg, linter)

  # setters
  expect_lint("setter(badName) <- good_name", lint_msg, linter)
  expect_no_lint("setter(good_name[[badName]]) <- good_name2", linter)

  # quotation
  expect_no_lint('"good_name" <- 42', linter)
  expect_lint('"badName" <- 42', lint_msg, linter)
  expect_no_lint("'good_name' <- 42", linter)
  expect_lint("'badName' <- 42", lint_msg, linter)
  expect_no_lint("`good_name` <- 42", linter)
  expect_lint("`badName` <- 42", lint_msg, linter)

  # subsetting with quotation
  expect_no_lint("good_name$\"badName\" <- 42", linter)
  expect_no_lint("good_name$'badName' <- 42", linter)
  expect_lint("badName$\"good_name\" <- 42", lint_msg, linter)
  expect_lint("badName$'good_name' <- 42", lint_msg, linter)
  expect_lint("`badName`$\"good_name\" <- 42", lint_msg, linter)
  expect_lint("`badName`$'good_name' <- 42", lint_msg, linter)
})

test_that("object_name_linter won't fail if an imported namespace is unavailable", {
  expect_length(
    lint_package(test_path("dummy_packages", "missing_dep"), linters = object_name_linter(), parse_settings = FALSE),
    3L
  )
})

test_that("object_name_linter supports custom regexes", {
  # disables default styles
  linter <- object_name_linter(
    regexes = c(shinyModule = rex::rex(start, lower, zero_or_more(alnum), "UI" %or% "Server", end))
  )
  msg <- rex::rex("Variable and function name style should match shinyModule.")
  linter2 <- object_name_linter(
    styles = c("snake_case", "symbols"),
    regexes = c(shinyModule = rex::rex(start, lower, zero_or_more(alnum), "UI" %or% "Server", end))
  )
  msg2 <- rex::rex("Variable and function name style should match snake_case, symbols or shinyModule.")

  # Can't allow 0 styles
  expect_error(
    object_name_linter(NULL),
    rex::rex("At least one style must be specified using `styles` or `regexes`.")
  )

  expect_lint(
    trim_some('
      snake_case <- 42L
      "%+%" <- function(...) ..1 + ..2

      myModuleUI <- function(id) {
        # blah
      }

      myModuleServer <- function(id) {
        # blah
      }

      myBadName <- 20L
    '),
    list(
      list(line_number = 1L, message = msg),
      list(line_number = 2L, message = msg),
      # argument "id" is linted if we only allow shinyModule names
      list(line_number = 4L, column_number = 24L, message = msg),
      list(line_number = 8L, column_number = 28L, message = msg),
      list(line_number = 12L, message = msg)
    ),
    linter
  )

  expect_lint(
    trim_some('
      snake_case <- 42L
      "%+%" <- function(...) ..1 + ..2

      myModuleUI <- function(id) {
        # blah
      }

      myModuleServer <- function(id) {
        # blah
      }

      myBadName <- 20L
    '),
    list(line_number = 12L, message = msg2),
    linter2
  )

  # Default regex naming works
  expect_lint(
    trim_some("
      a <- 42L
      b <- 1L
      c <- 2L
    "),
    list(line_number = 3L, message = rex::rex("Variable and function name style should match /^a$/ or /^b$/.")),
    object_name_linter(regexes = c("^a$", "^b$"))
  )

  expect_lint(
    trim_some("
      a <- 42L
      b <- 1L
      c <- 2L
    "),
    list(line_number = 3L, message = rex::rex("Variable and function name style should match a or /^b$/.")),
    object_name_linter(regexes = c(a = "^a$", "^b$"))
  )
})

test_that("complex LHS of := doesn't cause false positive", {
  # "_l" would be included under previous logic which tried ancestor::expr[ASSIGN] for STR_CONST,
  #   but only parent::expr[ASSIGN] is needed for strings.
  expect_no_lint('dplyr::mutate(df, !!paste0(v, "_l") := df$a * 2)', object_name_linter())
})

test_that("function shorthand also lints", {
  skip_if_not_r_version("4.1.0")

  expect_lint("aBc <- \\() NULL", "function name style", object_name_linter())
})

test_that("capture groups in style are fine", {
  expect_no_lint("a <- 1\nab <- 2", object_name_linter(regexes = c(capture = "^(a)")))
  expect_no_lint("ab <- 1\nabc <- 2", object_name_linter(regexes = c(capture = "^(a)(b)")))
})

test_that("rlang name injection is handled", {
  linter <- object_name_linter()

  expect_no_lint('tibble("{name}" := 2)', linter)
  expect_no_lint('x %>% mutate("{name}" := 2)', linter)
  expect_lint('DT[, "{name}" := 2]', "style should match snake_case", linter)
})
