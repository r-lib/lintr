context("object_name_linter")


test_that("styles are correctly identified", {
  styles <- names(style_regexes)
  #                                                                UpC   lowC     snake    dot  alllow  ALLUP
  expect_equivalent(lapply(styles, check_style, nms = "x"), list(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = ".x"), list(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  #expect_equivalent(lapply(styles, check_style, nms = "..x"), list(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "X"), list( TRUE, FALSE, FALSE, FALSE,  FALSE,  TRUE))
  expect_equivalent(lapply(styles, check_style, nms = "x."), list(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "X."), list(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "x_"), list(FALSE, FALSE, TRUE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "X_"), list(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "xy"), list(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "xY"), list(FALSE,  TRUE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "Xy"), list( TRUE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "XY"), list( TRUE, FALSE, FALSE, FALSE,  FALSE,  TRUE))
  expect_equivalent(lapply(styles, check_style, nms = "x1"), list(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "X1"), list( TRUE, FALSE, FALSE, FALSE,  FALSE,  TRUE))
  expect_equivalent(lapply(styles, check_style, nms = "x_y"), list(FALSE, FALSE,  TRUE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "X.Y"), list(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "x_2"), list(FALSE, FALSE,  TRUE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "X_2"), list(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "x.2"), list(FALSE, FALSE, FALSE,  TRUE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "X.2"), list(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))


  #                                                      UpC   lowC  snake    dot  alllow  ALLUP
  expect_equivalent(lapply(styles, check_style, nms = "IHave1Cat"    ), c( TRUE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "iHave1Cat"    ), c(FALSE,  TRUE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "i_have_1_cat" ), c(FALSE, FALSE,  TRUE, FALSE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "i.have.1.cat" ), c(FALSE, FALSE, FALSE,  TRUE,  FALSE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "ihave1cat"    ), c(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  expect_equivalent(lapply(styles, check_style, nms = "IHAVE1CAT"    ), c( TRUE, FALSE, FALSE, FALSE,  FALSE,  TRUE))
  expect_equivalent(lapply(styles, check_style, nms = "I.HAVE_ONECAT"), c(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
})

test_that("linter ignores some objects", {
  # names for which style check is ignored
  expect_lint("`%x%` <- t", NULL, object_name_linter("snake_case"))              # operator
  expect_lint("`t.test` <- t", NULL, object_name_linter("UPPERCASE"))         # std pkg
  expect_lint(".Deprecated('x')", NULL, object_name_linter("lowercase"))      # std pkg
  expect_lint("print.foo <- t", NULL, object_name_linter("CamelCase"))         # S3 generic
  expect_lint("names.foo <- t", NULL, object_name_linter("CamelCase"))      # int generic
  expect_lint("sapply(x,f,USE.NAMES=T)", NULL, object_name_linter("snake_case")) # defined elsewhere
})

test_that("linter returns correct linting", {
  msg <- "Variable and function names should be in camelCase."
  linter <- object_name_linter("camelCase")

  expect_lint("myObject <- 123", NULL, linter)
  expect_lint("`myObject` <- 123", NULL, linter)
  expect_lint("my.confused_NAME <- 1;", c(message=msg, line_number=1L, column_number=1L), linter)
  expect_lint("1 ->> read.data.frame;", c(message=msg, line_number=1L, column_number=7L), linter)
  expect_lint("object_name_linter <- function(...) {}",
              c(message=msg, line_number=1L, column_number=1L), linter)

  expect_lint(
    "Z = sapply('function', function(x=function(x){1}, b.a.z=F){identity(b.a.z)}, USE.NAMES=TRUE)",
      c(message=msg, line_number=1L, column_number=1L),
    linter
  )

  expect_lint("blah", NULL, linter)
  expect_lint("invokeRestartInteractively", NULL, linter)
  expect_lint("camelCase", NULL, linter)
  expect_lint("camelCase()", NULL, linter)
  expect_lint("pack::camelCase", NULL, linter)
  expect_lint("pack:::camelCase", NULL, linter)
  expect_lint("a(camelCase = 1)", NULL, linter)
})
