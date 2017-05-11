context("object_name_linter")


test_that("styles are correctly identified", {
  matches_styles <- lintr:::matches_styles

  #                                            UpC   lowC  snake    dot  alllow  ALLUP
  expect_equivalent(matches_styles("x"  ), c(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  expect_equivalent(matches_styles(".x" ), c(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  expect_equivalent(matches_styles("..x"), c(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  expect_equivalent(matches_styles("X"  ), c( TRUE, FALSE, FALSE, FALSE,  FALSE,  TRUE))
  expect_equivalent(matches_styles("x." ), c(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("X." ), c(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("x_" ), c(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("X_" ), c(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("xy" ), c(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  expect_equivalent(matches_styles("xY" ), c(FALSE,  TRUE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("Xy" ), c( TRUE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("XY" ), c( TRUE, FALSE, FALSE, FALSE,  FALSE,  TRUE))
  expect_equivalent(matches_styles("x1" ), c(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  expect_equivalent(matches_styles("X1" ), c( TRUE, FALSE, FALSE, FALSE,  FALSE,  TRUE))
  expect_equivalent(matches_styles("x_y"), c(FALSE, FALSE,  TRUE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("X.Y"), c(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("x_2"), c(FALSE, FALSE,  TRUE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("X_2"), c(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("x.2"), c(FALSE, FALSE, FALSE,  TRUE,  FALSE, FALSE))
  expect_equivalent(matches_styles("X.2"), c(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))


  #                                                      UpC   lowC  snake    dot  alllow  ALLUP
  expect_equivalent(matches_styles("IHave1Cat"    ), c( TRUE, FALSE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("iHave1Cat"    ), c(FALSE,  TRUE, FALSE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("i_have_1_cat" ), c(FALSE, FALSE,  TRUE, FALSE,  FALSE, FALSE))
  expect_equivalent(matches_styles("i.have.1.cat" ), c(FALSE, FALSE, FALSE,  TRUE,  FALSE, FALSE))
  expect_equivalent(matches_styles("ihave1cat"    ), c(FALSE,  TRUE,  TRUE,  TRUE,   TRUE, FALSE))
  expect_equivalent(matches_styles("IHAVE1CAT"    ), c( TRUE, FALSE, FALSE, FALSE,  FALSE,  TRUE))
  expect_equivalent(matches_styles("I.HAVE_ONECAT"), c(FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE))

  expect_error(lintr:::matches_styles("text", "invalid_style"), "Invalid style")
})


test_that("linter returns correct linting", {
  msg <- "Variable or function name should be lowerCamelCase."
  linter <- object_name_linter(style="lowerCamelCase")
  expect_is(linter, "linter")

  expect_lint("myObject <- 123", NULL, linter)
  expect_lint("`myObject` <- 123", NULL, linter)
  expect_lint("my.confused_NAME <- 1;", c(message=msg, line_number=1L, column_number=1L), linter)
  expect_lint("1 ->> read.data.frame;", c(message=msg, line_number=1L, column_number=7L), linter)
  expect_lint("object_name_linter <- function(...) {}",
              c(message=msg, line_number=1L, column_number=1L), linter)

  expect_lint(
    "Z = sapply('function', function(x=function(x){1}, b.a.z=F){identity(b.a.z)}, USE.NAMES=TRUE)",
    list(
      c(message=msg, line_number=1L, column_number=1L),
      c(message=msg, line_number=1L, column_number=51L)
    ),
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


test_that("linter ignores some objects", {
  # names for which style check is ignored
  expect_lint("`%x%` <- t", NULL, object_name_linter(style="snake_case"))              # operator
  expect_lint("`t.test` <- t", NULL, object_name_linter(style="ALLUPPERCASE"))         # std pkg
  expect_lint(".Deprecated('x')", NULL, object_name_linter(style="alllowercase"))      # std pkg
  expect_lint("abc::read_me <- t", NULL, object_name_linter(style="dotted.case"))      # ext pkg
  expect_lint("as.foo <- t", NULL, object_name_linter(style="UpperCamelCase"))         # S3 generic
  expect_lint("names.foo <- t", NULL, object_name_linter(style="UpperCamelCase"))      # int generic
  expect_lint("sapply(x,f,USE.NAMES=T)", NULL, object_name_linter(style="snake_case")) # defined
  # elsewhere
})
