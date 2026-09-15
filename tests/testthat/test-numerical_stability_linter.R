test_that("stable arithmetic replacements handle equivalent syntax", {
  linter <- numerical_stability_linter()
  cases <- list(
    c("log(1 + x)", "log1p(x)"),
    c("log(x + 1L)", "log1p(x)"),
    c("base::log(x = (1e0 + (x)))", "log1p((x))"),
    c("base:::log(0x1 + x)", "log1p(x)"),
    c("log(+1 + x)", "log1p(x)"),
    c("log(1 - (x + y))", "log1p(-((x + y)))"),
    c("exp(x) - 1", "expm1(x)"),
    c("(base::exp(x = x + y)) - (1.0)", "expm1(x + y)"),
    c("1L - exp(x)", "-expm1(x)"),
    c("1 / (1 + exp(-x))", "stats::plogis(x)"),
    c("1 / (base::exp(-(x + y)) + 1)", "stats::plogis((x + y))"),
    c("1 / (1 + exp(x + y))", "stats::plogis(-(x + y))"),
    c("exp(x) / (1 + exp(x))", "stats::plogis(x)"),
    c("base::exp(x = (x)) / (base::exp(x) + 1L)", "stats::plogis(x)"),
    c("log(p / (1 - p))", "stats::qlogis(p)"),
    c("log((p) / (1 - (p)))", "stats::qlogis(p)")
  )
  for (case in cases) expect_lint(case[1L], rex::rex(case[2L]), linter)
})

test_that("special logarithmic functions respect signs and argument matching", {
  linter <- numerical_stability_linter()
  cases <- list(
    c("log(beta(a, b))", "lbeta(a = a, b = b)"),
    c("base::log(base::beta(b = y, a = x))", "lbeta(a = x, b = y)"),
    c("log(beta(a, a = b))", "lbeta(a = b, b = a)"),
    c("log(abs(gamma(x)))", "lgamma(x = x)"),
    c("log(base::abs(base::factorial(x)))", "lfactorial(x = x)"),
    c("log(abs(choose(n, k)))", "lchoose(n = n, k = k)")
  )
  for (case in cases) expect_lint(case[1L], rex::rex(case[2L]), linter)
  for (code in c("log(gamma(x))", "log(factorial(x))", "log(choose(n, k))")) {
    expect_no_lint(code, linter)
    expect_lint(code, "only where.*non-negative.*absolute value", numerical_stability_linter(include_heuristics = TRUE))
  }
})

test_that("density and probability replacements preserve matched arguments", {
  linter <- numerical_stability_linter()
  cases <- list(
    c("log(dnorm(x))", "stats::dnorm(x = x, log = TRUE)"),
    c("log(stats::dnorm(x, 2, 3, FALSE))", "stats::dnorm(x = x, mean = 2, sd = 3, log = TRUE)"),
    c("log(dnorm(sd = 3, x = x, log = FALSE, mean = 2))", "stats::dnorm(x = x, mean = 2, sd = 3, log = TRUE)"),
    c("log(dgamma(x, shape = a, scale = s))", "stats::dgamma(x = x, shape = a, scale = s, log = TRUE)"),
    c("log(dnbinom(x, size = s, mu = m))", "stats::dnbinom(x = x, size = s, mu = m, log = TRUE)"),
    c("log(pnorm(x))", "stats::pnorm(q = x, log.p = TRUE)"),
    c("log(pnorm(x, 0, 1, FALSE, FALSE))", "stats::pnorm(q = x, mean = 0, sd = 1, lower.tail = FALSE, log.p = TRUE)"),
    c("1 - pnorm(x)", "stats::pnorm(q = x, lower.tail = FALSE)"),
    c("1 - stats::pnorm(q = x, lower = FALSE)", "stats::pnorm(q = x, lower.tail = TRUE)"),
    c("log(1 - pnorm(x))", "stats::pnorm(q = x, lower.tail = FALSE, log.p = TRUE)"),
    c("log((1 - pnorm(x, lower.tail = FALSE)))", "stats::pnorm(q = x, lower.tail = TRUE, log.p = TRUE)"),
    c("qnorm(1 - p)", "stats::qnorm(p = p, lower.tail = FALSE)"),
    c("stats::qnorm(sd = s, p = 1 - p, lower.tail = FALSE)", "stats::qnorm(p = p, sd = s, lower.tail = TRUE)"),
    c("qnorm(1 - p, 2, 3, FALSE, FALSE)", "stats::qnorm(p = p, mean = 2, sd = 3, lower.tail = TRUE, log.p = FALSE)"),
    c("qgamma(1 - p, shape = a, scale = s)", "stats::qgamma(p = p, shape = a, scale = s, lower.tail = FALSE)")
  )
  for (case in cases) expect_lint(case[1L], rex::rex(case[2L]), linter)
})

test_that("standard distribution signatures are covered without prefix guessing", {
  linter <- numerical_stability_linter()
  distributions <- c(
    beta = "x, 1, 2", binom = "x, 2, 0.5", cauchy = "x", chisq = "x, 2", exp = "x",
    f = "x, 2, 3", gamma = "x, 2", geom = "x, 0.5", hyper = "x, 2, 3, 1", lnorm = "x",
    logis = "x", nbinom = "x, 2, 0.5", norm = "x", pois = "x, 2", signrank = "x, 2",
    t = "x, 2", unif = "x", weibull = "x, 2", wilcox = "x, 2, 3"
  )
  for (distribution in names(distributions)) {
    arguments <- distributions[[distribution]]
    expect_lint(paste0("log(d", distribution, "(", arguments, "))"), "log = TRUE", linter)
    expect_lint(paste0("log(p", distribution, "(", arguments, "))"), "log.p = TRUE", linter)
    expect_lint(paste0("1 - p", distribution, "(", arguments, ")"), "lower.tail = FALSE", linter)
    expect_lint(paste0("q", distribution, "(1 - ", arguments, ")"), "lower.tail = FALSE", linter)
  }
  expect_lint("log(ptukey(x, 3, 10))", "log.p = TRUE", linter)
  expect_lint("qtukey(1 - p, 3, 10)", "lower.tail = FALSE", linter)
  expect_no_lint("log(dcustom(x))", linter)
  expect_no_lint("1 - pcustom(x)", linter)
  expect_no_lint("qcustom(1 - p)", linter)
})

test_that("already stable calls and unsafe near misses are ignored", {
  linter <- numerical_stability_linter()
  cases <- c(
    "log1p(x)", "expm1(x)", "plogis(x)", "qlogis(p)", "lbeta(a, b)", "lgamma(x)",
    "log(x)", "log(2 + x)", "log(x - 1)", "log(1 + x + y)", "log(1 / (1 - p))",
    "log(1 + x, 10)", "log(1 + x, base = exp(1))", "log(x = 1 + x, b = 10)",
    "log10(1 + x)", "log2(1 + x)", "exp(x) - 2", "2 - exp(x)", "exp(x) + 1",
    "exp(x) / (1 + exp(y))", "2 / (1 + exp(-x))", "1 / (2 + exp(-x))",
    "exp(f()) / (1 + exp(f()))", "log(f() / (1 - f()))", "log(p / (1 - q))",
    "log(dnorm(x, log = TRUE))", "log(dnorm(x, 0, 1, TRUE))", "log(dnorm(x, log = flag))",
    "log(pnorm(x, log.p = TRUE))", "log(pnorm(x, log.p = flag))",
    "1 - pnorm(x, log.p = TRUE)", "1 - pnorm(x, log.p = flag)",
    "1 - pnorm(x, lower.tail = flag)", "1 - pnorm(x, lower.tail = T)",
    "qnorm(1 - p, log.p = TRUE)", "qnorm(1 - p, log.p = flag)", "qnorm(1 - p, lower.tail = flag)",
    "other::log(1 + x)", "other::exp(x) - 1", "log(other::dnorm(x))", "1 - other::pnorm(x)",
    "other::qnorm(1 - p)", "log(other::beta(a, b))", "1 / (1 + other::exp(-x))",
    "log(1 + 1i)", "exp(1i) - 1", "1 / (1 + exp(1i))", "log(1 + complex(real = x))",
    "log()", "log(x = )", "log(1 + x, extra = 1)", "log(1 + x, x = y)",
    "log(...)", "log(1 + x, ...)", "exp() - 1", "exp(...)-1", "exp(x, y) - 1",
    "log(dnorm())", "log(dnorm(x, ))", "log(dnorm(x, ...))", "log(dnorm(x, unknown = y))",
    "log(dbeta(x))", "log(beta(a))", "log(beta(a = x, a = y))", "qnorm()", "qnorm(p = )",
    "log(abs(gamma()))", "log(sum())", "log(sum(exp(x), na.rm = TRUE))",
    "# log(1 + x)", "'exp(x) - 1'", "obj$log(1 + x)", "obj$exp(x) - 1"
  )
  for (code in cases) expect_no_lint(code, linter)
})

test_that("configuration separates real numeric assumptions and heuristics", {
  linter <- numerical_stability_linter(assume_real_numeric = FALSE)
  for (code in c("log(1 + x)", "exp(x) - 1", "1 / (1 + exp(-x))", "log(p / (1 - p))")) {
    expect_no_lint(code, linter)
  }
  expect_lint("log(pnorm(x))", "log.p = TRUE", linter)
  expect_lint("log(beta(a, b))", "lbeta", linter)
  for (code in c("log(sum(exp(x)))", "log(exp(a) + exp(b))")) {
    expect_no_lint(code, numerical_stability_linter())
    expect_lint(
      code, "Consider a stable log-sum-exp.*check empty inputs",
      numerical_stability_linter(include_heuristics = TRUE)
    )
  }
  for (invalid in list(NULL, NA, c(TRUE, FALSE), 1L, "yes")) {
    expect_error(numerical_stability_linter(assume_real_numeric = invalid))
    expect_error(numerical_stability_linter(include_heuristics = invalid))
  }
})

test_that("comments, parentheses, nesting, and positions are handled", {
  linter <- numerical_stability_linter()
  expect_lint("log(1 + # comment\n x)", rex::rex("log1p(x)"), linter)
  expect_lint("exp(# comment\n x) - 1", rex::rex("expm1(x)"), linter)
  expect_lint("log(1 - pnorm(# comment\n x))", "lower.tail = FALSE, log.p = TRUE", linter)
  expect_lint(
    c("{", "  a <- log(1 + x)", "  b <- exp(y) - 1", "  c <- log(1 - pnorm(z))", "}"),
    list(
      list(message = "log1p", line_number = 2L, column_number = 8L, ranges = list(c(8L, 17L))),
      list(message = "expm1", line_number = 3L, column_number = 8L),
      list(message = "log.p = TRUE", line_number = 4L, column_number = 8L)
    ),
    linter
  )
  expect_lint("f <- function(x) log(1 + x)", "log1p", linter)
  expect_lint("log(1 + log(1 + y))", list("log1p", "log1p"), linter)
  expect_lint("exp(log(1 + y)) - 1", list("expm1", "log1p"), linter)
  expect_lint("x |> log()", NULL, linter)
  expect_lint("x |> (function(y) log(1 + y))()", "log1p", linter)
})

test_that("linting never evaluates input expressions", {
  linter <- numerical_stability_linter()
  expect_lint("log(1 + stop('must not run'))", "log1p", linter)
  expect_lint("exp(stop('must not run')) - 1", "expm1", linter)
  expect_lint("log(1 + f(, y))", "log1p", linter)
  expect_lint("log(dnorm(stop('must not run')))", "log = TRUE", linter)
  expect_no_lint("log(dnorm(x, log = stop('must not run')))", linter)
})

test_that("suggested expressions remain useful at numerical extremes", {
  linter <- numerical_stability_linter()
  cases <- list(
    list(code = "log(1 + x)", values = list(x = 1e-20), expected = log1p(1e-20)),
    list(code = "exp(x) - 1", values = list(x = 1e-20), expected = expm1(1e-20)),
    list(code = "exp(x) / (1 + exp(x))", values = list(x = 1000.0), expected = 1.0),
    list(code = "1 - pnorm(x)", values = list(x = 10.0), expected = pnorm(10.0, lower.tail = FALSE)),
    list(code = "log(pnorm(x))", values = list(x = -40.0), expected = pnorm(-40.0, log.p = TRUE)),
    list(code = "log(dnorm(x))", values = list(x = 50.0), expected = dnorm(50.0, log = TRUE)),
    list(code = "log(beta(a, b))", values = list(a = 1000.0, b = 1000.0), expected = lbeta(1000.0, 1000.0)),
    list(code = "qnorm(1 - p)", values = list(p = 1e-20), expected = qnorm(1e-20, lower.tail = FALSE))
  )
  for (case in cases) {
    lint_message <- lint(text = case$code, linters = linter)[[1L]]$message
    replacement <- sub("^Use (.*) for better numerical stability\\.$", "\\1", lint_message)
    observed <- eval(str2lang(replacement), envir = list2env(case$values))
    expect_identical(observed, case$expected)
    expect_true(is.finite(observed))
  }
})
