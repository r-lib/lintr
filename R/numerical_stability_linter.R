#' Prefer numerically stable functions
#'
#' Detect expressions that lose accuracy through cancellation, underflow, or
#' overflow and recommend specialized functions. The linted code is never
#' evaluated. Suggestions are warnings, not automatic edits.
#'
#' @param assume_real_numeric Logical, default `TRUE`. Assume that unknown
#'   inputs to arithmetic, `log()`, and `exp()` are ordinary real numeric
#'   vectors. Set to `FALSE` to disable the `log1p()`, `expm1()`, `plogis()`,
#'   and `qlogis()` rules. Explicit complex literals are always excluded from
#'   these rules. This is relevant to R versions before 4.6.0, where `log1p()`
#'   and `expm1()` do not support complex inputs, and to class-specific methods.
#' @param include_heuristics Logical, default `FALSE`. Also report log-sum-exp
#'   expressions and direct logarithms of `gamma()`, `factorial()`, and
#'   `choose()`. These hints require reviewing the domain of the inputs;
#'   their logarithmic counterparts compute logarithms of absolute values.
#'
#' @section Recognized replacements:
#' * `log(1 + x)`, `log(x + 1)`, and `log(1 - x)` suggest `log1p()`.
#' * `exp(x) - 1` and `1 - exp(x)` suggest `expm1()`.
#' * `1 / (1 + exp(-x))` and `exp(x) / (1 + exp(x))` suggest `stats::plogis()`.
#' * `log(p / (1 - p))` suggests `stats::qlogis()`.
#' * `log(beta(a, b))` suggests `lbeta()`. Logarithms of absolute values of
#'   `gamma()`, `factorial()`, and `choose()` suggest their logarithmic APIs.
#' * `log(dnorm(x))` and analogous standard stats density calls suggest
#'   `log = TRUE`; distribution functions suggest `log.p = TRUE`.
#' * `1 - pnorm(x)` suggests the opposite `lower.tail` setting.
#'   `log(1 - pnorm(x))` combines this with `log.p = TRUE`.
#' * `qnorm(1 - p)` and analogous quantile calls suggest the opposite tail.
#'
#' @section Scope:
#' Unqualified calls are assumed to refer to the standard base/stats functions.
#' Explicit calls from other namespaces are ignored. Local function masking and
#' S3/S4 dispatch cannot be resolved statically. Named, partially named, and
#' positional arguments are matched against the standard function signatures.
#' Calls with missing arguments, `...`, or unknown log/tail flags are skipped.
#' Non-natural logarithms are skipped. Rewrites removing repeated evaluations
#' are restricted to repeated symbols or numeric constants.
#'
#' @examples
#' # will produce lints
#' lint(text = "log(1 + x)", linters = numerical_stability_linter())
#' lint(text = "exp(x) - 1", linters = numerical_stability_linter())
#' lint(text = "log(1 - pnorm(x))", linters = numerical_stability_linter())
#'
#' # okay
#' lint(text = "log1p(x)", linters = numerical_stability_linter())
#' lint(text = "pnorm(x, lower.tail = FALSE, log.p = TRUE)", linters = numerical_stability_linter())
#'
#' @evalRd rd_tags("numerical_stability_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
numerical_stability_linter <- function(assume_real_numeric = TRUE, include_heuristics = FALSE) {
  stopifnot(
    is.logical(assume_real_numeric), length(assume_real_numeric) == 1L, !is.na(assume_real_numeric),
    is.logical(include_heuristics), length(include_heuristics) == 1L, !is.na(include_heuristics)
  )

  distributions <- c(
    "beta", "binom", "cauchy", "chisq", "exp", "f", "gamma", "geom", "hyper",
    "lnorm", "logis", "nbinom", "norm", "pois", "signrank", "t", "unif", "weibull", "wilcox"
  )
  density_names <- paste0("d", distributions)
  probability_names <- c(paste0("p", distributions), "ptukey")
  quantile_names <- c(paste0("q", distributions), "qtukey")
  call_names <- c("log", quantile_names)
  xpath <- glue("
    //expr[
      OP-MINUS or OP-SLASH
      or expr/SYMBOL_FUNCTION_CALL[{xp_text_in_table(call_names)}]
    ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    candidates <- xml_find_all(source_expression$xml_parsed_content, xpath)
    covered_paths <- character()
    lints <- list()
    for (node in candidates) {
      # Report a combined replacement once, rather than also linting its parts.
      if (xml2::xml_path(node) %in% covered_paths) next
      expr <- xml2lang(node)
      lint_message <- stability_replacement(
        expr, assume_real_numeric, include_heuristics,
        density_names, probability_names, quantile_names
      )
      if (is.null(lint_message)) next
      covered_paths <- c(covered_paths, stability_component_paths(node))
      lints[[length(lints) + 1L]] <- xml_nodes_to_lints(
        node, source_expression, lint_message = lint_message, type = "warning"
      )
    }
    lints
  })
}

stability_component_paths <- function(node) {
  vapply(xml_find_all(node, "expr"), function(component) {
    repeat {
      inner <- xml_find_first(component, "self::expr[*[1][self::OP-LEFT-PAREN]]/expr")
      if (is.na(inner)) break
      component <- inner
    }
    xml2::xml_path(component)
  }, character(1L))
}

stability_unwrap <- function(expr) {
  while (is.call(expr) && identical(expr[[1L]], as.name("(")) && length(expr) == 2L) {
    expr <- expr[[2L]]
  }
  expr
}

stability_call_name <- function(expr, package = "base") {
  expr <- stability_unwrap(expr)
  if (!is.call(expr)) return("")
  call_head <- expr[[1L]]
  if (is.symbol(call_head)) return(as.character(call_head))
  stability_namespace_name(call_head, package)
}

stability_namespace_name <- function(call_head, package) {
  if (!is.call(call_head) || length(call_head) != 3L) return("")
  if (!is.symbol(call_head[[1L]]) || !is.symbol(call_head[[3L]])) return("")
  if (!as.character(call_head[[1L]]) %in% c("::", ":::")) return("")
  if (!identical(call_head[[2L]], as.name(package))) return("")
  as.character(call_head[[3L]])
}

stability_match_call <- function(expr, name, package = "base") {
  expr <- stability_unwrap(expr)
  if (stability_call_name(expr, package) != name) return(NULL)
  actual <- as.list(expr)[-1L]
  invalid <- vapply(actual, identical, logical(1L), quote(expr = )) |
    vapply(actual, identical, logical(1L), quote(...))
  if (any(invalid)) return(NULL)
  definition <- get(name, envir = asNamespace(package), inherits = FALSE)
  matched <- tryCatch(match.call(definition, expr, expand.dots = FALSE, envir = baseenv()), error = function(e) NULL)
  if (is.null(matched)) return(NULL)
  arguments <- as.list(matched)[-1L]
  required <- names(which(vapply(formals(definition), identical, logical(1L), quote(expr = ))))
  if (package == "stats") {
    # Central F/t distributions omit ncp; negative binomial calls supply prob OR mu.
    required <- setdiff(required, "ncp")
    if (endsWith(name, "nbinom") && any(c("prob", "mu") %in% names(arguments))) {
      required <- setdiff(required, c("prob", "mu"))
    }
  }
  if (!all(required %in% names(arguments))) return(NULL)
  arguments
}

stability_operator <- function(expr, operator, n = 2L) {
  expr <- stability_unwrap(expr)
  is.call(expr) && identical(expr[[1L]], as.name(operator)) && length(expr) == n + 1L
}

stability_one <- function(expr) {
  expr <- stability_unwrap(expr)
  if (stability_operator(expr, "+", 1L)) expr <- stability_unwrap(expr[[2L]])
  is.numeric(expr) && !is.complex(expr) && length(expr) == 1L && !is.na(expr) && expr == 1L
}

stability_offset <- function(expr, subtract = FALSE) {
  expr <- stability_unwrap(expr)
  if (stability_operator(expr, "+")) {
    if (stability_one(expr[[2L]])) return(expr[[3L]])
    if (stability_one(expr[[3L]])) return(expr[[2L]])
  }
  if (subtract && stability_operator(expr, "-") && stability_one(expr[[2L]])) {
    return(call("-", call("(", expr[[3L]])))
  }
  NULL
}

stability_has_complex <- function(expr) {
  if (is.complex(expr)) return(TRUE)
  if (!is.call(expr)) return(FALSE)
  if (stability_call_name(expr) %in% c("complex", "as.complex")) return(TRUE)
  parts <- as.list(expr)
  parts <- parts[!vapply(parts, identical, logical(1L), quote(expr = ))]
  any(vapply(parts, stability_has_complex, logical(1L)))
}

stability_text <- function(expr) paste(deparse(expr, width.cutoff = 500L), collapse = " ")

stability_suggest <- function(expr) paste0("Use ", stability_text(expr), " for better numerical stability.")

stability_new_call <- function(name, arguments, package = NULL) {
  call_head <- if (is.null(package)) as.name(name) else call("::", as.name(package), as.name(name))
  as.call(c(list(call_head), arguments))
}

stability_flag <- function(arguments, name, default) {
  if (!name %in% names(arguments)) return(default)
  value <- stability_unwrap(arguments[[name]])
  if (identical(value, TRUE) || identical(value, FALSE)) return(value)
  NA
}

stability_probability <- function(expr, probability_names, logarithm = FALSE, complement = FALSE) {
  name <- stability_call_name(expr, "stats")
  if (!name %in% probability_names) return(NULL)
  arguments <- stability_match_call(expr, name, "stats")
  if (is.null(arguments) || !identical(stability_flag(arguments, "log.p", FALSE), FALSE)) return(NULL)
  if (complement) {
    lower <- stability_flag(arguments, "lower.tail", TRUE)
    if (is.na(lower)) return(NULL)
    arguments$lower.tail <- !lower
  }
  if (logarithm) arguments$log.p <- TRUE
  stability_new_call(name, arguments, "stats")
}

stability_complement <- function(expr) {
  expr <- stability_unwrap(expr)
  if (stability_operator(expr, "-") && stability_one(expr[[2L]])) return(expr[[3L]])
  NULL
}

stability_replacement <- function(expr, assume_real_numeric, include_heuristics,
                                  density_names, probability_names, quantile_names) {
  expr <- stability_unwrap(expr)
  log_args <- stability_match_call(expr, "log")
  # Even an explicitly supplied natural base is skipped: evaluating it may have side effects.
  if (!is.null(log_args) && !"base" %in% names(log_args)) {
    return(stability_log_replacement(
      stability_unwrap(log_args$x), assume_real_numeric, include_heuristics,
      density_names, probability_names
    ))
  }
  complement <- stability_complement(expr)
  if (!is.null(complement)) {
    replacement <- stability_probability(complement, probability_names, complement = TRUE)
    if (!is.null(replacement)) return(stability_suggest(replacement))
  }
  replacement <- stability_quantile(expr, quantile_names)
  if (!is.null(replacement)) return(stability_suggest(replacement))
  if (assume_real_numeric && !stability_has_complex(expr)) return(stability_real_replacement(expr))
  NULL
}

stability_quantile <- function(expr, quantile_names) {
  name <- stability_call_name(expr, "stats")
  if (!name %in% quantile_names) return(NULL)
  arguments <- stability_match_call(expr, name, "stats")
  if (is.null(arguments) || !identical(stability_flag(arguments, "log.p", FALSE), FALSE)) return(NULL)
  probability <- stability_complement(arguments$p)
  lower <- stability_flag(arguments, "lower.tail", TRUE)
  if (is.null(probability) || is.na(lower)) return(NULL)
  arguments$p <- probability
  arguments$lower.tail <- !lower
  stability_new_call(name, arguments, "stats")
}

stability_log_replacement <- function(expr, assume_real_numeric, include_heuristics,
                                      density_names, probability_names) {
  complement <- stability_complement(expr)
  replacement <- stability_probability(
    complement %||% expr, probability_names,
    logarithm = TRUE, complement = !is.null(complement)
  )
  if (!is.null(replacement)) return(stability_suggest(replacement))
  density_call <- stability_log_density(expr, density_names)
  if (!is.null(density_call)) return(stability_suggest(density_call))
  special <- stability_special_log(expr, include_heuristics)
  if (!is.null(special)) return(special)
  if (stability_assume_real(expr, assume_real_numeric)) {
    replacement <- stability_real_log(expr)
    if (!is.null(replacement)) return(stability_suggest(replacement))
  }
  if (include_heuristics && stability_log_sum_exp(expr)) {
    return(paste(
      "Consider a stable log-sum-exp implementation such as matrixStats::logSumExp();",
      "check empty inputs, missing values, and infinite values before replacing."
    ))
  }
  NULL
}

stability_assume_real <- function(expr, assume_real_numeric) {
  assume_real_numeric && !stability_has_complex(expr)
}

stability_log_density <- function(expr, density_names) {
  name <- stability_call_name(expr, "stats")
  if (!name %in% density_names) return(NULL)
  arguments <- stability_match_call(expr, name, "stats")
  if (is.null(arguments) || !identical(stability_flag(arguments, "log", FALSE), FALSE)) return(NULL)
  arguments$log <- TRUE
  stability_new_call(name, arguments, "stats")
}

stability_real_log <- function(expr) {
  shifted <- stability_offset(expr, subtract = TRUE)
  if (!is.null(shifted)) return(call("log1p", shifted))
  if (!stability_operator(expr, "/")) return(NULL)
  probability <- stability_unwrap(expr[[2L]])
  other <- stability_complement(expr[[3L]])
  if (stability_repeatable(probability) && identical(probability, stability_unwrap(other))) {
    return(stability_new_call("qlogis", list(probability), "stats"))
  }
  NULL
}

stability_special_log <- function(expr, include_heuristics) {
  absolute <- stability_match_call(expr, "abs")
  inner <- if (is.null(absolute)) expr else stability_unwrap(absolute$x)
  name <- stability_call_name(inner)
  replacements <- c(beta = "lbeta", gamma = "lgamma", factorial = "lfactorial", choose = "lchoose")
  if (!name %in% names(replacements)) return(NULL)
  arguments <- stability_match_call(inner, name)
  if (is.null(arguments)) return(NULL)
  conditional <- name != "beta" && is.null(absolute)
  if (conditional && !include_heuristics) return(NULL)
  replacement <- stability_new_call(replacements[[name]], arguments)
  if (conditional) {
    return(paste0(
      "Consider ", stability_text(replacement), " only where ", name,
      "() is non-negative; the replacement computes the logarithm of its absolute value."
    ))
  }
  stability_suggest(replacement)
}

stability_repeatable <- function(expr) {
  is.symbol(expr) || (is.numeric(expr) && !is.complex(expr) && length(expr) == 1L)
}

stability_real_replacement <- function(expr) {
  replacement <- stability_expm1(expr)
  if (!is.null(replacement)) return(stability_suggest(replacement))
  if (!stability_operator(expr, "/")) return(NULL)
  denominator <- stability_offset(expr[[3L]])
  arguments <- stability_match_call(denominator, "exp")
  if (is.null(arguments)) return(NULL)
  exponent <- stability_unwrap(arguments$x)
  if (stability_one(expr[[2L]])) {
    return(stability_suggest(stability_new_call("plogis", list(stability_negate(exponent)), "stats")))
  }
  numerator <- stability_match_call(expr[[2L]], "exp")
  if (is.null(numerator)) return(NULL)
  if (stability_same_value(numerator$x, exponent)) {
    return(stability_suggest(stability_new_call("plogis", list(exponent), "stats")))
  }
  NULL
}

stability_same_value <- function(left, right) {
  stability_repeatable(right) && identical(stability_unwrap(left), right)
}

stability_negate <- function(expr) {
  if (stability_operator(expr, "-", 1L)) return(expr[[2L]])
  call("-", call("(", expr))
}

stability_expm1 <- function(expr) {
  if (!stability_operator(expr, "-")) return(NULL)
  left <- stability_unwrap(expr[[2L]])
  right <- stability_unwrap(expr[[3L]])
  if (stability_one(right)) {
    arguments <- stability_match_call(left, "exp")
    if (!is.null(arguments)) return(call("expm1", arguments$x))
  }
  if (stability_one(left)) {
    arguments <- stability_match_call(right, "exp")
    if (!is.null(arguments)) return(call("-", call("expm1", arguments$x)))
  }
  NULL
}

stability_log_sum_exp <- function(expr) {
  if (stability_operator(expr, "+")) {
    return(!is.null(stability_match_call(expr[[2L]], "exp")) &&
             !is.null(stability_match_call(expr[[3L]], "exp")))
  }
  if (stability_call_name(expr) != "sum") return(FALSE)
  arguments <- as.list(expr)[-1L]
  if (length(arguments) != 1L || (!is.null(names(arguments)) && nzchar(names(arguments)[1L]))) return(FALSE)
  !is.null(stability_match_call(arguments[[1L]], "exp"))
}
