#' Require idiomatic usage of `testthat::skip_*()` functions
#'
#' Enforces best practices for skipping tests in `{testthat}`:
#' 1. Do not wrap [testthat::test_that()] in an `if ()` block; call
#'    [testthat::skip_if()] or [testthat::skip_if_not()] inside `test_that()`
#'    so skipped tests are tracked and reported by `{testthat}`.
#' 2. Do not exit `test_that()` early with `if (...) return()`; use
#'    `skip_if()` or `skip_if_not()` instead.
#' 3. Prefer `skip_if()` and `skip_if_not()` over `if (...) skip(...)`.
#' 4. Prefer `skip_if_not(x)` over `skip_if(!x)` and `skip_if(x)` over
#'    `skip_if_not(!x)`.
#' 5. Prefer dedicated `{testthat}` helpers over manual condition checks:
#'    - [testthat::skip_unless_r()] instead of comparing [getRversion()]
#'    - [testthat::skip_if_not_installed()] instead of checking
#'      [requireNamespace()] or [utils::packageVersion()]
#'    - [testthat::skip_on_os()] instead of checking `.Platform$OS.type` or
#'      `Sys.info()[["sysname"]]`
#'    - [testthat::skip_on_cran()], [testthat::skip_on_ci()],
#'      [testthat::skip_on_covr()], and [testthat::skip_on_bioc()] instead of
#'      checking `Sys.getenv("NOT_CRAN")`, `Sys.getenv("CI")`,
#'      `Sys.getenv("R_COVR")`, or `Sys.getenv("IS_BIOC_BUILD_MACHINE")`
#' 6. Prefer `skip_on_ci()` over superseded [testthat::skip_on_travis()] and
#'    [testthat::skip_on_appveyor()].
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'if (cond) test_that("my test", { expect_true(TRUE) })',
#'   linters = testthat_skip_linter()
#' )
#'
#' lint(
#'   text = 'test_that("my test", { if (cond) return(); expect_true(TRUE) })',
#'   linters = testthat_skip_linter()
#' )
#'
#' lint(
#'   text = 'if (cond) skip("reason")',
#'   linters = testthat_skip_linter()
#' )
#'
#' lint(
#'   text = 'skip_if(!is_ready)',
#'   linters = testthat_skip_linter()
#' )
#'
#' lint(
#'   text = 'skip_if(getRversion() < "4.1.0")',
#'   linters = testthat_skip_linter()
#' )
#'
#' lint(
#'   text = 'skip_if_not(requireNamespace("pkg", quietly = TRUE))',
#'   linters = testthat_skip_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'test_that("my test", { skip_if_not(cond); expect_true(TRUE) })',
#'   linters = testthat_skip_linter()
#' )
#'
#' lint(
#'   text = 'skip_if_not(is_ready)',
#'   linters = testthat_skip_linter()
#' )
#'
#' lint(
#'   text = 'skip_unless_r(">= 4.1.0")',
#'   linters = testthat_skip_linter()
#' )
#'
#' lint(
#'   text = 'skip_if_not_installed("pkg")',
#'   linters = testthat_skip_linter()
#' )
#'
#' @evalRd rd_tags("testthat_skip_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
# nolint next: cyclocomp_linter. Helpers in factory inflate complexity (gaborcsardi/cyclocomp#30).
testthat_skip_linter <- function() {
  not_kwarg_cond <- "not(preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB])"

  if_test_that_xpath <- "
  //IF[
    not(following-sibling::ELSE)
    and following-sibling::expr[2]//SYMBOL_FUNCTION_CALL[text() = 'test_that']
    and not(ancestor::expr[expr[1]/SYMBOL_FUNCTION_CALL[text() = 'test_that']])
  ]
    /parent::expr
  "

  is_return_call <- "expr[1]/SYMBOL_FUNCTION_CALL[text() = 'return']"
  if_return_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[text() = 'test_that']
    /parent::expr
    /parent::expr
    //IF[
      not(following-sibling::ELSE)
      and not(ancestor::expr[
        (FUNCTION or OP-LAMBDA)
        and ancestor::expr[expr[1]/SYMBOL_FUNCTION_CALL[text() = 'test_that']]
      ])
      and following-sibling::expr[2][
        {is_return_call}
        or (OP-LEFT-BRACE and count(expr) = 1 and expr[1][{is_return_call}])
      ]
    ]
    /parent::expr
  ")

  if_skip_xpath <- "
  parent::expr
    /parent::expr[
      (
        IF
        and not(ELSE)
        and expr[2]/expr[1]/SYMBOL_FUNCTION_CALL[text() = 'skip']
      ) or (
        OP-LEFT-BRACE
        and count(expr) = 1
        and parent::expr[IF and not(ELSE) and expr[2][OP-LEFT-BRACE]]
      )
    ]
    /ancestor-or-self::expr[IF][1]
  "

  skip_if_cond_xpath <- glue("
  parent::expr/SYMBOL_SUB[text() = 'condition']/following-sibling::expr[1]
  | parent::expr[not(SYMBOL_SUB[text() = 'condition'])]/expr[{not_kwarg_cond}][2]
  ")

  is_neg_or_paren_xpath <- "
  boolean(self::expr[
    *[not(self::COMMENT)][1][self::OP-LEFT-PAREN or self::OP-EXCLAMATION]
    and count(expr) = 1
  ])
  "

  unwrap_negation_and_parens <- function(nodes) {
    needs_unwrap <- xml_find_lgl_(nodes, is_neg_or_paren_xpath)
    while (any(needs_unwrap)) {
      nodes[needs_unwrap] <- xml_find_first_(nodes[needs_unwrap], "expr")
      needs_unwrap <- xml_find_lgl_(nodes, is_neg_or_paren_xpath)
    }
    nodes
  }

  r_version_xpath <- "
  boolean(
    (LT or LE or GT or GE or EQ or NE)
    and expr[expr[1]/SYMBOL_FUNCTION_CALL[text() = 'getRversion']]
  )
  "

  require_ns_xpath <- "
  boolean(
    expr[1]/SYMBOL_FUNCTION_CALL[text() = 'requireNamespace']
    or (
      (AND2 or OR2)
      and .//SYMBOL_FUNCTION_CALL[text() = 'requireNamespace']
      and .//SYMBOL_FUNCTION_CALL[text() = 'packageVersion']
    )
  )
  "

  pkg_version_xpath <- "
  boolean(
    (LT or LE or GT or GE or EQ or NE)
    and expr[expr[1]/SYMBOL_FUNCTION_CALL[text() = 'packageVersion']]
  )
  "

  os_xpath <- "
  boolean(
    (EQ or NE or SPECIAL[text() = '%in%'])
    and expr[
      (OP-DOLLAR and expr[1][SYMBOL[text() = '.Platform']] and SYMBOL[text() = 'OS.type'])
      or (
        (self::expr | self::expr[expr[1]/SYMBOL_FUNCTION_CALL[text() = 'tolower']]/expr[2])[
          (LBB or OP-LEFT-BRACKET)
          and expr[1][expr[1]/SYMBOL_FUNCTION_CALL[text() = 'Sys.info']]
          and expr[2][STR_CONST[contains(text(), 'sysname')]]
        ]
      )
    ]
  )
  "

  env_helper_map <- c(
    NOT_CRAN = "skip_on_cran()",
    CI = "skip_on_ci()",
    R_COVR = "skip_on_covr()",
    IS_BIOC_BUILD_MACHINE = "skip_on_bioc()"
  )

  classify_conditions <- function(cond_nodes, caller_label, is_if_skip = FALSE) {
    inner_nodes <- unwrap_negation_and_parens(cond_nodes)
    is_outer_negated <- xml_find_lgl_(cond_nodes, "boolean(*[not(self::COMMENT)][1][self::OP-EXCLAMATION])")

    msg <- rep(NA_character_, length(cond_nodes))

    if (is_if_skip) {
      msg <- ifelse(
        is_outer_negated,
        "Use skip_if_not() instead of if (!...) skip().",
        "Use skip_if() instead of if (...) skip()."
      )
    } else {
      msg[is_outer_negated] <- ifelse(
        caller_label[is_outer_negated] == "skip_if()",
        "Use skip_if_not(x) instead of skip_if(!x).",
        "Use skip_if(x) instead of skip_if_not(!x)."
      )
    }

    is_r_ver <- xml_find_lgl_(inner_nodes, r_version_xpath)
    msg[is_r_ver] <- sprintf(
      "Use skip_unless_r() instead of comparing getRversion() in %s.",
      caller_label[is_r_ver]
    )

    is_req_ns <- xml_find_lgl_(inner_nodes, require_ns_xpath)
    msg[is_req_ns] <- sprintf(
      "Use skip_if_not_installed() instead of checking requireNamespace() in %s.",
      caller_label[is_req_ns]
    )

    is_pkg_ver <- !is_req_ns & xml_find_lgl_(inner_nodes, pkg_version_xpath)
    msg[is_pkg_ver] <- sprintf(
      "Use skip_if_not_installed() instead of checking packageVersion() in %s.",
      caller_label[is_pkg_ver]
    )

    is_os <- xml_find_lgl_(inner_nodes, os_xpath)
    msg[is_os] <- sprintf(
      "Use skip_on_os() instead of checking the operating system in %s.",
      caller_label[is_os]
    )

    has_sys_getenv <- xml_find_lgl_(
      inner_nodes,
      "boolean(not(AND2 or OR2 or AND or OR) and .//SYMBOL_FUNCTION_CALL[text() = 'Sys.getenv'])"
    )
    if (any(has_sys_getenv)) {
      env_nodes <- inner_nodes[has_sys_getenv]
      env_var <- get_r_string(
        env_nodes,
        "descendant-or-self::expr[expr[1]/SYMBOL_FUNCTION_CALL[text() = 'Sys.getenv']][1]/expr[2]/STR_CONST"
      )
      matched_env <- env_var %in% names(env_helper_map)
      if (any(matched_env)) {
        idx <- which(has_sys_getenv)[matched_env]
        msg[idx] <- sprintf(
          "Use %s instead of checking Sys.getenv('%s') in %s.",
          env_helper_map[env_var[matched_env]],
          env_var[matched_env],
          caller_label[idx]
        )
      }
    }

    msg
  }

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    test_that_calls <- source_expression$xml_find_function_calls("test_that")
    skip_calls <- source_expression$xml_find_function_calls("skip")
    skip_if_calls <- source_expression$xml_find_function_calls(c("skip_if", "skip_if_not"))
    superseded_calls <- source_expression$xml_find_function_calls(c("skip_on_travis", "skip_on_appveyor"))

    expr_list <- list()
    msg_list <- character()

    if (length(test_that_calls) > 0L) {
      if_test_that_expr <- xml_find_all_(xml, if_test_that_xpath)
      if (length(if_test_that_expr) > 0L) {
        expr_list <- c(expr_list, list(if_test_that_expr))
        msg_list <- c(
          msg_list,
          rep(
            "Use skip_if() or skip_if_not() inside test_that() instead of wrapping test_that() in an if () block.",
            length(if_test_that_expr)
          )
        )
      }

      if_return_expr <- xml_find_all_(xml, if_return_xpath)
      if (length(if_return_expr) > 0L) {
        expr_list <- c(expr_list, list(if_return_expr))
        msg_list <- c(
          msg_list,
          rep(
            "Use skip_if() or skip_if_not() instead of returning early with if (...) return() inside test_that().",
            length(if_return_expr)
          )
        )
      }
    }

    if (length(skip_calls) > 0L) {
      if_skip_expr <- xml_find_all_(skip_calls, if_skip_xpath)
      if (length(if_skip_expr) > 0L) {
        if_skip_cond <- xml_find_first_(if_skip_expr, "expr[1]")
        if_skip_msg <- classify_conditions(
          if_skip_cond,
          rep("if (...) skip()", length(if_skip_expr)),
          is_if_skip = TRUE
        )
        expr_list <- c(expr_list, list(if_skip_expr))
        msg_list <- c(msg_list, if_skip_msg)
      }
    }

    if (length(skip_if_calls) > 0L) {
      cond_nodes <- xml_find_all_(skip_if_calls, skip_if_cond_xpath)
      if (length(cond_nodes) > 0L) {
        call_nodes <- xml_find_first_(cond_nodes, "parent::expr")
        caller_label <- paste0(xp_call_name(call_nodes), "()")
        cond_msg <- classify_conditions(cond_nodes, caller_label, is_if_skip = FALSE)
        keep <- !is.na(cond_msg)
        if (any(keep)) {
          expr_list <- c(expr_list, list(call_nodes[keep]))
          msg_list <- c(msg_list, cond_msg[keep])
        }
      }
    }

    if (length(superseded_calls) > 0L) {
      superseded_expr <- xml_find_first_(superseded_calls, "parent::expr")
      superseded_fn <- xp_call_name(superseded_expr)
      expr_list <- c(expr_list, list(superseded_expr))
      msg_list <- c(msg_list, sprintf("Use skip_on_ci() instead of superseded %s().", superseded_fn))
    }

    if (length(expr_list) == 0L) {
      return(list())
    }

    bad_expr <- do.call(combine_nodesets, expr_list)
    xml_nodes_to_lints(bad_expr, source_expression, lint_message = msg_list, type = "warning")
  })
}
