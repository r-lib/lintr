#' Return linter
#'
#' This linter checks functions' [return()] expressions.
#'
#' @param return_style Character string naming the return style. `"implicit"`,
#'   the default, enforces the Tidyverse guide recommendation to leave terminal
#'   returns implicit. `"explicit"` style requires that `return()` always be
#'   explicitly supplied.
#' @param allow_implicit_else Logical, default `TRUE`. If `FALSE`, functions with a terminal
#'   `if` clause must always have an `else` clause, making the `NULL` alternative explicit
#'   if necessary. Similarly, functions with terminal [switch()] statements must have an
#'   explicit default case.
#' @param return_functions Character vector of functions that are accepted as terminal calls
#'   when `return_style = "explicit"`. These are in addition to exit functions
#'   from base that are always allowed: [stop()], [q()], [quit()], [invokeRestart()],
#'   `tryInvokeRestart()`, [UseMethod()], [NextMethod()], [standardGeneric()],
#'   [callNextMethod()], [.C()], [.Call()], [.External()], and [.Fortran()].
#' @param except,except_regex Character vector of functions that are not checked when
#'   `return_style = "explicit"`. These are in addition to namespace hook functions
#'   that are never checked: `.onLoad()`, `.onUnload()`, `.onAttach()`, `.onDetach()`,
#'   `.Last.lib()`, `.First()` and `.Last()`. `except` matches function names exactly,
#'   while `except_regex` does exclusion by pattern matching with [rex::re_matches()].
#'
#' @examples
#' # will produce lints
#' code <- "function(x) {\n  return(x + 1)\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = return_linter()
#' )
#'
#' code <- "function(x) {\n  x + 1\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = return_linter(return_style = "explicit")
#' )
#'
#' code <- "function(x) {\n  if (x > 0) 2\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = return_linter(allow_implicit_else = FALSE)
#' )
#'
#' # okay
#' code <- "function(x) {\n  x + 1\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = return_linter()
#' )
#'
#' code <- "function(x) {\n  return(x + 1)\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = return_linter(return_style = "explicit")
#' )
#'
#' code <- "function(x) {\n  if (x > 0) 2 else NULL\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = return_linter(allow_implicit_else = FALSE)
#' )
#'
#' @evalRd rd_tags("return_linter")
#' @seealso
#'  - [linters] for a complete list of linters available in lintr.
#'  - <https://style.tidyverse.org/functions.html?q=return#return>
#' @export
return_linter <- function(
  return_style = c("implicit", "explicit"),
  allow_implicit_else = TRUE,
  return_functions = NULL,
  except = NULL,
  except_regex = NULL
) {
  return_style <- match.arg(return_style)

  check_except <- !allow_implicit_else || return_style == "explicit"
  # We defer building the XPath strings in this case since we can't build the
  #   pattern-based "except" logic directly into the XPath (because of v1.0)
  defer_except <- check_except && !is.null(except_regex)

  if (check_except) {
    except_xpath_fmt <- "parent::expr[not(
      preceding-sibling::expr/SYMBOL[{ xp_text_in_table(except) }]
    )]"
    except <- union(special_funs, except)
    if (!defer_except) except_xpath <- glue(except_xpath_fmt, except = except)
  }

  if (return_style == "implicit") {
    # nolint next: object_usage. False positive.
    body_xpath <- "(//FUNCTION | //OP-LAMBDA)/following-sibling::expr[last()]"
    params <- list(
      implicit = TRUE,
      type = "style",
      lint_xpath = "SYMBOL_FUNCTION_CALL[text() = 'return']",
      lint_message = "Use implicit return behavior; explicit return() is not needed."
    )
  } else {
    base_return_functions <- c(
      # Normal calls
      "return", "stop", "q", "quit",
      "invokeRestart", "tryInvokeRestart",

      # Functions related to S3 methods
      "UseMethod", "NextMethod",

      # Functions related to S4 methods
      "standardGeneric", "callNextMethod",

      # Functions related to C interfaces
      ".C", ".Call", ".External", ".Fortran"
    )

    return_functions <- union(base_return_functions, return_functions)

    body_xpath_fmt <- "
    (//FUNCTION | //OP-LAMBDA)[{ except_xpath }]
      /following-sibling::expr[last()][OP-LEFT-BRACE and expr[last()]/@line1 != @line1]
      /expr[last()]
    "
    if (defer_except) {
      function_name_xpath <- "(//FUNCTION | //OP-LAMBDA)/parent::expr/preceding-sibling::expr/SYMBOL"
    } else {
      body_xpath <- glue(body_xpath_fmt, except_xpath = except_xpath)
    }

    params <- list(
      implicit = FALSE,
      type = "warning",
      lint_xpath = glue("self::*[not(
        (self::expr | following-sibling::SPECIAL[text() = '%>%']/following-sibling::expr/expr[1])
          /SYMBOL_FUNCTION_CALL[{ xp_text_in_table(return_functions) }]
      )]"),
      lint_message = "All functions must have an explicit return()."
    )
  }

  params$allow_implicit_else <- allow_implicit_else

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    if (defer_except) {
      assigned_functions <- xml_text(xml_find_all(xml, function_name_xpath))
      except <-
        union(except, assigned_functions[re_matches_logical(assigned_functions, except_regex)])
      except_xpath <- glue(except_xpath_fmt, except = except)
      body_xpath <- glue(body_xpath_fmt, except_xpath = except_xpath)
    }

    body_expr <- xml_find_all(xml, body_xpath)

    params$source_expression <- source_expression

    if (params$implicit && !params$allow_implicit_else) {
      # can't incorporate this into the body_xpath for implicit return style,
      #   since we still lint explicit returns for except= functions.
      allow_implicit_else <- is.na(xml_find_first(body_expr, except_xpath))
    } else {
      allow_implicit_else <- rep(params$allow_implicit_else, length(body_expr))
    }
    # nested_return_lints not "vectorized" due to xml_children()
    Map(
      function(expr, allow_implicit_else) {
        params$allow_implicit_else <- allow_implicit_else
        nested_return_lints(expr, params)
      },
      body_expr, allow_implicit_else
    )
  })
}

nested_return_lints <- function(expr, params) {
  child_expr <- xml_children(expr)
  if (length(child_expr) == 0L) {
    return(list())
  }
  names(child_expr) <- xml_name(child_expr)

  if (names(child_expr)[1L] == "OP-LEFT-BRACE") {
    brace_return_lints(child_expr, expr, params)
  } else if (names(child_expr)[1L] == "IF") {
    if_return_lints(child_expr, expr, params)
  } else if (!is.na(xml_find_first(expr, "expr/SYMBOL_FUNCTION_CALL[text() = 'switch']"))) {
    switch_return_lints(child_expr, expr, params)
  } else {
    xml_nodes_to_lints(
      xml_find_first(child_expr[[1L]], params$lint_xpath),
      source_expression = params$source_expression,
      lint_message = params$lint_message,
      type = params$type
    )
  }
}

brace_return_lints <- function(child_expr, expr, params) {
  expr_idx <- which(names(child_expr) %in% c("expr", "equal_assign", "expr_or_assign_or_help"))
  if (length(expr_idx) == 0L) { # empty brace expression {}
    if (params$implicit) {
      return(list())
    } else {
      return(list(xml_nodes_to_lints(
        expr,
        source_expression = params$source_expression,
        lint_message = params$lint_message,
        type = params$type
      )))
    }
  }
  nested_return_lints(child_expr[[tail(expr_idx, 1L)]], params)
}

if_return_lints <- function(child_expr, expr, params) {
  expr_idx <- which(names(child_expr) %in% c("expr", "equal_assign", "expr_or_assign_or_help"))
  return_lints <- lapply(child_expr[expr_idx[-1L]], nested_return_lints, params)
  if (params$allow_implicit_else || length(expr_idx) == 3L) {
    return(return_lints)
  }
  implicit_else_lints <- list(xml_nodes_to_lints(
    expr,
    source_expression = params$source_expression,
    lint_message = "All functions with terminal if statements must have a corresponding terminal else clause.",
    type = "warning"
  ))
  c(return_lints, implicit_else_lints)
}

switch_return_lints <- function(child_expr, expr, params) {
  # equal_assign/expr_or_assign_or_help not possible here
  expr_idx <- which(names(child_expr) == "expr")
  # switch(x, ...) | expr[1]: switch; expr[2]: x. Drop the first two, check usage in ...
  return_lints <- lapply(child_expr[tail(expr_idx, -2L)], nested_return_lints, params)
  # in addition to the two <expr> dropped above, a third unmatched <expr> would be the default case.
  if (params$allow_implicit_else || length(expr_idx) - sum(names(child_expr) == "EQ_SUB") == 3L) {
    return(return_lints)
  }
  implicit_else_lints <- list(xml_nodes_to_lints(
    expr,
    source_expression = params$source_expression,
    lint_message = "All functions with terminal switch statements must have a terminal default clause.",
    type = "warning"
  ))
  c(return_lints, implicit_else_lints)
}
