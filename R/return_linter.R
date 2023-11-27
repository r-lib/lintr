#' Return linter
#'
#' This linter checks functions' [return()] expressions.
#'
#' @param return_style Character string naming the return style. `"implicit"`,
#'   the default, enforeces the Tidyverse guide recommendation to leave terminal
#'   returns implicit. `"explicit"` style requires that `return()` always be
#'   explicitly supplied.
#' @param allow_implicit_else Logical, default `TRUE`. If `FALSE`, functions with a terminal
#'   `if` clause must always have an `else` clause, making the `NULL` alternative explicit
#'   if necessary.
#' @param return_functions Character vector of functions that are accepted as terminal calls
#'   when `return_style = "explicit"`. These are in addition to exit functions
#'   from base that are always allowed: [stop()], [q()], [quit()], [invokeRestart()],
#'   `tryInvokeRestart()`, [UseMethod()], [NextMethod()], [standardGeneric()],
#'   [callNextMethod()], [.C()], [.Call()], [.External()], and [.Fortran()].
#' @param except Character vector of functions that are not checked when
#'   `return_style = "explicit"`. These are in addition to namespace hook functions
#'   that are never checked: `.onLoad()`, `.onUnload()`, `.onAttach()`, `.onDetach()`,
#'   `.Last.lib()`, `.First()` and `.Last()`.
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
    except = NULL) {
  return_style <- match.arg(return_style)

  if (!allow_implicit_else || return_style == "explicit") {
    # See `?.onAttach`; these functions are all exclusively used for their
    #   side-effects, so implicit return is generally acceptable
    except <- union(special_funs, except)
  }

  if (return_style == "implicit") {
    return_xpath <- "
      (//FUNCTION | //OP-LAMBDA)
      /following-sibling::expr[1][*[1][self::OP-LEFT-BRACE]]
      /expr[last()][
        expr[1][
          not(OP-DOLLAR or OP-AT)
          and SYMBOL_FUNCTION_CALL[text() = 'return']
        ]
      ]
    "
    return_msg <- "Use implicit return behavior; explicit return() is not needed."
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

    control_calls <- c("IF", "FOR", "WHILE", "REPEAT")

    # from top, look for a FUNCTION definition that uses { (one-line
    #   function definitions are excepted), then look for failure to find
    #   return() on the last() expr of the function definition.
    # exempt .onLoad which shows up in the tree like
    #   <expr><expr><SYMBOL>.onLoad</></><LEFT_ASSIGN></><expr><FUNCTION>...
    # simple final expression (no control flow) must be
    # <expr><expr> CALL( <expr> ) </expr></expr>
    # NB: if this syntax _isn't_ used, the node may not be <expr>, hence
    #   the use of /*[...] below and self::expr here. position() = 1 is
    #   needed to guard against a few other cases.
    #   We also need to make sure that this expression isn't followed by a pipe
    #   symbol, which would indicate that we need to also check the last
    #   expression.
    # pipe expressions are like
    #   ...
    #   <SPECIAL>%&gt;%</SPECIAL>
    #   <expr><expr><SYMBOL_FUNCTION_CALL>return</SYMBOL_FUNCTION_CALL>
    #   </expr></expr>
    # Unlike the following case, the return should be the last expression in
    # the sequence.
    # conditional expressions are like
    #   <expr><IF> ( <expr> ) <expr> [ <ELSE> <expr>] </expr>
    # we require _any_ call to return() in either of the latter two <expr>, i.e.,
    #   we don't apply recursive logic to check every branch, only that the
    #   two top level branches have at least two return()s
    # because of special 'in' syntax for 'for' loops, the condition is
    #   tagged differently than for 'if'/'while' conditions (simple PAREN)
    return_xpath <- glue("
    (//FUNCTION | //OP-LAMBDA)[parent::expr[not(
      preceding-sibling::expr[SYMBOL[{ xp_text_in_table(except) }]]
    )]]
      /following-sibling::expr[OP-LEFT-BRACE and expr[last()]/@line1 != @line1]
      /expr[last()]
      /*[
        (
          position() = 1
          and (
            (
              { xp_or(paste0('self::', setdiff(control_calls, 'IF'))) }
            ) or (
              not({ xp_or(paste0('self::', control_calls)) })
              and not(
                following-sibling::PIPE
                or following-sibling::SPECIAL[text() = '%>%']
              )
              and not(self::expr/SYMBOL_FUNCTION_CALL[
                { xp_text_in_table(return_functions) }
              ])
            )
          )
        ) or (
          preceding-sibling::IF
          and self::expr
          and position() > 4
          and not(.//SYMBOL_FUNCTION_CALL[{ xp_text_in_table(return_functions) }])
        )
      ]
    ")
    return_msg <- "All functions must have an explicit return()."
  }

  if (!allow_implicit_else) {
    # for inline functions, terminal <expr> is a sibling of <FUNCTION>, otherwise
    #   it's a descendant of the <expr> following <FUNCTION>
    implicit_else_cond <- "position() = last() and IF and not(ELSE)"
    implicit_else_xpath <- glue("
      //FUNCTION[not(parent::expr/preceding-sibling::expr/SYMBOL[{ xp_text_in_table(except) }])]
        /following-sibling::expr[({implicit_else_cond}) or expr[{implicit_else_cond}]]
    ")

    # to land on the child if present since XPath 1.0 doesn't support
    #   /(following-sibling::expr | following-sibling::expr/expr)[...]
    child_xpath <- glue("./expr[{implicit_else_cond}]")

    implicit_else_msg <-
      "All functions with terminal if statements must have a corresponding terminal else clause"
  }

  Linter(function(source_expression) {
    xml <- source_expression$xml_parsed_content
    if (is.null(xml)) return(list())

    return_expr <- xml_find_all(xml, return_xpath)

    lints <- xml_nodes_to_lints(
      return_expr,
      source_expression = source_expression,
      lint_message = return_msg,
      type = "style"
    )

    if (!allow_implicit_else) {
      implicit_else_expr <- xml_find_all(xml, implicit_else_xpath)

      child_expr <- xml_find_first(implicit_else_expr, child_xpath)
      has_child_expr <- !is.na(child_expr)
      implicit_else_expr[has_child_expr] <- child_expr[has_child_expr]

      lints <- c(lints, xml_nodes_to_lints(
        implicit_else_expr,
        source_expression = source_expression,
        lint_message = implicit_else_msg,
        type = "warning"
      ))
    }

    lints
  }, linter_level = "expression")
}
