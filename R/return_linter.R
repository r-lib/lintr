#' Return linter
#'
#' This linter checks functions' [return()] expressions.
#'
#' @param return_style Character string naming the return style. `"implicit"`,
#'   the default, enforeces the Tidyverse guide recommendation to leave terminal
#'   returns implicit. `"explicit"` style requires that `return()` always be
#'   explicitly supplied.
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
#'
#' @evalRd rd_tags("return_linter")
#' @seealso
#'  - [linters] for a complete list of linters available in lintr.
#'  - <https://style.tidyverse.org/functions.html?q=return#return>
#' @export
return_linter <- function(
    return_style = c("implicit", "explicit"),
    return_functions = NULL,
    except = NULL) {
  return_style <- match.arg(return_style)

  if (return_style == "implicit") {
    xpath <- "
      (//FUNCTION | //OP-LAMBDA)
      /following-sibling::expr[1][*[1][self::OP-LEFT-BRACE]]
      /expr[last()][
        expr[1][
          not(OP-DOLLAR or OP-AT)
          and SYMBOL_FUNCTION_CALL[text() = 'return']
        ]
      ]
    "
    msg <- "Use implicit return behavior; explicit return() is not needed."
  } else {
    # See `?.onAttach`; these functions are all exclusively used for their
    #   side-effects, so implicit return is generally acceptable

    except <- union(special_funs, except)

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
    xpath <- glue("
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
    msg <- "All functions must have an explicit return()."
  }

  Linter(function(source_expression) {
    xml <- source_expression$xml_parsed_content

    xml_nodes <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      xml_nodes,
      source_expression = source_expression,
      lint_message = msg,
      type = "style"
    )
  }, linter_level = "expression")
}
