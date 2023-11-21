#' Return linter
#'
#' This linter checks for explicit [return()] at the end of a function
#'
#' @param use_implicit_returns Whether to use implicit or explicit returns
#'
#' @param additional_allowed_func Names of additional functions that are
#'  accepted as return if `!use_implicit_returns`
#'
#' @param additional_side_effect_func Names of additional functions that are
#'  not checked for an explicit retun if `!use_implicit_returns`
#'
#' @param use_runit Whether to ignore Runit like functions or not
#'  if `!use_implicit_returns`
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
#'   linters = return_linter(use_implicit_returns = FALSE)
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
#'   linters = return_linter(use_implicit_returns = FALSE)
#' )
#'
#'
#' @evalRd rd_tags("return_linter")
#' @seealso
#'  - [linters] for a complete list of linters available in lintr.
#'  - <https://style.tidyverse.org/functions.html?q=return#return>
#' @export
return_linter <- function(
  use_implicit_returns = TRUE, additional_allowed_func = NULL,
  additional_side_effect_func = NULL, use_runit = FALSE
) {
  if (use_implicit_returns) {
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
    side_effect_functions <- c(
      # namespace hooks
      ".onLoad", ".onUnload", ".onAttach", ".onDetach", ".Last.lib"
    )

    side_effect_functions <- union(side_effect_functions, additional_side_effect_func)

    allowed_functions <- c(
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

    allowed_functions <- union(allowed_functions, additional_allowed_func)

    if (use_runit) {

      side_effect_functions <- union(side_effect_functions, c(".setUp", ".tearDown"))

      # tests in the RUnit framework are functions ending with a call to one
      #   of the below. would rather users just use a different framework
      #   (e.g. testthat or tinytest), but already 250+ BUILD files depend
      #   on RUnit, so just cater to that. confirmed the efficiency impact
      #   of including these is minimal.
      # RUnit tests look like 'TestInCamelCase <- function()'
      #   NB: check for starts-with(text(), 'Test') below is not sufficient, e.g.
      #   in cases of a "driver" test function taking arguments and the main unit
      #   test iterating over those.
      allowed_functions <- union(
        allowed_functions,
        c(
          "checkEquals", "checkEqualsNumeric", "checkException", "checkIdentical",
          "checkStop", "checkTrue", "checkWarnings"
        )
      )

      ignore_start <- "
      or (
        preceding-sibling::expr/SYMBOL[starts-with(text(), 'Test')]
        and not(SYMBOL_FORMALS)
      )
      "
    } else {
      ignore_start <- ""
    }

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
      preceding-sibling::expr[SYMBOL[{ xp_text_in_table(side_effect_functions) }]] {ignore_start}
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
                { xp_text_in_table(allowed_functions) }
              ])
            )
          )
        ) or (
          preceding-sibling::IF
          and self::expr
          and position() > 4
          and not(.//SYMBOL_FUNCTION_CALL[{ xp_text_in_table(allowed_functions) }])
        )
      ]
    ")
    msg <- "All functions must have an explicit return()."
  }

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    xml_nodes <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      xml_nodes,
      source_expression = source_expression,
      lint_message = msg,
      type = "style"
    )
  })
}
