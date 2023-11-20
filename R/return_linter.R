#' Return linter
#'
#' This linter checks for explicit [return()] at the end of a function
#'
#' @param use_implicit_returns Whether to use implicit or explicit returns
#'
#' @evalRd rd_tags("return_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#'
#' @export
return_linter <- function(use_implicit_returns = TRUE) {
  if (use_implicit_returns) {
    xpath <- "
      (//FUNCTION | //OP-LAMBDA)[following-sibling::expr[1]/*[1][self::OP-LEFT-BRACE]]
      /following-sibling::expr[1]/
      expr[last()][
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
    return_not_needed_funs <- c(
      # namespace hooks
      ".onLoad", ".onUnload", ".onAttach", ".onDetach", ".Last.lib",

      # from RUnit
      ".setUp", ".tearDown"
    )

    allowed_functions <- c(
      # Normal calls
      "return", "stop", "warning", "message", "stopifnot", "q", "quit",
      "invokeRestart", "tryInvokeRestart",

      # Normal calls from non-default libraries
      "LOG", "abort",

      # tests in the RUnit framework are functions ending with a call to one
      #   of the below. would rather users just use a different framework
      #   (e.g. testthat or tinytest), but already 250+ BUILD files depend
      #   on RUnit, so just cater to that. confirmed the efficiency impact
      #   of including these is minimal.
      # RUnit tests look like 'TestInCamelCase <- function()'
      #   NB: check for starts-with(text(), 'Test') below is not sufficient, e.g.
      #   in cases of a "driver" test function taking arguments and the main unit
      #   test iterating over those.
      "checkEquals", "checkEqualsNumeric", "checkException", "checkIdentical",
      "checkStop", "checkTrue", "checkWarnings",

      # Functions related to S3 methods
      "UseMethod", "NextMethod",

      # Functions related to S4 methods
      "standardGeneric", "callNextMethod",

      # Functions related to C interfaces
      ".C", ".Call", ".External", ".Fortran"
    )

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
      preceding-sibling::expr[SYMBOL[{ xp_text_in_table(return_not_needed_funs) }]]
      or (
        preceding-sibling::expr/SYMBOL[starts-with(text(), 'Test')]
        and not(SYMBOL_FORMALS)
      )
    )]]
      /following-sibling::expr[OP-LEFT-BRACE and expr[last()]/@line1 != @line1]
      /expr[last()]
      /*[1][
        (
          self::IF
          and (
            not(following-sibling::expr[2]//SYMBOL_FUNCTION_CALL[{ xp_text_in_table(allowed_functions) }])
            or (
              count(following-sibling::expr) = 3
              and not(following-sibling::expr[3]//SYMBOL_FUNCTION_CALL[{ xp_text_in_table(allowed_functions) }])
            )
          )
        ) or (
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
