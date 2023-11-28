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
    body_xpath <- "(//FUNCTION | //OP-LAMBDA)/following-sibling::expr[1]"
    lint_xpath <- "SYMBOL_FUNCTION_CALL[text() = 'return']"
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

    body_xpath <- glue("
    (//FUNCTION | //OP-LAMBDA)[parent::expr[not(
      preceding-sibling::expr[SYMBOL[{ xp_text_in_table(except) }]]
    )]]
      /following-sibling::expr[OP-LEFT-BRACE and expr[last()]/@line1 != @line1]
      /expr[last()]
    ")
    lint_xpath <- glue("self::*[not(
      (self::expr | following-sibling::SPECIAL[text() = '%>%']/following-sibling::expr/expr[1])
        /SYMBOL_FUNCTION_CALL[{ xp_text_in_table(return_functions) }]
    )]")
    msg <- "All functions must have an explicit return()."
  }

  Linter(function(source_expression) {
    xml <- source_expression$xml_parsed_content
    if (is.null(xml)) return(list())

    body_expr <- xml_find_all(xml, body_xpath)

    # lints_from_terminal_expr not "vectorized" due to xml_children()
    lapply(body_expr, lints_from_terminal_expr, lint_xpath, source_expression, msg)
  }, linter_level = "expression")
}

lints_from_terminal_expr <- function(expr, lint_xpath, source_expression, lint_message) {
  child_expr <- xml_children(expr)
  if (length(child_expr) == 0L) {
    return(list())
  }
  top_node <- xml_name(child_expr[[1L]])

  if (top_node == "OP-LEFT-BRACE") {
    lints_from_terminal_expr(child_expr[[length(child_expr) - 1L]], lint_xpath, source_expression, lint_message)
  } else if (top_node == "IF") {
    c(
      lints_from_terminal_expr(child_expr[[5L]], lint_xpath, source_expression, lint_message),
      if (length(child_expr) > 5L) lints_from_terminal_expr(child_expr[[7L]], lint_xpath, source_expression, lint_message)
    )
  } else {
    list(xml_nodes_to_lints(
      xml_find_first(child_expr[[1L]], lint_xpath),
      source_expression = source_expression,
      lint_message = lint_message,
      type = "style"
    ))
  }
}
