#' Return linter
#'
#' This linter checks functions' [return()] expressions.
#'
#' @param return_style Character string naming the return style. `"implicit"`,
#'   the default, enforces the Tidyverse guide recommendation to leave terminal
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
    # nolint next: object_usage. False positive from {codetools} says 'params' isn't used.
    params <- list(
      implicit = TRUE,
      type = "style",
      lint_xpath = "SYMBOL_FUNCTION_CALL[text() = 'return']",
      lint_message = "Use implicit return behavior; explicit return() is not needed."
    )
  } else {
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

    body_xpath <- glue("
    (//FUNCTION | //OP-LAMBDA)[parent::expr[not(
      preceding-sibling::expr[SYMBOL[{ xp_text_in_table(except) }]]
    )]]
      /following-sibling::expr[OP-LEFT-BRACE and expr[last()]/@line1 != @line1]
      /expr[last()]
    ")
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

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    if (is.null(xml)) return(list())

    body_expr <- xml_find_all(xml, body_xpath)

    params$source_expression <- source_expression
    # nested_return_lints not "vectorized" due to xml_children()
    lapply(body_expr, nested_return_lints, params)
  })
}

nested_return_lints <- function(expr, params) {
  child_expr <- xml_children(expr)
  if (length(child_expr) == 0L) {
    return(list())
  }
  child_node <- xml_name(child_expr)

  if (child_node[1L] == "OP-LEFT-BRACE") {
    expr_idx <- which(child_node %in% c("expr", "equal_assign", "expr_or_assign_or_help"))
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
  } else if (child_node[1L] == "IF") {
    expr_idx <- which(child_node %in% c("expr", "equal_assign", "expr_or_assign_or_help"))
    lapply(child_expr[expr_idx[-1L]], nested_return_lints, params)
  } else {
    xml_nodes_to_lints(
      xml_find_first(child_expr[[1L]], params$lint_xpath),
      source_expression = params$source_expression,
      lint_message = params$lint_message,
      type = params$type
    )
  }
}
