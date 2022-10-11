#' Require usage of `sort()` over `.[order(.)]`
#'
#' `sort()` is the dedicated option to sort a list or vector. It is more legible
#' and around twice as fast as `.[order(.)]`, with the gap in performance
#' growing with the vector size.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x[order(x)]",
#'   linters = sort_linter()
#' )
#'
#' lint(
#'   text = "x[order(x, decreasing = TRUE)]",
#'   linters = sort_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x[sample(order(x))]",
#'   linters = sort_linter()
#' )
#'
#' lint(
#'   text = "y[order(x)]",
#'   linters = sort_linter()
#' )
#'
#' @evalRd rd_tags("sort_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
sort_linter <- function() {
  xpath <- "
  //OP-LEFT-BRACKET
    /following-sibling::expr[
      expr[1][
        SYMBOL_FUNCTION_CALL[text() = 'order']
        and following-sibling::expr =
          parent::expr
            /parent::expr
            /expr
      ]
    ]
  "

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    var <- xml2::xml_text(xml2::xml_find_first(bad_expr, ".//SYMBOL"))

    args <- ".//SYMBOL_SUB[text() = 'method' or
                           text() = 'decreasing' or
                           text() = 'na.last']"

    arg_names <- xml2::xml_text(xml2::xml_find_all(bad_expr, args))
    arg_values <- xml2::xml_text(
      xml2::xml_find_all(bad_expr, glue::glue("{args}/following-sibling::expr[1]"))
    )

    orig_call <- sprintf(
      "%s[order(%s)]",
      var,
      toString(c(var, paste(arg_names, arg_values, sep = " = ")))
    )

    if (!"na.last" %in% arg_names) {
      arg_names <- c(arg_names, "na.last")
      arg_values <- c(arg_values, "TRUE")
    }

    new_call <- glue::glue(
      "sort({toString(c(var, paste(arg_names, arg_values, sep = ' = ')))})"
    )

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste0(new_call, " is better than ", orig_call, "."),
      type = "warning"
    )
  })
}
