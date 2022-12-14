#' Require usage of `sort()` over `.[order(.)]`
#'
#' [sort()] is the dedicated option to sort a list or vector. It is more legible
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
#' # If you are sorting several objects based on the order of one of them, such
#' # as:
#' x <- sample(1:26)
#' y <- letters
#' newx <- x[order(x)]
#' newy <- y[order(x)]
#' # This will be flagged by the linter. However, in this very specific case,
#' # it would be clearer and more efficient to run order() once and assign it
#' # to an object, rather than mix and match order() and sort()
#' index <- order(x)
#' newx <- x[index]
#' newy <- y[index]
#'
#' @evalRd rd_tags("sort_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
sort_linter <- function() {
  xpath <- "
  //OP-LEFT-BRACKET
    /following-sibling::expr[1][
      expr[1][
        SYMBOL_FUNCTION_CALL[text() = 'order']
        and following-sibling::expr =
          parent::expr[1]
            /parent::expr[1]
            /expr[1]
      ]
    ]
  "

  args_xpath <- ".//SYMBOL_SUB[text() = 'method' or
                               text() = 'decreasing' or
                               text() = 'na.last']"

  arg_values_xpath <- glue::glue("{args_xpath}/following-sibling::expr[1]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    var <- xml2::xml_text(
      xml2::xml_find_first(
        bad_expr,
        ".//SYMBOL_FUNCTION_CALL[text() = 'order']/parent::expr[1]/following-sibling::expr[1]"
      )
    )

    orig_call <- sprintf(
      "%1$s[%2$s]",
      var,
      get_r_string(bad_expr)
    )

    # Reconstruct new argument call for each expression separately
    args <- vapply(bad_expr, function(e) {
      arg_names <- xml2::xml_text(xml2::xml_find_all(e, args_xpath))
      arg_values <- xml2::xml_text(
        xml2::xml_find_all(e, arg_values_xpath)
      )
      if (!"na.last" %in% arg_names) {
        arg_names <- c(arg_names, "na.last")
        arg_values <- c(arg_values, "TRUE")
      }
      toString(paste(arg_names, "=", arg_values))
    }, character(1L))

    new_call <- sprintf(
      "sort(%1$s, %2$s)",
      var,
      args
    )

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste0(new_call, " is better than ", orig_call, "."),
      type = "warning"
    )
  })
}
