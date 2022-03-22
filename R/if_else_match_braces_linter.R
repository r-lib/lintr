#' Require both or neither if/else branches to use curly braces
#'
#' This linter catches `if`/`else` clauses where the `if` branch is wrapped
#'   in `{...}` but the `else` branch is not, or vice versa, i.e., it ensures
#'   that either both branches use `{...}` or neither does.
#'
#' @evalRd rd_tags("if_else_match_braces_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
if_else_match_braces_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    # if (x) { ... } else if (y) { ... } else { ... } is OK; fully exact pairing
    #   of if/else would require this to be
    #   if (x) { ... } else { if (y) { ... } else { ... } } since there's no
    #   elif operator/token in R, which is pretty unseemly
    xpath <- "
    //IF[
      following-sibling::expr[2][OP-LEFT-BRACE]
      and following-sibling::ELSE
          /following-sibling::expr[1][not(OP-LEFT-BRACE or IF/following-sibling::expr[2][OP-LEFT-BRACE])]
    ]

    |

    //ELSE[
      following-sibling::expr[1][OP-LEFT-BRACE]
      and preceding-sibling::IF/following-sibling::expr[2][not(OP-LEFT-BRACE)]
    ]
    "
    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = "Either both or neither branch in `if`/`else` should use curly braces.",
      type = "warning"
    ))
  })
}
