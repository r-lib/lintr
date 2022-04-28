#' Require usage of correctly-typed literals over literal coercions
#'
#' `as.integer(1)` is the same as `1L` but the latter is more concise and
#'   gets typed correctly at compilation.
#'
#' The same applies to missing sentinels like `NA` -- typically, it is not
#'   necessary to specify the storage type of `NA`, but when it is, prefer
#'   using the typed version (e.g. `NA_real_`) instead of a coercion
#'   (like `as.numeric(NA)`).
#'
#' @evalRd rd_tags("literal_coercion_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
literal_coercion_linter <- function() {
  Linter(function(source_expression) {
    if (length(source_expression$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    coercers <- xp_text_in_table(paste0(
      "as.",
      c("logical", "integer", "numeric", "double", "character")
    ))
    # notes for clarification:
    #  - as.integer(1e6) is arguably easier to read than 1000000L
    #  - in x$"abc", the "abc" STR_CONST is at the top level, so exclude OP-DOLLAR
    #  - need condition against STR_CONST w/ EQ_SUB to skip quoted keyword arguments (see tests)
    xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[ {coercers} ]]
      and expr[2][
        not(OP-DOLLAR)
        and (
          NUM_CONST[not(contains(translate(text(), 'E', 'e'), 'e'))]
          or STR_CONST[not(following-sibling::*[1][self::EQ_SUB])]
        )
      ]
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_expression = source_expression,
      lint_message = paste(
        "Use literals directly where possible, instead of coercion.",
        "c.f. 1L instead of as.integer(1), or NA_real_ instead of as.numeric(NA)."
      ),
      type = "warning"
    ))
  })
}
