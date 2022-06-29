#' Identify cases where stringsAsFactors should be supplied explicitly
#'
#' Designed for code bases written for versions of R before 4.0 seeking to upgrade to R >= 4.0, where
#'   one of the biggest pain points will surely be the flipping of the
#'   default value of `stringsAsFactors` from `TRUE` to `FALSE`.
#'
#' It's not always possible to tell statically whether the change will break
#'   existing code because R is dynamically typed -- e.g. in `data.frame(x)`
#'   if `x` is a string, this code will be affected, but if `x` is a number,
#'   this code will be unaffected. However, in `data.frame(x = 'a')`, the
#'   output will unambiguously be affected. We can instead supply
#'   `stringsAsFactors = TRUE`, which will make this code backwards-compatible.
#'
#' See <https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/>.
#'
#' @evalRd rd_tags("strings_as_factors_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
strings_as_factors_linter <- function() {
  # a call to c() with only literal string inputs,
  #   e.g. c("a") or c("a", "b"), but not c("a", b)
  c_combine_strings <- "
    expr[1][SYMBOL_FUNCTION_CALL[text() = 'c']]
    and expr[STR_CONST]
    and not(expr[SYMBOL or expr])
  "

  # like data.frame(character()) or data.frame(as.character(1))
  known_character_funs <- c(
    "character", "as.character", "paste", "sprintf",
    "format", "formatC", "prettyNum", "toString", "encodeString"
  )

  # four inclusions of arguments to data.frame():
  #   (1) "a"                 (but exclude argument names, e.g. in c("a b" = 1), #1036)
  #   (2) c("a", "b")
  #   (3) rep("a", 2)
  #   (4) rep(c("a", "b"), 2)
  # two exclusions
  #   (1) above argument is to row.names=
  #   (2) stringsAsFactors is manually supplied (with any value)
  xpath <- glue::glue("//expr[
    expr[1][SYMBOL_FUNCTION_CALL[text() = 'data.frame']]
    and expr[
      (
        STR_CONST[not(following-sibling::*[1][self::EQ_SUB])]
        or ( {c_combine_strings} )
        or expr[1][
          SYMBOL_FUNCTION_CALL[text() = 'rep']
          and following-sibling::expr[1][STR_CONST or ({c_combine_strings})]
        ]
        or expr[1][SYMBOL_FUNCTION_CALL[ {xp_text_in_table(known_character_funs)} ]]
      )
      and not(preceding-sibling::*[2][self::SYMBOL_SUB and text() = 'row.names'])
    ]
    and not(SYMBOL_SUB[text() = 'stringsAsFactors'])
  ]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste(
        "This code relies on the default value of stringsAsFactors,",
        "which changed in version R 4.0. Please supply an explicit value for",
        "stringsAsFactors for this code to work with versions of R both before",
        "and after this switch."
      ),
      type = "warning"
    )
  })
}
