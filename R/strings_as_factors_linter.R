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
#' See <https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors>.
#'
#' @evalRd rd_tags("strings_as_factors_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
strings_as_factors_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    atomic_const_cond <- "STR_CONST"
    # like data.frame('a') and data.frame(c('a', 'b'))
    c_cond <- xp_and(
      "expr[SYMBOL_FUNCTION_CALL[text() = 'c']]",
      "expr[STR_CONST]",
      "not(expr[SYMBOL or expr])"
    )
    # save and re-use for rep_cond which satisfies the same condition
    atomic_or_c_cond <- sprintf("%s or (%s)", atomic_const_cond, c_cond)
    # like data.frame(rep(c('a', 'b'), 10L))
    rep_cond <- xp_and(
      "SYMBOL_FUNCTION_CALL[text() = 'rep']",
      sprintf("following-sibling::expr[1][%s]", atomic_or_c_cond)
    )

    # like data.frame(character()) or data.frame(as.character(1))
    known_character_funs <- c(
      "character", "as.character", "paste", "sprintf",
      "format", "formatC", "prettyNum", "toString", "encodeString"
    )
    character_cond <- sprintf(
      "expr[SYMBOL_FUNCTION_CALL[%s]]",
      xp_text_in_table(known_character_funs)
    )

    expr_cond <- xp_and(
      'expr[SYMBOL_FUNCTION_CALL[text() = "data.frame"]]',
      sprintf(
        "expr[((%s) or (expr[%s]) or (%s)) and not(%s)]",
        atomic_or_c_cond,
        rep_cond,
        character_cond,
        "preceding-sibling::SYMBOL_SUB[1][text() = 'row.names']"
      ),
      'not(SYMBOL_SUB[text() = "stringsAsFactors"])'
    )
    bad_expr <- xml2::xml_find_all(xml, sprintf("//expr[%s]", expr_cond))

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = paste(
        "This code relies on stringsAsFactors=TRUE, which changed",
        "in version R 4.0. Please supply an explicit value for",
        "stringsAsFactors for this code to work with earlier versions of R."
      ),
      type = "warning"
    ))
  })
}
