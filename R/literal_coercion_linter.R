#' Require usage of correctly-typed literals over literal coercions
#'
#' `as.integer(1)` (or `rlang::int(1)`) is the same as `1L` but the latter is
#' more concise and gets typed correctly at compilation.
#'
#' The same applies to missing sentinels like `NA` -- typically, it is not
#'   necessary to specify the storage type of `NA`, but when it is, prefer
#'   using the typed version (e.g. `NA_real_`) instead of a coercion
#'   (like `as.numeric(NA)`).
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "int(1)",
#'   linters = literal_coercion_linter()
#' )
#'
#' lint(
#'   text = "as.character(NA)",
#'   linters = literal_coercion_linter()
#' )
#'
#' lint(
#'   text = "rlang::lgl(1L)",
#'   linters = literal_coercion_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "1L",
#'   linters = literal_coercion_linter()
#' )
#'
#' lint(
#'   text = "NA_character_",
#'   linters = literal_coercion_linter()
#' )
#'
#' lint(
#'   text = "TRUE",
#'   linters = literal_coercion_linter()
#' )
#'
#' @evalRd rd_tags("literal_coercion_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
literal_coercion_linter <- function() {
  rlang_coercers <- c("lgl", "int", "dbl", "chr")
  coercers <- c(
    # base coercers
    paste0("as.", c("logical", "integer", "numeric", "double", "character")),
    rlang_coercers
  )

  # notes for clarification:
  #  - as.integer(1e6) is arguably easier to read than 1000000L
  #  - in x$"abc", the "abc" STR_CONST is at the top level, so exclude OP-DOLLAR (ditto OP-AT)
  #  - need condition against STR_CONST w/ EQ_SUB to skip quoted keyword arguments (see tests)
  #  - for {rlang} coercers, both `int(1)` and `int(1, )` need to be linted
  not_extraction_or_scientific <- "
    not(OP-DOLLAR or OP-AT)
    and (
      NUM_CONST[not(contains(translate(text(), 'E', 'e'), 'e'))]
      or STR_CONST[not(following-sibling::*[1][self::EQ_SUB])]
    )
  "
  xpath <- glue("
  parent::expr[
    count(expr) = 2
    and expr[2][ {not_extraction_or_scientific} ]
  ]")

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(coercers)
    bad_expr <- xml_find_all(xml_calls, xpath)

    coercer <- xp_call_name(bad_expr)
    # tiptoe around the fact that we don't require {rlang}
    is_rlang_coercer <- coercer %in% rlang_coercers
    if (any(is_rlang_coercer) && !requireNamespace("rlang", quietly = TRUE)) {
      # nocov start: test suite will have 'rlang' available
      # NB: we _could_ do some extreme customization where each lint
      #   gets a message according to whether the coercer is from rlang,
      #   but this seems like overkill. Just use a generic message and move on.
      lint_message <- paste(
        "Use literals directly where possible, instead of coercion.",
        "c.f. 1L instead of as.integer(1) or rlang::int(1), or NA_real_ instead of as.numeric(NA).",
        "NB: this message can be improved to show a specific replacement if 'rlang' is installed."
      )
      # nocov end
    } else {
      bad_expr <- strip_comments_from_subtree(bad_expr)
      # duplicate, unless we add 'rlang::' and it wasn't there originally
      coercion_str <- report_str <- xml_text(bad_expr)
      if (any(is_rlang_coercer) && !("package:rlang" %in% search())) {
        needs_prefix <- is_rlang_coercer & !startsWith(coercion_str, "rlang::")
        coercion_str[needs_prefix] <- paste0("rlang::", coercion_str[needs_prefix])
      }
      # the linter logic & rlang requirement should ensure that it's safe to run eval() here;
      #   suppressWarnings() is for cases like 'as.integer("a")' which have an NA result, #2566.
      # TODO(#2473): Avoid a recommendation like '1' that clashes with implicit_integer_linter().
      literal_equivalent_str <- vapply(
        str2expression(coercion_str),
        function(expr) deparse1(suppressWarnings(eval(expr))),
        character(1L)
      )
      lint_message <- sprintf(
        "Use %s instead of %s, i.e., use literals directly where possible, instead of coercion.",
        literal_equivalent_str, report_str
      )
    }

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}
