#' Require usage of nzchar where appropriate
#'
#' [nzchar()] efficiently determines which of a vector of strings are empty
#'   (i.e., are `""`). It should in most cases be used instead of
#'   constructions like `string == ""` or `nchar(string) == 0`.
#'
#' One crucial difference is in the default handling of `NA_character_`, i.e.,
#'   missing strings. `nzchar(NA_character_)` is `TRUE`, while `NA_character_ == ""`
#'   and `nchar(NA_character_) == 0` are both `NA`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x[x == '']",
#'   linters = nzchar_linter()
#' )
#'
#' lint(
#'   text = "x[nchar(x) > 0]",
#'   linters = nzchar_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x[nchar(x) > 1]",
#'   linters = nzchar_linter()
#' )
#'
#' # nzchar()'s primary benefit is for vector input;
#' #   for guaranteed-scalar cases like if() conditions, comparing to "" is OK.
#' lint(
#'   text = "if (x == '') y",
#'   linters = nzchar_linter()
#' )
#'
#' @evalRd rd_tags("nzchar_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
nzchar_linter <- function() {
  comparator_nodes <- infix_metadata$xml_tag[infix_metadata$comparator]

  # use string-length to capture both "" and ''
  # if (any(x == "")) is not treated like it's part of if(), but
  #   any(if (x == "") y else z) _is_ treated so. this condition looks for the
  #   expr to be inside a call that's _not_ above an IF/WHILE.
  comparison_xpath <- glue("
  //STR_CONST[string-length(text()) = 2]
    /parent::expr
    /parent::expr[
      ({ xp_or(comparator_nodes) })
      and (
        not(ancestor-or-self::expr[
          preceding-sibling::IF
          or preceding-sibling::WHILE
        ])
        or ancestor-or-self::expr[
          (
            preceding-sibling::expr[SYMBOL_FUNCTION_CALL]
            or preceding-sibling::OP-LEFT-BRACKET
          ) and not(
            descendant-or-self::expr[IF or WHILE]
          )
        ]
      )
    ]
  ")

  # nchar(., type="width") not strictly compatible with nzchar
  # unsure allowNA compatible, so allow it just in case (see TODO in tests)
  nchar_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[text() = 'nchar']
    /parent::expr
    /parent::expr
    /parent::expr[
      ({ xp_or(comparator_nodes) })
      and not(expr[SYMBOL_SUB[
        (
          text() = 'type'
          and following-sibling::expr[1][STR_CONST[contains(text(), 'width')]]
        ) or (
          text() = 'allowNA'
          and following-sibling::expr[1][NUM_CONST[text() = 'TRUE']]
        )
      ]])
      and expr[NUM_CONST[text() = '0' or text() = '0L' or text() = '0.0']]
    ]
  ")

  keepna_note <- paste(
    "Whenever missing data is possible,",
    "please take care to use nzchar(., keepNA = TRUE);",
    "nzchar(NA) is TRUE by default."
  )

  Linter(function(source_expression) {
    xml <- source_expression$xml_parsed_content

    comparison_expr <- xml_find_all(xml, comparison_xpath)
    comparison_lints <- xml_nodes_to_lints(
      comparison_expr,
      source_expression = source_expression,
      lint_message = paste(
        'Instead of comparing strings to "", use nzchar().',
        "Note that if x is a factor, you'll have use ",
        'as.character() to replicate an implicit conversion that happens in x == "".',
        keepna_note
      ),
      type = "warning"
    )

    nchar_expr <- xml_find_all(xml, nchar_xpath)
    nchar_lints <- xml_nodes_to_lints(
      nchar_expr,
      source_expression = source_expression,
      lint_message = paste(
        "Instead of comparing nchar(x) to 0, use nzchar().",
        keepna_note
      ),
      type = "warning"
    )

    c(comparison_lints, nchar_lints)
  }, linter_level = "expression")
}
