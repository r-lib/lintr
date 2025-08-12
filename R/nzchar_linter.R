#' Require usage of nzchar where appropriate
#'
#' [nzchar()] efficiently determines which of a vector of strings are empty
#'   (i.e., are `""`). It should in most cases be used instead of
#'   constructions like `string == ""` or `nchar(string) == 0`.
#'
#' One crucial difference is in the default handling of `NA_character_`, i.e.,
#'   missing strings. `nzchar(NA_character_)` is `TRUE`, while `NA_character_ == ""`
#'   and `nchar(NA_character_) == 0` are both `NA`. Therefore, for strict
#'   compatibility, use `nzchar(x, keepNA = TRUE)`. If the input is known to be
#'   complete (no missing entries), this argument can be dropped for conciseness.
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
#'   text = "x[!nzchar(x, keepNA = TRUE)]",
#'   linters = nzchar_linter()
#' )
#'
#' lint(
#'   text = "x[nzchar(x, keepNA = TRUE)]",
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
            preceding-sibling::expr/SYMBOL_FUNCTION_CALL
            or preceding-sibling::OP-LEFT-BRACKET
          ) and not(
            descendant-or-self::expr[IF or WHILE]
          )
        ]
      )
    ]
  ")

  comparison_msg_map <- c(
    GT = 'Use nzchar(x) instead of x > "". ',
    NE = 'Use nzchar(x) instead of x != "". ',
    LE = 'Use !nzchar(x) instead of x <= "". ',
    EQ = 'Use !nzchar(x) instead of x == "". ',
    GE = 'x >= "" is always true, maybe you want nzchar(x)? ',
    LT = 'x < "" is always false, maybe you want !nzchar(x)? '
  )

  # nchar(., type="width") not strictly compatible with nzchar
  # nchar(., allowNA=TRUE) neither
  nchar_xpath <- glue("
  parent::expr
    /parent::expr[
      ({ xp_or(comparator_nodes) })
      and not(expr/SYMBOL_SUB[
        (
          text() = 'type'
          and following-sibling::expr[1]/STR_CONST[contains(text(), 'width')]
        ) or (
          text() = 'allowNA'
          and following-sibling::expr[1]/NUM_CONST[text() = 'TRUE']
        )
      ])
      and expr/NUM_CONST[text() = '0' or text() = '0L' or text() = '0.0']
    ]
  ")

  nchar_msg_map <- c(
    GT = "Use nzchar(x) instead of nchar(x) > 0. ",
    NE = "Use nzchar(x) instead of nchar(x) != 0. ",
    LE = "Use !nzchar(x) instead of nchar(x) <= 0. ",
    EQ = "Use !nzchar(x) instead of nchar(x) == 0. ",
    GE = "nchar(x) >= 0 is always true, maybe you want nzchar(x)? ",
    LT = "nchar(x) < 0 is always false, maybe you want !nzchar(x)? "
  )

  keepna_note <- paste(
    "Whenever missing data is possible,",
    "please take care to use nzchar(., keepNA = TRUE);",
    "nzchar(NA) is TRUE by default."
  )

  # For ordered operators like '>', we need to give the message for
  #   its "opposite" (not inverse) if the bad usage is on the RHS,
  #   e.g. 0 < nchar(x) has to be treated as nchar(x) > 0.
  op_for_msg <- function(expr, const) {
    op <- xml_name(xml_find_first(expr, "*[not(self::COMMENT)][2]"))
    maybe_needs_flip <-
      !is.na(xml_find_first(expr, sprintf("*[not(self::COMMENT)][1][%s]", const)))

    ordered_ops <- c("GT", "GE", "LE", "LT")
    ordered_idx <- match(op, ordered_ops)

    needs_flip <- maybe_needs_flip & !is.na(ordered_idx)
    # un-benchmarked, but should be faster (though less readable) as
    # > ordered_ops[5L - ordered_idx[needs_flip]]
    op[needs_flip] <- rev(ordered_ops)[ordered_idx[needs_flip]]
    op
  }

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    comparison_expr <- xml_find_all(xml, comparison_xpath)
    comparison_op <- op_for_msg(comparison_expr, const = "STR_CONST")
    comparison_lints <- xml_nodes_to_lints(
      comparison_expr,
      source_expression = source_expression,
      lint_message = paste0(
        comparison_msg_map[comparison_op],
        "Note that unlike nzchar(), ", comparison_op, " coerces to character, ",
        "so you'll have to use as.character() if x is a factor. ",
        keepna_note
      ),
      type = "warning"
    )

    xml_calls <- source_expression$xml_find_function_calls("nchar")
    nchar_expr <- xml_find_all(xml_calls, nchar_xpath)
    nchar_op <- op_for_msg(nchar_expr, const = "NUM_CONST")
    nchar_lints <- xml_nodes_to_lints(
      nchar_expr,
      source_expression = source_expression,
      lint_message = paste0(nchar_msg_map[nchar_op], keepna_note),
      type = "warning"
    )

    c(comparison_lints, nchar_lints)
  })
}
