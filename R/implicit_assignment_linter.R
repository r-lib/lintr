#' Avoid implicit assignment in function calls
#'
#' Assigning inside function calls makes the code difficult to read, and should
#' be avoided, except for functions that capture side-effects (e.g. [capture.output()]).
#'
#' @param except A character vector of functions to be excluded from linting.
#' @param allow_lazy logical, default `FALSE`. If `TRUE`, assignments that only
#'   trigger conditionally (e.g. in the RHS of `&&` or `||` expressions) are skipped.
#' @param allow_scoped Logical, default `FALSE`. If `TRUE`, "scoped assignments",
#'   where the object is assigned in the statement beginning a branch and used only
#'   within that branch, are skipped.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "if (x <- 1L) TRUE",
#'   linters = implicit_assignment_linter()
#' )
#'
#' lint(
#'   text = "mean(x <- 1:4)",
#'   linters = implicit_assignment_linter()
#' )
#'
#' # okay
#' lines <- "x <- 1L\nif (x) TRUE"
#' writeLines(lines)
#' lint(
#'   text = lines,
#'   linters = implicit_assignment_linter()
#' )
#'
#' lines <- "x <- 1:4\nmean(x)"
#' writeLines(lines)
#' lint(
#'   text = lines,
#'   linters = implicit_assignment_linter()
#' )
#'
#' lint(
#'   text = "A && (B <- foo(A))",
#'   linters = implicit_assignment_linter(allow_lazy = TRUE)
#' )
#'
#' lines <- c(
#'   "if (any(idx <- x < 0)) {",
#'   "  stop('negative elements: ', toString(which(idx)))",
#'   "}"
#' )
#' writeLines(lines)
#' lint(
#'   text = lines,
#'   linters = implicit_assignment_linter(allow_scoped = TRUE)
#' )
#'
#' @evalRd rd_tags("implicit_assignment_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#assignment>
#'
#' @export
implicit_assignment_linter <- function(except = c("bquote", "expression", "expr", "quo", "quos", "quote"),
                                       allow_lazy = FALSE,
                                       allow_scoped = FALSE) {
  stopifnot(is.null(except) || is.character(except))

  if (length(except) > 0L) {
    exceptions <- xp_text_in_table(except)
    xpath_exceptions <- glue("SYMBOL_FUNCTION_CALL[ not({exceptions}) ]")
  } else {
    xpath_exceptions <- "SYMBOL_FUNCTION_CALL"
  }

  # The walrus operator `:=` is also `LEFT_ASSIGN`, but is not a relevant operator
  # to be considered for the present linter.
  assignments <- paste(
    "//LEFT_ASSIGN[text() != ':=']",
    "//RIGHT_ASSIGN",
    sep = " | "
  )

  xpath <- glue("
    ({assignments})
      /parent::expr[
        preceding-sibling::*[not(self::COMMENT)][2][self::IF or self::WHILE]
        or parent::forcond
        or preceding-sibling::expr/{xpath_exceptions}
        or parent::expr/*[1][self::OP-LEFT-PAREN]
      ]
  ")

  if (allow_lazy) {
    xpath <- paste0(xpath, "[not(ancestor::expr/preceding-sibling::*[self::AND2 or self::OR2])]")
  }
  if (allow_scoped) {
    # force 2nd preceding to ensure we're in the loop condition, not the loop expression
    in_branch_cond <- "ancestor-or-self::expr[preceding-sibling::*[not(self::COMMENT)][2][self::IF or self::WHILE]]"
    xpath <- paste0(
      xpath,
      # _if_ we're in an IF/WHILE branch, lint if the assigned SYMBOL appears anywhere later on.
      glue("[not({in_branch_cond}) or expr[1]/SYMBOL = {in_branch_cond}/parent::expr/following::SYMBOL]")
    )
  }

  implicit_message <- paste(
    "Avoid implicit assignments in function calls.",
    "For example, instead of `if (x <- 1L) { ... }`, write `x <- 1L; if (x) { ... }`."
  )

  print_message <- "Call print() explicitly instead of relying on implicit printing behavior via '('."

  Linter(linter_level = "file", function(source_expression) {
    # need the full file to also catch usages at the top level
    xml <- source_expression$full_xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    print_only <- !is.na(xml_find_first(bad_expr, "parent::expr[parent::exprlist and *[1][self::OP-LEFT-PAREN]]"))

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = ifelse(print_only, print_message, implicit_message),
      type = "warning"
    )
  })
}
