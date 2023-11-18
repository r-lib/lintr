#' Block assigning any variables whose name clashes with a `base` R function
#'
#' Re-using existing names creates a risk of subtle error best avoided.
#'   Avoiding this practice also encourages using better, more descriptive names.
#'
#' @param packages Character vector of packages to search for names that should
#'   be avoided. Defaults to the most common default packages: base, stats,
#'   utils, tools, methods, graphics, and grDevices.
#'
#' @examples
#' # will produce lints
#' code <- "function(x) {\n  data <- x\n  data\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = object_overwrite_linter()
#' )
#'
#' code <- "function(x) {\n  lint <- 'fun'\n  lint\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = object_overwrite_linter(packages = "lintr")
#' )
#'
#' # okay
#' code <- "function(x) {\n  data('mtcars')\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = object_overwrite_linter()
#' )
#'
#' code <- "function(x) {\n  data <- x\n  data\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = object_overwrite_linter(packages = "base")
#' )
#'
#' @evalRd rd_tags("object_overwrite_linter")
#' @seealso
#'  - [linters] for a complete list of linters available in lintr.
#'  - <https://style.tidyverse.org/syntax.html#object-names>
#' @export
object_overwrite_linter <- function(
    packages = c("base", "stats", "utils", "tools", "methods", "graphics", "grDevices")) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Package '", package, "' is not available.")
    }
  }
  pkg_exports <- lapply(packages, getNamespaceExports)
  pkg_exports <- data.frame(package = rep(packages, lengths(pkg_exports)), name = unlist(pkg_exports))

  # test that the symbol doesn't match an argument name in the function
  # NB: data.table := has parse token LEFT_ASSIGN as well
  xpath <- glue("
    //SYMBOL[
      not(text() = ancestor::expr/preceding-sibling::SYMBOL_FORMALS/text())
      and ({ xp_text_in_table(pkg_exports$name) })
    ]/
      parent::expr[
        count(*) = 1
        and following-sibling::LEFT_ASSIGN[text() = '<-']
        and ancestor::expr/preceding-sibling::FUNCTION
      ]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)
    bad_symbol <- xml_text(xml_find_first(bad_expr, "SYMBOL"))
    source_pkg <- pkg_exports$package[match(bad_symbol, pkg_exports$name)]
    lint_message <-
      sprintf("'%s' is an exported object from package '%s'. Avoid re-using such symbols.", bad_symbol, source_pkg)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}
