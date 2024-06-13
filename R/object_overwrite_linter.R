#' Block assigning any variables whose name clashes with a `base` R function
#'
#' Re-using existing names creates a risk of subtle error best avoided.
#'   Avoiding this practice also encourages using better, more descriptive names.
#'
#' @param packages Character vector of packages to search for names that should
#'   be avoided. Defaults to the most common default packages: base, stats,
#'   utils, tools, methods, graphics, and grDevices.
#' @param allow_names Character vector of object names to ignore, i.e., which
#'   are allowed to collide with exports from `packages`.
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
#' # names in function signatures are ignored
#' lint(
#'   text = "function(data) data <- subset(data, x > 0)",
#'   linters = object_overwrite_linter()
#' )
#'
#' @evalRd rd_tags("object_overwrite_linter")
#' @seealso
#'  - [linters] for a complete list of linters available in lintr.
#'  - <https://style.tidyverse.org/syntax.html#object-names>
#' @export
object_overwrite_linter <- function(
    packages = c("base", "stats", "utils", "tools", "methods", "graphics", "grDevices"),
    allow_names = character()) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      cli_abort("Package {.pkg {package}} is required, but not available.")
    }
  }
  pkg_exports <- lapply(
    packages,
    # .__C__ etc.: drop 150+ "virtual" names since they are very unlikely to appear anyway
    function(pkg) setdiff(grep("^[.]__[A-Z]__", getNamespaceExports(pkg), value = TRUE, invert = TRUE), allow_names)
  )
  pkg_exports <- data.frame(
    package = rep(packages, lengths(pkg_exports)),
    name = unlist(pkg_exports)
  )

  # Take the first among duplicate names, e.g. 'plot' resolves to base::plot, not graphics::plot
  pkg_exports <- pkg_exports[!duplicated(pkg_exports$name), ]

  # test that the symbol doesn't match an argument name in the function
  # NB: data.table := has parse token LEFT_ASSIGN as well
  xpath_assignments <- glue("
    (//SYMBOL | //STR_CONST)[
      not(text() = ancestor::expr/preceding-sibling::SYMBOL_FORMALS/text())
    ]/
      parent::expr[
        count(*) = 1
        and (
          following-sibling::LEFT_ASSIGN[text() != ':=']
          or following-sibling::EQ_ASSIGN
          or preceding-sibling::RIGHT_ASSIGN
        )
        and ancestor::*[
          (self::expr or self::expr_or_assign_or_help or self::equal_assign)
          and (preceding-sibling::FUNCTION or preceding-sibling::OP-LAMBDA)
        ]
      ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    assigned_exprs <- xml_find_all(xml, xpath_assignments)
    assigned_symbols <- get_r_string(assigned_exprs, "SYMBOL|STR_CONST")
    is_quoted <- startsWith(assigned_symbols, "`")
    assigned_symbols[is_quoted] <- substr(assigned_symbols[is_quoted], 2L, nchar(assigned_symbols[is_quoted]) - 1L)
    is_bad <- assigned_symbols %in% pkg_exports$name
    source_pkg <- pkg_exports$package[match(assigned_symbols[is_bad], pkg_exports$name)]
    lint_message <- sprintf(
      "'%s' is an exported object from package '%s'. Avoid re-using such symbols.",
      assigned_symbols[is_bad], source_pkg
    )

    xml_nodes_to_lints(
      assigned_exprs[is_bad],
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}
