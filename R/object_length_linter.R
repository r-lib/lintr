#' Object length linter
#'
#' Check that object names are not too long.
#' The length of an object name is defined as the length in characters, after removing extraneous parts:
#'
#'  * generic prefixes for implementations of S3 generics, e.g. `as.data.frame.my_class` has length 8.
#'  * leading `.`, e.g. `.my_hidden_function` has length 18.
#'  * "%%" for infix operators, e.g. `%my_op%` has length 5.
#'  * trailing `<-` for assignment functions, e.g. `my_attr<-` has length 7.
#'
#' Note that this behavior relies in part on having packages in your Imports available;
#'   see the detailed note in [object_name_linter()] for more details.
#'
#' @param length maximum variable name length allowed.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "very_very_long_variable_name <- 1L",
#'   linters = object_length_linter(length = 10L)
#' )
#'
#' # okay
#' lint(
#'   text = "very_very_long_variable_name <- 1L",
#'   linters = object_length_linter(length = 30L)
#' )
#'
#' lint(
#'   text = "var <- 1L",
#'   linters = object_length_linter(length = 10L)
#' )
#'
#' @evalRd rd_tags("object_length_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
object_length_linter <- function(length = 30L) {
  lint_message <- paste("Variable and function names should not be longer than", length, "characters.")

  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content

    assignments <- xml_find_all(xml, object_name_xpath)

    # Retrieve assigned name
    nms <- strip_names(
      xml_text(assignments)
    )

    # run namespace_imports at run-time, not "compile" time to allow package structure to change
    pkg <- find_package(source_expression$filename)
    ns_imports <- namespace_imports(pkg)
    generics <- strip_names(c(
      declared_s3_generics(xml),
      imported_s3_generics(ns_imports)$fun,
      exported_s3_generics(pkg)$fun,
      .base_s3_generics
    ))
    generics <- unique(generics[nzchar(generics)])

    # Remove generic function names from generic implementations
    # This only lints S3 implementations if the class names are too long, still lints generics if they are too long.
    nms_stripped <- re_substitutes(nms, rex(start, or(generics), "."), "")

    too_long <- nchar(nms_stripped) > length

    xml_nodes_to_lints(
      assignments[too_long],
      source_expression = source_expression,
      lint_message = lint_message,
      type = "style"
    )
  })
}
