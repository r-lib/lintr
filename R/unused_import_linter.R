get_function_calls <- function(x) {
  as.character(xml2::xml_find_all(x, "//SYMBOL_FUNCTION_CALL/text()"))
}

get_import_exprs <- function(x) {
  xml2::xml_find_all(x, "//expr[expr[SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']]]")
}

strip_names <- function(x) {
  x <- re_substitutes(x, rex(start, some_of(".", quote, "`", "%", "$", "@")), "")
  x <- re_substitutes(x, rex(some_of(quote, "`", "<", "-", "%", "$", "@"), end), "")
  x
}

#' @describeIn linters checks that libraries imported are actually used.
#' @export
unused_import_linter <-  function(source_file) {
  x <- global_xml_parsed_content(source_file)
  if (is.null(x)) {
    return()
  }
  import_exprs <- get_import_exprs(x)
  if (length(import_exprs) == 0) {
    return()
  }
  pkg_exprs <- xml2::xml_find_first(import_exprs,"./expr[STR_CONST|SYMBOL]")
  import_pkgs <- strip_names(as.character(xml2::xml_find_first(pkg_exprs,"./*/node()")))

  function_calls <- get_function_calls(x)

  lapply(seq_along(import_pkgs),
    function(i) {
      pkg <- import_pkgs[[i]]

      # Tidyverse is only used to load other packages
      if (pkg == "tidyverse") {
        return()
      }

      package_exports <- getNamespaceExports(pkg)
      if (!any(package_exports %in% function_calls)) {
        import_expr <- xml2::as_list(import_exprs[[i]])
        pkg_expr <- xml2::as_list(pkg_exprs[[i]])
        line_num <- import_expr@line1
        Lint(
          filename = source_file[["filename"]],
          line_number = line_num,
          column_number = pkg_expr@col1,
          type = "warning",
          message = "import is never used",
          line = source_file[["file_lines"]][[as.numeric(line_num)]],
          ranges = list(as.numeric(c(import_expr@col1, import_expr@col2))),
          linter = "unused_import_linter"
          )
      }
    })
}
