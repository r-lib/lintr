# Parse namespace files and return imports exports, methods
namespace_imports <- function(path = find_package()) {
  imports <- tryCatch({
    pkg_name <- suppressWarnings(pkg_name(path))
    data <- parseNamespaceFile(package = pkg_name, package.lib = file.path(path, ".."))
    data$imports
  }, error = function(e) {
    list()
  })

  full_imports <- lengths(imports) == 1

  # this loads the namespaces, but is the easiest way to do it
  imports[full_imports] <- lapply(imports[full_imports], function(x) list(x, getNamespaceExports(asNamespace(x))))

  data.frame(
    pkg = unlist(lapply(imports, function(x) rep(x[[1L]], length(x[[2L]])))),
    fun = unlist(lapply(imports, `[[`, 2L)),
    stringsAsFactors = FALSE)
}
