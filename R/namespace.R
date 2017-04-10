# Parse namespace files and return imports exports, methods
namespace_imports <- function(path = ".") {
  tryCatch({
    d <- desc::desc(file.path(path, "DESCRIPTION"))
    data <- parseNamespaceFile(d$get("Package"), file.path(path, ".."))
  }, error = function(e) {
    data <- list()
  })

  full_imports <- lengths(data$imports) == 1

  # this loads the namespaces, but is the easiest way to do it
  data$imports[full_imports] <- lapply(data$imports[full_imports], function(x) list(x, getNamespaceExports(asNamespace(x))))

  data.frame(
    pkg = unlist(lapply(data$imports, function(x) rep(x[[1L]], length(x[[2L]])))),
    fun = unlist(lapply(data$imports, `[[`, 2L)),
    stringsAsFactors = FALSE)
}
