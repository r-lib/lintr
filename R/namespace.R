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
    stringsAsFactors = FALSE
  )
}

# filter namespace_imports() for S3 generics
# this loads all imported namespaces
imported_s3_generics <- function(ns_imports) {
  is_generic <- vapply(
    seq_len(nrow(ns_imports)),
    function(i) {
      ns <- asNamespace(ns_imports$pkg[i])
      fun <- ns_imports$fun[i]
      if (exists(fun, envir = ns)) {
        fun_obj <- get(ns_imports$fun[i], envir = asNamespace(ns_imports$pkg[i]))
        is.function(fun_obj) && utils::isS3stdGeneric(fun_obj)
      } else {
        FALSE
      }
    },
    logical(1L)
  )

  ns_imports[is_generic, ]
}
