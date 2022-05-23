# Parse namespace files and return imports exports, methods
namespace_imports <- function(path = find_package()) {
  imports <- tryCatch({
    pkg_name <- suppressWarnings(pkg_name(path))
    data <- parseNamespaceFile(package = basename(path), package.lib = file.path(path, ".."))
    data$imports
  }, error = function(e) {
    list()
  })

  full_imports <- lengths(imports) == 1L

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
      fun_obj <- get(ns_imports$fun[i], envir = asNamespace(ns_imports$pkg[i]))
      is.function(fun_obj) && is_s3_generic(fun_obj)
    },
    logical(1L)
  )

  ns_imports[is_generic, ]
}

is_s3_generic <- function(fun) {
  # Inspired by `utils::isS3stdGeneric`, though it will detect functions that
  # have `useMethod()` in places other than the first expression.
  bdexpr <- body(fun)
  while (is.call(bdexpr) && bdexpr[[1L]] == "{") bdexpr <- bdexpr[[length(bdexpr)]]
  ret <- is.call(bdexpr) && identical(bdexpr[[1L]], as.name("UseMethod"))
  if (ret)
    names(ret) <- bdexpr[[2L]]
  ret
}

.base_s3_generics <- c(
  names(.knownS3Generics),
  .S3PrimitiveGenerics,
  if (getRversion() >= "3.5.0") {
    .S3_methods_table[, 1L]
  } else {
    # R < 3.5.0 doesn't provide .S3_methods_table
    # fallback: search baseenv() for generic methods
    imported_s3_generics(data.frame(pkg = "base", fun = ls(baseenv()), stringsAsFactors = FALSE))$fun
  }
)
