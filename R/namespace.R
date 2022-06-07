# Parse namespace files and return imports exports, methods
namespace_imports <- function(path = find_package()) {
  namespace_data <- tryCatch(
    parseNamespaceFile(basename(path), package.lib = file.path(path, "..")),
    error = function(e) NULL
  )

  if (length(namespace_data$imports) == 0L) {
    return(empty_namespace_data())
  }

  do.call(rbind, lapply(namespace_data$imports, safe_get_exports))
}

# this loads the namespaces, but is the easiest way to do it
# test package availablity to avoid failing out as in #1360
#   typically, users are running this on their own package directories and thus
#   will have the namespace dependencies installed, but we can't guarantee this.
safe_get_exports <- function(ns) {
  # check package exists for both import(x) and importFrom(x, y) usages
  if (!requireNamespace(ns[[1L]], quietly = TRUE)) {
    return(empty_namespace_data())
  }
  # importFrom directives appear as list(ns, imported_funs)
  if (length(ns) > 1L) {
    return(data.frame(pkg = ns[[1L]], fun = ns[[2L]], stringsAsFactors = FALSE))
  }

  data.frame(pkg = ns, fun = getNamespaceExports(ns), stringsAsFactors = FALSE)
}

empty_namespace_data <- function() {
  data.frame(pkg = character(), ns = character(), stringsAsFactors = FALSE)
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
