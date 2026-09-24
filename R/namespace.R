.namespace_cache <- new.env(parent = emptyenv())

# Parse namespace files and return imports exports, methods
namespace_imports <- function(path = find_package(".")) {
  if (is.null(path) || length(path) == 0L || is.na(path)) {
    return(empty_namespace_data())
  }
  ns_file <- file.path(path, "NAMESPACE")
  mtime <- if (file.exists(ns_file)) as.numeric(file.mtime(ns_file)) else 0.0
  cache_key <- paste("imports", path, mtime, sep = "@")
  if (exists(cache_key, envir = .namespace_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .namespace_cache, inherits = FALSE))
  }

  namespace_data <- tryCatch(
    parseNamespaceFile(basename(path), package.lib = file.path(path, "..")),
    error = \(e) NULL
  )

  if (length(namespace_data$imports) == 0L) {
    res <- empty_namespace_data()
  } else {
    res <- do.call(rbind, lapply(namespace_data$imports, safe_get_exports))
  }

  assign(cache_key, res, envir = .namespace_cache)
  res
}

# this loads the namespaces, but is the easiest way to do it
# test package availability to avoid failing out as in #1360
#   typically, users are running this on their own package directories and thus
#   will have the namespace dependencies installed, but we can't guarantee this.
safe_get_exports <- function(ns) {
  # check package exists for both import(x) and importFrom(x, y) usages
  if (!requireNamespace(ns[[1L]], quietly = TRUE)) {
    return(empty_namespace_data())
  }

  # importFrom directives appear as list(ns, imported_funs)
  if (length(ns) > 1L) {
    return(data.frame(pkg = ns[[1L]], fun = ns[[2L]]))
  }

  # relevant only if there are any exported objects
  fun <- getNamespaceExports(ns)
  if (length(fun) > 0L) {
    data.frame(pkg = ns, fun = fun)
  }
}

empty_namespace_data <- function() {
  data.frame(pkg = character(), fun = character())
}

# filter namespace_imports() for S3 generics
# this loads all imported namespaces
imported_s3_generics <- function(ns_imports) {
  if (is.null(ns_imports) || NROW(ns_imports) == 0L) {
    return(empty_namespace_data())
  }
  cache_key <- paste(sort(unique(ns_imports$pkg)), collapse = ";")
  if (nzchar(cache_key) && exists(cache_key, envir = .namespace_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .namespace_cache, inherits = FALSE))
  }

  # `NROW()` for the `NULL` case of 0-export dependencies (cf. #1503)
  is_generic <- vapply(
    seq_len(NROW(ns_imports)),
    function(i) {
      fun_obj <- get(ns_imports$fun[i], envir = asNamespace(ns_imports$pkg[i]))
      is.function(fun_obj) && is_s3_generic(fun_obj)
    },
    logical(1L)
  )

  res <- ns_imports[is_generic, ]
  if (nzchar(cache_key)) {
    assign(cache_key, res, envir = .namespace_cache)
  }
  res
}

exported_s3_generics <- function(path = find_package(".")) {
  if (is.null(path) || length(path) == 0L || is.na(path)) {
    return(empty_namespace_data())
  }
  ns_file <- file.path(path, "NAMESPACE")
  mtime <- if (file.exists(ns_file)) as.numeric(file.mtime(ns_file)) else 0.0
  cache_key <- paste("exports", path, mtime, sep = "@")
  if (exists(cache_key, envir = .namespace_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .namespace_cache, inherits = FALSE))
  }

  namespace_data <- tryCatch(
    parseNamespaceFile(basename(path), package.lib = file.path(path, "..")),
    error = \(e) NULL
  )

  if (length(namespace_data$S3methods) == 0L || nrow(namespace_data$S3methods) == 0L) {
    res <- empty_namespace_data()
  } else {
    res <- data.frame(pkg = basename(path), fun = unique(namespace_data$S3methods[, 1L]))
  }

  assign(cache_key, res, envir = .namespace_cache)
  res
}

is_s3_generic <- function(fun) {
  # Inspired by `utils::isS3stdGeneric`, though it will detect functions that
  # have `useMethod()` in places other than the first expression.
  bdexpr <- body(fun)
  while (is.call(bdexpr) && bdexpr[[1L]] == "{") bdexpr <- bdexpr[[length(bdexpr)]]
  ret <- is.call(bdexpr) && identical(bdexpr[[1L]], as.name("UseMethod"))
  if (ret) {
    names(ret) <- bdexpr[[2L]]
  }
  ret
}

.base_s3_generics <- unique(c(
  names(.knownS3Generics),
  .S3_methods_table[, 1L],
  # Contains S3 generic groups, see ?base::groupGeneric and src/library/base/R/zzz.R
  ls(.GenericArgsEnv)
))
