.namespace_cache <- new.env(parent = emptyenv())

with_namespace_cache <- function(cache_key, expr) {
  if (exists(cache_key, envir = .namespace_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .namespace_cache, inherits = FALSE))
  }
  res <- expr
  assign(cache_key, res, envir = .namespace_cache)
  res
}

# Parse namespace files and return imports exports, methods
namespace_imports <- function(path = find_package(".")) {
  if (length(path) == 0L) {
    return(empty_namespace_data())
  }
  mtime <- as.numeric(file.mtime(file.path(path, "NAMESPACE")))
  cache_key <- paste("imports", path, mtime, sep = "@")
  with_namespace_cache(cache_key, {
    namespace_data <- tryCatch(
      parseNamespaceFile(basename(path), package.lib = file.path(path, "..")),
      error = \(e) NULL
    )

    if (length(namespace_data$imports) == 0L) {
      empty_namespace_data()
    } else {
      do.call(rbind, lapply(namespace_data$imports, safe_get_exports))
    }
  })
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
  # import(ns, except = excluded_funs) directives appear as list(ns, except = excluded_funs)
  except <- if (is.list(ns)) ns[["except"]]
  if (length(ns) > 1L && is.null(except)) {
    return(data.frame(pkg = ns[[1L]], fun = ns[[2L]]))
  }

  # relevant only if there are any exported objects
  fun <- setdiff(getNamespaceExports(ns[[1L]]), except)
  if (length(fun) > 0L) {
    data.frame(pkg = ns[[1L]], fun = fun)
  }
}

empty_namespace_data <- function() {
  data.frame(pkg = character(), fun = character())
}

# filter namespace_imports() for S3 generics
# this loads all imported namespaces
imported_s3_generics <- function(ns_imports) {
  # `NROW()` for the `NULL` case of 0-export dependencies (cf. #1503)
  if (NROW(ns_imports) == 0L) {
    return(empty_namespace_data())
  }
  cache_key <- paste("s3_generics", digest::digest(ns_imports, algo = "sha1"), sep = "@")
  with_namespace_cache(cache_key, {
    is_generic <- vapply(
      seq_len(nrow(ns_imports)),
      function(i) {
        fun_obj <- get(ns_imports$fun[i], envir = asNamespace(ns_imports$pkg[i]))
        is.function(fun_obj) && is_s3_generic(fun_obj)
      },
      logical(1L)
    )

    ns_imports[is_generic, ]
  })
}

exported_s3_generics <- function(path = find_package(".")) {
  if (length(path) == 0L) {
    return(empty_namespace_data())
  }
  mtime <- as.numeric(file.mtime(file.path(path, "NAMESPACE")))
  cache_key <- paste("exports", path, mtime, sep = "@")
  with_namespace_cache(cache_key, {
    namespace_data <- tryCatch(
      parseNamespaceFile(basename(path), package.lib = file.path(path, "..")),
      error = \(e) NULL
    )

    if (NROW(namespace_data$S3methods) == 0L) {
      empty_namespace_data()
    } else {
      data.frame(pkg = basename(path), fun = unique(namespace_data$S3methods[, 1L]))
    }
  })
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
