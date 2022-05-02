#' Modify lintr defaults
#'
#' Modify a list of defaults by name, allowing for replacement, deletion and addition of new elements.
#'
#' @param ... arguments of elements to change. If unnamed, the argument is automatically named.
#' If the named argument already exists in "default", it is replaced by the new element.
#' If it does not exist, it is added. If the value is `NULL`, the element is removed.
#' @param default named list of elements to modify.
#' @return A modified list of elements, sorted by name. To achieve this sort in a platform-independent way, two
#'   transformations are applied to the names: (1) replace `_` with `0` and (2) convert [tolower()].
#' @seealso [linters_with_tags], [linters_with_defaults] for creating linter lists.
#' @examples
#' # custom list of undesirable functions:
#' #    remove sapply (using NULL)
#' #    add cat (with a accompanying message),
#' #    add print (unnamed, i.e. with no accompanying message)
#' #    add return (as taken from all_undesirable_functions)
#' my_undesirable_functions <- modify_defaults(default = default_undesirable_functions,
#'   sapply=NULL, "cat"="No cat allowed", "print", all_undesirable_functions[["return"]])
#' @export
modify_defaults <- function(default, ...) {
  if (missing(default) || !is.list(default) || !all(nzchar(names2(default)))) {
    stop("`default` must be a named list.")
  }
  vals <- list(...)
  nms <- names2(vals)
  missing <- !nzchar(nms, keepNA = TRUE)
  if (any(missing)) {
    args <- as.character(eval(substitute(alist(...)[missing])))
    # foo_linter(x=1) => "foo"
    # var[["foo"]]    => "foo"
    nms[missing] <- re_substitutes(
      re_substitutes(
          # Very long input might have newlines which are not caught
          #  by . in a perl regex; see #774
        re_substitutes(args, rex("(", anything), "", options = "s"),
        rex(start, anything, '["'),
        ""
      ),
      rex('"]', anything, end),
      ""
    )
  }

  to_null <- vapply(vals, is.null, logical(1L))
  if (!all(nms[to_null] %in% names(default))) {
    bad_nms <- setdiff(nms[to_null], names(default))
    is_are <- if (length(bad_nms) > 1L) "are" else "is"
    warning(
      "Trying to remove ", glue::glue_collapse(sQuote(bad_nms), sep = ", ", last = " and "),
      ", which ", is_are, " not in `default`."
    )
  }

  is.na(vals) <- nms == vals
  default[nms] <- vals

  res <- default[!vapply(default, is.null, logical(1L))]

  res[] <- lapply(res, function(x) {
    prev_class <- class(x)
    if (is.function(x) && !inherits(x, "lintr_function")) {
      class(x) <- c(prev_class, "lintr_function")
    }
    x
  })
  res <- res[platform_independent_order(names(res))]
  res
}

#' Create a tag-based linter configuration
#'
#' Make a new list based on all linters provided by `packages` and tagged with `tags`.
#' The result of this function is meant to be passed to the `linters` argument of `lint()`,
#' or to be put in your configuration file.
#'
#' @param ... Arguments of elements to change. If unnamed, the argument is automatically named.
#' If the named argument already exists in the list of linters, it is replaced by the new element.
#' If it does not exist, it is added. If the value is `NULL`, the linter is removed.
#' @inheritParams available_linters
#'
#' @return A modified list of linters.
#' @seealso
#' [linters_with_defaults] for basing off lintr's set of default linters.
#' [available_linters] to get a data frame of available linters.
#' [linters] for a complete list of linters available in lintr.
#' @examples
#' # `linters_with_defaults()` and `linters_with_tags("default")` are the same:
#' all.equal(linters_with_defaults(), linters_with_tags("default"))
#'
#' # Get all linters useful for package development
#' linters_with_tags(tags = "package_development")
#'
#' # Get all linters provided by lintr
#' linters_with_tags(tags = NULL)
#'
#' # Get all linters tagged as "default" from lintr and mypkg
#' \dontrun{linters_with_tags("default", packages = c("lintr", "mypkg"))}
#' @export
linters_with_tags <- function(tags, ..., packages = "lintr", exclude_tags = "deprecated") {
  if (!is.character(tags) && !is.null(tags)) {
    stop("`tags` must be a character vector, or NULL.")
  }
  tagged_linters <- list()

  for (package in packages) {
    pkg_ns <- loadNamespace(package)
    ns_exports <- getNamespaceExports(pkg_ns)
    available <- available_linters(packages = package, tags = tags, exclude_tags = exclude_tags)
    if (nrow(available) > 0L) {
      if (!all(available$linter %in% ns_exports)) {
        missing_linters <- setdiff(available$linter, ns_exports)
        stop(
          "Linters ", glue::glue_collapse(sQuote(missing_linters), sep = ", ", last = "and"),
          " advertised by `available_linters()` but not exported by package ", package, "."
        )
      }
      linter_factories <- mget(available$linter, envir = pkg_ns)
      linters <- mapply(
        call_linter_factory,
        linter_factory = linter_factories,
        linter_name = names(linter_factories),
        MoreArgs = list(package = package)
      )
      tagged_linters <- c(tagged_linters, linters)
    }
  }

  modify_defaults(..., default = tagged_linters)
}

#' Create a linter configuration based on defaults
#'
#' Make a new list based on \pkg{lintr}'s default linters.
#' The result of this function is meant to be passed to the `linters` argument of `lint()`,
#' or to be put in your configuration file.
#'
#' @param default Default list of linters to modify. Must be named.
#' @inheritParams linters_with_tags
#' @seealso
#' [linters_with_tags] for basing off tags attached to linters, possibly across multiple packages.
#' [available_linters] to get a data frame of available linters.
#' [linters] for a complete list of linters available in lintr.
#' @export
#' @examples
#' # When using interactively you will usually pass the result onto `lint` or `lint_package()`
#' \dontrun{
#' lint("foo.R", linters = linters_with_defaults(line_length_linter = line_length_linter(120)))
#' }
#' # the default linter list with a different line length cutoff
#' my_linters <- linters_with_defaults(line_length_linter = line_length_linter(120))
#'
#' # omit the argument name if you are just using different arguments
#' my_linters <- linters_with_defaults(default = my_linters, object_name_linter("camelCase"))
#'
#' # remove assignment checks (with NULL), add absolute path checks
#' my_linters <- linters_with_defaults(
#'   default = my_linters,
#'   assignment_linter = NULL,
#'   absolute_path_linter()
#' )
linters_with_defaults <- function(..., default = default_linters) {
  modify_defaults(..., default = default)
}

#' @rdname linters_with_defaults
#' @export
with_defaults <- function(..., default = default_linters) {
  lintr_deprecated("with_defaults", "linters_with_defaults", "2.0.9001")
  linters_with_defaults(..., default = default)
}

call_linter_factory <- function(linter_factory, linter_name, package) {
  linter <- tryCatch(
    linter_factory(),
    error = function(e) {
      stop("Could not create linter with ", package, "::", linter_name, "(): ", conditionMessage(e))
    }
  )
  # Otherwise, all linters would be called "linter_factory".
  attr(linter, "name") <- linter_name
  linter
}
