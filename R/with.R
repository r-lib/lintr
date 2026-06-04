#' Modify lintr defaults
#'
#' Modify a list of defaults by name, allowing for replacement, deletion and addition of new elements.
#'
#' @param ... arguments of elements to change. If unnamed, the argument is automatically named.
#' If the named argument already exists in `defaults`, it is replaced by the new element.
#' If it does not exist, it is added. If the value is `NULL`, the element is removed.
#' @param defaults named list of elements to modify.
#' @return A modified list of elements, sorted by name. To achieve this sort in a platform-independent way, two
#'   transformations are applied to the names: (1) replace `_` with `0` and (2) convert [tolower()].
#'
#' @seealso
#' - [linters_with_defaults] for basing off lintr's set of default linters.
#' - [all_linters] for basing off all available linters in lintr.
#' - [linters_with_tags] for basing off tags attached to linters, possibly across multiple packages.
#' - [available_linters] to get a data frame of available linters.
#' - [linters] for a complete list of linters available in lintr.
#'
#' @examples
#' # custom list of undesirable functions:
#' #    remove `sapply` (using `NULL`)
#' #    add `cat` (with an accompanying message),
#' #    add `print` (unnamed, i.e. with no accompanying message)
#' #    add `source` (as taken from `all_undesirable_functions`)
#' my_undesirable_functions <- modify_defaults(
#'   defaults = default_undesirable_functions,
#'   sapply = NULL, "cat" = "No cat allowed", "print", all_undesirable_functions[["source"]]
#' )
#'
#' # list names of functions specified as undesirable
#' names(my_undesirable_functions)
#' @export
modify_defaults <- function(defaults, ...) {
  if (missing(defaults)) {
    cli_abort("{.arg defaults} is a required argument, but is missing.")
  }
  if (!(is.list(defaults) || is.environment(defaults)) || !all(nzchar(names2(defaults)))) {
    cli_abort("{.arg defaults} must be a named list or environment, not {.obj_type_friendly {defaults}}.")
  }
  vals <- list(...)
  nms <- names2(vals)
  missing_index <- !nzchar(nms, keepNA = TRUE)
  if (any(missing_index)) {
    nms[missing_index] <- guess_names(..., missing_index = missing_index)
  }

  to_null <- vapply(vals, is.null, logical(1L))
  if (!all(nms[to_null] %in% names(defaults))) {
    bad_nms <- setdiff(nms[to_null], names(defaults)) # nolint: object_usage_linter. TODO(#2252).
    cli_warn(c(
      i = "Trying to remove {.field {bad_nms}}, which {?is/are} not in {.arg defaults}."
    ))
  }

  is.na(vals) <- nms == vals
  for (ii in seq_along(nms)) defaults[[nms[ii]]] <- vals[[ii]]
  if (is.environment(defaults)) defaults <- as.list(defaults)

  res <- defaults[!vapply(defaults, is.null, logical(1L))]
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
#' - [linters_with_defaults] for basing off lintr's set of default linters.
#' - [all_linters] for basing off all available linters in lintr.
#' - [available_linters] to get a data frame of available linters.
#' - [linters] for a complete list of linters available in lintr.
#'
#' @examples
#' # `linters_with_defaults()` and `linters_with_tags("default")` are the same:
#' all.equal(linters_with_defaults(), linters_with_tags("default"))
#'
#' # Get all linters useful for package development
#' linters <- linters_with_tags(tags = c("package_development", "style"))
#' names(linters)
#'
#' # Get all linters tagged as "default" from lintr and mypkg
#' if (FALSE) {
#'   linters_with_tags("default", packages = c("lintr", "mypkg"))
#' }
#' @export
linters_with_tags <- function(tags, ..., packages = "lintr", exclude_tags = "deprecated") {
  if (missing(tags)) {
    cli_abort("{.arg tags} was not specified. Available tags: {available_tags()}")
  }
  if (!is.character(tags) && !is.null(tags)) {
    cli_abort("{.arg tags} must be a character vector, or {.code NULL}, not {.obj_type_friendly {tags}}.")
  }
  tagged_linter_env <- new.env()

  for (package in packages) {
    ns_exports <- getNamespaceExports(package)
    available <- available_linters(packages = package, tags = tags, exclude_tags = exclude_tags)
    if (nrow(available) > 0L) {
      if (!all(available$linter %in% ns_exports)) {
        missing_linters <- setdiff(available$linter, ns_exports) # nolint: object_usage_linter. TODO(#2252).
        cli_abort(c(
          x = "Can't find linters {.fn {missing_linters}}.",
          i = "These are advertised by {.fn available_linters}, but are not exported by package {.pkg {package}}."
        ))
      }
      for (linter in available$linter) lazily_assign_linter_(linter, package, tagged_linter_env)
    }
  }

  modify_defaults(..., defaults = tagged_linter_env)
}

#' Avoid call_linter_factory up-front to delay displaying warnings until needed.
#'   This e.g. allows modify_defaults() to skip warnings from linters that aren't needed.
#' NB: this helper has the very subtle effect of ensuring 'linter' is associated correctly;
#'   an earlier attempt had this logic directly in a loop in linters_with_tags, but
#'   that results in the value of 'linter' matching that of the loop index after the loop
#'   completes, rather than what its value was when 'delayedAssign()' is called. local({})
#'   _can_ work, but requires the befuddling line 'linter <- linter' to ensure that the
#'   local() environment retains a copy of that variable; the formals of a helper
#'   have the same effect.
#' @noRd
lazily_assign_linter_ <- function(linter, package, env) {
  linter_factory <- get(linter, envir = getNamespace(package), inherits = FALSE)
  delayedAssign(linter, call_linter_factory(linter_factory, linter, package), assign.env = env)
}

#' Create a linter configuration based on all available linters
#'
#' @inheritParams linters_with_tags
#'
#' @examples
#' names(all_linters())
#'
#' @seealso
#' - [linters_with_defaults] for basing off lintr's set of default linters.
#' - [linters_with_tags] for basing off tags attached to linters, possibly across multiple packages.
#' - [available_linters] to get a data frame of available linters.
#' - [linters] for a complete list of linters available in lintr.
#' @export
all_linters <- function(..., packages = "lintr") {
  linters_with_tags(tags = NULL, packages = packages, ...)
}

#' Create a linter configuration based on defaults
#'
#' Make a new list based on \pkg{lintr}'s default linters.
#' The result of this function is meant to be passed to the `linters` argument of `lint()`,
#' or to be put in your configuration file.
#'
#' @param defaults Default list of linters to modify. Must be named.
#' @inheritParams linters_with_tags
#' @examples
#' # When using interactively you will usually pass the result onto `lint` or `lint_package()`
#' f <- tempfile()
#' writeLines("my_slightly_long_variable_name <- 2.3", f)
#' lint(f, linters = linters_with_defaults(line_length_linter = line_length_linter(120L)))
#' unlink(f)
#'
#' # the default linter list with a different line length cutoff
#' my_linters <- linters_with_defaults(line_length_linter = line_length_linter(120L))
#'
#' # omit the argument name if you are just using different arguments
#' my_linters <- linters_with_defaults(defaults = my_linters, object_name_linter("camelCase"))
#'
#' # remove assignment checks (with NULL), add absolute path checks
#' my_linters <- linters_with_defaults(
#'   defaults = my_linters,
#'   assignment_linter = NULL,
#'   absolute_path_linter()
#' )
#'
#' # checking the included linters
#' names(my_linters)
#'
#' @seealso
#' - [linters_with_tags] for basing off tags attached to linters, possibly across multiple packages.
#' - [all_linters] for basing off all available linters in lintr.
#' - [available_linters] to get a data frame of available linters.
#' - [linters] for a complete list of linters available in lintr.
#' @export
linters_with_defaults <- function(..., defaults = default_linters) {
  modify_defaults(..., defaults = defaults)
}

#' @keywords internal
#' @noRd
call_linter_factory <- function(linter_factory, linter_name, package) {
  linter <- tryCatch(
    linter_factory(),
    error = function(e) {
      cli_abort(
        "Could not create linter with {.fun {package}::{linter_name}}.",
        parent = e
      )
    }
  )
  # Otherwise, all linters would be called "linter_factory".
  attr(linter, "name") <- linter_name
  linter
}

#' @keywords internal
#' @noRd
guess_names <- function(..., missing_index) {
  arguments <- as.character(eval(substitute(alist(...)[missing_index])))
  # foo_linter(x=1) => "foo"
  # var[["foo"]]    => "foo"
  # strip call: foo_linter(x=1) --> foo_linter
  # NB: Very long input might have newlines which are not caught
  #  by . in a perl regex; see #774
  arguments <- re_substitutes(arguments, rex("(", anything), "", options = "s")
  # strip extractors: pkg::foo_linter, var[["foo_linter"]] --> foo_linter
  arguments <- re_substitutes(arguments, rex(start, anything, '["' %or% "::"), "")
  re_substitutes(arguments, rex('"]', anything, end), "")
}
