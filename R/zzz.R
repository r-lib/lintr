#' Default linters
#'
#' @description List of default linters for [lint()]. Use
#' [linters_with_defaults()] to customize it.
#'
#' The set of default linters is as follows (any parameterised linters, eg, `line_length_linter` use their default
#' argument(s), see `?<linter_name>` for details):
#'
#' @evalRd rd_linters("default")
#' @seealso [linters] for a complete list of linters available in lintr.
#'
#' @export
default_linters <- modify_defaults(
  defaults = list(),
  assignment_linter(),
  brace_linter(),
  commas_linter(),
  commented_code_linter(),
  cyclocomp_linter(),
  equals_na_linter(),
  function_left_parentheses_linter(),
  infix_spaces_linter(),
  line_length_linter(),
  no_tab_linter(),
  object_length_linter(),
  object_name_linter(),
  object_usage_linter(),
  paren_body_linter(),
  pipe_continuation_linter(),
  semicolon_linter(),
  seq_linter(),
  single_quotes_linter(),
  spaces_inside_linter(),
  spaces_left_parentheses_linter(),
  T_and_F_symbol_linter(),
  trailing_blank_lines_linter(),
  trailing_whitespace_linter(),
  vector_logic_linter()
)

#' Default undesirable functions and operators
#'
#' Lists of function names and operators for [undesirable_function_linter()] and [undesirable_operator_linter()].
#' There is a list for the default elements and another that contains all available elements.
#' Use [modify_defaults()] to produce a custom list.
#'
#' @details
#' The following functions are sometimes regarded as undesirable:
#'
#'  * [attach()] modifies the global search path. Use roxygen2's @importFrom statement in packages, or `::` in scripts.
#'  * [browser()] pauses execution when run and is likely a leftover from debugging. It should be removed.
#'  * [debug()] traps a function and causes execution to pause when that function is run. It should be removed.
#'  * [debugcall()] works similarly to [debug()], causing execution to pause. It should be removed.
#'  * [debugonce()] is only useful for interactive debugging. It should be removed.
#'  * [detach()] modifies the global search path. Detaching environments from the search path is rarely necessary in
#'    production code.
#'  * [ifelse()] isn't type stable. Use an `if`/`else` block for scalar logic, or use
#'    [dplyr::if_else()]/[data.table::fifelse()] for type stable vectorized logic.
#'  * [.libPaths()] permanently modifies the library location. Use [withr::with_libpaths()] for a temporary change
#'    instead.
#'  * [library()] modifies the global search path. Use roxygen2's @importFrom statement in packages, or `::` in scripts.
#'  * [loadNamespace()] doesn't provide an easy way to signal failures. Use the return value of [requireNamespace()]
#'    instead.
#'  * [mapply()] isn't type stable. Use [Map()] to guarantee a list is returned and simplify accordingly.
#'  * [options()] permanently modifies the session options. Use [withr::with_options()] for a temporary change instead.
#'  * [par()] permanently modifies the graphics device parameters. Use [withr::with_par()] for a temporary change
#'    instead.
#'  * [require()] modifies the global search path. Use roxygen2's @importFrom statement in packages, and [library()]
#'    or `::` in scripts.
#'  * [return()] is not necessary as the last value of a function is automatically returned. # TODO remove this
#'  * [sapply()] isn't type stable. Use [vapply()] with an appropriate `FUN.VALUE=` argument to obtain type stable
#'    simplification.
#'  * [setwd()] modifies the global working directory. Use [withr::with_dir()] for a temporary change instead.
#'  * [sink()] permanently redirects output. Use [withr::with_sink()] for a temporary redirection instead.
#'  * [source()] loads code into the global environment unless `local = TRUE` is used, which can cause unexpected
#'    behaviour.
#'  * [substring()] should be replaced by [substr()] with appropriate `stop=` value.
#'  * [Sys.setenv()] permanently modifies the global environment variables. Use [withr::with_envvar()] for a temporary
#'    change instead.
#'  * [Sys.setlocale()] permanently modifies the session locale. Use [withr::with_locale()] for a temporary change
#'    instead.
#'  * [trace()] traps a function and causes execution of arbitrary code when that function is run. It should be removed.
#'  * [undebug()] is only useful for interactive debugging with [debug()]. It should be removed.
#'  * [untrace()] is only useful for interactive debugging with [trace()]. It should be removed.
#'
#' @format A named list of character strings.
#' @rdname default_undesirable_functions
#' @export
all_undesirable_functions <- modify_defaults(
  defaults = list(),
  "attach" = "use roxygen2's @importFrom statement in packages, or `::` in scripts",
  "browser" = "remove debugging markers from 'final' code",
  "debug" = "remove debugging markers from 'final' code",
  "debugcall" = "remove debugging markers from 'final' code",
  "debugonce" = "remove debugging markers from 'final' code",
  "detach" = "use roxygen2's @importFrom statement in packages, or `::` in scripts",
  "ifelse" = "use an if () {} else {} block",
  ".libPaths" = "use withr::with_libpaths()",
  "library" = "use roxygen2's @importFrom statement in packages, or `::` in scripts",
  "loadNamespace" = "use `::` or requireNamespace()",
  "mapply" = "use Map()",
  "options" = "use withr::with_options()",
  "par" = "use withr::with_par()",
  "require" = "use roxygen2's @importFrom statement in packages, or `::` in scripts",
  "return" = "let the last value of a function automatically be returned",
  "sapply" = "use vapply() or lapply()",
  "setwd" = "use withr::with_dir()",
  "sink" = "use withr::with_sink()",
  "source" = NA,
  "substring" = "use substr()",
  "Sys.setenv" = "use withr::with_envvar()",
  "Sys.setlocale" = "use withr::with_locale()",
  "trace" = "remove debugging markers from 'final' code",
  "undebug" = "remove debugging markers from 'final' code",
  "untrace" = "remove debugging markers from 'final' code"
)

#' @rdname default_undesirable_functions
#' @export
default_undesirable_functions <- do.call(modify_defaults, c(
  list(default = list()),
  all_undesirable_functions[c(
    "attach",
    "browser",
    "debug",
    "debugcall",
    "debugonce",
    "detach",
    ".libPaths",
    "library",
    "mapply",
    "options",
    "par",
    "require",
    "sapply",
    "setwd",
    "sink",
    "source",
    "Sys.setenv",
    "Sys.setlocale",
    "trace",
    "undebug",
    "untrace"
  )]
))

#' @rdname default_undesirable_functions
#' @export
all_undesirable_operators <- modify_defaults(
  defaults = list(),
  ":::" = NA,
  "<<-" = NA,
  "->>" = NA
)

#' @rdname default_undesirable_functions
#' @export
default_undesirable_operators <- do.call(modify_defaults, c(
  list(default = list()),
  all_undesirable_operators[c(
    ":::",
    "<<-",
    "->>"
  )]
))


#' Default lintr settings
#' @seealso [read_settings()], [default_linters]
#' @export
default_settings <- NULL

settings <- NULL

# nocov start
.onLoad <- function(libname, pkgname) {
  op <- options()
  op_lintr <- list(
    lintr.linter_file = ".lintr"
  )
  toset <- !(names(op_lintr) %in% names(op))
  if (any(toset)) options(op_lintr[toset])

  default_settings <<- list(
    linters = default_linters,
    encoding = "UTF-8",
    exclude = rex::rex("#", any_spaces, "nolint"),
    exclude_start = rex::rex("#", any_spaces, "nolint start"),
    exclude_end = rex::rex("#", any_spaces, "nolint end"),
    exclude_linter = rex::rex(start, any_spaces, ":", any_spaces,
                              capture(
                                name = "linters",
                                zero_or_more(one_or_more(none_of(",.")), any_spaces, ",", any_spaces),
                                one_or_more(none_of(",."))
                              ), "."),
    exclude_linter_sep = rex::rex(any_spaces, ",", any_spaces),
    exclusions = list(),
    cache_directory = R_user_dir("lintr", "cache"),
    comment_token = Sys.getenv("GITHUB_TOKEN", unset = NA) %||% rot(
      paste0(
        "0n12nn72507",
        "r6273qnnp34",
        "43qno7q42n1",
        "n71nn28")
      , 54L - 13L),
    comment_bot = logical_env("LINTR_COMMENT_BOT") %||% TRUE,
    error_on_lint = logical_env("LINTR_ERROR_ON_LINT") %||% FALSE
  )

  settings <<- list2env(default_settings, parent = emptyenv())
  invisible()
}
# nocov end
