#' Default linters
#'
#' @description List of default linters for [lint()]. Use
#' [with_defaults()] to customize it.
#'
#' The set of default linters is as follows (any parameterised linters, eg, `line_length_linter` use their default
#' argument(s), see `?<linter_name>` for details):
#'
#' @evalRd rd_linters("default")
#' @seealso [linters] for a complete list of linters available in lintr.
#'
#' @export
default_linters <- with_defaults(
  default = list(),
  assignment_linter(),
  brace_linter(),
  commas_linter(),
  commented_code_linter(),
  cyclocomp_linter(),
  equals_na_linter(),
  function_brace_linter(),
  function_left_parentheses_linter(),
  if_else_match_braces_linter(),
  infix_spaces_linter(),
  line_length_linter(),
  no_tab_linter(),
  object_length_linter(),
  object_name_linter(),
  object_usage_linter(),
  open_curly_linter(),
  paren_body_linter(),
  paren_brace_linter(),
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
#' Use [with_defaults()] to produce a custom list.
#'
#' @format A named list of character strings.
#' @rdname default_undesirable_functions
#' @export
all_undesirable_functions <- with_defaults(
  default = list(),
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
default_undesirable_functions <- do.call(with_defaults, c(
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
all_undesirable_operators <- with_defaults(
  default = list(),
  ":::" = NA,
  "<<-" = NA,
  "->>" = NA
)

#' @rdname default_undesirable_functions
#' @export
default_undesirable_operators <- do.call(with_defaults, c(
  list(default = list()),
  all_undesirable_operators[c(
    ":::",
    "<<-",
    "->>"
  )]
))


#' Default lintr settings
#' @seealso [read_settings()], [default_linters()]
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
    cache_directory = "~/.R/lintr_cache",
    comment_token = Sys.getenv("GITHUB_TOKEN", unset = NA) %||% rot(
      paste0(
        "0n12nn72507",
        "r6273qnnp34",
        "43qno7q42n1",
        "n71nn28")
      , 54 - 13),
    comment_bot = logical_env("LINTR_COMMENT_BOT") %||% TRUE,
    error_on_lint = logical_env("LINTR_ERROR_ON_LINT") %||% FALSE
  )

  settings <<- list2env(default_settings, parent = emptyenv())
  invisible()
}
# nocov end
