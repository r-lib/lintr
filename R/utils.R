`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (is.atomic(x[[1L]]) && is.na(x[[1L]]))) {
    y
  } else {
    x
  }
}

`%==%` <- function(x, y) {
  identical(x, y)
}

`%:::%` <- function(p, f) {
  get(f, envir = asNamespace(p))
}

flatten_lints <- function(x) {
  x <- flatten_list(x, class = "lint")
  class(x) <- "lints"
  x
}

# any function using unlist or c was dropping the classnames,
# so need to brute force copy the objects
flatten_list <- function(x, class) {
  res <- list()
  itr <- 1L
  assign_item <- function(x) {
    if (inherits(x, class)) {
      res[[itr]] <<- x
      itr <<- itr + 1L
    } else if (is.list(x)) {
      lapply(x, assign_item)
    }
  }
  assign_item(x)
  res
}

fix_names <- function(x, default) {
  nms <- names(x)

  if (is.null(nms)) {
    nms <- default
  } else {
    nms[!nzchar(nms)] <- default
  }
  names(x) <- nms
  x
}

linter_auto_name <- function(which = -3L) {
  sys_call <- sys.call(which = which)
  nm <- paste(deparse(sys_call, 500L), collapse = " ")
  regex <- rex(start, one_or_more(alnum %or% "." %or% "_" %or% ":"))
  if (re_matches(nm, regex)) {
    match_data <- re_matches(nm, regex, locations = TRUE)
    nm <- substr(nm, start = 1L, stop = match_data[1L, "end"])
    nm <- re_substitutes(nm, rex(start, alnums, "::"), "")
  }
  nm
}

auto_names <- function(x) {
  nms <- names2(x)
  empty <- !nzchar(nms, keepNA = TRUE)
  if (!any(empty)) {
    return(nms)
  }

  default_name <- function(x) {
    if (is_linter(x)) {
      attr(x, "name", exact = TRUE)
    } else {
      deparse1(x)
    }
  }
  defaults <- vapply(x[empty], default_name, character(1L), USE.NAMES = FALSE)

  nms[empty] <- defaults
  nms
}

# The following functions is from dplyr
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

get_content <- function(lines, info) {
  lines[is.na(lines)] <- ""

  if (!missing(info)) {
    if (is_node(info)) {
      info <- lapply(stats::setNames(nm = c("col1", "col2", "line1", "line2")), function(attr) {
        as.integer(xml_attr(info, attr))
      })
    }

    lines <- lines[seq(info$line1, info$line2)]
    lines[length(lines)] <- substr(lines[length(lines)], 1L, info$col2)
    lines[1L] <- substr(lines[1L], info$col1, nchar(lines[1L]))
  }
  paste(lines, collapse = "\n")
}

logical_env <- function(x) {
  res <- as.logical(Sys.getenv(x))
  if (is.na(res)) {
    return(NULL)
  }
  res
}

# from ?chartr
rot <- function(ch, k = 13L) {
  p0 <- function(...) paste(c(...), collapse = "")
  alphabet <- c(letters, LETTERS, " '")
  idx <- seq_len(k)
  chartr(p0(alphabet), p0(c(alphabet[-idx], alphabet[idx])), ch)
}

try_silently <- function(expr) {
  suppressWarnings(
    suppressMessages(
      try(expr, silent = TRUE)
    )
  )
}

# interface to work like options() or setwd() -- returns the old value for convenience
set_lang <- function(new_lang) {
  old_lang <- Sys.getenv("LANGUAGE", unset = NA)
  Sys.setenv(LANGUAGE = new_lang) # nolint: undesirable_function_call. Avoiding {withr} dep in pkg.
  old_lang
}
# handle the logic of either unsetting if it was previously unset, or resetting
reset_lang <- function(old_lang) {
  if (is.na(old_lang)) {
    Sys.unsetenv("LANGUAGE")
  } else {
    Sys.setenv(LANGUAGE = old_lang) # nolint: undesirable_function_call. Avoiding {withr} dep in pkg.
  }
}

#' Create a `linter` closure
#'
#' @param fun A function that takes a source file and returns `lint` objects.
#' @param name Default name of the Linter.
#' Lints produced by the linter will be labelled with `name` by default.
#' @param linter_level Which level of expression is the linter working with?
#'   `"expression"` means an individual expression in `xml_parsed_content`, while `"file"` means all expressions
#'   in the current file are available in `full_xml_parsed_content`.
#'   `NA` means the linter will be run with both, expression-level and file-level source expressions.
#'
#' @return The same function with its class set to 'linter'.
#' @export
Linter <- function(fun, name = linter_auto_name(), linter_level = c(NA_character_, "file", "expression")) { # nolint: object_name, line_length.
  if (!is.function(fun) || length(formals(args(fun))) != 1L) {
    cli_abort("{.arg fun} must be a function taking exactly one argument.")
  }
  linter_level <- match.arg(linter_level)
  force(name)
  class(fun) <- c("linter", "function")
  attr(fun, "name") <- name
  attr(fun, "linter_level") <- linter_level
  fun
}

read_lines <- function(file, encoding = settings$encoding, ...) {
  terminal_newline <- TRUE
  lines <- withCallingHandlers(
    readLines(file, warn = TRUE, ...),
    warning = function(w) {
      if (grepl("incomplete final line found on", w$message, fixed = TRUE)) {
        terminal_newline <<- FALSE
        invokeRestart("muffleWarning")
      }
    }
  )
  lines_conv <- iconv(lines, from = encoding, to = "UTF-8")
  lines[!is.na(lines_conv)] <- lines_conv[!is.na(lines_conv)]
  Encoding(lines) <- "UTF-8"
  attr(lines, "terminal_newline") <- terminal_newline
  lines
}

# nocov start
# support for usethis::use_release_issue(). Make sure to use devtools::load_all() beforehand!
release_bullets <- function() {}
# nocov end

# see issue #923, PR #2455 -- some locales ignore _ when running sort(), others don't.
#   We want to consistently treat "_" < "n" = "N"; C locale does this, which 'radix' uses.
platform_independent_order <- function(x) order(tolower(x), method = "radix")
platform_independent_sort <- function(x) x[platform_independent_order(x)]

#' re_matches with type-stable logical output
#' TODO(r-lib/rex#94): Use re_matches() option directly & deprecate this.
#' @noRd
re_matches_logical <- function(x, regex, ...) {
  res <- re_matches(x, regex, ...)
  if (is.data.frame(res)) {
    res <- complete.cases(res)
  }
  res
}

#' Extract text from `STR_CONST` nodes
#'
#' Convert `STR_CONST` `text()` values into R strings. This is useful to account for arbitrary
#'  character literals, e.g. `R"------[hello]------"`, which is parsed in R as `"hello"`.
#'  It is quite cumbersome to write XPaths allowing for strings like this, so whenever your
#'  linter logic requires testing a `STR_CONST` node's value, use this function.
#' NB: this is also properly vectorized on `s`, and accepts a variety of inputs. Empty inputs
#'  will become `NA` outputs, which helps ensure that `length(get_r_string(s)) == length(s)`.
#'
#' @param s An input string or strings. If `s` is an `xml_node` or `xml_nodeset` and `xpath` is `NULL`,
#'   extract its string value with [xml2::xml_text()]. If `s` is an `xml_node` or `xml_nodeset`
#'   and `xpath` is specified, it is extracted with [xml2::xml_find_chr()].
#' @param xpath An XPath, passed on to [xml2::xml_find_chr()] after wrapping with `string()`.
#'
#' @examples
#' tmp <- tempfile()
#' writeLines("c('a', 'b')", tmp)
#' expr_as_xml <- get_source_expressions(tmp)$expressions[[1L]]$xml_parsed_content
#' writeLines(as.character(expr_as_xml))
#' get_r_string(expr_as_xml, "expr[2]")
#' get_r_string(expr_as_xml, "expr[3]")
#' unlink(tmp)
#'
#' # more importantly, extract raw strings correctly
#' tmp_raw <- tempfile()
#' writeLines("c(R'(a\\b)', R'--[a\\\"\'\"\\b]--')", tmp_raw)
#' expr_as_xml_raw <- get_source_expressions(tmp_raw)$expressions[[1L]]$xml_parsed_content
#' writeLines(as.character(expr_as_xml_raw))
#' get_r_string(expr_as_xml_raw, "expr[2]")
#' get_r_string(expr_as_xml_raw, "expr[3]")
#' unlink(tmp_raw)
#'
#' @export
get_r_string <- function(s, xpath = NULL) {
  if (is_node(s) || is_nodeset(s)) {
    if (is.null(xpath)) {
      s <- xml_text(s)
    } else {
      s <- xml_find_chr(s, sprintf("string(%s)", xpath))
    }
  }
  # parse() skips "" elements --> offsets the length of the output,
  #   but NA in --> NA out
  is.na(s) <- !nzchar(s)
  out <- as.character(parse(text = s, keep.source = FALSE))
  is.na(out) <- is.na(s)
  out
}

is_linter <- function(x) inherits(x, "linter")
is_lint <- function(x) inherits(x, "lint")

is_error <- function(x) inherits(x, "error")

is_tainted <- function(lines) {
  is_error(tryCatch(nchar(lines), error = identity))
}

#' Check that the entries in ... are valid
#'
#' @param dot_names Supplied names, from [...names()].
#' @param ref_calls Functions consuming these `...` (character).
#' @param ref_help Help page to refer users hitting an error to.
#' @noRd
check_dots <- function(dot_names, ref_calls, ref_help = as.character(sys.call(-1L)[[1L]])) {
  valid_args <- unlist(lapply(ref_calls, function(f) names(formals(f))))
  is_valid <- dot_names %in% valid_args
  if (all(is_valid)) {
    return(invisible())
  }
  invalid_args <- dot_names[!is_valid] # nolint: object_usage_linter. TODO(#2252).
  cli_abort(c(
    x = "Found unknown arguments in `...`: {.arg {invalid_args}}.",
    i = "Check for typos and see ?{ref_help} for valid arguments."
  ))
}

cli_abort_internal <- function(...) {
  cli_abort(..., .internal = TRUE)
}
