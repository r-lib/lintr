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
    nms[nms == ""] <- default
  }
  names(x) <- nms
  x
}

linter_auto_name <- function(which = -3L) {
  call <- sys.call(which = which)
  nm <- paste(deparse(call, 500L), collapse = " ")
  regex <- rex(start, one_or_more(alnum %or% "." %or% "_" %or% ":"))
  if (re_matches(nm, regex)) {
    match <- re_matches(nm, regex, locations = TRUE)
    nm <- substr(nm, start = 1L, stop = match[1L, "end"])
    nm <- re_substitutes(nm, rex(start, alnums, "::"), "")
  }
  nm
}

auto_names <- function(x) {
  nms <- names2(x)
  missing <- nms == ""
  if (!any(missing)) {
    return(nms)
  }

  default_name <- function(x) {
    if (is_linter(x)) {
      attr(x, "name", exact = TRUE)
    } else {
      paste(deparse(x, 500L), collapse = " ")
    }
  }
  defaults <- vapply(x[missing], default_name, character(1L), USE.NAMES = FALSE)

  nms[missing] <- defaults
  nms
}

# The following functions is from dplyr
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

safe_parse_to_xml <- function(parsed_content) {
  if (is.null(parsed_content)) {
    return(xml2::xml_missing())
  }
  tryCatch(
    xml2::read_xml(xmlparsedata::xml_parse_data(parsed_content)),
    # use xml_missing so that code doesn't always need to condition on XML existing
    error = function(e) xml2::xml_missing()
  )
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
  paste0(collapse = "\n", lines)
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

# imitate sQuote(x, q) [requires R>=3.6]
quote_wrap <- function(x, q) paste0(q, x, q)

# interface to work like options() or setwd() -- returns the old value for convenience
set_lang <- function(new_lang) {
  old_lang <- Sys.getenv("LANGUAGE", unset = NA)
  Sys.setenv(LANGUAGE = new_lang) # nolint: undesirable_function. Avoiding {withr} dep in pkg.
  old_lang
}
# handle the logic of either unsetting if it was previously unset, or resetting
reset_lang <- function(old_lang) {
  if (is.na(old_lang)) {
    Sys.unsetenv("LANGUAGE")
  } else {
    Sys.setenv(LANGUAGE = old_lang) # nolint: undesirable_function. Avoiding {withr} dep in pkg.
  }
}

#' Create a `linter` closure
#'
#' @param fun A function that takes a source file and returns `lint` objects.
#' @param name Default name of the Linter.
#' Lints produced by the linter will be labelled with `name` by default.
#' @return The same function with its class set to 'linter'.
#' @export
Linter <- function(fun, name = linter_auto_name()) { # nolint: object_name.
  if (!is.function(fun) || length(formals(args(fun))) != 1L) {
    stop("`fun` must be a function taking exactly one argument.", call. = FALSE)
  }
  force(name)
  class(fun) <- c("linter", "function")
  attr(fun, "name") <- name
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
release_bullets <- function() {
}
# nocov end

# see issue #923 -- some locales ignore _ when running sort(), others don't.
#   we want to consistently treat "_" < "n" = "N"
platform_independent_order <- function(x) order(tolower(gsub("_", "0", x, fixed = TRUE)))
platform_independent_sort <- function(x) x[platform_independent_order(x)]

#' Extract text from `STR_CONST` nodes
#'
#' Convert `STR_CONST` `text()` values into R strings. This is useful to account for arbitrary
#'  character literals valid since R 4.0, e.g. `R"------[hello]------"`, which is parsed in
#'  R as `"hello"`. It is quite cumbersome to write XPaths allowing for strings like this,
#'  so whenever your linter logic requires testing a `STR_CONST` node's value, use this
#'  function.
#' NB: this is also properly vectorized on `s`, and accepts a variety of inputs. Empty inputs
#'  will become `NA` outputs, which helps ensure that `length(get_r_string(s)) == length(s)`.
#'
#' @param s An input string or strings. If `s` is an `xml_node` or `xml_nodeset` and `xpath` is `NULL`,
#'   extract its string value with [xml2::xml_text()]. If `s` is an `xml_node` or `xml_nodeset`
#'   and `xpath` is specified, it is extracted with [xml2::xml_find_chr()].
#' @param xpath An XPath, passed on to [xml2::xml_find_chr()] after wrapping with `string()`.
#'
#' @examplesIf requireNamespace("withr", quietly = TRUE)
#' tmp <- withr::local_tempfile(lines = "c('a', 'b')")
#' expr_as_xml <- get_source_expressions(tmp)$expressions[[1L]]$xml_parsed_content
#' writeLines(as.character(expr_as_xml))
#' get_r_string(expr_as_xml, "expr[2]") # "a"
#' get_r_string(expr_as_xml, "expr[3]") # "b"
#'
#' # more importantly, extract strings under R>=4 raw strings
#' @examplesIf getRversion() >= "4.0.0"
#' tmp4.0 <- withr::local_tempfile(lines = "c(R'(a\\b)', R'--[a\\\"\'\"\\b]--')")
#' expr_as_xml4.0 <- get_source_expressions(tmp4.0)$expressions[[1L]]$xml_parsed_content
#' writeLines(as.character(expr_as_xml4.0))
#' get_r_string(expr_as_xml4.0, "expr[2]") # "a\\b"
#' get_r_string(expr_as_xml4.0, "expr[3]") # "a\\\"'\"\\b"
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

#' str2lang, but for xml children.
#'
#' [xml2::xml_text()] is deceptively close to obviating this helper, but it collapses
#'   text across lines. R is _mostly_ whitespace-agnostic, so this only matters in some edge cases,
#'   in particular when there are comments within an expression (`<expr>` node). See #1919.
#'
#' @noRd
xml2lang <- function(x) {
  x_strip_comments <- xml_find_all(x, ".//*[not(self::COMMENT or self::expr)]")
  str2lang(paste(xml_text(x_strip_comments), collapse = " "))
}

is_linter <- function(x) inherits(x, "linter")

is_tainted <- function(lines) {
  inherits(tryCatch(nchar(lines), error = identity), "error")
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
  stop(
    "Found unknown arguments in ...: ", toString(dot_names[!is_valid]), ".\n",
    "Check for typos and see ?", ref_help, " for valid arguments.",
    call. = FALSE
  )
}
