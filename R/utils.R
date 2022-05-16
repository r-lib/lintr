`%||%` <- function(x, y) {
  if (is.null(x) || length(x) <= 0 || is.na(x[[1L]])) {
    y
  } else {
    x
  }
}

`%==%` <- function(x, y) {
  identical(x, y)
}

`%!=%` <- function(x, y) {
  !identical(x, y)
}

"%:::%" <- function(p, f) {
  get(f, envir = asNamespace(p))
}

flatten_lints <- function(x) {
  structure(
    flatten_list(x, class = "lint"),
    class = "lints"
  )
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

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

linter_auto_name <- function(which = -3L) {
  call <- sys.call(which = which)
  nm <- paste(deparse(call, 500L), collapse = " ")
  regex <- rex(start, one_or_more(alnum %or% "." %or% "_"))
  if (re_matches(nm, regex)) {
    match <- re_matches(nm, regex, locations = TRUE)
    nm <- substr(nm, start = 1L, stop = match[1L, "end"])
  }
  nm
}

auto_names <- function(x) {
  nms <- names2(x)
  missing <- nms == ""
  if (!any(missing)) return(nms)

  default_name <- function(x) {
    if (inherits(x, "linter")) {
      attr(x, "name", exact = TRUE)
    } else {
      paste(deparse(x, 500L), collapse = " ")
    }
  }
  defaults <- vapply(x[missing], default_name, character(1), USE.NAMES = FALSE)

  nms[missing] <- defaults
  nms
}

# The following functions is from dplyr
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

safe_parse_to_xml <- function(parsed_content) {
  if (is.null(parsed_content)) return(NULL)
  tryCatch(xml2::read_xml(xmlparsedata::xml_parse_data(parsed_content)), error = function(e) NULL)
}

get_content <- function(lines, info) {
  lines[is.na(lines)] <- ""

  if (!missing(info)) {
    lines[length(lines)] <- substr(lines[length(lines)], 1L, info$col2)
    lines[1] <- substr(lines[1], info$col1, nchar(lines[1]))
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
rot <- function(ch, k = 13) {
  p0 <- function(...) paste(c(...), collapse = "")
  alphabet <- c(letters, LETTERS, " '")
  idx <- seq_len(k)
  chartr(p0(alphabet), p0(c(alphabet[-idx], alphabet[idx])), ch)
}

base_backport <- function(name, replacement) {
  if (exists(name, asNamespace("base"))) {
    return(NULL)
  }
  assign(name, replacement, parent.frame())
}

base_backport("trimws", function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
})

global_xml_parsed_content <- function(source_expression) {
  if (exists("file_lines", source_expression)) {
    source_expression$full_xml_parsed_content
  }
}

get_file_line <- function(source_expression, line) {
  unname(source_expression$file_lines[[as.numeric(line)]])
}

base_backport("lengths", function(x) vapply(x, length, integer(1L)))

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
  Sys.setenv(LANGUAGE = new_lang)
  old_lang
}
# handle the logic of either unsetting if it was previously unset, or resetting
reset_lang <- function(old_lang) {
  if (is.na(old_lang)) Sys.unsetenv("LANGUAGE") else Sys.setenv(LANGUAGE = old_lang)
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
  structure(fun, class = "linter", name = name)
}

read_lines <- function(file, encoding = settings$encoding, ...) {
  terminal_newline <- TRUE
  lines <- withCallingHandlers({
    readLines(file, warn = TRUE, ...)
  },
  warning = function(w) {
    if (grepl("incomplete final line found on", w$message, fixed = TRUE)) {
      terminal_newline <<- FALSE
      invokeRestart("muffleWarning")
    }
  })
  lines_conv <- iconv(lines, from = encoding, to = "UTF-8")
  lines[!is.na(lines_conv)] <- lines_conv[!is.na(lines_conv)]
  Encoding(lines) <- "UTF-8"
  attr(lines, "terminal_newline") <- terminal_newline
  lines
}

# nocov start
# support for usethis::use_release_issue(). Make sure to use devtools::load_all() beforehand!
release_bullets <- function() {
  "Make sure README.md lists all available linters"
}
# nocov end

# see issue #923 -- some locales ignore _ when running sort(), others don't.
#   we want to consistently treat "_" < "n" = "N"
platform_independent_order <- function(x) order(tolower(gsub("_", "0", x, fixed = TRUE)))
platform_independent_sort <- function(x) x[platform_independent_order(x)]

# convert STR_CONST text() values into R strings. mainly to account for arbitrary
#   character literals valid since R 4.0, e.g. R"------[ hello ]------".
# NB: this is also properly vectorized.
get_r_string <- function(s) {
  if (inherits(s, "xml_nodeset")) s <- xml2::xml_text(s)
  out <- as.character(parse(text = s, keep.source = FALSE))
  is.na(out) <- is.na(s)
  out
}
