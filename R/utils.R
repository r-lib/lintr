`%||%` <- function(x, y) {
  if (is.null(x) || length(x) <= 0L || is.na(x[[1L]])) {
    y
  } else {
    x
  }
}

`%==%` <- function(x, y) {
  identical(x, y)
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

linter_auto_name <- function(which = -3L) {
  call <- sys.call(which = which)
  nm <- paste(deparse(call, 500L), collapse = " ")
  regex <- rex(start, one_or_more(alnum %or% "." %or% "_" %or% ":"))
  if (re_matches(nm, regex)) {
    match <- re_matches(nm, regex, locations = TRUE)
    nm <- substr(nm, start = 1L, stop = match[1L, "end"])
    nm <- re_substitutes(nm, rex::rex(start, alnums, "::"), "")
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
    if (inherits(info, "xml_node")) {
      info <- lapply(stats::setNames(nm = c("col1", "col2", "line1", "line2")), function(attr) {
        as.integer(xml2::xml_attr(info, attr))
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

base_backport <- function(name, replacement) {
  if (exists(name, asNamespace("base"))) {
    return(NULL)
  }
  assign(name, replacement, parent.frame())
}

base_backport("trimws", function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
})

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
}
# nocov end

# see issue #923 -- some locales ignore _ when running sort(), others don't.
#   we want to consistently treat "_" < "n" = "N"
platform_independent_order <- function(x) order(tolower(gsub("_", "0", x, fixed = TRUE)))
platform_independent_sort <- function(x) x[platform_independent_order(x)]

# convert STR_CONST text() values into R strings. mainly to account for arbitrary
#   character literals valid since R 4.0, e.g. R"------[ hello ]------".
# NB: this is also properly vectorized.
get_r_string <- function(s, xpath = NULL) {
  if (inherits(s, c("xml_node", "xml_nodeset"))) {
    if (is.null(xpath)) {
      s <- xml2::xml_text(s)
    } else {
      s <- xml2::xml_find_chr(s, sprintf("string(%s)", xpath))
    }
  }
  # parse() skips "" elements --> offsets the length of the output,
  #   but NA in --> NA out
  is.na(s) <- !nzchar(s)
  out <- as.character(parse(text = s, keep.source = FALSE))
  is.na(out) <- is.na(s)
  out
}

#' Convert XML node to R code within
#'
#' NB this is not equivalent to `xml2::xml_text(xml)` in the presence of line breaks
#'
#' @param xml An `xml_node`.
#'
#' @return A source-code equivalent of `xml` with unnecessary whitespace removed.
#'
#' @noRd
get_r_code <- function(xml) {
  # shortcut if xml has line1 and line2 attrs and they are equal
  # if they are missing, xml_attr() returns NA, so we continue
  if (isTRUE(xml2::xml_attr(xml, "line1") == xml2::xml_attr(xml, "line2"))) {
    return(xml2::xml_text(xml))
  }
  # find all unique line numbers
  line_numbers <- sort(unique(xml2::xml_find_num(
    xml2::xml_find_all(xml, "./descendant-or-self::*[@line1]"),
    "number(./@line1)"
  )))
  if (length(line_numbers) <= 1L) {
    # no line breaks necessary
    return(xml2::xml_text(xml))
  }
  lines <- vapply(line_numbers, function(line_num) {
    # all terminal nodes starting on line_num
    paste(xml2::xml_text(
      xml2::xml_find_all(xml, sprintf("./descendant-or-self::*[@line1 = %d and not(*)]", line_num))
    ), collapse = "")
  }, character(1L))
  paste(lines, collapse = "\n")
}

is_tainted <- function(lines) {
  inherits(tryCatch(nchar(lines), error = identity), "error")
}
