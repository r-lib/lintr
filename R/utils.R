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
    }
    else if (is.list(x)) {
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
  }
  else {
    nms[nms == ""] <- default
  }
  names(x) <- nms
  x
}

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

auto_names <- function(x) {
  nms <- names2(x)
  missing <- nms == ""
  if (all(!missing)) return(nms)

  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(x[missing], deparse2, character(1), USE.NAMES = FALSE)

  nms[missing] <- defaults
  nms
}

blank_text <- function(s, re, shift_start = 0, shift_end = 0) {
  m <- gregexpr(re, s, perl = TRUE)
  regmatches(s, m) <- lapply(regmatches(s, m),
    quoted_blanks,
    shift_start = shift_start,
    shift_end = shift_end)

  s
}

quoted_blanks <- function(matches, shift_start = 0, shift_end = 0) {
  lengths <- nchar(matches)
  blanks <- vapply(Map(rep.int,
      rep.int(" ", length(lengths - (shift_start + shift_end))),
      lengths - (shift_start + shift_end), USE.NAMES = FALSE),
    paste, "", collapse = "")

  substr(matches, shift_start + 1L, nchar(matches) - shift_end) <- blanks
  matches
}


# The following functions is from dplyr
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

recursive_ls <- function(env) {
  if (parent.env(env) %!=% emptyenv()) {
    c(ls(envir = env), recursive_ls(parent.env(env)))
  }
  else {
    ls(envir = env)
  }
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
  A <- c(letters, LETTERS, " '")
  I <- seq_len(k); chartr(p0(A), p0(c(A[-I], A[I])), ch)
}

trim_ws <- function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
}

`@` <- function(x, y) {
  name <- as.character(substitute(y))
  attr(x, name, exact = TRUE)
}

global_xml_parsed_content <- function(source_file) {
  if (exists("file_lines", source_file)) {
    source_file$full_xml_parsed_content
  }
}

get_file_line <- function(source_file, line) {
  unname(source_file$file_lines[[as.numeric(line)]])
}

p <- function(...) paste0(...)

lengths <- function(x) vapply(x, length, integer(1L))

try_silently <- function(expr) {
  suppressWarnings(
    suppressMessages(
      try(expr, silent = TRUE)
    )
  )
}

viapply <- function(x, ...) vapply(x, ..., FUN.VALUE = integer(1))
vcapply <- function(x, ...) vapply(x, ..., FUN.VALUE = character(1))

unquote <- function(str, q="`") {
  # Remove surrounding quotes (select either single, double or backtick) from given character vector
  # and unescape special characters.
  str <- re_substitutes(str, rex(start, q, capture(anything), q, end), "\\1")
  unescape(str, q)
}

escape_chars <- c(
  "\\\\" = "\\",  # backslash
  "\\n"  = "\n",  # newline
  "\\r"  = "\r",  # carriage return
  "\\t"  = "\t",  # tab
  "\\b"  = "\b",  # backspace
  "\\a"  = "\a",  # alert (bell)
  "\\f"  = "\f",  # form feed
  "\\v"  = "\v"   # vertical tab
  # dynamically-added:
  #"\\'"  = "'",  # ASCII apostrophe
  #"\\\"" = "\"", # ASCII quotation mark
  #"\\`"  = "`"   # ASCII grave accent (backtick)
)

unescape <- function(str, q="`") {
  names(q) <- paste0("\\", q)
  my_escape_chars <- c(escape_chars, q)
  res <- gregexpr(text=str, pattern=rex(or(names(my_escape_chars))))
  all_matches <- regmatches(str, res)
  regmatches(str, res) <- lapply(
    all_matches,
    function(string_matches) {
      my_escape_chars[string_matches]
    }
  )
  str
}
