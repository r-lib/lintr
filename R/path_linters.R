control_char_regex <- rex(one_of(intToUtf8(seq.int(0L, 31L), multiple=TRUE)))
# control chars (non-printing)

win32_bad_punct_regex <- rex(one_of("*", "?", "\"", "<", ">", "|", ":", "/", "\\"))
win32_good_punct_regex <- rex(one_of("!", "#", "$", "%", "&", "'", "(", ")", "+", ",", "-", ".", ";",
                                     "=", "@", "[", "]", "^", "_", "`", "{", "}", "~"))
# win32BadPunct + win32AllowedPunct = [:punct:]

unsafe_char_regex <- rex(or(control_char_regex, win32_bad_punct_regex))

safe_char_regex <- rex(or(alnum, " ", win32_good_punct_regex))
# alphanumeric chars (including UTF-8), spaces, and win32-allowed punctuation

portable_char_regex <- rex(character_class("A-Za-z0-9_\\-."))
# ASCII letters, digits, dot, hyphen and underscore

protocol_regex <- rex(one_or_more(letter), "://")

root_regex <- list(
  unix  = rex(start, "/"),
  tilde = rex(start, "~", zero_or_more(portable_char_regex), zero_or_more("/")),
  win32 = rex(start, letter, ":", maybe(one_of("/", "\\"))),
  unc   = rex(start, "\\\\", maybe(one_or_more(portable_char_regex), maybe("\\")))
)

root_path_regex <- rex(start, or(root_regex), end)

absolute_path_regex <- rex(or(
  root_regex[["unix"]]  %if_next_isnt% one_of(space, "/"),
  root_regex[["tilde"]] %if_next_isnt% one_of(space, quote),
  root_regex[["win32"]] %if_next_isnt% one_of(space, "/", "\\"),
  root_regex[["unc"]]   %if_next_isnt% one_of(space, "\\", quote)
))

relative_path_regex <- rex(
  start %if_next_isnt% or(quote, absolute_path_regex, protocol_regex),
  or(                                                  # not an absolute path or protocol, and then
    group(one_or_more(safe_char_regex), or("/", "\\")),  # (back)slash-separated paths
    group(dot, maybe(dot), end)                        # or single or double dot
  )                                                    # (paths without slash or dots left out)
  %if_next_isnt% quote
)

path_regex <- rex(or(absolute_path_regex, relative_path_regex))

is_absolute_path <- function(str) {
  re_matches(str, absolute_path_regex)
}

is_root_path <- function(str) {
  re_matches(str, root_path_regex)
}

is_relative_path <- function(str) {
  re_matches(str, relative_path_regex)
}

is_path <- function(str) {
  re_matches(str, path_regex)
}

is_valid_path <- function(str, lax=FALSE) {
  # Given a character vector of paths, return FALSE for directory or file having valid characters.
  # On Windows, invalid chars are all control chars and: * ? " < > | :
  # On Unix, all characters are valid, except when lax=TRUE (use same invalid chars as Windows).
  as.logical(
    Map(
      function(dirs, is_win32) {
        if (is_win32 || lax) {
          if (length(dirs) && is_root_path(dirs[[1L]])) {
            dirs <- tail(dirs, -1L)  # remove root element ("/", "C:", or "\\")
          }
          !any(re_matches(dirs, unsafe_char_regex))
        } else {
          TRUE  # either Unix path or strict (lax=FALSE)
        }
      },
      split_path(str),
      re_matches(str, rex(or(list(root_regex[["win32"]], root_regex[["unc"]], "\\"))))
    )
  )
}

is_long_path <- function(str) {
  # TRUE , e.g.: "./foo", "C:\\foo", "foo/bar"
  # FALSE, e.g.: "/",  "\\", "n/a", "/foo", "foo/"
  re_matches(
    re_substitutes(str, ":", ""),
    rex(at_least(safe_char_regex, 1L), one_of("/", "\\"), at_least(safe_char_regex, 2L))
  )
}

is_valid_long_path <- function(str, lax=FALSE) {
  # Convenience function to avoid linting short paths and those unlikely to be valid paths
  ret <- is_valid_path(str, lax)
  if (lax) {
    ret <- ret & is_long_path(str)
  }
  ret
}


split_path <- function(fp, fsep="/|\\\\") {
  if (!is.character(fp)) {
    stop("argument 'fp' should be a character vector")
  }
  if (!is.character(fsep) || length(fsep) != 1L || !nzchar(fsep)) {
    stop("argument 'fsep' should be a non-empty regular expression character string")
  }
  Map(
    function(dirs, prefix) {
      # add root dir if needed
      i <- 1L
      for (dir in seq_along(dirs)) {
        if (!nzchar(dirs[[i]])) {
          i <- i + 1L
        } else {
          break
        }
      }
      i <- i - 1L
      if (i > 0L) {
        dirs <- c(paste0(rep(prefix, i), collapse=""), tail(dirs, -i))
      }
      # add // to protocols (like http, smb, ...)
      if (length(dirs)) {
        l <- nchar(dirs[[1L]])
        if (l > 2L && substr(dirs[[1L]], l, l) == ":") {
          dirs[[1L]] <- paste0(dirs[[1L]], "//")
        }
      }
      # remove empty dirs
      dirs[nzchar(dirs)]
    },
    strsplit(fp, fsep),
    substr(fp, 1L, 1L)
  )
}


unquote <- function(str, type=c("'", "\"", "`")) {
  # Remove the outtermost quotes and unescape backslashes
  for (q in type) {
    str <- re_substitutes(str, rex(start, q, capture(zero_or_more(any)), q, end), "\\1")
  }
  stringi::stri_unescape_unicode(str)
}


#' @describeIn linters  Check that no absolute paths are used (e.g. "/var", "C:\\System", "~/docs").
#' @param lax  Less stringent linting, leading to fewer false positives.
#' @export
absolute_path_linter <- function(lax=TRUE) {
  function(source_file) {
    lapply(
      ids_with_token(source_file, "STR_CONST"),
      function(id) {
        token <- with_id(source_file, id)
        path <- unquote(token[["text"]])
        if (is_absolute_path(path) && is_valid_long_path(path, lax)) {
          start <- token[["col1"]] + 1L
          end <- token[["col2"]] - 1L
          Lint(
            filename = source_file[["filename"]],
            line_number = token[["line1"]],
            column_number = start,
            type = "warning",
            message = "Do not use absolute paths.",
            line = source_file[["lines"]][[as.character(token[["line1"]])]],
            ranges = list(c(start, end)),
            "absolute_path_linter"
          )
        }
      }
    )
  }
}


#' @describeIn linters  Check that file.path() is used to construct safe and portable paths.
#' @export
nonportable_path_linter <- function(lax=TRUE) {
  function(source_file) {
    lapply(
      ids_with_token(source_file, "STR_CONST"),
      function(id) {
        token <- with_id(source_file, id)
        path <- unquote(token[["text"]])
        if (is_path(path) && is_valid_long_path(path, lax) && path != "/" &&
            re_matches(path, rex(one_of("/", "\\")))) {
          start <- token[["col1"]] + 1L
          end <- token[["col2"]] - 1L
          Lint(
            filename = source_file[["filename"]],
            line_number = token[["line1"]],
            column_number = start,
            type = "warning",
            message = "Use file.path() to construct portable file paths.",
            line = source_file[["lines"]][[as.character(token[["line1"]])]],
            ranges = list(c(start, end)),
            "nonportable_filepath_linter"
          )
        }
      }
    )
  }
}

