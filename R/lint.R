#' @import rex
NULL

#' Lint a file
#'
#' @param filename the given filename to lint.
#' @param linters a list of linter functions to apply see \code{\link{linters}}
#' for a full list of default and available linters.
#' @export
#' @name lint_file
lint <- function(filename, linters = default_linters) {

  source_file <- get_source_file(filename)

  flatten_lints(
    append(
      lapply(linters,
        function(linter) {
          linter(source_file)
        }),
      if(!is.null(source_file$error)) list(source_file$error)
    )
  )
}

#' Lint all files in a package
#'
#' @param path the path to the base directory of the package, if \code{NULL},
#' the base directory will be searched for by looking in the parent directories
#' of the current directory.
#' @param relative_path if \code{TRUE}, file paths are printed using their path
#' relative to the package base directory.  If \code{FALSE}, use the full
#' absolute path.
#' @export
lint_package <- function(path = NULL, relative_path = TRUE) {
  if (is.null(path)) {
    path <- find_package()
  }
  files <- dir(path=path,
    pattern = rex(".", one_of("Rr"), end),
    recursive = TRUE,
    full.names = TRUE)

  lints <- flatten_lints(lapply(files, lint))

  if (relative_path) {
    lints[] <- lapply(lints,
      function(x) {
        x$filename <- re_substitutes(x$filename, rex(path), ".")
        x
      })
  }

  lints
}

find_package <- function() {
  start_path <- getwd()
  on.exit(setwd(start_path))

  prev_path <- ""
  while (!file.exists(file.path(prev_path, "DESCRIPTION"))) {
    # this condition means we are at the root directory, so give up
    if (prev_path %==% getwd()) {
      return(NULL)
    }
    prev_path <- getwd()
    setwd("..")
  }
  prev_path
}

get_source_file <- function(filename) {
  source_file <- srcfile(filename)
  lines <- readLines(filename)
  source_file$content <- paste0(collapse = "\n", lines)

  source_file$stripped_comments <-
    blank_text(source_file$content, rex("#", except_any_of("\n")))

  newline_search <-
    re_matches(source_file$content,
      rex("\n"),
      locations = TRUE,
      global = TRUE)[[1]]$start

  newline_locs <- c(0L,
    if (!is.na(newline_search[1])) newline_search,
    nchar(source_file$content) + 1L)

  source_file$lengths <-
    (newline_locs[-1L]) - (newline_locs[-length(newline_locs)] + 1L)

  source_file$find_line <- function(x) {
    which(newline_locs >= x)[1L] - 1L
  }

  source_file$find_column <- function(x) {
    line_number <- which(newline_locs >= x)[1L] - 1L
    x - newline_locs[line_number]
  }

  source_file$num_lines <- length(lines)

  lint_error <- function(e) {

    message_info <- re_matches(e$message,
      rex(except_some_of(":"),
        ":",
        capture(name = "line",
          digits),
        ":",
        capture(name = "column",
          digits),
        ":",
        space,
        capture(name = "message",
          anything),
        "\n")
      )

    line_number <- as.integer(message_info$line)
    column_number <- as.integer(message_info$column)

    # If the column number is zero it means the error really occurred at the
    # end of the previous line
    if (column_number == 0L){
      line_number <- line_number - 1L
      line <- getSrcLines(source_file, line_number, line_number)
      column_number <- nchar(line)
    }

    Lint(
      filename = source_file$filename,
      line_number = line_number,
      column_number = column_number,
      type = "error",
      message = message_info$message,
      line = getSrcLines(source_file, line_number, line_number)
      )
  }

  e <- tryCatch(parse(text=source_file$content, srcfile=source_file),
    error = lint_error)

  # This needs to be done twice to avoid
  #   https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16041
  e <- tryCatch(parse(text=source_file$content, srcfile=source_file),
    error = lint_error)

  if (inherits(e, "lint")) {
    source_file$error <- e
  }

  source_file$parsed_content <- getParseData(source_file)

  source_file
}

is_not_empty_list <- function(x) {
  is.list(x) && length(x) != 0L
}

#' Create a \code{Lint} object
#' @param filename path to the source file that was linted.
#' @param line_number line number where the lint occurred.
#' @param column_number column the lint occurred.
#' @param type type of lint.
#' @param message message used to describe the lint error
#' @param line code source where the lint occured
#' @param ranges ranges on the line that should be emphasized.
#' @export
Lint <- function(filename, line_number = 1L, column_number = NULL,
  type = c("style", "warning", "error"),
  message = "", line = "", ranges = NULL) {

  type <- match.arg(type)

  structure(
    list(
      filename = filename,
      line_number = as.integer(line_number),
      column_number = as.integer(column_number) %||% 1L,
      type = type,
      message = message,
      line = line,
      ranges = ranges
      ),
    class="lint")
}

#' @export
print.lint <- function(x, ...) {

  color <- switch(x$type,
    "warning" = crayon::magenta,
    "error" = crayon::red,
    "style" = crayon::blue,
    crayon::bold
  )

  message(
    crayon::bold(x$filename, ":",
    as.character(x$line_number), ":",
    as.character(x$column_number), ": ", sep = ""),
    color(x$type, ": ", sep = ""),
    crayon::bold(x$message), "\n",
    x$line, "\n",
    highlight_string(x$message, x$column_number, x$ranges)
    )
  invisible(x)
}

#' @export
print.lints <- function(x, ...) {
  lapply(x, print, ...)
  invisible(x)
}

#TODO: the arrow/ranges get off sync if there is an escape (and likely with
# unicode chars as well)

highlight_string <- function(message, column_number = NULL, ranges = NULL) {

  adjust <- adjust_position_fun(message)

  column_number <- adjust(column_number)

  ranges[] <- lapply(ranges, adjust)

  maximum <- max(column_number, unlist(ranges))

  line <- fill_with(" ", maximum)

  lapply(ranges, function(range) {
    substr(line, range[1], range[2]) <<-
      fill_with("~", range[2] - range[1] + 1L)
    })

  substr(line, column_number, column_number + 1L) <- "^"

  line
}

adjust_position_fun <- function(message) {
  positions <- re_matches(
    encodeString(message),
    rex("\\" %if_prev_isnt% "\\",

      or(

        # ascii escapes
        one_of("nrtbafv\'\"\`\\"),

        # octal code
        group(range(0, 7) %>% between(1, 3)),

        # hex code
        group("x", one_of(digit, "abcdefABCDEF") %>% between(1, 2)),

        # unicode hex code
        group("u", one_of(digit, "abcdefABCDEF") %>% between(1, 4)),

        # extended unicode hex code
        group("U", one_of(digit, "abcdefABCDEF") %>% between(1, 8))
        )
      ),
    locations = TRUE,
    global = TRUE)[[1]]

  if (is.na(positions$end[1L])) {
    positions$length <- 0L
  }
  else {
    positions$length <- positions$end - positions$start
  }

  function(position) {
    escapes <- which(positions$start < position)

    if (escapes %==% integer(0)) {
      position
    }
    else {
      position + positions$length[which(positions$start < position)]
    }
  }
}

fill_with <- function(character = " ", length = 1L) {
  paste0(collapse = "", rep.int(character, length))
}

# need to register rex shortcuts as globals to avoid CRAN check errors
rex::register_shortcuts("lintr")
