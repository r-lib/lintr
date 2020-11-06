#' Exclude lines or files from linting
#'
#' @param lints that need to be filtered.
#' @param exclusions manually specified exclusions
#' @param ... additional arguments passed to \code{\link{parse_exclusions}}
#' @details
#' Exclusions can be specified in three different ways.
#' \enumerate{
#' \item{single line in the source file. default: \code{# nolint}}
#' \item{line range in the source file. default: \code{# nolint start}, \code{# nolint end}}
#' \item{exclusions parameter, a named list of the files and lines to exclude, or just the filenames
#' if you want to exclude the entire file, or the directory names if you want to exclude all files
#' in a directory.}
#' }
exclude <- function(lints, exclusions = settings$exclusions, ...) {
  if (length(lints) <= 0) {
    return(lints)
  }

  df <- as.data.frame(lints)

  filenames <- unique(df$filename)
  source_exclusions <- lapply(filenames, parse_exclusions, ...)
  names(source_exclusions) <- filenames


  exclusions <- normalize_exclusions(c(source_exclusions, exclusions))
  to_exclude <- vapply(seq_len(nrow(df)),
    function(i) {
      file <- df$filename[i]
      file %in% names(exclusions) &&
        length(exclusions[[file]]) == 1 &&
        exclusions[[file]] == Inf ||
        df$line_number[i] %in% exclusions[[file]]
     },
    logical(1))

  if (any(to_exclude)) {
    lints <- lints[!to_exclude]
  }

  lints
}
#' read a source file and parse all the excluded lines from it
#'
#' @param file R source file
#' @param exclude regular expression used to mark lines to exclude
#' @param exclude_start regular expression used to mark the start of an excluded range
#' @param exclude_end regular expression used to mark the end of an excluded range
parse_exclusions <- function(file, exclude = settings$exclude,
                             exclude_start = settings$exclude_start,
                             exclude_end = settings$exclude_end) {
  lines <- readLines(file)

  exclusions <- numeric(0)

  starts <- which(rex::re_matches(lines, exclude_start))
  ends <- which(rex::re_matches(lines, exclude_end))

  if (length(starts) > 0) {
    if (length(starts) != length(ends)) {
      starts_msg <- sprintf(ngettext(length(starts), "%d range start", "%d range starts"), length(starts))
      ends_msg <- sprintf(ngettext(length(ends), "%d range end", "%d range ends"), length(ends))
      stop(file, " has ", starts_msg, " but only ", ends_msg, " for exclusion from linting!")
    }

    for (i in seq_along(starts)) {
      exclusions <- c(exclusions, seq(starts[i], ends[i]))
    }
  }

  sort(unique(c(exclusions, which(rex::re_matches(lines, exclude)))))
}

#' Normalize lint exclusions
#'
#' @param x Exclusion specification
#'  - A character vector of filenames or directories relative to \code{root}
#'  - A named list of integers specifying lines to be excluded per file
#' @param normalize_path Should the names of the returned exclusion list be normalized paths?
#' If no, they will be relative to \code{root}.
#' @param root Base directory for relative filename resolution.
#' @param pattern If non-NULL, only exclude files in excluded directories if they match
#' \code{pattern}. Passed to \link{base::list.files} if a directory is excluded.
#'
#' @value A named list of line numbers to exclude, or the sentinel \code{Inf} for completely
#' excluded files. The names of the list specify the filenames to be excluded.
#' If \code{normalize_path} is \code{TRUE}, they will be normalized relative to \code{root}.
#' Otherwise the paths are left as provided (relative to \code{root} or absolute).
#'
#' @keywords internal
normalize_exclusions <- function(x, normalize_path = TRUE,
                                 root = getwd(),
                                 pattern = NULL) {
  if (is.null(x) || length(x) <= 0) {
    return(list())
  }

  # no named parameters at all
  if (is.null(names(x))) {
    x <- structure(relist(rep(Inf, length(x)), x), names = x)
  } else {
    unnamed <- names(x) == ""
    if (any(unnamed)) {

      # must be character vectors of length 1
      bad <- vapply(seq_along(x),
        function(i) {
          unnamed[i] & (!is.character(x[[i]]) | length(x[[i]]) != 1)
        },
        logical(1))

      if (any(bad)) {
        stop("Full file exclusions must be character vectors of length 1. items: ",
             paste(collapse = ", ", which(bad)),
             " are not!",
             call. = FALSE)
      }
      names(x)[unnamed] <- x[unnamed]
      x[unnamed] <- Inf
    }
  }

  paths <- names(x)
  rel_path <- !is_absolute_path(paths)
  paths[rel_path] <- file.path(root, paths[rel_path])

  is_dir <- dir.exists(paths)
  if (any(is_dir)) {
    dirs <- names(x)[is_dir]
    x <- x[!is_dir]
    all_file_names <- unlist(lapply(
      dirs,
      function(dir) {
        dir_path <- if (is_absolute_path(dir)) dir else file.path(root, dir)
        files <- list.files(
          path = dir_path,
          pattern = pattern,
          recursive = TRUE
        )
        file.path(dir, files) # non-normalized relative paths
      }
    ))

    # Only exclude file if there is no more specific exclusion already
    all_file_names <- setdiff(all_file_names, names(x))

    dir_exclusions <- as.list(rep_len(Inf, length(all_file_names)))
    names(dir_exclusions) <- all_file_names
    x <- c(x, dir_exclusions)
  }

  if (normalize_path) {
    paths <- names(x)
    # specify relative paths w.r.t. root
    rel_path <- !is_absolute_path(paths)
    paths[rel_path] <- file.path(root, paths[rel_path])
    names(x) <- paths
    x <- x[file.exists(paths)]       # remove exclusions for non-existing files
    names(x) <- normalizePath(names(x)) # get full path for remaining files
  }

  remove_line_duplicates(
    remove_file_duplicates(
      remove_empty(x)
    )
  )
}

remove_file_duplicates <- function(x) {
  unique_names <- unique(names(x))

  ## check for duplicate files
  if (length(unique_names) < length(names(x))) {
    x <- lapply(unique_names,
                function(name) {
                  vals <- unname(unlist(x[names(x) == name]))
                  if (any(vals == Inf)) {
                    Inf
                  } else {
                    vals
                  }
                })

    names(x) <- unique_names
  }

  x
}

remove_line_duplicates <- function(x) {
  x[] <- lapply(x, unique)

  x
}
remove_empty <- function(x) {
  x[vapply(x, length, numeric(1)) > 0]
}
