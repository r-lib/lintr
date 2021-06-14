#' Exclude lines or files from linting
#'
#' @param lints that need to be filtered.
#' @param exclusions manually specified exclusions
#' @param ... additional arguments passed to \code{\link{parse_exclusions}}
#' @details
#' Exclusions can be specified in three different ways.
#' \enumerate{
#' \item{single line in the source file. default: \code{# nolint}, possibly followed by a listing of linters to exclude.
#' If the listing is missing, all linters are excluded on that line. The default listing format is
#' \code{# nolint: linter_name, linter2_name.}. There may not be anything between the colon and the line exclusion tag
#' and the listing must be terminated with a full stop (\code{.}) for the linter list to be respected.}
#' \item{line range in the source file. default: \code{# nolint start}, \code{# nolint end}.
#' \code{#}\code{ nolint start} accepts linter lists in the same form as code{# nolint}.}
#' \item{exclusions parameter, a named list of files with named lists of linters and lines to exclude them on,
#' a named list of the files and lines to exclude, or just the filenames if you want to exclude the entire file,
#' or the directory names if you want to exclude all files in a directory.}
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
  to_exclude <- vapply(
    seq_len(nrow(df)),
    function(i) {
      file <- df$filename[i]
      file %in% names(exclusions) &&
        is_excluded(df$line_number[i], df$linter[i], exclusions[[file]])
    },
    logical(1L)
  )

  if (any(to_exclude)) {
    lints <- lints[!to_exclude]
  }

  lints
}

is_excluded <- function(line_number, linter, file_exclusion) {
  excluded_lines <- unlist(file_exclusion[names2(file_exclusion) %in% c("", linter)])
  Inf %in% excluded_lines || line_number %in% excluded_lines
}

is_excluded_file <- function(file_exclusion) {
  Inf %in% file_exclusion[[names2(file_exclusion) == ""]]
}

#' read a source file and parse all the excluded lines from it
#'
#' @param file R source file
#' @param exclude regular expression used to mark lines to exclude
#' @param exclude_start regular expression used to mark the start of an excluded range
#' @param exclude_end regular expression used to mark the end of an excluded range
#' @param exclude_linter regular expression used to capture a list of to-be-excluded linters immediately following a
#' \code{exclude} or \code{exclude_start} marker.
#' @param exclude_linter_sep regular expression used to split a linter list into indivdual linter names for exclusion.
#' @param lines a character vector of the content lines of \code{file}
#'
#' @return A possibly named list of excluded lines, possibly for specific linters.
parse_exclusions <- function(file, exclude = settings$exclude,
                             exclude_start = settings$exclude_start,
                             exclude_end = settings$exclude_end,
                             exclude_linter = settings$exclude_linter,
                             exclude_linter_sep = settings$exclude_linter_sep,
                             lines = NULL) {
  if (is.null(lines)) {
    lines <- read_lines(file)
  }

  exclusions <- list()

  e <- tryCatch(nchar(lines), error = identity)
  if (inherits(e, "error")) {
    # Invalid encoding. Don't parse exclusions.
    return(list())
  }

  start_locations <- rex::re_matches(lines, exclude_start, locations = TRUE)[, "end"] + 1L
  starts <- which(!is.na(start_locations))
  ends <- which(rex::re_matches(lines, exclude_end))

  if (length(starts) > 0) {
    if (length(starts) != length(ends)) {
      starts_msg <- sprintf(ngettext(length(starts), "%d range start", "%d range starts"), length(starts))
      ends_msg <- sprintf(ngettext(length(ends), "%d range end", "%d range ends"), length(ends))
      stop(file, " has ", starts_msg, " but only ", ends_msg, " for exclusion from linting!")
    }

    for (i in seq_along(starts)) {
      excluded_lines <- seq(starts[i], ends[i])
      linters_string <- substring(lines[starts[i]], start_locations[starts[i]])
      linters_string <- rex::re_matches(linters_string, exclude_linter)[, 1L]

      exclusions <- add_exclusions(exclusions, excluded_lines, linters_string, exclude_linter_sep)
    }
  }

  nolint_locations <- rex::re_matches(lines, exclude, locations = TRUE)[, "end"] + 1L
  nolints <- which(!is.na(nolint_locations))
  # Disregard nolint tags if they also match nolint start / end
  nolints <- setdiff(nolints, c(starts, ends))

  for (i in seq_along(nolints)) {
    linters_string <- substring(lines[nolints[i]], nolint_locations[nolints[i]])
    linters_string <- rex::re_matches(linters_string, exclude_linter)[, 1L]
    exclusions <- add_exclusions(exclusions, nolints[i], linters_string, exclude_linter_sep)
  }

  exclusions[] <- lapply(exclusions, function(lines) sort(unique(lines)))

  exclusions
}

add_excluded_lines <- function(exclusions, excluded_lines, excluded_linters) {
  for (linter in excluded_linters) {
    if (linter %in% names2(exclusions)) {
      i <- which(names2(exclusions) %in% linter)
      exclusions[[i]] <- c(exclusions[[i]], excluded_lines)
    } else {
      exclusions <- c(exclusions, list(excluded_lines))
      if (nzchar(linter)) {
        if (is.null(names(exclusions))) {
          # Repair names if linter == "" is the first exclusion added.
          names(exclusions) <- ""
        }
        names(exclusions)[length(exclusions)] <- linter
      }
    }
  }

  exclusions
}

add_exclusions <- function(exclusions, lines, linters_string, exclude_linter_sep) {
  # No match for linter list: Add to global excludes
  if (is.na(linters_string)) {
    exclusions <- add_excluded_lines(exclusions, lines, "")
  } else {
    # Matched a linter list: only add excluded lines for the listed linters.
    excluded_linters <- strsplit(linters_string, exclude_linter_sep)[[1L]]
    exclusions <- add_excluded_lines(exclusions, lines, excluded_linters)
  }
  exclusions
}

#' Normalize lint exclusions
#'
#' @param x Exclusion specification
#'  - A character vector of filenames or directories relative to \code{root}
#'  - A named list of integers specifying lines to be excluded per file
#'  - A named list of named lists specifying linters and lines to be excluded for the linters per file.
#' @param normalize_path Should the names of the returned exclusion list be normalized paths?
#' If no, they will be relative to \code{root}.
#' @param root Base directory for relative filename resolution.
#' @param pattern If non-NULL, only exclude files in excluded directories if they match
#' \code{pattern}. Passed to \link[base]{list.files} if a directory is excluded.
#'
#' @return A named list of file exclusions.
#' The names of the list specify the filenames to be excluded.
#'
#' Each file exclusion is a possibly named list containing line numbers to exclude, or the sentinel \code{Inf} for
#' completely excluded files. If the an entry is named, the exclusions only take effect for the linter with the same
#' name.
#'
#' If \code{normalize_path} is \code{TRUE}, file names will be normalized relative to \code{root}.
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
    bad <- vapply(
      seq_along(x),
      function(i) {
        !is.character(x[[i]]) || length(x[[i]]) != 1L
      },
      logical(1L)
    )

    if (any(bad)) {
      stop("Full file exclusions must be character vectors of length 1. items: ",
           toString(which(bad)),
           " are not!",
           call. = FALSE)
    }
    # x is a character vector (or list) of file names
    # Normalize to list(<filename> = list(Inf), ...)
    x <- structure(rep(list(Inf), length(x)), names = x)
  } else {
    unnamed <- names(x) == ""
    if (any(unnamed)) {

      # must be character vectors of length 1
      bad <- vapply(
        seq_along(x),
        function(i) {
          unnamed[i] && (!is.character(x[[i]]) || length(x[[i]]) != 1L)
        },
        logical(1L)
      )

      if (any(bad)) {
        stop("Full file exclusions must be character vectors of length 1. items: ",
             toString(which(bad)),
             " are not!",
             call. = FALSE)
      }
      names(x)[unnamed] <- x[unnamed]
      x[unnamed] <- rep_len(list(list(Inf)), sum(unnamed))
    }

    full_line_exclusions <- !vapply(x, is.list, logical(1L))

    if (any(full_line_exclusions)) {

      # must be integer or numeric vectors
      bad <- vapply(
        seq_along(x),
        function(i) {
          full_line_exclusions[i] && !is.numeric(x[[i]])
        },
        logical(1L)
      )

      if (any(bad)) {
        stop("Full line exclusions must be numeric or integer vectors. items: ",
             toString(which(bad)),
             " are not!",
             call. = FALSE)
      }

      # Normalize list(<filename> = c(<lines>)) to
      # list(<filename> = list(c(<lines>)))
      x[full_line_exclusions] <- lapply(x[full_line_exclusions], list)
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

    dir_exclusions <- rep_len(list(Inf), length(all_file_names))
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
    remove_linter_duplicates(
      remove_file_duplicates(
        remove_empty(x)
      )
    )
  )
}

# Combines file exclusions for identical files.
remove_file_duplicates <- function(x) {
  unique_names <- unique(names(x))

  ## check for duplicate files
  if (length(unique_names) < length(names(x))) {
    x <- lapply(
      unique_names,
      function(name) {
        vals <- unname(x[names(x) == name])
        do.call(c, vals)
      }
    )

    names(x) <- unique_names
  }

  x
}

# Removes duplicate line information for each linter within each file.
remove_line_duplicates <- function(x) {
  x[] <- lapply(x, function(ex) {
    ex[] <- lapply(ex, unique)
    ex
  })

  x
}

# Combines line exclusions for identical linters within each file.
remove_linter_duplicates <- function(x) {
  x[] <- lapply(x, function(ex) {
    unique_linters <- unique(names2(ex))

    if (length(unique_linters) < length(ex)) {
      ex <- lapply(unique_linters, function(linter) {
        lines <- unlist(ex[names2(ex) == linter])
        if (Inf %in% lines) {
          Inf
        } else {
          lines
        }
      })

      if (!identical(unique_linters, "")) {
        names(ex) <- unique_linters
      }
    }

    ex
  })

  x
}

# Removes linter exclusions without lines and files without any linter exclusions.
remove_empty <- function(x) {
  x[] <- lapply(x, function(ex) ex[lengths(ex) > 0L])
  x[lengths(x) > 0L]
}
