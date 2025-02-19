#' Exclude lines or files from linting
#'
#' @param lints that need to be filtered.
#' @param exclusions manually specified exclusions
#' @param linter_names character vector of names of the active linters, used for parsing inline exclusions.
#' @param ... additional arguments passed to [parse_exclusions()]
#' @eval c(
#'   # we use @eval for the details section to avoid a literal nolint exclusion tag with non-existing linter names
#'   # those produce a warning from [parse_exclusions()] otherwise. See #1219 for details.
#'   "@details",
#'   "Exclusions can be specified in three different ways.",
#'   "",
#'   "1. Single line in the source file. default: `# nolint`, possibly followed by a listing of linters to exclude.",
#'   "   If the listing is missing, all linters are excluded on that line. The default listing format is",
#'   paste(
#'     "   `#",
#'     "nolint: linter_name, linter2_name.`. There may not be anything between the colon and the line exclusion tag"
#'   ),
#'   "   and the listing must be terminated with a full stop (`.`) for the linter list to be respected.",
#'   "2. Line range in the source file. default: `# nolint start`, `# nolint end`. `# nolint start` accepts linter",
#'   "   lists in the same form as `# nolint`.",
#'   "3. Exclusions parameter, a list with named and/or unnamed entries. ",
#'   "   Outer elements have the following characteristics:",
#'   "   1. Unnamed elements specify filenames or directories.",
#'   "   2. Named elements are a vector or list of line numbers, with `Inf` indicating 'all lines'.",
#'   "      The name gives a path relative to the config.",
#'   "      1. Unnamed elements denote exclusion of all linters in the given path or directory.",
#'   "      2. Named elements, where the name specifies a linter, denote exclusion for that linter.",
#'   "   For convenience, a vector can be used in place of a list whenever it would not introduce ambiguity, e.g.",
#'   "   a character vector of files to exclude or a vector of lines to exclude.",
#'   "",
#'   "   Note also that all paths are interpreted as globs ([Sys.glob()]), so that e.g. `*` does pattern expansion.",
#'   NULL
#' )
#'
#' @keywords internal
exclude <- function(lints, exclusions = settings$exclusions, linter_names = NULL, ...) {
  if (length(lints) <= 0L) {
    return(lints)
  }

  lint_df <- as.data.frame(lints)

  filenames <- unique(lint_df$filename)
  source_exclusions <- lapply(filenames, parse_exclusions, linter_names = linter_names, ...)
  names(source_exclusions) <- filenames


  exclusions <- normalize_exclusions(c(source_exclusions, exclusions))
  to_exclude <- vapply(
    seq_len(nrow(lint_df)),
    function(i) {
      filename <- lint_df$filename[i]
      filename %in% names(exclusions) &&
        is_excluded(lint_df$line_number[i], lint_df$linter[i], exclusions[[filename]])
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
  any(vapply(
    file_exclusion[!nzchar(names2(file_exclusion))],
    function(full_exclusion) Inf %in% full_exclusion,
    logical(1L)
  ))
}

line_info <- function(line_numbers, type = c("start", "end")) {
  type <- match.arg(type)
  range_word <- paste0("range ", type, if (length(line_numbers) != 1L) "s")
  n <- length(line_numbers)
  if (n == 0L) {
    paste("0", range_word)
  } else if (n == 1L) {
    paste0("1 ", range_word, " (line ", line_numbers, ")")
  } else {
    paste0(n, " ", range_word, " (lines ", toString(line_numbers), ")")
  }
}

#' read a source file and parse all the excluded lines from it
#'
#' @param file R source file
#' @param exclude Regular expression used to mark lines to exclude.
#' @param exclude_next Regular expression used to mark lines immediately preceding excluded lines.
#' @param exclude_start Regular expression used to mark the start of an excluded range.
#' @param exclude_end Regular expression used to mark the end of an excluded range.
#' @param exclude_linter Regular expression used to capture a list of to-be-excluded linters immediately following a
#' `exclude` or `exclude_start` marker.
#' @param exclude_linter_sep Regular expression used to split a linter list into individual linter names for exclusion.
#' @param lines A character vector of the content lines of `file`.
#' @param linter_names Names of active linters.
#'
#' @return A possibly named list of excluded lines, possibly for specific linters.
#' @keywords internal
parse_exclusions <- function(file,
                             exclude = settings$exclude,
                             exclude_next = settings$exclude_next,
                             exclude_start = settings$exclude_start,
                             exclude_end = settings$exclude_end,
                             exclude_linter = settings$exclude_linter,
                             exclude_linter_sep = settings$exclude_linter_sep,
                             lines = NULL,
                             linter_names = NULL) {
  if (is.null(lines)) {
    lines <- read_lines(file)
  }

  exclusions <- list()

  if (is_tainted(lines)) {
    # Invalid encoding. Don't parse exclusions.
    return(list())
  }

  start_locations <- re_matches(lines, exclude_start, locations = TRUE)[, "end"] + 1L
  end_locations <- re_matches(lines, exclude_end, locations = TRUE)[, "start"]
  starts <- which(!is.na(start_locations))
  ends <- which(!is.na(end_locations))

  if (length(starts) > 0L) {
    if (length(starts) != length(ends)) {
      starts_msg <- line_info(starts, type = "start") # nolint: object_usage_linter. TODO(#2252).
      ends_msg <- line_info(ends, type = "end") # nolint: object_usage_linter. TODO(#2252).
      cli_abort(c(
        i = "Equal number of line starts and ends expected for exclusion from linting.",
        x = "{.file {file}} has {.strong {starts_msg}} and {.strong {ends_msg}}."
      ))
    }

    for (i in seq_along(starts)) {
      excluded_lines <- seq(starts[i], ends[i])
      linters_string <- substring(lines[starts[i]], start_locations[starts[i]])
      linters_string <- re_matches(linters_string, exclude_linter)[, 1L]

      exclusions <- add_exclusions(exclusions, excluded_lines, linters_string, exclude_linter_sep, linter_names)
    }
  }

  next_locations <- re_matches(lines, exclude_next, locations = TRUE)[, "end"] + 1L
  nexts <- which(!is.na(next_locations))

  nolint_locations <- re_matches(lines, exclude, locations = TRUE)[, "end"] + 1L
  nolints <- which(!is.na(nolint_locations))

  # Disregard nolint tags if they also match nolint next / start / end
  nolints <- setdiff(nolints, c(nexts, starts, ends))

  for (nolint in nolints) {
    linters_string <- get_linters_string(lines[nolint], nolint_locations[nolint], exclude_linter)
    exclusions <- add_exclusions(exclusions, nolint, linters_string, exclude_linter_sep, linter_names)
  }
  for (nextt in nexts) {
    linters_string <- get_linters_string(lines[nextt], next_locations[nextt], exclude_linter)
    exclusions <- add_exclusions(exclusions, nextt + 1L, linters_string, exclude_linter_sep, linter_names)
  }

  exclusions[] <- lapply(exclusions, function(lines) sort(unique(lines)))

  exclusions
}

get_linters_string <- function(line, loc, exclude_linter) {
  linters_string <- substring(line, loc)
  re_matches(linters_string, exclude_linter)[, 1L]
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

add_exclusions <- function(exclusions, lines, linters_string, exclude_linter_sep, linter_names) {
  # No match for linter list: Add to global excludes
  if (is.na(linters_string)) {
    exclusions <- add_excluded_lines(exclusions, lines, "")
  } else {
    # Matched a linter list: only add excluded lines for the listed linters.
    excluded_linters <- strsplit(linters_string, exclude_linter_sep)[[1L]]
    if (!is.null(linter_names)) {
      idxs <- pmatch(excluded_linters, linter_names, duplicates.ok = TRUE)
      matched <- !is.na(idxs)
      if (!all(matched)) {
        bad <- excluded_linters[!matched] # nolint: object_usage_linter. TODO(#2252).
        cli_warn(c(
          x = "Could not find linter{?s} named {.field {bad}} in the list of active linters.",
          i = "Make sure the linter is uniquely identified by the given name or prefix."
        ))
      }
      excluded_linters[matched] <- linter_names[idxs[matched]]
    }
    exclusions <- add_excluded_lines(exclusions, lines, excluded_linters)
  }
  exclusions
}

#' Normalize lint exclusions
#'
#' @param x Exclusion specification
#'  - A character vector of filenames or directories relative to `root`. Interpreted as globs, see [Sys.glob()].
#'  - A named list of integers specifying lines to be excluded per file
#'  - A named list of named lists specifying linters and lines to be excluded for the linters per file.
#' @param normalize_path Should the names of the returned exclusion list be normalized paths?
#'   If `FALSE`, they will be relative to `root`.
#' @param root Base directory for relative filename resolution.
#' @param pattern If non-NULL, only exclude files in excluded directories if they match
#'   `pattern`. Passed to [list.files()] if a directory is excluded.
#'
#' @return A named list of file exclusions.
#'   The names of the list specify the filenames to be excluded.
#'
#' Each file exclusion is a possibly named list containing line numbers to exclude, or the sentinel `Inf` for
#'   completely excluded files. If the an entry is named, the exclusions only take effect for the linter with
#'   the same name.
#'
#' If `normalize_path` is `TRUE`, file names will be normalized relative to `root`.
#'   Otherwise the paths are left as provided (relative to `root` or absolute). That also means
#'   existence is not checked.
#'
#' @keywords internal
normalize_exclusions <- function(x, normalize_path = TRUE,
                                 root = getwd(),
                                 pattern = NULL) {
  if (is.null(x) || length(x) <= 0L) {
    return(list())
  }

  x <- as.list(x)
  unnamed <- !nzchar(names2(x))
  if (any(unnamed)) {
    bad <- vapply(
      seq_along(x),
      function(i) {
        unnamed[i] && (!is.character(x[[i]]) || length(x[[i]]) != 1L)
      },
      logical(1L)
    )

    if (any(bad)) {
      cli_abort(c(
        i = "Full file exclusions must be {.cls character} vectors of length 1.",
        x = "Items at the following indices are not: {.val {which(bad)}}."
      ))
    }
    # Normalize unnamed entries to list(<filename> = list(Inf), ...)
    names(x)[unnamed] <- x[unnamed]
    x[unnamed] <- rep_len(list(list(Inf)), sum(unnamed))
  }

  full_line_exclusions <- !vapply(x, is.list, logical(1L))

  if (any(full_line_exclusions)) {
    # must be integer or numeric vectors
    are_numeric <- vapply(x, is.numeric, logical(1L))
    bad <- full_line_exclusions & !are_numeric

    if (any(bad)) {
      cli_abort(c(
        i = "Full line exclusions must be {.cls numeric} or {.cls integer} vectors.",
        x = "Items at the following indices are not: {.val {which(bad)}}."
      ))
    }

    # Normalize list(<filename> = c(<lines>)) to
    # list(<filename> = list(c(<lines>)))
    x[full_line_exclusions] <- lapply(x[full_line_exclusions], list)
  }

  paths <- names(x)
  rel_path <- !is_absolute_path(paths)
  paths[rel_path] <- file.path(root, paths[rel_path])

  globbed_paths <- lapply(paths, Sys.glob)
  n_files <- lengths(globbed_paths)
  # restore unmatched globs
  if (!normalize_path) {
    empty <- n_files == 0L
    globbed_paths[empty] <- as.list(names(x)[empty])
    n_files[empty] <- 1L
  }
  x <- rep(x, n_files)
  paths <- unlist(globbed_paths)
  names(x) <- paths

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
    x <- x[file.exists(paths)] # remove exclusions for non-existing files
    names(x) <- normalize_path(names(x)) # get full path for remaining files
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
        excluded_lines <- unlist(ex[names2(ex) == linter])
        if (Inf %in% excluded_lines) {
          Inf
        } else {
          excluded_lines
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
