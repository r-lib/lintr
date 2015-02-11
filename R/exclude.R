exclude <- function(lints, exclusions = NULL, ...) {
  df <- as.data.frame(lints)
  
  filenames <- unique(df$filename)
  
  full_filenames <-
    if (!is.null(attr(lints, "path"))) {
      file.path(attr(lints, "path"), filenames)
    } else {
      filenames
    }
  
  source_exclusions <- lapply(full_filenames, parse_exclusions, ...)
  names(source_exclusions) <- filenames
  
  excl <- normalize_exclusions(c(source_exclusions, exclusions))
  
  to_exclude <- vapply(seq_len(NROW(df)),
    function(i) {
      file <- df$filename[i]
      file %in% names(excl) &&
        excl[[file]] == Inf |
        df$line_number[i] %in% excl[[file]]
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
      stop(file, " has ", length(starts), " starts but only ", length(ends), " ends!")
    }
    
    for(i in seq_along(starts)) {
      exclusions <- c(exclusions, seq(starts[i], ends[i]))
    }
  }
  
  sort(unique(c(exclusions, which(rex::re_matches(lines, exclude)))))
}

normalize_exclusions <- function(x) {
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
  
  remove_line_duplicates(
    remove_file_duplicates(
      remove_empty(x)
    )
  )
}

remove_file_duplicates <- function(x) {
  unique_names <- unique(names(x))
  if (length(unique_names) < length(names(x))) { # must be duplicate files
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