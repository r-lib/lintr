`%||%` <- function(x, y) {
  if (!is.null(x)) {
    x
  } else {
    y
  }
}

`%==%` <- function(x, y) {
  identical(x, y)
}
`%!=%` <- function(x, y) {
  !identical(x, y)
}

expect_lint <- function(content, checks, ...) {

  results <- expectation_lint(content, checks, ...)

  reporter <- get_reporter()

  # flatten list if a list of lists
  if (is.list(results) &&
    is.list(results[[1]]) &&
    !is.expectation(results[[1]])) {

    results <- unlist(recursive = FALSE, results)
  }

  if (is.expectation(results)) {
    reporter$add_result(results)
  }
  else {
    lapply(results, reporter$add_result)
  }

  invisible(results)
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

expectation_lint <- function(content, checks, ...) {

  filename <- tempfile()
  on.exit(unlink(filename))
  cat(file=filename, content, sep="\n")

  lints <- lint(filename, list(...))

  linter_names <- substitute(alist(...))[-1]

  if (is.null(checks)) {
    return(expectation(length(lints) %==% 0L,
        paste0(paste(collapse=", ", linter_names),
          " returned ", print(lints),
          " lints when it was expected to return none!"),
        paste0(paste(collapse=", ", linter_names),
          " returned 0 lints as expected.")))
  }

  if (!is.list(checks)) {
    checks <- list(checks)
  }
  checks[] <- lapply(checks, fix_names, "message")

  if (length(lints) != length(checks)) {
    return(expectation(FALSE,
        paste0(paste(collapse=", ", linter_names),
          " did not return ", length(checks),
          " lints as expected from content:", content, str(lints))))
  }

  itr <- 0L
  mapply(function(lint, check) {
    itr <- itr + 1L
    lapply(names(check), function(field) {
      value <- lint[[field]]
      check <- check[[field]]
      expectation(re_matches(value, check),
        sprintf("lint: %d %s: %s did not match: %s",
          itr,
          field,
          value,
          check
        ),
        sprintf("lint: %d %s: %s matched: %s",
          itr,
          field,
          value,
          check
        )
      )
    })
  },
  lints,
  checks)
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

ids_with_token <- function(source_file, exact = NULL, re = NULL) {
  if (!is.null(exact)) {
    source_file$parsed_content[source_file$parsed_content$token == exact, "id"]
  } else if (!is.null(re)) {
    source_file$parsed_content[
      re_matches(source_file$parsed_content$token, re),
      "id"]
  } else {
    stop("Must specify exact or re")
  }
}

# The following functions is from dplyr
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}
