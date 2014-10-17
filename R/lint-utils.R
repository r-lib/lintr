strip_comments <- function(content) {
  gsub("#.*", "", content, perl = TRUE)
}

has_absolute_paths <- function(content) {
  absolute_path_types <- list(
    rex(letter, ":", some_of("/", "\\")), ## windows-style absolute paths
    rex("\\", "\\"), ## windows UNC paths
    rex("/", except_some_of("/")),## unix-style absolute paths
    rex("~"),
    NULL
  )

  regex <- vapply(absolute_path_types,
    function(x) {
      c(
        rex(quote, any_spaces, x),
        rex("[", except_any_of("]"), "]", "(", any_spaces, x)
        )
    },
    character(2),
    USE.NAMES=FALSE)

  results <- as.logical(Reduce(any, lapply(regex, function(rex) {
    grepl(rex, content, perl = TRUE)
  })))
}

no_match <- function(x) {
  identical(attr(x, "match.length"), -1L)
}

bad_relative_paths <- function(content, project, path) {

  ## Figure out how deeply the path of the file is nested
  ## (it is relative to the project root)
  slash_matches <- gregexpr("/", path)
  nest_level <- if (no_match(slash_matches)) 0 else length(slash_matches[[1]])

  ## Identify occurrences of "../"
  regex_results <- gregexpr("../", content, fixed = TRUE)

  ## Figure out sequential runs of `../`
  runs <- lapply(regex_results, function(x) {
    if (no_match(x)) return(NULL)
    rle <- rle(as.integer(x) - seq(0, by = 3, length.out = length(x)))
    rle$lengths
  })

  bad_paths <- vapply(runs, function(x) {
    any(x > nest_level)
  }, logical(1))

  bad_paths
}

enumerate <- function(X, FUN, ...) {
  FUN <- match.fun(FUN)
  result <- vector("list", length(X))
  for (i in seq_along(X)) {
    result[[i]] <- FUN(X[[i]], i, ...)
  }
  names(result) <- names(X)
  result
}

#' Construct a Linter Message
#'
#' Pretty-prints a linter message. Primarily used as a helper
#' for constructing linter messages with \code{\link{linter}}.
#'
#' @param header A header message describing the linter.
#' @param content The content of the file that was linted.
#' @param lines The line numbers from \code{content} that contain lint.
make_linter_message <- function(header, content, lines) {

  c( # test
    paste0(header, ":"),
    paste(lines, ": ", content[lines], sep = ""),
    "\n"
  )
}

has_lint <- function(x) {
  any(unlist(lapply(x, function(x) {
    lapply(x, function(x) {
      length(x$indices) > 0
    })
  })))
}

is_r_code_file <- function(path) {
  grepl("\\.[rR]$|\\.[rR]md$|\\.[rR]nw$", path)
}
