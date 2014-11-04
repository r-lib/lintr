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

ids_with_token <- function(source_file, value, fun = `==`) {
  if (source_file$parsed_content$col1 %==% integer(0)) {
    return(NULL)
  }
  as.character(
    source_file$parsed_content[fun(source_file$parsed_content$token, value), "id"]
    )
}

# The following functions is from dplyr
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

recursive_ls <- function(env) {
  if (parent.env(env) %!=% emptyenv()) {
    c(ls(envir=env), recursive_ls(parent.env(env)))
  }
  else {
    ls(envir=env)
  }
}
