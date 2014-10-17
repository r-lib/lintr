.__LINTERS__. <- new.env(parent = emptyenv())

#' Add a Linter
#'
#' Add a linter, to be used in subsequent calls to \code{\link{lint}}.
#'
#' @param name The name of the linter, as a string.
#' @param linter A \code{\link{linter}}.
#' @export
add_linter <- function(name, linter) {
  assign(name, linter, envir = .__LINTERS__.)
}


#' Create a Linter
#'
#' Generate a linter, which can identify errors or problematic regions in a
#' project.
#'
#' @param apply Function that, given the content of a file, returns the indices
#'   at which problems were found.
#' @param takes Function that, given a set of paths, returns the subset of
#'   paths that this linter uses.
#' @param message Function that, given content and lines, returns an
#'   informative message for the user. Typically generated with
#'   \code{\link{make_linter_message}}.
#' @param suggestion String giving a prescribed fix for the linted problem.
#' @export
linter <- function(apply, takes, message, suggestion) {
  result <- list(
    apply = apply,
    takes = takes,
    message = message,
    suggestion = suggestion
  )
  class(result) <- "linter"
  result
}

get_linter_applicable_files <- function(linter, files) {
  result <- linter$takes(files)
  if (is.numeric(result) || is.logical(result)) {
    files[result]
  } else {
    result
  }
}

apply_linter <- function(linter, ...) {
  result <- linter$apply(...)
  if (is.logical(result)) {
    return(which(result))
  } else {
    return(as.numeric(result))
  }
}

#' Lint a Project
#'
#' Takes the set of active linters (see \code{\link{add_linter}}), and applies
#' them to all files within a project.
#'
#' @param project Path to a project directory.
#' @export
lint <- function(files) {

  linters <- mget(objects(.__LINTERS__.), envir = .__LINTERS__.)

  # Identify all files that will be read in by one or more linters
  project_files_to_lint <- Reduce(union, lapply(linters, function(linter) {
    get_linter_applicable_files(linter, project_files)
  }))

  # Read in the files
  # TODO: perform this task more lazily?
  project_content <- suppressWarnings(lapply(project_files_to_lint, readLines))
  names(project_content) <- project_files_to_lint
  lint_results <- vector("list", length(linters))
  names(lint_results) <- names(linters)

  ## Apply each linter
  for (i in seq_along(linters)) {
    linter <- linters[[i]]
    applicable_files <- get_linter_applicable_files(linter, project_files_to_lint)
    lint_indices <- vector("list", length(applicable_files))
    names(lint_indices) <- applicable_files

    ## Apply linter to each file
    for (j in seq_along(applicable_files)) {
      file <- applicable_files[[j]]
      lint_indices[[j]] <- apply_linter(linter,
                                      project_content[[file]],
                                      project = project,
                                      path = file)
    }

    ## Get the messages associated with each lint
    lint_messages <- enumerate(lint_indices, function(x, i) {
      if (length(x)) {
        message <- linter$message
        if (is.function(message)) {
          linter$message(project_content[[names(lint_indices)[i]]], x)
        } else {
          make_linter_message(message, project_content[[names(lint_indices)[i]]], x)
        }
      } else {
        character()
      }
    })

    ## Assign the result
    lint_results[[i]] <- list(
      files = applicable_files,
      indices = lint_indices,
      message = lint_messages,
      suggestion = linter$suggestion
    )

  }

  ## Get all of the linted files, and transform the results into a by-file format
  linted_files <- Reduce(union, lapply(lint_results, function(x) {
    names(x$indices)
  }))

  lint_fields <- c("indices", "message")
  file_results <- lapply(linted_files, function(file) {
    result <- lapply(lint_results, function(result) {
      sub_result <- lapply(lint_fields, function(field) {
        result[[field]][[file]]
      })
      names(sub_result) <- lint_fields
      sub_result$suggestion <- result$suggestion
      sub_result$file <- file
      class(sub_result) <- "lint"
      sub_result
    })
    class(result) <- "lintList"
    result
  })
  names(file_results) <- linted_files
  class(file_results) <- "linterResults"
  invisible(file_results)
}

print_lint_header <- function(x) {
  if (!length(x$message)) return(invisible(NULL))
  dash_sep <- paste(rep("-", nchar(x$file)), collapse = "")
  header <- paste(dash_sep, "\n",
                  x$file, "\n",
                  dash_sep, "\n", sep = "")
  message(paste(header, collapse = "\n"), append_LF = FALSE)
  invisible(x)
}

print_lint_body <- function(x, ...) {
  message(paste(x$message, collapse = "\n"), append_LF = FALSE)
  invisible(x)
}

print_lint_footer <- function(x, ...) {
  message(paste(collect_suggestions(x), collapse = "\n"))
  invisible(x)
}

print_linter_results <- function(x, ...) {
  lapply(x, print_lint_list, ...)
  print_lint_footer(x)
  invisible(x)
}

print_lint_list <- function(x, ...) {
  print_lint_header(x[[1]])
  lapply(x, print_lint_body, ...)
  invisible(x)
}

print_lint <- function(x, ...) {
  print_lint_header(x)
  print_lint_body(x, ...)
  invisible(x)
}

collect_suggestions <- function(file_results) {
  suggestions <- lapply(file_results, function(file_result) {
    unlist(lapply(file_result, function(lint_info) {
      if (length(lint_info$indices) > 0) {
        paste(as.character(lint_info$suggestion), collapse = "\n")
      }
    }))
  })
  Reduce(intersect, suggestions)
}
