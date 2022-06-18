#' Get Linter metadata from a package
#'
#' `available_linters()` obtains a tagged list of all Linters available in a package.
#'
#' @param packages A character vector of packages to search for linters.
#' @param tags Optional character vector of tags to search. Only linters with at least one matching tag will be
#' returned. If `tags` is `NULL`, all linters will be returned.
#' @param exclude_tags Tags to exclude from the results. Linters with at least one matching tag will not be returned.
#' If `except_tags` is `NULL`, no linters will be excluded.
#'
#' @section Package Authors:
#'
#' To implement `available_linters()` for your package, include a file `inst/lintr/linters.csv` in your
#' package.
#' The CSV file must contain the columns 'linter' and 'tags', and be UTF-8 encoded.
#' Additional columns will be silently ignored if present and the columns are identified by name.
#' Each row describes a linter by
#'
#'  1. its function name (e.g. `"assignment_linter"`) in the column 'linter'.
#'  2. space-separated tags associated with the linter (e.g. `"style consistency default"`) in the column 'tags'.
#'
#' Tags should be snake_case.
#'
#' See `available_tags("lintr")` to find out what tags are already used by lintr.
#'
#' @return
#' `available_linters` returns a data frame with columns 'linter', 'package' and 'tags':
#'
#' \describe{
#' \item{linter}{A character column naming the function associated with the linter.}
#' \item{package}{A character column containing the name of the package providing the linter.}
#' \item{tags}{A list column containing tags associated with the linter.}
#' }
#'
#' @examples
#' lintr_linters <- available_linters()
#'
#' # If the package doesn't exist or isn't installed, an empty data frame will be returned
#' available_linters("does-not-exist")
#'
#' lintr_linters2 <- available_linters(c("lintr", "does-not-exist"))
#' identical(lintr_linters, lintr_linters2)
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
available_linters <- function(packages = "lintr", tags = NULL, exclude_tags = "deprecated") {
  if (!is.character(packages)) {
    stop("`packages` must be a character vector.")
  }
  if (!is.null(tags) && !is.character(tags)) {
    stop("`tags` must be a character vector.")
  }
  if (!is.null(exclude_tags) && !is.character(exclude_tags)) {
    stop("`exclude_tags` must be a character vector.")
  }

  # Handle multiple packages
  if (length(packages) > 1L) {
    return(do.call(rbind, lapply(packages, available_linters, tags = tags, exclude_tags = exclude_tags)))
  }

  csv_file <- system.file("lintr", "linters.csv", package = packages)

  if (!file.exists(csv_file)) {
    return(empty_linters())
  }
  available <- utils::read.csv(csv_file, encoding = "UTF-8", as.is = TRUE)

  if (!validate_linter_db(available, packages)) {
    return(empty_linters())
  }

  build_available_linters(available, packages, tags, exclude_tags)
}

build_available_linters <- function(available, package, tags, exclude_tags) {
  available_df <- data.frame(
    linter = available[["linter"]],
    package,
    stringsAsFactors = FALSE
  )
  available_df$tags <- strsplit(available[["tags"]], split = " ", fixed = TRUE)
  if (!is.null(tags)) {
    matches_tags <- vapply(available_df$tags, function(linter_tags) any(linter_tags %in% tags), logical(1L))
    available_df <- available_df[matches_tags, ]
  }
  if (!is.null(exclude_tags)) {
    matches_exclude <- vapply(available_df$tags, function(linter_tags) any(linter_tags %in% exclude_tags), logical(1L))
    available_df <- available_df[!matches_exclude, ]
  }
  available_df
}

#' Make sure we always return a valid data frame
#'
#' `data.frame` constructors don't handle zero-row list-columns properly, so supply `tags` afterwards.
#' @noRd
empty_linters <- function() {
  empty_df <- data.frame(linter = character(), package = character(), stringsAsFactors = FALSE)
  empty_df$tags <- list()
  empty_df
}

validate_linter_db <- function(available, package) {
  # Check that the csv file contains two character columns, named 'linter' and 'tags'.
  # Otherwise, fallback to an empty data frame.
  if (!all(c("linter", "tags") %in% colnames(available))) {
    warning(
      "`linters.csv` must contain the columns 'linter' and 'tags'.\nPackage '",
      package, "' is missing ",
      paste0("'", setdiff(c("linter", "tags"), names(available)), "'", collapse = " and "),
      "."
    )
    return(FALSE)
  }
  nrow(available) > 0L
}

#' @rdname available_linters
#'
#' @description
#' `available_tags()` searches for available tags.
#'
#' @return `available_tags` returns a character vector of linter tags used by the packages.
#' @export
#' @examples
#' available_tags()
available_tags <- function(packages = "lintr") {
  platform_independent_sort(unique(unlist(available_linters(packages = packages, exclude_tags = NULL)[["tags"]])))
}

#' Generate Rd fragment for the Tags section of a linter
#'
#' @param linter_name Name of the linter to generate Rd code for.
#'
#' @noRd
rd_tags <- function(linter_name) {
  linters <- available_linters(exclude_tags = NULL)
  tags <- platform_independent_sort(linters[["tags"]][[match(linter_name, linters[["linter"]])]])
  if (length(tags) == 0L) {
    stop("tags are required, but found none for ", linter_name)
  }

  c(
    "\\section{Tags}{",
    paste0("\\link[=", tags, "_linters]{", tags, "}", collapse = ", "),
    "}"
  )
}

#' Generate Rd fragment for the Linters section of a tag
#'
#' @param tag_name Name of the tag to generate Rd code for.
#'
#' @noRd
rd_linters <- function(tag_name) {
  linters <- available_linters(tags = tag_name, exclude_tags = NULL)
  tagged <- platform_independent_sort(linters[["linter"]])
  if (length(tagged) == 0L) {
    stop("No linters found associated with tag ", tag_name)
  }

  c(
    "\\section{Linters}{",
    paste0("The following linters are tagged with '", tag_name, "':"),
    "\\itemize{",
    paste0("\\item{\\code{\\link{", tagged, "}}}"),
    "}", # itemize
    "}"  # section
  )
}

#' Generate Rd fragment for the main help page, listing all tags
#'
#' @noRd
rd_taglist <- function() {
  linters <- available_linters(exclude_tags = NULL)

  tag_table <- table(unlist(linters[["tags"]]))
  tags <- platform_independent_sort(unique(unlist(linters[["tags"]])))
  # re-order
  tag_table <- tag_table[tags]

  c(
    "\\section{Tags}{",
    "The following tags exist:",
    "\\itemize{",
    vapply(tags, function(tag) {
      paste0("\\item{\\link[=", tag, "_linters]{", tag, "} (", tag_table[[tag]], " linters)}")
    }, character(1L)),
    "}", # itemize
    "}"  # section
  )
}

#' Generate Rd fragment for the main help page, listing all linters
#'
#' @noRd
rd_linterlist <- function() {
  linters <- available_linters(exclude_tags = NULL)
  linter_names <- platform_independent_sort(linters[["linter"]])

  c(
    "\\section{Linters}{",
    "The following linters exist:",
    "\\itemize{",
    vapply(linter_names, function(linter_name) {
      tags <- platform_independent_sort(linters[["tags"]][[match(linter_name, linters[["linter"]])]])
      paste0("\\item{\\code{\\link{", linter_name, "}} (tags: ", toString(tags), ")}")
    }, character(1L)),
    "}", # itemize
    "}" # section
  )
}
