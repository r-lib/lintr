#' Get Linter metadata from a package
#'
#' Obtain a tagged list of all Linters available in a package.
#'
#' @param packages A character vector of packages to search for linters.
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
#' @return
#' A data frame with columns 'linter', 'package' and 'tags':
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
available_linters <- function(packages = "lintr") {
  if (!is.character(packages)) {
    stop("`packages` must be a character vector.")
  }

  # Handle multiple packages
  if (length(packages) > 1L) {
    return(do.call(rbind, lapply(packages, available_linters)))
  }

  csv_file <- system.file("lintr", "linters.csv", package = packages)

  # Make sure we always return a valid data frame
  # data.frame(...) and as.data.frame(...) don't handle zero-row list-columns properly, so we need to manually create
  # the structure.
  empty_linters <- data.frame(linter = character(), package = character(), stringsAsFactors = FALSE)
  empty_linters$tags <- list()

  if (!file.exists(csv_file)) {
    return(empty_linters)
  }
  available <- utils::read.csv(csv_file, encoding = "UTF-8", as.is = TRUE)

  # Check that the csv file contains two character columns, named 'linter' and 'tags'.
  # Otherwise, fallback to an empty data frame.
  if (!all(c("linter", "tags") %in% colnames(available))) {
    warning(
      "`linters.csv` must contain the columns 'linter' and 'tags'.\nPackage '",
      packages, "' is missing ",
      paste0("'", setdiff(c("linter", "tags"), colnames(available)), "'", collapse = " and "),
      "."
    )
    return(empty_linters)
  } else if (nrow(available) == 0L) {
    # Circumvent empty list problem in data.frame(...)
    return(empty_linters)
  }

  res <- data.frame(
    linter = available[["linter"]],
    package = rep_len(packages, nrow(available)),
    stringsAsFactors = FALSE
  )
  res$tags <- strsplit(available[["tags"]], split = " ", fixed = TRUE)
  res
}

#' Generate Rd fragment for the Tags section of a linter
#'
#' @param linter_name Name of the linter to generate Rd code for.
#'
#' @noRd
rd_tags <- function(linter_name) {
  linters <- available_linters()
  tags <- platform_independent_sort(linters[["tags"]][[match(linter_name, linters[["linter"]])]])

  c(
    "\\section{Tags}{",
    if (length(tags)) {
      paste0("\\link[=", tags, "_linters]{", tags, "}", collapse = ", ")
    } else {
      "No tags are given."
    },
    "}"
  )
}

#' Generate Rd fragment for the Linters section of a tag
#'
#' @param tag_name Name of the tag to generate Rd code for.
#'
#' @noRd
rd_linters <- function(tag_name) {
  linters <- available_linters()
  tagged <- platform_independent_sort(linters[["linter"]][vapply(
    linters[["tags"]],
    function(tag_list) tag_name %in% tag_list,
    logical(1L)
  )])

  c(
    "\\section{Linters}{",
    if (length(tagged)) {
      c(
        paste0("The following linters are tagged with '", tag_name, "':"),
        "\\itemize{",
        paste0("\\item{\\code{\\link{", tagged, "}}}"),
        "}"
      )
    } else {
      paste0("No linters are tagged with '", tag_name, "'.")
    },
    "}"
  )
}

#' Generate Rd fragment for the main help page, listing all tags
#'
#' @noRd
rd_taglist <- function() {
  linters <- available_linters()
  tags <- platform_independent_sort(unique(unlist(linters[["tags"]])))

  c(
    "\\section{Tags}{",
    "The following tags exist:",
    "\\itemize{",
    vapply(tags, function(tag) {
      n_linters <- sum(vapply(linters[["tags"]], function(tags) tag %in% tags, logical(1L)))
      paste0("\\item{\\link[=", tag, "_linters]{", tag, "} (", n_linters, " linters)}")
    }, character(1L)),
    "}", # itemize
    "}" # section
  )
}

#' Generate Rd fragment for the main help page, listing all linters
#'
#' @noRd
rd_linterlist <- function() {
  linters <- available_linters()
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

platform_independent_sort <- function(x) {
  # see issue #923 -- some locales ignore _ when running sort(), others don't.
  #   we want to consistently treat "_" < "n" = "N"
  x[order(tolower(gsub("_", "0", x, fixed = TRUE)))]
}
