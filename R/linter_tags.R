#' Get Linter metadata from a package
#'
#' `available_linters()` obtains a tagged list of all Linters available in a package.
#'
#' @param packages A character vector of packages to search for linters.
#' @param tags Optional character vector of tags to search. Only linters with at least one matching tag will be
#'   returned. If `tags` is `NULL`, all linters will be returned. See `available_tags("lintr")` to find out what
#'   tags are already used by lintr.
#' @param exclude_tags Tags to exclude from the results. Linters with at least one matching tag will not be returned.
#'   If `exclude_tags` is `NULL`, no linters will be excluded. Note that `tags` takes priority, meaning that any
#'   tag found in both `tags` and `exclude_tags` will be included, not excluded. Note that linters with tag `"defunct"`
#'   (which do not work and can no longer be run) cannot be queried directly. See [lintr-deprecated] instead.
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
#' @seealso
#'  - [linters] for a complete list of linters available in lintr.
#'  - [available_tags()] to retrieve the set of valid tags.
#' @export
available_linters <- function(packages = "lintr", tags = NULL, exclude_tags = "deprecated") {
  if (!is.character(packages)) {
    cli_abort("{.arg packages} must be a {.cls character} vector.")
  }
  if (!is.null(tags) && !is.character(tags)) {
    cli_abort("{.arg tags} must be a {.cls character} vector.")
  }
  if (!is.null(exclude_tags) && !is.character(exclude_tags)) {
    cli_abort("{.arg exclude_tags} must be a {.cls character} vector.")
  }

  # any tags specified explicitly will not be excluded (#1959)
  # never include defunct linters, which don't work / error on instantiation (#2284).
  exclude_tags <- unique(c(setdiff(exclude_tags, tags), "defunct"))

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
  available_df <- data.frame(linter = available[["linter"]], package)
  available_df$tags <- strsplit(available[["tags"]], split = " ", fixed = TRUE)
  if (!is.null(tags)) {
    matches_tags <- vapply(available_df$tags, function(linter_tags) any(linter_tags %in% tags), logical(1L))
    available_df <- available_df[matches_tags, ]
  }
  if (!is.null(exclude_tags)) {
    matches_exclude <- vapply(available_df$tags, function(linter_tags) any(linter_tags %in% exclude_tags), logical(1L))
    available_df <- available_df[!matches_exclude, ]
  }

  # Due to removal of deprecated linters in the returned data frame, there can be gaps in row numbers.
  # To avoid this inconsistency, regenerate row names.
  rownames(available_df) <- NULL
  available_df
}

#' Make sure we always return a valid data frame
#'
#' `data.frame` constructors don't handle zero-row list-columns properly, so supply `tags` afterwards.
#' @noRd
empty_linters <- function() {
  empty_df <- data.frame(linter = character(), package = character())
  empty_df$tags <- list()
  empty_df
}

validate_linter_db <- function(available, package) {
  # Check that the csv file contains two character columns, named 'linter' and 'tags'.
  # Otherwise, fallback to an empty data frame.
  if (!all(c("linter", "tags") %in% colnames(available))) {
    cli_warn(c(
      i = "{.file linters.csv} must contain the columns {.val {c('linter', 'tags')}}.",
      x = "Package {.pkg package} is missing {.str {setdiff(c('linter', 'tags'), names(available))}}."
    ))
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

# nocov start

#' Generate Rd fragment for the Tags section of a linter
#'
#' @param linter_name Name of the linter to generate Rd code for.
#'
#' @noRd
rd_tags <- function(linter_name, tags = NULL) {
  if (is.null(tags)) {
    linters <- available_linters(exclude_tags = NULL)
    tags <- platform_independent_sort(linters[["tags"]][[match(linter_name, linters[["linter"]])]])
    if (length(tags) == 0L) {
      cli_abort("Tags are required, but found none for {.fn {linter_name}}.")
    }
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
  linters <- available_linters(tags = tag_name)
  tagged <- platform_independent_sort(linters[["linter"]])
  if (length(tagged) == 0L) {
    section_body <- paste0("There are not currently any linters tagged with '", tag_name, "'.")
  } else {
    section_body <- c(
      paste0("The following linters are tagged with '", tag_name, "':"),
      "\\itemize{",
      paste0("  \\item{\\code{\\link{", tagged, "}}}"),
      "}"
    )
  }

  c("\\section{Linters}{", section_body, "}")
}

#' Generate Rd fragment for the main help page, listing all tags
#'
#' @noRd
rd_taglist <- function() {
  linters <- available_linters(exclude_tags = NULL)
  # don't count tags on deprecated linters to the counts of other tags
  linters$tags <- lapply(linters$tags, function(x) if ("deprecated" %in% x) "deprecated" else x)

  tag_table <- table(unlist(linters[["tags"]]))
  tags <- platform_independent_sort(names(tag_table))
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

# nocov end
