in_github_actions <- function() {
  identical(Sys.getenv("GITHUB_ACTIONS"), "true")
}

# Output logging commands for any lints found
github_actions_log_lints <- function(lints) {
  for (x in lints) {
    cat(sprintf("::warning file=%s,line=%s,col=%s::%s\n", x$filename, x$line_number, x$column_number, x$message), sep = "")
  }
}


rstudio_source_markers <- function(lints) {

  # package path will be NULL unless it is a relative path
  package_path <- attr(lints, "path")

  # generate the markers
  markers <- lapply(lints, function(x) {
    filename <- if (!is.null(package_path)) {
      file.path(package_path, x$filename)
    } else {
      x$filename
    }

    marker <- list()
    marker$type <- x$type
    marker$file <- filename
    marker$line <- x$line_number
    marker$column <- x$column_number
    marker$message <- x$message
    marker
  })

  # request source markers
  rstudioapi::callFun("sourceMarkers",
                      name = "lintr",
                      markers = markers,
                      basePath = package_path,
                      autoSelect = "first")
}
