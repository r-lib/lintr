#' Use lintr in your project
#'
#' Create a minimal lintr config file as a starting point for customization
#'
#' @param path Path to project root, where a `.lintr` file should be created.
#' If it already exists, an error will be thrown.
#'
#' @return Path to the generated configuration, invisibly.
#'
#' @export
#' @examples
#' \dontrun{
#' # use the default set of linters
#' lintr::use_lintr()
#' # or try all linters
#' lintr::use_lintr(type = "full")
#'
#' # then
#' lintr::lint_dir()
#' }
use_lintr <- function(path = ".", type = c("minimal", "full")) {
  config_file <- normalizePath(file.path(path, getOption("lintr.linter_file")))
  if (file.exists(config_file)) {
    stop("File '", config_file, "' already exists.")
  }
  type <- match.arg(type)
  the_config <- switch(
    type,
    minimal = list(
      linters = "linters_with_defaults()",
      encoding = "UTF-8"
    ),
    full = list(
      linters = "linters_with_tags(tags = NULL, packages = \"lintr\")",
      encoding = "UTF-8",
      exclusions = "list(\"renv\", \"packrat\") # see ?exclude"
    )
  )
  write.dcf(the_config, config_file)
  invisible(config_file)
}
