#' Use lintr in your project
#'
#' Create a minimal lintr config file as a starting point for customization and add it to the .Rbuildignore
#'
#' @param path Path to project root, where a `.lintr` file should be created.
#' If the `.lintr` file already exists, an error will be thrown.
#' @param type What kind of configuration to create?
#'
#'  * `tidyverse` creates a minimal lintr config, based on the default linters ([linters_with_defaults()]).
#'    These are suitable for following [the tidyverse style guide](https://style.tidyverse.org/).
#'  * `full` creates a lintr config using all available linters via [all_linters()].
#'
#' @return Path to the generated configuration, invisibly.
#'
#' @export
#' @seealso `vignette("lintr")` for detailed introduction to using and configuring lintr.
#' @examples
#' if (FALSE) {
#'   # use the default set of linters
#'   lintr::use_lintr()
#'   # or try all linters
#'   lintr::use_lintr(type = "full")
#'
#'   # then
#'   lintr::lint_dir()
#' }
use_lintr <- function(path = ".", type = c("tidyverse", "full")) {
  config_file <- normalize_path(file.path(path, lintr_option("linter_file")), mustWork = FALSE)
  if (file.exists(config_file)) {
    cli_abort("Found an existing configuration file at {.file {config_file}}.")
  }
  type <- match.arg(type)
  the_config <- switch(type,
    tidyverse = list(
      linters = 'linters_with_defaults() # see vignette("lintr")',
      encoding = '"UTF-8"'
    ),
    full = list(
      linters = 'all_linters(packages = "lintr") # see vignette("lintr")',
      encoding = '"UTF-8"',
      exclusions = 'list("renv", "packrat") # see ?lintr::exclude'
    )
  )
  write.dcf(the_config, config_file, width = Inf)

  if (file.exists(file.path(path, "DESCRIPTION"))) {
    # Some OS can only normalize a path if the associated file or folder exists, so the path needs to be re-normalized
    tryCatch({
      pkg_path <- normalizePath(path, mustWork = TRUE, winslash = "/")
      config_file <- normalizePath(file.path(path, lintr_option("linter_file")), mustWork = TRUE, winslash = "/")
    }, error = function(e) {
      stop("No entry could be added to the .Rbuildignore.", call. = FALSE)
    })
    # Check if config_file is in package i.e. lintr_option("linter_file") != "../.lintr"
    if (startsWith(config_file, prefix = pkg_path)) {
      # Skip a extra character for the leading `/`
      rel_path <- substring(config_file, first = nchar(pkg_path) + 2L, last = nchar(config_file))
      ignore_path <- file.path(pkg_path, ".Rbuildignore")
      if (!file.exists(ignore_path)) file.create(ignore_path)
      # Follow the same procedure as base R to see if the file is already ignored
      ignore <- tryCatch({
        trimws(readLines(ignore_path))
      }, warning = function(e) {
        cat(file = ignore_path, "\n", append = TRUE)
        trimws(readLines(ignore_path))
      })
      ignore <- ignore[nzchar(ignore)]
      already_ignored <-
        any(vapply(ignore, FUN = grepl, x = rel_path, perl = TRUE, ignore.case = TRUE, FUN.VALUE = logical(1L)))
      if (!already_ignored) {
        cat(file = ignore_path, rex::rex(start, rel_path, end), sep = "\n", append = TRUE)
        message("Adding ", rel_path, " to .Rbuildignore")
      }
    }
  }

  invisible(config_file)
}
