#' Use lintr in your project
#'
#' Create a minimal lintr config file as a starting point for customization
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

  pkg_path <- find_package(path)

  if (is.null(pkg_path)) {
    return(invisible(config_file))
  }

  rbuildignore_path <- file.path(pkg_path, ".Rbuildignore")
  rel_path <- substring(
    config_file,
    first = nchar(pkg_path) + 2L,
    last = nchar(config_file)
  )
  escaped_config_path <- rex::rex(start, rel_path, end)

  if (!file.exists(rbuildignore_path)) {
    writeLines(escaped_config_path, rbuildignore_path)
    cli_inform("Added {.val {escaped_config_file}} to {.file {rbuildignore_path}}.")
    return(invisible(config_file))
  }

  ignored <- readLines(rbuildignore_path, warn = FALSE)

  if (escaped_config_path %in% ignored) {
    cli_inform("Configuration file {.val {escaped_config_file}} is already ignored in {.file {rbuildignore_path}}.")
    return(invisible(config_file))
  }

  writeLines(c(ignored, escaped_config_path), rbuildignore_path)
  cli_inform("Added {.val {escaped_config_file}} to {.file {rbuildignore_path}}.")

  invisible(config_file)
}
