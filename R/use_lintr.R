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
#'  * `full` creates a lintr config using all available linters via [linters_with_tags()].
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
use_lintr <- function(path = ".", type = c("tidyverse", "full")) {
  config_file <- normalizePath(file.path(path, getOption("lintr.linter_file")), mustWork = FALSE)
  if (file.exists(config_file)) {
    stop("Found an existing configuration file at '", config_file, "'.")
  }
  type <- match.arg(type)
  the_config <- switch(
    type,
    tidyverse = list(
      linters = 'linters_with_defaults() # see vignette("using_lintr")',
      encoding = '"UTF-8"'
    ),
    full = list(
      linters = 'linters_with_tags(tags = NULL, packages = "lintr") # see vignette("using_lintr")',
      encoding = '"UTF-8"',
      exclusions = 'list("renv", "packrat") # see ?lintr::exclude'
    )
  )
  write.dcf(the_config, config_file)
  invisible(config_file)
}
