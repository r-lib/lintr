# nocov start
addin_lint <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    cli_abort("{.pkg rstudioapi} is required for add-ins.")
  }
  filename <- rstudioapi::getSourceEditorContext()
  if (filename$path == "") {
    return("Current source has no path. Please save before continuing.")
  }

  config_file <- (get("find_config", asNamespace("lintr")))(filename$path)
  if (length(config_file) == 0L) {
    config_linters <- NULL
  } else {
    config <- read.dcf(config_file, all = TRUE)
    config_linters <- config[["linters"]]
  }
  linters <- if (length(config_linters) == 0L) {
    cli_inform("No configuration found. Using default linters.")
    default_linters
  } else {
    eval(parse(text = config_linters))
  }

  lintr::lint(filename$path, linters = linters)
}

addin_lint_package <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    cli_abort("{.pkg rstudioapi} is required for add-ins.")
  }
  project <- rstudioapi::getActiveProject()
  project_path <- if (is.null(project)) getwd() else project

  if (is.null(project)) {
    cli_inform("No project found, passing current directory")
  }

  lintr::lint_package(project_path)
}
# nocov end
