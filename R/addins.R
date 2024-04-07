# nocov start
addin_lint <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    cli_abort("{.pkg rstudioapi} is required for add-ins.")
  }
  filename <- rstudioapi::getSourceEditorContext()
  if (filename$path == "") {
    cli_warn("Current source has no path. Please save before continuing.")
    return(list())
  }

  lint(filename$path)
}

addin_lint_package <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    cli_abort("{.pkg rstudioapi} is required for add-ins.")
  }
  project <- rstudioapi::getActiveProject()
  if (is.null(project)) {
    cli_inform("No configuration found. Using default linters.")
    project_path <- getwd()
  } else {
    project_path <- project
  }

  lint_package(project_path)
}
# nocov end
