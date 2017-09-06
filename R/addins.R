addin_lint <- function() {
  filename <- rstudioapi::getActiveDocumentContext()
  if (filename$path == "") {
    return("Current source has no path. Please save before continue")
  }
  lintr::lint(filename$path)
}

addin_lint_package <- function() {
  project <- rstudioapi::getAciveProject()
  project_path <- if (is.null(project)) getwd() else project

  if (is.null(project)) message("No project found, passing current directory")

  lintr::lint_package(project_path)
}
