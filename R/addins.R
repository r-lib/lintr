# nocov start
addin_lint <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("'rstudioapi' is required for add-ins.", call. = FALSE)
  }
  filename <- rstudioapi::getSourceEditorContext()
  if (filename$path == "") {
    return("Current source has no path. Please save before continue")
  }

  lint(filename$path)
}

addin_lint_package <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("'rstudioapi' is required for add-ins.", call. = FALSE)
  }
  project <- rstudioapi::getActiveProject()
  if (is.null(project)) {
    message("No project found, passing current directory")
    project_path <- getwd()
  } else {
    project_path <- project
  }

  lint_package(project_path)
}
# nocov end
