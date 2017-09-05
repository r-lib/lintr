addin_lint <- function () {
  filename <- rstudioapi::getActiveDocumentContext()
  lintr::lint(filename$path)
}

addin_lint_package <- function () {
  lintr::lint_package()
}
