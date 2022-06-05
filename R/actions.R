in_github_actions <- function() {
  identical(Sys.getenv("GITHUB_ACTIONS"), "true")
}

# Output logging commands for any lints found
github_actions_log_lints <- function(lints, project_dir = "") {
  for (x in lints) {
    if (nzchar(project_dir)) {
      x$filename <- file.path(project_dir, x$filename)
    }
    file_line_col <- sprintf(
      "file=%s,line=%s,col=%s", x$filename, x$line_number, x$column_number
    )
    cat(sprintf(
      "::warning %s::%s,[%s] %s\n",
      file_line_col, file_line_col, x$linter, x$message
    ), sep = "")
  }
}
