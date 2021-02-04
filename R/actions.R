in_github_actions <- function() {
  identical(Sys.getenv("GITHUB_ACTIONS"), "true")
}

# Output logging commands for any lints found
github_actions_log_lints <- function(lints) {
  for (x in lints) {
    file_line_col <- sprintf(
      "file=%s,line=%s,col=%s", x$filename, x$line_number, x$column_number
    )
    cat(sprintf(
      "::warning %s::%s,%s\n",
      file_line_col, file_line_col, x$message
    ), sep = "")
  }
}
