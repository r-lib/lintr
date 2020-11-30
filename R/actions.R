in_github_actions <- function() {
  identical(Sys.getenv("GITHUB_ACTIONS"), "true")
}

# Output logging commands for any lints found
github_actions_log_lints <- function(lints) {
  for (x in lints) {
    cat(
      sprintf("::warning file=%s,line=%s,col=%s::%s\n", x$filename, x$line_number, x$column_number, x$message),
      sep = ""
    )
  }
}
