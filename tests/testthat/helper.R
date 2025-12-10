# Helpers for lintr tests

single_quote <- function(x) paste0("'", x, "'")
double_quote <- function(x) paste0('"', x, '"')

#' Trim some leading whitespace
#'
#' Trim `num` characters from the start of every line in `x`, or auto-detect to remove the maximum number whitespace
#' from all lines while preserving relative indentation
#'
#' @param x a string containing newlines
#' @param num number of characters to strip from the start of each line.
#' `NULL` will auto-detect this based on the minimum number of leading spaces greater than one.
#'
#' @return A modified version of `x` with `num` characters removed from the start of every line and with a possible
#' leading and trailing blank line removed.
#'
#' @examples
#' my_var <- local({
#'   out <- trim_some("
#'     This will be the content
#'     of the file where
#'     only the following line
#'       is indented by two spaces.
#'   ")
#' })
#'
#' stopifnot(identical(
#'   my_var,
#'   "This will be the content\nof the file where\nonly the following line\n  is indented by two spaces."
#' ))
trim_some <- function(x, num = NULL) {
  x <- rex::re_substitutes(
    x,
    rex::rex(list(start, any_blanks, newline) %or% list(newline, any_blanks, end)),
    replacement = "",
    global = TRUE
  )

  if (is.null(num)) {
    ms <- rex::re_matches(x, "^\\s+", locations = TRUE, global = TRUE, options = "multi-line")[[1L]]
    num <- min(ms$end - ms$start) + 1L
  }

  rex::re_substitutes(x, rex::rex(start, n_times(any, num)), "", global = TRUE, options = "multi-line")
}

local_config <- function(contents, config_dir = ".", filename = ".lintr", .local_envir = parent.frame()) {
  config_path <- file.path(config_dir, filename)
  writeLines(contents, config_path)
  withr::defer(unlink(config_path), envir = .local_envir)
  config_path
}

with_config <- function(contents, code, config_dir = ".", filename = ".lintr") {
  local_config(contents, config_dir, filename)
  code
}

skip_if_not_utf8_locale <- function() {
  testthat::skip_if_not(l10n_info()[["UTF-8"]], "Not a UTF-8 locale")
}

safe_load_help_db <- function() {
  help_db <- tryCatch(tools::Rd_db("lintr"), error = \(e) NULL)
  # e.g. in dev under pkgload::load_all()
  if (length(help_db) == 0L) {
    help_db <- tryCatch(tools::Rd_db(dir = testthat::test_path("..", "..")), error = \(e) NULL)
    testthat::skip_if_not(length(help_db) > 0L, message = "Package help corrupted or not installed")
  }
  help_db
}

pipes <- function(exclude = NULL) {
  all_pipes <- c(
    standard = "%>%",
    greedy = "%!>%",
    tee = "%T>%",
    assignment = "%<>%",
    extraction = "%$%",
    native = "|>"
  )
  all_pipes[!all_pipes %in% exclude]
}
