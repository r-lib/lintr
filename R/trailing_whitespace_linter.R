#' @describeIn linters  Check that there are no space characters at the end of
#' source lines.
#' @export
trailing_whitespace_linter <- function() {
  Linter(function(source_file) {
    res <- re_matches(
      source_file$lines,
      rex(capture(name = "space", some_of(" ", regex("\\t"))), or(newline, end)),
      global = TRUE,
      locations = TRUE
    )

    lapply(seq_along(source_file$lines), function(itr) {

      mapply(
        FUN = function(start, end) {
          if (is.na(start)) {
            return()
          }
          line_number <- names(source_file$lines)[itr]
          Lint(
            filename = source_file$filename,
            line_number = line_number,
            column_number = start,
            type = "style",
            message = "Trailing whitespace is superfluous.",
            line = source_file$lines[as.character(line_number)],
            ranges = list(c(start, end))
          )
        },
        start = res[[itr]]$space.start,
        end = res[[itr]]$space.end,
        SIMPLIFY = FALSE
      )
    })

  })
}
