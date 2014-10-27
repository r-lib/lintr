#' @describeIn linters checks that closures have the proper usage using
#' \code{\link[codetools]{checkUsage}}.  Note this runs
#' \code{\link[base]{eval}} on the code, so do not use with untrusted code.
object_usage_linter <-  function(source_file) {
  # we need to evaluate each expression in order to use checkUsage on it.

  env <- parent.frame()

  lapply(which(
      re_matches(source_file$parsed_content$token,
        rex(start, "FUNCTION"))),
    function(id) {
      parent_id <- source_file$parsed_content[id, "parent"]

      expr <- eval(
        parse(
          text=getParseText(source_file$parsed_content, parent_id)),
        envir=env
        )

      res <- parse_check_usage(expr)

      lapply(which(!is.na(res$message)),
        function(row_num) {
          row <- res[row_num, ]

          org_line_num <- as.integer(row$line_number) +
            source_file$parsed_content[
              source_file$parsed_content$id == parent_id,
              "line1"] - 1L

          line <- getSrcLines(source_file, org_line_num, org_line_num)
          location <- re_matches(line,
            rex(regex("\\b"), row$name, regex("\\b")),
            locations = TRUE)

          Lint(
            filename = source_file$filename,
            line_number = org_line_num,
            column_number = location$start,
            type = "warning",
            message = row$message,
            line = line,
            ranges = list(c(location$start, location$end))
            )
        })
  })
}

parse_check_usage <- function(expression) {

  vals <- list()

  report <- function (x) {
    vals[[length(vals) + 1L]] <<- x
  }

  codetools::checkUsage(expression, report = report)

  res <- re_matches(vals,
    rex(anything, ": ",
      capture(name = "message", anything,
        "‘", capture(name = "name", anything), "’",
        anything),
      " ", "(", anything, ":", capture(name = "line_number", digits), ")"))
  res
}
