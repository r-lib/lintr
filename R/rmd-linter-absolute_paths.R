absolute_paths_rmd_linter <- function(source_file) {

  regexes <- vapply(absolute_path_types,
    function(x) {
      rex("[", except_any_of("]"), "]", "(", any_spaces, x)
    },
    character(1),
    USE.NAMES=FALSE)

  newline_locs <-
    c(0L,
      re_matches(source_file$content, rex("\n"), locations = TRUE)$start
      )

  newline_locs[ is.na(newline_locs) ] <- nchar(source_file$content)

  lapply(regexes, function(regex) {
    res <-
      re_matches(
        source_file$content,
        regex,
        global = TRUE,
        locations = TRUE)

    lapply(res, function(match){
      if(!is.na(match$`1.start`)){

        line_number <- which(newline_locs > match$`1.start`)[1L] - 1L
        start <- match$`1.start` - newline_locs[line_number]
        end <- match$`1.end` - newline_locs[line_number]

        lint(
          filename = source_file$filename,
          line_number = line_number,
          column_number = start,
          type = "style",
          message = "Do not use absolute paths.",
          line = getSrcLines(source_file, line_number, line_number),
          ranges = list(c(start, end))
          )
      }
      })
    })
}
