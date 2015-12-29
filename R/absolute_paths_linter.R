# This is used in both .R and .Rmd linters
absolute_path_types <- list(

  # note text uses encodeStrings

  # windows-style absolute paths
  rex(
    capture(
      letter,
      ":",
      some_of("/", "\\\\"),
      except_any_of(space, quote)
      )
    ),

  # windows UNC paths
  rex(
    capture(
      "\\\\", "\\\\",
      except_some_of("\\\\", space, quote)
      )
    ),

  # unix-style absolute paths
  rex(capture("/", except_some_of(space, quote, "/"))),

  # unix-style tilda expansions
  rex(capture("~", except_some_of(space, quote)))
  )

#' @describeIn linters checks that no absolute paths are used.
#' @export
absolute_paths_linter <- function(source_file) {

  regexes <- vapply(absolute_path_types,
    function(x) {
      rex(quote, x)
    },
    character(1),
    USE.NAMES = FALSE)

  lapply(ids_with_token(source_file, "STR_CONST"),
    function(id) {

      parsed <- with_id(source_file, id)

      for (regex in regexes){
        res <- re_matches(parsed$text, regex, locations = TRUE)
        if (!is.na(res$`1.start`)){
          start <- parsed$col1 + res$`1.start` - 1L
          end <- parsed$col1 + res$`1.end` - 1L
          return(
            Lint(
              filename = source_file$filename,
              line_number = parsed$line1,
              column_number = start,
              type = "warning",
              message = "Do not use absolute paths.",
              line = source_file$lines[as.character(parsed$line1)],
              ranges = list(c(start, end)),
              "absolute_paths_linter"
              )
            )
        }
      }
    })
}

absolute_paths_rmd_linter <- function(source_file) {

  regexes <- vapply(absolute_path_types,
    function(x) {
      rex("[", except_any_of("]"), "]", "(", any_spaces, x)
    },
    character(1),
    USE.NAMES = FALSE)

  newline_locs <-
    c(0L,
      re_matches(source_file$content, rex("\n"), locations = TRUE)$start
      )

  newline_locs[is.na(newline_locs)] <- nchar(source_file$content)

  lapply(regexes, function(regex) {
    res <-
      re_matches(
        source_file$content,
        regex,
        global = TRUE,
        locations = TRUE)

    lapply(res, function(match){
      if (!is.na(match$`1.start`)){

        line_number <- which(newline_locs > match$`1.start`)[1L] - 1L
        start <- match$`1.start` - newline_locs[line_number]
        end <- match$`1.end` - newline_locs[line_number]

        Lint(
          filename = source_file$filename,
          line_number = line_number,
          column_number = start,
          type = "style",
          message = "Do not use absolute paths.",
          line = source_file$lines[as.character(line_number)],
          ranges = list(c(start, end))
          )
      }
      })
    })
}
