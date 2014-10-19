assignment_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token == "EQ_ASSIGN"),
    function(id) {
      parsed <- source_file$parsed_content[id, ]
      lint(
        filename = source_file$filename,
        line_number = parsed$line1,
        column_number = parsed$col1,
        type = "style",
        message = "Use <-, not =, for assignment.",
        line = getSrcLines(source_file, parsed$line1, parsed$line1)
        )
    })
}

single_quotes_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token == "STR_CONST"),
    function(id) {
      parsed <- source_file$parsed_content[id, ]
      if(re_matches(parsed$text, rex(start, "'", anything, "'", end))) {
        lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "Only use Double-Quotes.",
          line = getSrcLines(source_file, parsed$line1, parsed$line1),
          ranges = list(c(parsed$col1, parsed$col2))
          )
      }
    })
}

# This is used in both .R and .Rmd linters

# note text uses encodeStrings
absolute_path_types <- list(
  rex(capture(letter, ":", some_of("/", "\\\\"), except_any_of(space, quote))), ## windows-style absolute paths
  rex(capture("\\\\", "\\\\", except_some_of("\\\\", space, quote))), ## windows UNC paths
  rex(capture("/", except_some_of(space, quote))),## unix-style absolute paths
  rex(capture("~", except_some_of(space, quote)))
  )

absolute_paths_linter <- function(source_file) {

  regexes <- vapply(absolute_path_types,
    function(x) {
      rex(quote, x)
    },
    character(1),
    USE.NAMES=FALSE)

  lapply(which(source_file$parsed_content$token == "STR_CONST"),
    function(id) {

      parsed <- source_file$parsed_content[id, ]

      for(regex in regexes){
        res <- re_matches(parsed$text, regex, locations = TRUE)
        if(!is.na(res$`1.start`)){
          start <- parsed$col1 + res$`1.start` - 1L
          end <- parsed$col1 + res$`1.end` - 1L
          return(
            lint(
              filename = source_file$filename,
              line_number = parsed$line1,
              column_number = start,
              type = "style",
              message = "Do not use absolute paths.",
              line = getSrcLines(source_file, parsed$line1, parsed$line1),
              ranges = list(c(start, end))
              )
            )
        }
      }
    })
}

default_linters <- c(

  assignment_linter,
  single_quotes_linter,
  absolute_paths_linter,

  NULL
)
