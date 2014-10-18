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
          line = getSrcLines(source_file, parsed$line1, parsed$lin1),
          ranges = list(c(parsed$col1, parsed$col2))
          )
      }
    })
}

absolute_paths_linter <- function(source_file) {
  absolute_path_types <- list(
    rex(capture(letter, ":", some_of("/", "\\"))), ## windows-style absolute paths
    rex(capture("\\", "\\")), ## windows UNC paths
    rex(capture("/", except_some_of("/"))),## unix-style absolute paths
    rex(capture("~", except_any_of("~/")))
    )

  regexes <- vapply(absolute_path_types,
    function(x) {
      c(
        rex(quote, any_spaces, x),
        rex("[", except_any_of("]"), "]", "(", any_spaces, x)
        )
    },
    character(2),
    USE.NAMES=FALSE)

  lapply(which(source_file$parsed_content$token == "STR_CONST"),
    function(id) {

      parsed <- source_file$parsed_content[id, ]

      for(regex in regexes){
        res <- re_matches(parsed$text, regex, locations = TRUE)
        if(!is.na(res$`1.start`)){
          return(
            lint(
              filename = source_file$filename,
              line_number = parsed$line1,
              column_number = res$`1.start`,
              type = "style",
              message = "Do not use absolute paths.",
              line = getSrcLines(source_file, parsed$line1, parsed$line1),
              ranges = list(c(res$`1.start`, res$`1.end`))
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
