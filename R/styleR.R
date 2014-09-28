#' @import rex
NULL

file_names <- list(
  "Should not be in camel case" = rex(lower, upper),
  "Should not end in .r" = rex(".r"),
  "Should not differ only in capitalization" = quote(rex(regex("(?i)"), or(files[files != file])))
)

variable <- rex("." %or% list(maybe("."), one_of(alnum, ".") %>% zero_or_more()))
variable_names = list(
  "Should not be in camel case" = rex(lower, upper),
  "Should not have multiple dots" = rex(".", except(".", regex("\\s")) %>% one_or_more(), ".")
)

operators <- rex(or(
    "=" %if_prev_isnt% one_of("=", "!", "<", ">") %if_next_isnt% "=",
      "==",
      "!=",
      "<=",
      ">=",
    "+",
    "-" %if_prev_isnt% "<" %if_next_isnt% one_of(digit, ">"),
      "<-",
      "->",
    "<" %if_next_isnt% one_of(">", "=", "-"),
    ">" %if_prev_isnt% one_of("<", "-") %if_next_isnt% "=",
      "<>",
    "%%",
    "/",
    "^",
    "*" %if_prev_isnt% "*" %if_next_isnt% "*",
      "**",
    "|" %if_prev_isnt% "|" %if_next_isnt% "|",
      "||",
    "&" %if_prev_isnt% "&" %if_next_isnt% "&",
      "&&",
    "!" %if_next_isnt% "=") %if_next_isnt% ("%") %if_prev_isnt% ("%") %or%
  list("%", one_or_more(except("%", space)), "%")
  )

spacing <- list(
  "Should have spaces around operators" = rex(group(operators %if_prev_isnt% space) %or% group(operators %if_next_isnt% or(space, end))),
  "Commas should have one space after them, and none before" = rex(group(space, ",") %or% group(",", except(space)))
)

quotation <- list(
  "Only use Double-Quotes" = rex("'" %if_prev_isnt% "#")
)

assignment <- list(
  "Use <- not = for assignment" =
  rex(start,
    zero_or_more(space),
    variable,
    zero_or_more(space), "=",
    # ignore variables that are in a list or function assignment
    one_or_more(except("=")), except(")", ","), end)
)

check_style <- function(files, searches){
  structure(remove_nulls(unlist(lapply(files, function(file){
    lines <- readLines(file)
    unlist(lapply(seq_along(lines), function(line_num) {
      line <- blank_quoted_text(lines[line_num])
      c(Map(searches, names(searches), f = function(search, search_message){
        m <- gregexpr(search, line, perl = TRUE)
        if(any(m[[1]] != -1)){
          line2 <- lines[line_num]
          regmatches(line2, m) <- lapply(regmatches(line2, m), function(x) { paste0("{HERE}>", x) })
          style_diff(file, line_num, search_message, line2)
        }
      }))
    }), use.names = FALSE, recursive = FALSE)
    }), use.names = FALSE, recursive = FALSE)), class = "diffs")
}

print.diffs <- function(x) {
  sapply(x, print)
}
remove_nulls <- function(x) { Filter(function(x) ! is.null(x), x) }


quoted_blanks <- function(matches) {
  lengths = nchar(matches)
  blanks <- vapply(Map(rep.int, rep.int(" ", length(lengths - 2)), lengths - 2, USE.NAMES = FALSE), paste, "", collapse = "")
  substr(matches, 2, nchar(matches)-1) <- blanks
  matches
}

blank_quoted_text <- function(s) {
  m <- gregexpr(rex(
        #quote followed by
        capture(quotes),

        #anything that is not an unescaped quote
        not(capture_group(1) %if_prev_isnt% "\\\\"),

        #closing quote (same type as opening quote)
        capture_group(1)
      ),
    s, perl = TRUE)

  regmatches(s, m) <- lapply(regmatches(s, m), quoted_blanks)
  s
}

check_filenames <- function(files, search) {
  for(file in files) {
    for(term_num in seq_along(search)){
      search[[term_num]] <- eval(search[[term_num]])
      if(grepl(search[term_num], file, perl = TRUE)){
        message("file: ", file, " ", names(search)[term_num])
      }
    }
  }
}
