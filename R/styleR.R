#' @import rex
NULL

camel_case <- function(x, exclude = NULL) {
  re = rex_(~capture(any_of(alnum, "."), lower, upper, any_of(alnum, ".")))
  res <- gregexpr(re, x, perl = TRUE)

  mapply(function(match) {
    ret = data.frame(start = attr(match, "capture.start"), end = attr(match, "capture.length") - 1L)
    str(ret)
  },
  res,
  SIMPLIFY = FALSE)
}
file_names <- list(
  "Should not be in camel case" = rex(capture(any_of(alnum, "."), lower, upper, any_of(alnum, "."))),
  "Should not end in .r" = rex(".r"),
  "Should not differ only in capitalization" = quote(rex(regex("(?i)"), or(files[files != file])))
)

base_functions <- names(Filter(is.function, mget(ls("package:base"), inherits = TRUE)))

variable <- rex(some_of(alnum, "."))
#"." %or% list(maybe("."), some_of(alnum, ".")))
variable_names = list(
  "Should not be in camel case" = rex(lower, upper),
  "Should not have multiple dots" = rex(".", except_some_of(".", space), ".")
)

operators <- rex(or(
    "=" %if_prev_isnt% one_of("=", "!", "<", ">") %if_next_isnt% "=",
      "==",
      '!=',
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
      "&&") %if_next_isnt% ("%") %if_prev_isnt% ("%") %or%
  list("%", one_or_more(none_of("%", space)), "%")
  )

spacing <- list(
  "Should have spaces around infix operators" = rex(group(operators %if_prev_isnt% space) %or% group(operators %if_next_isnt% or(space, end))),
  "Commas should have one space after them, and none before" = rex(group(space, ",") %or% group(",", except(space)))
)

quotation <- list(
  "Only use Double-Quotes" = rex("'" %if_prev_isnt% "#")
)

parens <- regex("(\\((?:[^()]++|(?-1))*+\\))+")
assignment <- list(
  "Use <- not = for assignment" =
  rex(start,
    any_spaces,
    variable,
    any_spaces, "=",
    one_or_more(none_of("(,"), type = "possessive"),
    parens %or% list("(", not(parens)) %or% list(not(","), end)
  )
)

quoted <- rex(
  #quote followed by
  capture(quotes),

  #anything that is not an unescaped quote
  not(capture_group(1) %if_prev_isnt% "\\\\"),

  #closing quote (same type as opening quote)
  capture_group(1)
  )

comments <- rex("#", anything)

check_style <- function(files, searches){

  structure(remove_nulls(unlist(lapply(files, function(file){
    lines <- readLines(file)
    unlist(lapply(seq_along(lines), function(line_num) {
      line <- lines[line_num] %>% blank_text(quoted) %>% blank_text(comments, shift_start = 1, shift_end = 0)
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

quoted_blanks <- function(matches, shift_start = 2, shift_end = 1) {
  lengths <- nchar(matches)
  blanks <- vapply(Map(rep.int,
      rep.int(" ", length(lengths - (shift_start - 1 + shift_end))),
      lengths - (shift_start - 1 + shift_end), USE.NAMES = FALSE), paste, "", collapse = "")

  substr(matches, shift_start, nchar(matches)-shift_end) <- blanks
  matches
}

blank_text <- function(s, re, shift_start = 2, shift_end = 1) {
  m <- gregexpr(re, s, perl = TRUE)
  regmatches(s, m) <- lapply(regmatches(s, m), quoted_blanks, shift_start = shift_start, shift_end = shift_end)
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
