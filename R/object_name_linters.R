#' @include lint.R
NULL

make_object_linter <- function(fun) {
  Linter(function(source_file) {
    lapply(
      ids_with_token(source_file, rex(start, "SYMBOL" %if_next_isnt% "_SUB"), fun=re_matches),
      function(token_num) {
        token <- with_id(source_file, token_num)
        name <- unquote(token[["text"]])  # remove surrounding backticks
        if (is_declared_here(token, source_file) &&
            !is_operator(name) &&
            !is_known_generic(name) &&
            !is_base_function(name) &&
            !is_external_reference(source_file, token[["id"]])) {
          fun(source_file, token)
        }
      }
    )
  })
}

known_generic_regex <- rex(
  start,
  or(
    unique(
      # Clean up "as.data.frame" to "as", "names<-" to "names", etc
      re_substitutes(c(names(.knownS3Generics), .S3PrimitiveGenerics),
                     rex(or(dot, "<-"), anything, end), "")
    )
  ),
  dot
)

is_known_generic <- function(name) {
  re_matches(name, known_generic_regex)
}

is_declared_here <- function(token, source_file) {
  # The object was declared here if one of the following is true:
  #   * its name precedes a left assign ("<-" or "<<-") or equal assign ("=")
  #   * its name follows a right assign ("->" or "->>")
  #   * its name is not "..." and its first sibling token is a function definition
  filt <- filter_out_token_type(source_file[["parsed_content"]], "expr")
  assign_regex <- rex(start, or("EQ_ASSIGN", "LEFT_ASSIGN"), end)
  l <- which(filt[, "id"] == token[["id"]])
  if ( (l + 1L <= dim(filt)[[1L]] && re_matches(filt[l + 1L, "token"], assign_regex)) ||
       (l >= 2L  &&  filt[l - 1L, "token"] == "RIGHT_ASSIGN") ) {
    # assigned variable or function parameter
    TRUE
  } else {
    sibling_ids <- siblings(source_file[["parsed_content"]], token[["id"]], 1L)
    if (token[["text"]] != "..." &&
        length(sibling_ids) &&
        with_id(source_file, sibling_ids[[1L]])[["text"]] == "function" ) {
      # parameter in function definition
      TRUE
    } else {
      FALSE
    }
  }
}

is_operator <- function(name) {
  name != make.names(name)
}

is_external_reference <- function(source_file, id) {
  sibling_tokens <- with_id(source_file, siblings(source_file$parsed_content, id, 1))$token
  any(sibling_tokens %in% c("NS_GET", "NS_GET_INT"))
}

base_pkgs <- c(
  "base",
  "tools",
  "utils",
  "grDevices",
  "graphics",
  "stats",
  "datasets",
  "methods",
  "grid",
  "splines",
  "stats4",
  "compiler",
  "parallel",
  "MASS",
  "lattice",
  "Matrix",
  "nlme",
  "survival",
  "boot",
  "cluster",
  "codetools",
  "foreign",
  "KernSmooth",
  "rpart",
  "class",
  "nnet",
  "spatial",
  "mgcv"
)

base_funs <- unlist(lapply(base_pkgs,
                           function(x) {
                             name <- try(getNamespace(x))
                             if (!inherits(name, "try-error")) {
                               ls(name, all.names = TRUE)
                             }
                           }))

is_base_function <- function(x) {
  x %in% base_funs
}

object_lint <- function(source_file, token, message, type) {
  Lint(
    filename = source_file$filename,
    line_number = token$line1,
    column_number = token$col1,
    type = "style",
    message = message,
    line = source_file$lines[as.character(token$line1)],
    ranges = list(c(token$col1, token$col2)),
    linter = type
    )
}


#' @describeIn linters  Check that object names conform to a single naming style.
#' @param style UpperCamelCase, lowerCamelCase, alllowercase, ALLUPPERCASE, snake_case or
#'              dotted.case
#' @export
object_name_linter <- function(style = "snake_case") {
  make_object_linter(
    function(source_file, token) {
      name <- unquote(token[["text"]])
      if (!matches_styles(name, style)) {
        object_lint(
          source_file,
          token,
          sprintf("Variable or function name should be %s.", style),
          "object_name_linter"
        )
      }
    }
  )
}


loweralnum <- rex(one_of(lower, digit))
upperalnum <- rex(one_of(upper, digit))

style_regexes <- list(
  "UpperCamelCase" = rex(start, upper, zero_or_more(alnum), end),
  "lowerCamelCase" = rex(start, lower, zero_or_more(alnum), end),
  "snake_case"     = rex(start, one_or_more(loweralnum), zero_or_more("_", one_or_more(loweralnum)), end),
  "dotted.case"    = rex(start, one_or_more(loweralnum), zero_or_more(dot, one_or_more(loweralnum)), end),
  "alllowercase"   = rex(start, one_or_more(loweralnum), end),
  "ALLUPPERCASE"   = rex(start, one_or_more(upperalnum), end)
)
# Note: snake_case and dotted.case styles are assumed to be lowercase.
# other styles include:
#   SCREAMING_SNAKE_CASE, same as snake_case, but uppercase
#   kebab-case, i.e. dash-separated (not practical in R since it should be quoted with backticks)

matches_styles <- function(name, styles=names(style_regexes)) {
  invalids <- paste(styles[!styles %in% names(style_regexes)], collapse=", ")
  if (nzchar(invalids)) {
    valids <- paste(names(style_regexes), collapse=", ")
    stop(sprintf("Invalid style(s) requested: %s\nValid styles are: %s\n", invalids, valids))
  }
  name <- re_substitutes(name, rex(start, one_or_more(dot)), "")  # remove leading dots
  vapply(
    style_regexes[styles],
    re_matches,
    logical(1L),
    data=name
  )
}


#' @describeIn linters check that object names are not too long.
#' @export
object_length_linter <- function(length = 20L) {
  make_object_linter(function(source_file, token) {
    if (nchar(token$text) > length) {
        object_lint(
          source_file,
          token,
          paste0("Variable and function names should not be longer than ", length, " characters."),
          "object_length_linter"
        )
      }
  })
}
