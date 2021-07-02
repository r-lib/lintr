#' @describeIn linters  Check that object names conform to a naming style.
#' @param styles A subset of
#'   \Sexpr[stage=render, results=rd]{lintr:::regexes_rd}. A name should
#'   match at least one of these styles.
#' @export
object_name_linter <- function(styles = c("snake_case", "symbols")) {

  .or_string <- function(xs) {
    # returns "<S> or <T>"
    # where <T> is the last string in xs
    # where <S> is a comma-separated string of all entries in xs except <T>
    len <- length(xs)
    if (len <= 1) {
      return(xs)
    }
    comma_sepd_prefix <- toString(xs[-len])
    paste(comma_sepd_prefix, "or", xs[len])
  }

  styles <- match.arg(styles, names(style_regexes), several.ok = TRUE)

  lint_msg <- paste0(
    "Variable and function name style should be ", .or_string(styles), "."
  )

  Linter(function(source_file) {
    if (is.null(source_file$full_xml_parsed_content)) return(list())

    xml <- source_file$full_xml_parsed_content

    xpath <- paste0(
      # assignments
      "//SYMBOL[",
      " not(preceding-sibling::OP-DOLLAR)",
      " and ancestor::expr[",
      "  following-sibling::LEFT_ASSIGN",
      "  or preceding-sibling::RIGHT_ASSIGN",
      "  or following-sibling::EQ_ASSIGN",
      " ]",
      " and not(ancestor::expr[",
      "  preceding-sibling::OP-LEFT-BRACKET",
      "  or preceding-sibling::LBB",
      " ])",
      "]",

      " | ",

      "//STR_CONST[",
      " not(preceding-sibling::OP-DOLLAR)",
      " and ancestor::expr[",
      "  following-sibling::LEFT_ASSIGN",
      "  or preceding-sibling::RIGHT_ASSIGN",
      "  or following-sibling::EQ_ASSIGN",
      " ]",
      " and not(ancestor::expr[",
      "  preceding-sibling::OP-LEFT-BRACKET",
      "  or preceding-sibling::LBB",
      " ])",
      "]",

      # Or
      " | ",

      # Formal argument names
      "//SYMBOL_FORMALS"
    )

    assignments <- xml2::xml_find_all(xml, xpath)

    # Retrieve assigned name
    nms <- strip_names(
      xml2::xml_text(assignments)
    )

    generics <- strip_names(c(
      declared_s3_generics(xml),
      imported_s3_generics(namespace_imports(find_package(source_file$filename)))$fun,
      .base_s3_generics
    ))
    generics <- unique(generics[nzchar(generics)])

    style_matches <- lapply(styles, function(style) {
      check_style(nms, style, generics)
    })

    matches_a_style <- Reduce(`|`, style_matches)

    lapply(
      assignments[!matches_a_style],
      object_lint2,
      source_file,
      lint_msg
    )
  })
}

check_style <- function(nms, style, generics = character()) {
  conforming <- re_matches(nms, style_regexes[[style]])

  # mark empty names and NA names as conforming
  conforming[!nzchar(nms) | is.na(conforming)] <- TRUE

  if (any(!conforming)) {
    possible_s3 <- re_matches(
      nms[!conforming],
      rex(start, capture(name = "generic", or(generics)), ".", capture(name = "method", something), end)
    )
    if (any(!is.na(possible_s3))) {
      has_generic <- possible_s3$generic %in% generics

      # If they are not conforming, but are S3 methods then ignore them
      conforming[!conforming][has_generic] <- TRUE
    }
    # exclude namespace hooks like .onLoad, .Last.lib, etc (#500)
    is_special <- is_special_function(nms[!conforming])
    conforming[!conforming][is_special] <- TRUE
  }
  conforming
}

# Remove quotes or other things from names
strip_names <- function(x) {
  x <- re_substitutes(x, rex(start, some_of(".", quote, "`", "%", "$", "@")), "")
  x <- re_substitutes(x, rex(some_of(quote, "`", "<", "-", "%", "$", "@"), end), "")
  x
}

object_lint2 <- function(expr, source_file, message) {
  symbol <- xml2::as_list(expr)
  Lint(
    filename = source_file$filename,
    line_number = symbol@line1,
    column_number = symbol@col1,
    type = "style",
    message = message,
    line = source_file$file_lines[as.numeric(symbol@line1)],
    ranges = list(as.numeric(c(symbol@col1, symbol@col2))),
  )
}

make_object_linter <- function(fun, name = linter_auto_name()) {
  force(name)
  Linter(function(source_file) {

    token_nums <- ids_with_token(
      source_file, rex(start, "SYMBOL" %if_next_isnt% "_SUB"), fun = re_matches
    )
    if (length(token_nums) == 0) {
      return(list())
    }
    tokens <- with_id(source_file, token_nums)
    names <- unquote(tokens[["text"]]) # remove surrounding backticks

    keep_indices <- which(
      !is_operator(names) &
        !is_known_generic(names) &
        !is_base_function(names)
    )

    lapply(
      keep_indices,
      function(i) {
        token <- tokens[i, ]
        if (is_declared_here(token, source_file) &&
            !is_external_reference(source_file, token[["id"]])) {
          fun(source_file, token)
        }
      }
    )
  }, name = name)
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
  if ((l + 1L <= dim(filt)[[1L]] && re_matches(filt[l + 1L, "token"], assign_regex)) ||
       (l >= 2L  &&  filt[l - 1L, "token"] == "RIGHT_ASSIGN")) {
    # assigned variable or function parameter
    TRUE
  } else {
    sibling_ids <- siblings(source_file[["parsed_content"]], token[["id"]], 1L)
    if (token[["text"]] != "..." &&
        length(sibling_ids) &&
        with_id(source_file, sibling_ids[[1L]])[["text"]] == "function") {
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

# via unlist(tools:::.get_standard_package_names(), use.names = FALSE)
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

# some duplicates such as .onLoad appear in multiple packages; sort for efficiency
base_funs <- sort(unique(unlist(lapply(
  base_pkgs,
  function(x) {
    name <- try_silently(getNamespace(x))
    if (!inherits(name, "try-error")) {
      ls(name, all.names = TRUE)
    }
  }
))))

is_base_function <- function(x) {
  x %in% base_funs
}

# see ?".onLoad", ?Startup, and ?quit. Remove leading dot to match behavior of strip_names().
#   All of .onLoad, .onAttach, and .onUnload are used in base packages,
#   and should be caught in is_base_function; they're included here for completeness / stability
#   (they don't strictly _have_ to be defined in base, so could in principle be removed).
#   .Last.sys and .First.sys are part of base itself, so aren't included here.
special_funs <- c(
  "onLoad",
  "onAttach",
  "onUnload",
  "onDetach",
  "Last.lib",
  "First",
  "Last"
)

is_special_function <- function(x) {
  x %in% special_funs
}

object_lint <- function(source_file, token, message) {
  Lint(
    filename = source_file$filename,
    line_number = token$line1,
    column_number = token$col1,
    type = "style",
    message = message,
    line = source_file$lines[as.character(token$line1)],
    ranges = list(c(token$col1, token$col2))
  )
}


loweralnum <- rex(one_of(lower, digit))
upperalnum <- rex(one_of(upper, digit))

style_regexes <- list(
  "symbols"     = rex(start, zero_or_more(none_of(alnum)), end),
  "CamelCase"   = rex(start, maybe("."), upper, zero_or_more(alnum), end),
  "camelCase"   = rex(start, maybe("."), lower, zero_or_more(alnum), end),
  "snake_case"  = rex(start, maybe("."), some_of(lower, digit), any_of("_", lower, digit), end),
  "SNAKE_CASE"  = rex(start, maybe("."), some_of(upper, digit), any_of("_", upper, digit), end),
  "dotted.case" = rex(start, maybe("."), one_or_more(loweralnum), zero_or_more(dot, one_or_more(loweralnum)), end),
  "lowercase"   = rex(start, maybe("."), one_or_more(loweralnum), end),
  "UPPERCASE"   = rex(start, maybe("."), one_or_more(upperalnum), end)
)

regexes_rd <- toString(paste0("\\sQuote{", names(style_regexes), "}"))

#' @describeIn linters check that object names are not too long.
#' @export
object_length_linter <- function(length = 30L) {
  make_object_linter(function(source_file, token) {
    if (nchar(token$text) > length) {
        object_lint(
          source_file,
          token,
          paste0("Variable and function names should not be longer than ", length, " characters.")
        )
      }
  })
}
