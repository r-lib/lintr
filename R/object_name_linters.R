object_name_xpath <- local({
  xp_assignment_target <- paste(
    "not(preceding-sibling::OP-DOLLAR)",
    "and ancestor::expr[",
    " following-sibling::LEFT_ASSIGN",
    " or preceding-sibling::RIGHT_ASSIGN",
    " or following-sibling::EQ_ASSIGN",
    "]",
    "and not(ancestor::expr[",
    " preceding-sibling::OP-LEFT-BRACKET",
    " or preceding-sibling::LBB",
    "])"
  )

  paste0(
    "//SYMBOL[", xp_assignment_target, "] | ",
    "//STR_CONST[", xp_assignment_target, "] | ",
    "//SYMBOL_FORMALS"
  )
})

#' Object name linter
#'
#' Check that object names conform to a naming style.
#' The default naming styles are "snake_case" and "symbols".
#'
#' Note when used in a package, in order to ignore objects imported
#'   from other namespaces, this linter will attempt [getNamespaceExports()]
#'   whenever an `import(PKG)` or `importFrom(PKG, ...)` statement is found
#'   in your NAMESPACE file. If [requireNamespace()] fails (e.g., the package
#'   is not yet installed), the linter won't be able to ignore some usages
#'   that would otherwise be allowed.
#'
#' Suppose, for example, you have `import(upstream)` in your NAMESPACE,
#'   which makes available its exported S3 generic function
#'   `a_really_quite_long_function_name` that you then extend in your package
#'   by defining a corresponding method for your class `my_class`.
#'   Then, if `upstream` is not installed when this linter runs, a lint
#'   will be thrown on this object (even though you don't "own" its full name).
#'
#' The best way to get lintr to work correctly is to install the package so
#'   that it's available in the session where this linter is running.
#'
#' @param styles A subset of
#'   \Sexpr[stage=render, results=rd]{lintr:::regexes_rd}. A name should
#'   match at least one of these styles.
#' @param regexes A (possibly named) character vector specifying a custom naming convention.
#'   If named, the names will be used in the lint message. Otherwise, "custom" will be used as a name for the style.
#'   Quotes (`` `"' ``) and specials (`%` and trailing `<-`) are not considered part of the name to be matched.
#' @evalRd rd_tags("object_name_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
object_name_linter <- function(styles = c("snake_case", "symbols"), regexes = character()) {
  if (length(styles) > 0L) {
    # Allow `object_name_linter(NULL, "my_regex")`
    styles <- match.arg(styles, names(style_regexes), several.ok = TRUE)
    style_list <- style_regexes[styles]
  } else {
    style_list <- list()
  }
  if (length(regexes) > 0L) {
    if (!is.character(regexes)) {
      stop("`regexes` must be a character vector.")
    }
    if (is.null(names(regexes))) {
      names(regexes) <- "custom"
    }
    style_list <- c(style_list, as.list(regexes))
  }
  if (length(style_list) == 0L) {
    stop("At least one style must be specified using `styles` or `regexes`.")
  }

  lint_message <- paste0(
    "Variable and function name style should be ",
    glue::glue_collapse(unique(names(style_list)), sep = ", ", last = " or "), "."
  )

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    assignments <- xml2::xml_find_all(xml, object_name_xpath)

    # Retrieve assigned name
    nms <- strip_names(
      xml2::xml_text(assignments)
    )

    # run namespace_imports at run-time, not "compile" time to allow package structure to change
    generics <- c(
      declared_s3_generics(xml),
      imported_s3_generics(namespace_imports(find_package(source_expression$filename)))$fun,
      .base_s3_generics
    )
    generics <- unique(generics[nzchar(generics)])

    style_matches <- lapply(style_list, function(style) {
      check_style(nms, style, generics)
    })

    matches_a_style <- Reduce(`|`, style_matches)

    xml_nodes_to_lints(
      assignments[!matches_a_style],
      source_expression,
      lint_message = lint_message,
      type = "style"
    )
  })
}

check_style <- function(nms, style, generics = character()) {
  conforming <- re_matches(nms, style)

  # mark empty names and NA names as conforming
  conforming[!nzchar(nms) | is.na(conforming)] <- TRUE

  if (!all(conforming)) {
    possible_s3 <- re_matches(
      nms[!conforming],
      rex(start, capture(name = "generic", or(generics)), ".", capture(name = "method", something), end)
    )
    if (!all(is.na(possible_s3))) {
      has_generic <- possible_s3$generic %in% generics

      # If they are not conforming, but are S3 methods then ignore them
      conforming[!conforming][has_generic] <- TRUE
    }
    # exclude namespace hooks like .onLoad, .Last.lib, etc (#500) and ...
    is_special <- is_special_function(nms[!conforming]) | nms[!conforming] == "..."
    conforming[!conforming][is_special] <- TRUE
  }
  conforming
}

# Remove quotes or other things from names
strip_names <- function(x) {
  x <- re_substitutes(x, rex(start, some_of(quote, "`", "%")), "")
  x <- re_substitutes(x, rex(some_of(quote, "`", "<", "-", "%"), end), "")
  x
}

# see ?".onLoad", ?Startup, and ?quit. Remove leading dot to match behavior of strip_names().
#   All of .onLoad, .onAttach, and .onUnload are used in base packages,
#   and should be caught in is_base_function; they're included here for completeness / stability
#   (they don't strictly _have_ to be defined in base, so could in principle be removed).
#   .Last.sys and .First.sys are part of base itself, so aren't included here.
special_funs <- c(
  ".onLoad",
  ".onAttach",
  ".onUnload",
  ".onDetach",
  ".Last.lib",
  ".First",
  ".Last"
)

is_special_function <- function(x) {
  x %in% special_funs
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

#' Object length linter
#'
#' Check that object names are not too long.
#' The length of an object name is defined as the length in characters, after removing extraneous parts:
#'
#'  * generic prefixes for implementations of S3 generics, e.g. `as.data.frame.my_class` has length 8.
#'  * leading `.`, e.g. `.my_hidden_function` has length 18.
#'  * "%%" for infix operators, e.g. `%my_op%` has length 5.
#'  * trailing `<-` for assignment functions, e.g. `my_attr<-` has length 7.
#'
#' Note that this behavior relies in part on having packages in your Imports available;
#'   see the detailed note in [object_name_linter()] for more details.
#'
#' @param length maximum variable name length allowed.
#' @evalRd rd_tags("object_length_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
object_length_linter <- function(length = 30L) {
  lint_message <- paste("Variable and function names should not be longer than", length, "characters.")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    assignments <- xml2::xml_find_all(xml, object_name_xpath)

    # Retrieve assigned name
    nms <- strip_names(
      xml2::xml_text(assignments)
    )

    # run namespace_imports at run-time, not "compile" time to allow package structure to change
    ns_imports <- namespace_imports(find_package(source_expression$filename))
    generics <- strip_names(c(
      declared_s3_generics(xml),
      imported_s3_generics(ns_imports)$fun,
      .base_s3_generics
    ))
    generics <- unique(generics[nzchar(generics)])

    # Remove generic function names from generic implementations
    # This only lints S3 implementations if the class names are too long, still lints generics if they are too long.
    nms_stripped <- re_substitutes(nms, rex(start, or(generics), "."), "")

    too_long <- nchar(nms_stripped) > length

    xml_nodes_to_lints(
      assignments[too_long],
      source_expression = source_expression,
      lint_message = lint_message,
      type = "style"
    )
  })
}
