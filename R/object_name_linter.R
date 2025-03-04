#' Object name linter
#'
#' Check that object names conform to a naming style.
#' The default naming styles are "snake_case" and "symbols".
#'
#' Quotes (`` `"' ``) and specials (`%` and trailing `<-`) are not considered part of the object name.
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
#'   match at least one of these styles. The `"symbols"` style refers to
#'   names containing *only* non-alphanumeric characters; e.g., defining `%+%`
#'   from ggplot2 or `%>%` from magrittr would not generate lint markers,
#'   whereas `%m+%` from lubridate (containing both alphanumeric *and*
#'   non-alphanumeric characters) would.
#'
#' @param regexes A (possibly named) character vector specifying a custom naming convention.
#'   If named, the names will be used in the lint message. Otherwise, the regexes enclosed by `/` will be used in the
#'   lint message.
#'   Note that specifying `regexes` overrides the default `styles`. So if you want to combine `regexes` and `styles`,
#'   both need to be explicitly specified.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "my_var <- 1L",
#'   linters = object_name_linter(styles = "CamelCase")
#' )
#'
#' lint(
#'   text = "xYz <- 1L",
#'   linters = object_name_linter(styles = c("UPPERCASE", "lowercase"))
#' )
#'
#' lint(
#'   text = "MyVar <- 1L",
#'   linters = object_name_linter(styles = "dotted.case")
#' )
#'
#' lint(
#'   text = "asd <- 1L",
#'   linters = object_name_linter(regexes = c(my_style = "F$", "f$"))
#' )
#'
#' # okay
#' lint(
#'   text = "my_var <- 1L",
#'   linters = object_name_linter(styles = "snake_case")
#' )
#'
#' lint(
#'   text = "xyz <- 1L",
#'   linters = object_name_linter(styles = "lowercase")
#' )
#'
#' lint(
#'   text = "my.var <- 1L; myvar <- 2L",
#'   linters = object_name_linter(styles = c("dotted.case", "lowercase"))
#' )
#'
#' lint(
#'   text = "asdf <- 1L; asdF <- 1L",
#'   linters = object_name_linter(regexes = c(my_style = "F$", "f$"))
#' )
#'
#' @evalRd rd_tags("object_name_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
object_name_linter <- function(styles = c("snake_case", "symbols"), regexes = character()) {
  if ((!missing(styles) || missing(regexes)) && length(styles) > 0L) {
    # Allow `object_name_linter(NULL, "my_regex")`
    styles <- match.arg(styles, names(style_regexes), several.ok = TRUE)
    style_list <- style_regexes[styles]
  } else {
    style_list <- list()
  }
  if (length(regexes) > 0L) {
    if (!is.character(regexes)) {
      cli_abort("{.arg regexes} must be a {.cls character} vector.")
    }
    rx_names <- names2(regexes)
    missing_name <- !nzchar(rx_names)
    rx_names[missing_name] <- paste0("/", regexes[missing_name], "/") # auto-name regex "asd" -> /asd/
    names(regexes) <- rx_names

    style_list <- c(style_list, as.list(regexes))
  }
  if (length(style_list) == 0L) {
    cli_abort("At least one style must be specified using {.arg styles} or {.arg regexes}.")
  }

  lint_message <- paste0(
    "Variable and function name style should match ",
    glue_collapse(unique(names(style_list)), sep = ", ", last = " or "), "."
  )

  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content

    assignments <- xml_find_all(xml, object_name_xpath)

    # Retrieve assigned name
    nms <- strip_names(
      xml_text(assignments)
    )

    # run namespace_imports at run-time, not "compile" time to allow package structure to change
    pkg <- find_package(source_expression$filename)
    generics <- c(
      declared_s3_generics(xml),
      imported_s3_generics(namespace_imports(pkg))$fun,
      exported_s3_generics(pkg)$fun,
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
  conforming <- re_matches_logical(nms, style)

  # mark empty or NA names as conforming
  conforming <- is.na(nms) | !nzchar(nms) | conforming

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

loweralnum <- rex(one_of(lower, digit))
upperalnum <- rex(one_of(upper, digit))

style_regexes <- list(
  symbols     = rex(start, zero_or_more(none_of(alnum)), end),
  CamelCase   = rex(start, maybe("."), upper, zero_or_more(alnum), end),
  camelCase   = rex(start, maybe("."), lower, zero_or_more(alnum), end),
  snake_case  = rex(start, maybe("."), some_of(lower, digit), any_of("_", lower, digit), end),
  SNAKE_CASE  = rex(start, maybe("."), some_of(upper, digit), any_of("_", upper, digit), end),
  dotted.case = rex(start, maybe("."), one_or_more(loweralnum), zero_or_more(dot, one_or_more(loweralnum)), end),
  lowercase   = rex(start, maybe("."), one_or_more(loweralnum), end),
  UPPERCASE   = rex(start, maybe("."), one_or_more(upperalnum), end)
)

regexes_rd <- toString(paste0("\\sQuote{", names(style_regexes), "}"))
