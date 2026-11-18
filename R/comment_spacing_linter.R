#' Comment spacing linter
#'
#' Check the spacing between the comment marker (`#`, `##`, `#'`, ...) and the
#' comment text, which by default must be at least one space. The spacing can be
#' set per marker.
#'
#' Comments consisting only of the marker (e.g. `#` or `####`) are not linted,
#' nor are a shebang on the first line and the special comments used by knitr's
#' `spin()`, Quarto chunk options, xaringan and printed output (`#+`, `#-`,
#' `#|`, `#<<` and `#>`).
#'
#' @param default_style Spacing style after comment markers not listed in
#'   `overrides`. `"one_or_more"` (the default) requires at least one space, so
#'   that text inside a comment can be indented; `"one"` requires exactly one
#'   space; `"one_or_hanging"` requires exactly one space, except on a line
#'   continuing a comment with the same marker in the same column on the line
#'   above, as in a hanging indent.
#' @param overrides Named list of character vectors assigning a spacing style to
#'   specific comment markers. Names are the style, one of `"one_or_more"`,
#'   `"one"`, `"one_or_hanging"`, `"none"` (no space) or `"any"` (not linted),
#'   and values are the markers, e.g. `list(any = "#'")` to skip roxygen
#'   comments. A marker matches exactly, so `"#"` does not cover `"##"`. Each
#'   marker can appear only once.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "#comment",
#'   linters = comment_spacing_linter()
#' )
#'
#' lint(
#'   text = "x <- 1 #comment",
#'   linters = comment_spacing_linter()
#' )
#'
#' lint(
#'   text = "#  comment",
#'   linters = comment_spacing_linter(default_style = "one")
#' )
#'
#' # okay
#' lint(
#'   text = "# comment",
#'   linters = comment_spacing_linter()
#' )
#'
#' lint(
#'   text = "x <- 1 # comment",
#'   linters = comment_spacing_linter()
#' )
#'
#' lint(
#'   text = "# comment",
#'   linters = comment_spacing_linter(default_style = "one")
#' )
#'
#' lint(
#'   text = "#'@export",
#'   linters = comment_spacing_linter(overrides = list(any = "#'"))
#' )
#'
#' lint(
#'   text = "#' @param x A number,\n#'   or a string.",
#'   linters = comment_spacing_linter(overrides = list(one_or_hanging = "#'"))
#' )
#'
#' @evalRd rd_tags("comment_spacing_linter")
#' @seealso
#'  - [linters] for a complete list of linters available in lintr.
#'  - <https://style.tidyverse.org/syntax.html#comments>
#' @export
comment_spacing_linter <- function(default_style = c("one_or_more", "one", "one_or_hanging"), overrides = NULL) {
  default_style <- match.arg(default_style)
  validate_comment_overrides(overrides)
  marker_styles <- resolve_comment_styles(overrides)
  comment_regex <- "^(?<marker>#++['*]*+)(?<spaces> *)(?<text>.*)$"

  # spaces allowed after the marker, by style
  min_spaces <- c(one_or_more = 1L, one = 1L, one_or_hanging = 1L, none = 0L, any = 0L)
  max_spaces <- c(one_or_more = Inf, one = 1L, one_or_hanging = 1L, none = 0L, any = Inf)
  lint_messages <- c(
    missing = "Put a space after the comment marker `%s`.",
    extra = "Put only one space after the comment marker `%s`.",
    unwanted = "Remove the space after the comment marker `%s`."
  )

  # file level, since a hanging indent continues a comment on the line above,
  #   which may belong to another expression
  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content

    comment_expr <- xml_find_all_(xml, "//COMMENT")
    comment_text <- xml_text(comment_expr)
    line1 <- as.integer(xml_attr_(comment_expr, "line1"))
    col1 <- as.integer(xml_attr_(comment_expr, "col1"))

    is_shebang <- startsWith(comment_text, "#!") & line1 == 1L & col1 == 1L
    is_special <- is_shebang | grepl("^#[-+|<>]", comment_text)
    comment_parts <- re_matches(comment_text, comment_regex)
    marker <- comment_parts$marker
    n_spaces <- nchar(comment_parts$spaces)
    has_text <- nzchar(comment_parts$text)

    style <- marker_styles[marker]
    style[is.na(style)] <- default_style

    comment_position <- paste(line1, col1, marker)
    is_continuation <- paste(line1 - 1L, col1, marker) %in% comment_position
    max_allowed <- max_spaces[style]
    max_allowed[style == "one_or_hanging" & is_continuation] <- Inf

    problem <- rep(NA_character_, length(comment_text))
    problem[n_spaces < min_spaces[style]] <- "missing"
    is_over <- n_spaces > max_allowed
    problem[is_over] <- ifelse(max_allowed[is_over] == 0L, "unwanted", "extra")
    problem[!has_text | is_special] <- NA_character_

    is_lint <- !is.na(problem)
    xml_nodes_to_lints(
      comment_expr[is_lint],
      source_expression = source_expression,
      lint_message = sprintf(lint_messages[problem[is_lint]], marker[is_lint]),
      type = "style"
    )
  })
}

# style for each comment marker given in overrides, named by the marker
resolve_comment_styles <- function(overrides) {
  if (is.null(overrides)) {
    return(character())
  }
  markers <- unlist(overrides, use.names = FALSE)
  styles <- rep(names(overrides), lengths(overrides))
  invalid_markers <- markers[!grepl("^#+['*]*$", markers)]
  if (length(invalid_markers) > 0L) {
    cli_abort("Unknown comment marker {.val {invalid_markers}} in {.arg overrides}.")
  }
  repeated_markers <- unique(markers[duplicated(markers)])
  if (length(repeated_markers) > 0L) {
    cli_abort("Comment marker {.val {repeated_markers}} is given more than once in {.arg overrides}.")
  }
  names(styles) <- markers
  styles
}

validate_comment_overrides <- function(overrides) {
  if (is.null(overrides)) {
    return(invisible())
  }
  if (!is.list(overrides) || !all(nzchar(names2(overrides)))) {
    cli_abort("{.arg overrides} must be a named list.")
  }
  # "none" (no space after the marker) has no known use case
  valid_styles <- c("one_or_more", "one", "one_or_hanging", "none", "any")
  invalid_styles <- setdiff(names(overrides), valid_styles)
  if (length(invalid_styles) > 0L) {
    cli_abort("Names of {.arg overrides} must be one of {.val {valid_styles}}, not {.val {invalid_styles}}.")
  }
}
