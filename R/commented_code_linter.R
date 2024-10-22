#' Commented code linter
#'
#' Check that there is no commented code outside roxygen blocks.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "# x <- 1",
#'   linters = commented_code_linter()
#' )
#'
#' lint(
#'   text = "x <- f() # g()",
#'   linters = commented_code_linter()
#' )
#'
#' lint(
#'   text = "x + y # + z[1, 2]",
#'   linters = commented_code_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x <- 1; x <- f(); x + y",
#'   linters = commented_code_linter()
#' )
#'
#' lint(
#'   text = "#' x <- 1",
#'   linters = commented_code_linter()
#' )
#'
#' @evalRd rd_tags("commented_code_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
commented_code_linter <- function() {
  ops <- list(
    "+",
    # "-",
    "=",
    "==",
    "!=",
    "<=",
    ">=",
    "<-",
    "<<-",
    "<",
    ">",
    "->",
    "->>",
    "%%",
    "/",
    "^",
    "*",
    "**",
    "|",
    "||",
    "&",
    "&&",
    rex("%", except_any_of("%"), "%")
  )

  code_candidate_regex <- rex(
    some_of("#"),
    any_spaces,
    capture(
      name = "code",
      anything,
      or(
        some_of("{}[]"), # code-like parentheses
        or(ops), # any operator
        group(graphs, "(", anything, ")"), # a function call
        group("!", alphas) # a negation
      ),
      anything
    )
  )

  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content

    all_comment_nodes <- xml_find_all(xml, "//COMMENT")
    all_comments <- xml_text(all_comment_nodes)
    code_candidates <- re_matches(all_comments, code_candidate_regex, global = FALSE, locations = TRUE)
    extracted_code <- code_candidates[, "code"]
    # ignore trailing ',' or pipes ('|>', '%>%') when testing for parsability
    extracted_code <- re_substitutes(extracted_code, rex(or(",", "|>", "%>%"), any_spaces, end), "")
    extracted_code <- re_substitutes(extracted_code, rex(start, any_spaces, ","), "")

    is_parsable <- which(vapply(extracted_code, parsable, logical(1L)))

    lint_list <- xml_nodes_to_lints(
      all_comment_nodes[is_parsable],
      source_expression = source_expression,
      lint_message = "Remove commented code."
    )

    # Location info needs updating
    for (i in seq_along(lint_list)) {
      rng <- lint_list[[i]]$ranges[[1L]]

      rng[2L] <- rng[1L] + code_candidates[is_parsable[i], "code.end"] - 1L
      rng[1L] <- rng[1L] + code_candidates[is_parsable[i], "code.start"] - 1L

      lint_list[[i]]$column_number <- rng[1L]
      lint_list[[i]]$ranges <- list(rng)
    }

    lint_list
  })
}

# is given text parsable
parsable <- function(x) {
  if (anyNA(x)) {
    return(FALSE)
  }
  res <- try_silently(parse(text = x))
  !inherits(res, "try-error")
}
