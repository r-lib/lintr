#' @describeIn linters check for \code{1:length(...)}, \code{1:nrow(...)},
#' \code{1:ncol(...)}, \code{1:NROW(...)} and \code{1:NCOL(...)}
#' expressions. These often cause bugs when the right hand side is zero.
#' It is safer to use \code{\link[base]{seq_len}} or
#' \code{\link[base]{seq_along}} instead.
#' @export
seq_linter <- function(source_file) {

  if (!length(source_file$parsed_content)) return(list())

  content <- source_file$parsed_content
  seq_idx <- which(content$token == "':'")
  n_seq <- length(seq_idx)
  if (n_seq == 0L) return(list())

  # (probably) overallocate for now
  out <- vector('list', n_seq)
  bad_funcs <- c("length", "nrow", "ncol", "NROW", "NCOL", "dim")
  out_i <- 1L
  for (seq_i in seq_idx) {
    seq_parent <- content$parent[seq_i]
    sibling_ids <- content$id[content$parent == seq_parent]

    # assumes content is in line-col order
    seq_lhs_id <- sibling_ids[1L]
    seq_lhs_children <- which(content$parent == seq_lhs_id)
    if (length(seq_lhs_children) != 1L || content$text[seq_lhs_children] != "1")
      next

    seq_rhs_id <- sibling_ids[length(sibling_ids)]
    seq_rhs_children <- content[content$parent == seq_rhs_id, ]
    if (!any(seq_rhs_children$text == "(")) next
    if (seq_rhs_children$token[1L] != "expr") next
    rhs_first_expr_children <- which(content$parent == seq_rhs_children$id[1L])
    if (length(rhs_first_expr_children) != 1L ||
          content$token[rhs_first_expr_children] != "SYMBOL_FUNCTION_CALL" ||
          ! content$text[rhs_first_expr_children] %in% bad_funcs)
      next
    rhs_expr <- content[content$id == seq_rhs_id, ]
    rhs_text <- with(rhs_expr, substr(source_file$lines[[line1]], col1, col2))

    line1 = content$line1[seq_lhs_children]
    col1 = content$col1[seq_lhs_children]
    out[[out_i]] = Lint(
      filename = source_file$filename,
      line_number = line1,
      column_number = col1,
      type = "warning",
      message = paste0(
        "Avoid " , content$text[seq_lhs_children], ":", rhs_text,
        " expressions, use seq_len"
      ),
      line = source_file$lines[[line1]],
      ranges = list(c(col1, content$col2[seq_lhs_children])),
      linter = "seq_linter"
    )

    out_i <- out_i + 1L
  }
  # trim the overallocated parts
  if (out_i <= n_seq) out[out_i:n_seq] = NULL
  out
}
