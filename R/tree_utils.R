generate_top_level_map <- function(pc) {
  if (is.null(pc)) {
    return(NULL)
  }
  tl_ids <- pc$id[pc$parent <= 0L]
  tl_parent <- pc$parent
  tl_parent[pc$parent <= 0L] <- tl_ids
  i_not_assigned <- which(!tl_parent %in% tl_ids)
  while ((prev_length <- length(i_not_assigned)) > 0L) { # nolint: implicit_assignment_linter. TODO(#2015): remove this.
    tl_parent[i_not_assigned] <- pc$parent[match(tl_parent[i_not_assigned], pc$id)]
    i_not_assigned <- which(!tl_parent %in% tl_ids)
    # nocov start
    if (length(i_not_assigned) >= prev_length) {
      cli_abort_internal("Logical error: unassigned set did not shrink. Check file syntax.")
    }
    # nocov end
  }
  tl_parent
}

lag <- function(x) {
  c(NA, x[seq_len(length(x) - 1L)])
}
