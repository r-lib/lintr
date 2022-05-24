generate_top_level_map <- function(pc) {
  if (is.null(pc)) {
    return(NULL)
  }
  tl_ids <- pc$id[pc$parent <= 0L]
  tl_parent <- pc$parent
  tl_parent[pc$parent <= 0L] <- tl_ids
  i_not_assigned <- which(!tl_parent %in% tl_ids)
  while (length(i_not_assigned)) {
    tl_parent[i_not_assigned] <- pc$parent[match(tl_parent[i_not_assigned], pc$id)]
    i_not_assigned <- which(!tl_parent %in% tl_ids)
  }
  tl_parent
}

lag <- function(x) {
  c(NA, x[seq_len(length(x) - 1L)])
}
