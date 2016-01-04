generate_tree <- function(pc) {
  if (is.null(pc)) {
    return(NULL)
  }
  edges <- matrix(as.character(c(pc$parent, pc$id)), ncol = 2)
  igraph::graph.edgelist(edges, directed = TRUE)
}
children <- function(data, id, levels = Inf, simplify = TRUE) {

  child_ids <- function(ids) {
    data$id[data$parent %in% ids]
  }

  ids <- list()
  itr <- 0L
  ids[[itr <- itr + 1L]] <- child_ids(id)
  while (levels >= 1L && length(ids[[itr]]) != 0L) {
    ids[[itr <- itr + 1L]] <- child_ids(ids[[itr]])
    levels <- levels - 1L
  }
  ids <- ids[-length(ids)]

  if (simplify) {
    as.character(unlist(ids))
  } else {
    as.character(ids)
  }
}

parents <- function(data, id, levels = Inf, simplify = TRUE) {

  parent_ids <- function(ids) {
    data$parent[data$id %in% ids]
  }

  ids <- list()
  itr <- 0L
  ids[[itr <- itr + 1L]] <- parent_ids(id)
  while (levels >= 1L && length(ids[[itr]]) != 0L) {
    ids[[itr <- itr + 1L]] <- parent_ids(ids[[itr]])
    levels <- levels - 1L
  }
  ids <- ids[-length(ids)]

  if (simplify) {
    as.character(unlist(ids))
  } else {
    as.character(ids)
  }
}

family <- function(data, id, parent_levels = 1L, child_levels = Inf) {
  parents <- parents(data, id, parent_levels)
  c(parents,
    unlist(lapply(
        parents,
        children,
        data = data,
        levels = child_levels)
      )
    )
}

siblings <- function(data, id, child_levels = Inf) {
  parents <- parents(data, id, 1L)
  res <- unlist(lapply(
      parents,
      children,
      data = data,
      levels = child_levels
      )
    )
  res[res != id]
}

lag <- function(x) {
  c(NA, x[seq_len(length(x) - 1L)])
}
