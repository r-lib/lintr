children <- function(data, id, levels = Inf) {
  ids <- list()

  itr <- 1L
  ids[[itr]] <- data$id[data$parent == id]
  while (levels > 1L && length(ids[[itr]]) != 0L) {

    for (id in ids[[itr]]){
      itr <- itr + 1L
      ids[[itr]] <- data$id[data$parent == id]
    }
    levels <- levels - 1L
  }
  as.character(unlist(ids))
}

parents <- function(data, id, levels = Inf, inclusive = T) {
  ids <- list()

  itr <- 1L
  ids[[itr]] <- data$parent[data$id == id]
  while (levels > 1L && length(ids[[itr]]) != 0L) {

    for (id in ids[[itr]]){
      itr <- itr + 1L
      ids[[itr]] <- data$parent[data$id == id]
    }
    levels <- levels - 1L
  }
  if (length(ids[[length(ids)]]) == 0L) {
    as.character(ids[-length(ids)])
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
  parents <- parents(data, id, 1L)[[1]]
  res <- unlist(lapply(
      parents,
      children,
      data = data,
      levels = child_levels
      )
    )
  res[res != id]
}
