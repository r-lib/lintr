children <- function(data, id, levels = Inf) {
  ids <- list()

  itr <- 1L
  ids[[itr]] <- data[ data$parent == id, "id"]
  while(levels > 1L && length(ids[[itr]]) != 0L) {

    for(id in ids[[itr]]){
      itr <- itr + 1L
      ids[[itr]] <- data[ data$parent == id, "id"]
    }
    levels <- levels - 1L
  }
  unlist(ids)
}

parents <- function(data, id, levels = Inf) {
  ids <- list()

  itr <- 1L
  ids[[itr]] <- data[ data$id == id, "parent"]
  while(levels > 1L && length(ids[[itr]]) != 0L) {

    for(id in ids[[itr]]){
      itr <- itr + 1L
      ids[[itr]] <- data[ data$id == id, "parent"]
    }
    levels <- levels - 1L
  }
  unlist(ids)
}

family <- function(data, id, parent_levels = 1L, child_levels = Inf) {
  unlist(lapply(
      parents(data, id, parent_levels),
      children,
      data = data,
      levels = child_levels)
    )
}

siblings <- function(data, id) {
  res <- family(data, id, 1L, 1L)
  res[ res != id ]
}
