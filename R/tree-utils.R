generate_tree <- function(pc) {
  if (is.null(pc)) {
    return(NULL)
  }
  edges <- matrix(as.character(c(pc$parent, pc$id)), ncol = 2)
  list(edges = edges, adjlist = adjlist_from_edgelist(edges))
}

## Create an adjacency list from an edge list

adjlist_from_edgelist <- function(edges) {
  tapply(edges[, 2], edges[, 1], c, simplify = FALSE)
}

## Take the subcomponent of id (mode out), and then all edges
## that start at these vertices

component_edges <- function(graph, id) {
  sc <- newv <- unique(id)
  size <- length(sc)
  repeat {
    neis <- unlist(graph$adjlist[newv])
    newv <- setdiff(neis, sc)
    sc <- c(sc, newv)
    if (length(sc) == size) break
    size <- length(sc)
  }

  which(graph$edges[, 1] %in% sc)
}

children <- function(data, id, levels = Inf) {

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

  as.character(unlist(ids))
}

parents <- function(data, id, levels = Inf) {

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

  as.character(unlist(ids))
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
