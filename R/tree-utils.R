generate_tree <- function(pc) {
  if (is.null(pc)) {
    return(NULL)
  }
  edges <- matrix(as.character(c(pc$parent, pc$id)), ncol = 2L)
  list(edges = edges, adjlist = adjlist_from_edgelist(edges))
}

## Create an adjacency list from an edge list

adjlist_from_edgelist <- function(edges) {
  tapply(edges[, 2L], edges[, 1L], c, simplify = FALSE)
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

  which(graph$edges[, 1L] %in% sc)
}

lag <- function(x) {
  c(NA, x[seq_len(length(x) - 1L)])
}
