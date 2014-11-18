generate_tree <- function(pc) {
  if (is.null(pc)) {
    return(NULL)
  }
  edges <- matrix(as.character(c(pc$parent, pc$id)), ncol=2)
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

fix_eq_assign <- function(pc) {
  if (is.null(pc)) {
    return(NULL)
  }
  eq_assign_locs <- which(pc$token == "EQ_ASSIGN")

  id_itr <- max(pc$id)

  line1 <- integer(length(eq_assign_locs))
  col1 <- integer(length(eq_assign_locs))

  line2 <- integer(length(eq_assign_locs))
  col2 <- integer(length(eq_assign_locs))

  id <- integer(length(eq_assign_locs))

  parent <- integer(length(eq_assign_locs))

  token <- character(length(eq_assign_locs))

  terminal <- logical(length(eq_assign_locs))

  text <- character(length(eq_assign_locs))

  for (itr in seq_along(eq_assign_locs)) {
    loc <- eq_assign_locs[itr]
    prev_loc <- prev_with_parent(pc, loc)
    next_loc <- next_with_parent(pc, loc)

    line1[itr] <- pc[prev_loc, "line1"]
    col1[itr] <- pc[prev_loc, "col1"]

    line2[itr] <- pc[next_loc, "line2"]
    col2[itr] <- pc[next_loc, "col2"]

    id[itr] <- id_itr <- id_itr + 1L

    parent[itr] <- pc[loc, "parent"]

    token[itr] <- "expr"

    terminal[itr] <- FALSE

    text[itr] <- ""

    pc[prev_loc, "parent"] <- id[itr]
    pc[loc, "parent"] <- id[itr]
    pc[next_loc, "parent"] <- id[itr]
  }
  rbind(pc, data.frame(line1, col1, line2, col2, id, parent, token, terminal, text, row.names=id))
}

prev_with_parent <- function(pc, loc) {

  parent <- pc[loc, "parent"]

  loc <- loc - 1L
  while (loc > 0L) {
    if (pc[loc, "parent"] %==% parent) {
      return(loc)
    }
    loc <- loc - 1L
  }

}

next_with_parent <- function(pc, loc) {

  parent <- pc[loc, "parent"]

  loc <- loc + 1L
  while (loc <= NROW(pc)) {
    if (pc[loc, "parent"] %==% parent) {
      return(loc)
    }
    loc <- loc + 1L
  }

}

top_level_expressions <- function(pc) {
  if (is.null(pc)) {
    return(NULL)
  }
  which(pc$parent == 0L & pc$token == "expr")
}
