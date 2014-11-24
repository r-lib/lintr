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

lag <- function(x) {
  c(NA, x[seq_len(length(x) - 1L)])
}

fix_eq_assign <- function(pc) {
  if (is.null(pc)) {
    return(NULL)
  }

  eq_assign_locs <- which(pc$token == "EQ_ASSIGN")
  if (length(eq_assign_locs) == 0L) {
    return(pc)
  }

  prev_locs <- vapply(eq_assign_locs, prev_with_parent, pc = pc, integer(1))
  next_locs <- vapply(eq_assign_locs, next_with_parent, pc = pc, integer(1))
  expr_locs <- (function(x){
    x[is.na(x)] <- FALSE
    !x
    })(prev_locs == lag(next_locs))

  id_itr <- max(pc$id)

  line1 <- integer(sum(expr_locs))
  col1 <- integer(sum(expr_locs))

  line2 <- integer(sum(expr_locs))
  col2 <- integer(sum(expr_locs))

  id <- integer(sum(expr_locs))

  parent <- integer(sum(expr_locs))

  token <- character(sum(expr_locs))

  terminal <- logical(sum(expr_locs))

  text <- character(sum(expr_locs))

  true_locs <- which(expr_locs == TRUE)
  for (i in seq_along(true_locs)) {
    start <- true_locs[i]

    end <- true_locs[i]
    j <- end + 1L
    while(j <= length(expr_locs) && expr_locs[j] == FALSE) {
      end <- j
      j <- j + 1L
    }

    prev_loc <- prev_locs[start]
    next_loc <- next_locs[end]

    line1[i] <- pc[prev_loc, "line1"]
    col1[i] <- pc[prev_loc, "col1"]

    line2[i] <- pc[next_loc, "line2"]
    col2[i] <- pc[next_loc, "col2"]

    id[i] <- id_itr <- id_itr + 1L

    parent[i] <- pc[eq_assign_locs[true_locs[i]], "parent"]

    token[i] <- "expr"

    terminal[i] <- FALSE

    text[i] <- ""

    pc[eq_assign_locs[true_locs[i]], "parent"] <- id[i]
    for (j in start:end) {
      pc[prev_locs[j], "parent"] <- id[i]
      pc[eq_assign_locs[j], "parent"] <- id[i]
      pc[next_locs[j], "parent"] <- id[i]
    }
    pc[next_loc, "parent"] <- id[i]
  }
  rbind(pc, data.frame(line1, col1, line2, col2, id, parent, token, terminal, text, row.names=id))
}

prev_with_parent <- function(pc, loc) {

  id <- pc$id[loc]
  parent_id <- pc$parent[loc]

  with_parent <- pc[pc$parent == parent_id,]
  with_parent <- with_parent[order(with_parent$line1, with_parent$col1, with_parent$line2, with_parent$col2),]

  loc <- which(with_parent$id == id)

  return(which(pc$id == with_parent$id[loc - 1L]))
}

next_with_parent <- function(pc, loc) {

  id <- pc$id[loc]
  parent_id <- pc$parent[loc]

  with_parent <- pc[pc$parent == parent_id,]
  with_parent <- with_parent[order(with_parent$line1, with_parent$col1, with_parent$line2, with_parent$col2),]

  loc <- which(with_parent$id == id)

  return(which(pc$id == with_parent$id[loc + 1L]))
}

top_level_expressions <- function(pc) {
  if (is.null(pc)) {
    return(NULL)
  }
  which(pc$parent == 0L & pc$token == "expr")
}
