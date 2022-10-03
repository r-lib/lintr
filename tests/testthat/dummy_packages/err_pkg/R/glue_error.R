# Regression test for #1459: syntax errors in code get helpful failure
foo <- function() {
  remote_groups <- 1
  message(glue::glue("Groups found: {glue::glue_collapse(purrr::map_chr(remote_groups$results, ~ .x$name), sep = \", \")"))
}

