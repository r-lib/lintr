`%||%` <- function(x, y) {
  if(!is.null(x)) { x } else { y }
}

`%==%` <- function(x, y) {
  identical(x, y)
}
`%!=%` <- function(x, y) {
  !identical(x, y)
}
