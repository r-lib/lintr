style_diff <- function(file, number, message, line) {
  structure(list(file=file, number=number, message=message, line=line), class="style_diff")
}

print.style_diff <- function(x, ...) {
  cat(sep="", x$file, ":", x$number, " ", x$message, ": ", x$line, "\n")
}
