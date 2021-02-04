complexity <- function(x) {
  if (x > 0) {
    if (x > 10) {
      if (x > 20) {
        x <- x / 2
      } else {
        return(x)
      }
    } else {
      return(x)
    }
  } else {
    if (x < -10) {
      if (x < -20) {
        x <- x * 2
      } else {
        return(x)
      }
    } else {
      return(x)
    }
  }
  x
}
