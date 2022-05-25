complexity <- function(x) {
  if (x > 0.0) {
    if (x > 10.0) {
      if (x > 20.0) {
        x <- x / 2.0
      } else {
        return(x)
      }
    } else {
      return(x)
    }
  } else {
    if (x < -10.0) {
      if (x < -20.0) {
        x <- x * 2.0
      } else {
        return(x)
      }
    } else {
      return(x)
    }
  }
  x
}
