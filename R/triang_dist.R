dtriang <- function(x, min, max, mode) {

  if (class(x) != "numeric") {
    stop("x must be a numeric vector or a number.")
  }
  if (min > max) {
    stop("min can't be greater than max")
  }

  if (mode < min | mode > max) {
    stop("mode must be between min and max.")
  }

  total_area <- (mode - min)/(max-min) + (max - mode)/(max-min)

  if (x < min | x > max) {
    resultado <- 0
    return(resultado)
  }

  if (x <= mode) {
    resultado <-  2 * (x - min) / ((max - min) * (mode - min))
  } else {
    resultado <- 2 * (max - x) / ((max - min) * (max - mode))
  }

  return(resultado)
}
