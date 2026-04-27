dtriang <- function(x, min, max, mode) {
  if (length(x) != 1 & length(x) != 2) {
    stop("vector must be length 2.")
  }

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
  }

  return(resultado)
}
