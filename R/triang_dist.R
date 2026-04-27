dtriang <- function(x, min, max, mode) {

  if (class(x) != "numeric") {
    stop("x must be a numeric vector or a number.")
  }
  if (min > max) {
    stop("min can't be greater than max")
  }

  if (mode < min || mode > max) {
    stop("mode must be between min and max.")
  }

  if (x < min || x > max) {
    result <- 0
    return(result)
  }

  if (x <= mode) {
    result <-  2 * (x - min) / ((max - min) * (mode - min))
  } else {
    result <- 2 * (max - x) / ((max - min) * (max - mode))
  }

  result
}

#' Triangular density
#'
#' @param x numeric vector
#' @param min left limit
#' @param max right limit
#' @param mode mode
#' @return PDF result
#' @export

ptriang <- function(q, min, max, mode) {
  if (class(q) != "numeric") {
    stop("x must be a numeric vector or a number.")
  }
  if (min > max) {
    stop("min can't be greater than max")
  }

  if (mode < min || mode > max) {
    stop("mode must be between min and max.")
  }

  if (q <= min) {
    result <- 0
    return(result)
  }
  if (q >= max) {
    result <- 1
    return(result)
  }
  if (q <= mode) {
    result <- (q - min)^2 / ((max - min) * (mode - min))
  }
  if (q >= mode) {
    result <- 1 - (max - q)^2 / ((max - min) * (max - mode))
  }
  result
}
