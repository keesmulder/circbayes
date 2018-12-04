force_neg_pi_pi <- function(x) {
  ((x + pi) %% (2*pi)) - pi
}


circular_to_circrad <- function(x) {

  xrad <- circular::conversion.circular(x, units = "radians")

  attributes(xrad) <- NULL

  circrad(xrad)
}


circrad <- function(x) {
  out <- force_neg_pi_pi(x)
  class(out) <- c("circrad", "numeric")
  out
}



as.circrad <- function(x) {

  # Special treatment for 'circular' objects
  if (inherits(x, "circular")) {
    return(circular_to_circrad(x))
  } else {
    return(circrad(as.numeric(x)))
  }
}


is.circrad <- function(x) inherits(x, "circrad")



resultant_length <- function(x) {
  sqrt(sum(sin(x))^2 + sum(cos(x))^2)/length(x)
}

mean.circrad <- function(x, ...) {
  circrad(atan2(sum(sin(x)), sum(cos(x))))
}

var.circrad <- function(x, ...) {
  1 - resultant_length(x)
}

sd.circrad <- function(x, ...) {
  sqrt(-2 * log(resultant_length(x)))
}

