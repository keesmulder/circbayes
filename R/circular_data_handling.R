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


#' Print a \code{circrad} object.
#'
#' @param x Object.
#' @param ... Further arguments passed to \code{print.numeric}
#'
#' @export
#'
#' @examples
#' print(circrad(pi/2))
#'
print.circrad <- function(x, ...) {
  cat("# Circular data in radians: \n")
  print(as.numeric(x), ...)
}



as.circrad <- function(x) {

  # Special treatment for 'circular' objects
  if (inherits(x, "circular")) {
    return(circular_to_circrad(x))
  }

  if (inherits(x, "data.frame") || inherits(x, "matrix")) {
    x <- x[, 1]
  }

  if (diff(range(x)) > 2 * pi) {
    warning(paste("Range of input larger than 2 * pi.",
                  "Make sure the angles are either radians or entered as",
                  "`circular` objects."))
  }
  return(circrad(as.numeric(x)))
}


is.circrad <- function(x) inherits(x, "circrad")



resultant_length <- function(x) {
  sqrt(sum(sin(x))^2 + sum(cos(x))^2)/length(x)
}

mean.circrad <- function(x, ...) {
  S <- sum(sin(x))
  C <- sum(cos(x))

  if (S == 0 && C == 0) return(NA)

  circrad(atan2(S, C))
}

var.circrad <- function(x, ...) {
  1 - resultant_length(x)
}

sd.circrad <- function(x, ...) {
  sqrt(-2 * log(resultant_length(x)))
}

