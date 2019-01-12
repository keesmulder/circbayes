

#' Generic method to extract posterior (MCMC) samples from a Bayesian model fit
#' object.
#'
#' @param object Object to extract samples from.
#' @param ...
#'
#' @return A matrix of posterior samples.
#' @export
#'
#' @examples
#' mod <- vm_posterior(rvm(30, 2, 5), niter = 30)
#' posterior_samples(mod)
posterior_samples <- function(object, ...)  {
  UseMethod("posterior_samples", object)
}


#' Obtain the prediction function of a VM regression object.
#'
#' @param x Object.
#' @param ... Further arguments.
#'
#' @return A function
#' @export
#'
predict_function <- function(x, ...) {
  UseMethod("predict_function", x)
}

