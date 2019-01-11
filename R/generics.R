

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




