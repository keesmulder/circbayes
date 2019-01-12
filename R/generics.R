

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




#' Extract information criteria from fit model object.
#'
#' @param x Fit model object.
#' @param ... Further arguments.
#'
#' @return A dataframe or list of information criteria.
#' @export
#'
inf_crit <- function(x, ...) {
  UseMethod("inf_crit", x)
}


# This is the fall-back generic. If this doesn't work, a custom method must be
# written.
inf_crit.list <- function(x, ...) {
  nms <- names(x)
  ics <- x[grep("IC", nms, value = TRUE)]
  if (all(vapply(ics, length, FUN.VALUE = 0) == 1)) {
    ic_df <- t(data.frame(ics))
    colnames(ic_df) <- "value"
    return(ic_df)
  } else {
    return(ics)
  }
}