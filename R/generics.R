

#' Generic method to extract posterior (MCMC) samples from a Bayesian model fit
#' object.
#'
#' @param object Object to extract samples from.
#' @param ... Further arguments.
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


#' Obtain the prediction function of a regression object.
#'
#' @param object Object.
#' @param ... Further arguments.
#'
#' @return A function.
#' @export
#'
predict_function <- function(object, ...) {
  UseMethod("predict_function", object)
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



#' Compute the Marginal likelihood of circular Bayesian models.
#'
#' For most models in \code{circbayes}, this uses \code{\link[bridgesampling]{bridge_sampler}}.
#'
#' @param x Fit model object.
#' @param ... Further arguments.
#'
#' @return Numeric; the log marginal likelihood.
#' @export
#'
#' @examples
#' th <- rvm(40, 5, 8)
#' vm_mod  <- vm_posterior(th)
#' pn_mod  <- pn_posterior(th)
#' bat_mod <- bat_posterior(th)
#' mlvm <- marg_lik(vm_mod)
#' mlpn <- marg_lik(pn_mod)
#' mlbt <- marg_lik(bat_mod)
#'
#' bht_compare(mlvm, mlpn, mlbt)
#'
marg_lik <- function(x, ...) {
  UseMethod("marg_lik")
}


# This is the fall-back generic. If this doesn't work, a custom method must be
# written.
#' @export
inf_crit.list <- function(x, ...) {
  nms <- names(x)
  ic_nms <- sort(grep("IC", nms, value = TRUE))
  ics <- x[ic_nms]
  if (all(vapply(ics, length, FUN.VALUE = 0) == 1)) {
    ic_df <- t(data.frame(ics))
    colnames(ic_df) <- "value"
    return(ic_df)
  } else {
    return(ics)
  }
}