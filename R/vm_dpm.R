#' Bayesian inference for Dirichlet Process Mixture of Von Mises distributions.
#'
#' @param th Circular observations, either \code{numeric} in radians, or
#'   \code{circular}.
#' @param niter Number of iterations to perform MCMC for.
#' @param ... Further arguments passed to \code{circglmbayes::fitbatmix}.
#'
#' @return Object of type \code{vm_dpm_mod}.
#' @export
#'
#' @examples
#' vm_dpm(rvm(30, 2, 5))
#'
vm_dpm <- function(th,
                   g0Priors = c(0, 0, 1),
                   alphaPriors = c(2, 4),
                   niter = 1000, ...) {

  th <- as.circrad(th)

  # Run Von Mises Dirichlet Process Mixture model.
  dp_obj <- DirichletProcessVonMises(y = th,
                                     g0Priors = g0Priors,
                                     alphaPriors = alphaPriors, ...)
  dp_obj <- dirichletprocess::Fit(dpObj = dp_obj, its = niter)

  dp_obj$niter  <- niter
  class(dp_obj) <- c("vm_dpm_mod", class(dp_obj))

  dp_obj
}


#' @export
print.vm_dpm_mod <- function(x, digits = 3, ...) {
  NextMethod()
}


#' @export
coef.vm_dpm_mod <- coefficients.vm_dpm_mod <- function(x, ...) {
  NextMethod()
}


#' @export
posterior_samples.vm_dpm_mod <- function(x) {
  NextMethod()
}


#' @export
plot.vm_dpm_mod <- function(x, ...) {
  plot_circbayes_dpm(x, ...)
}





