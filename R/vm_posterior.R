
#' Random generation for the von Mises distribution.
#'
#' @param n Number of values to sample.
#' @param mu Mean direction.
#' @param kp Concentration parameter.
#'
#' @return Numeric vector of \code{n} samples from the von Mises disttribution,
#'   in radians.
#' @export
#'
#' @examples
#' rvm(40, 3, 2)
#'
rvm <- function(n, mu = 0, kp = 1) {
  circrad(circglmbayes::rvmc(n, mu, kp))
}


logBesselI <- function(x, nu = 0) {
  x + log(besselI(x, nu, expon.scaled = TRUE))
}


dvm <- function(x, mu = 0, kp = 1, log = FALSE) {
  logp <- kp * cos(x - mu) - log(2 * pi) - logBesselI(kp, 0)

  if (log) {
    return(logp)
  } else {
    return(exp(logp))
  }
}


print.vm_posterior_mod <- function(x, digits = 3, ...) {
  print(round(coef(x), digits))
}


coef.vm_posterior_mod <- coefficients.vm_posterior_mod <- function(x, ...) {
  x$coef
}


posterior_samples.vm_posterior_mod <- function(x) {
  cbind(mu = x$mu_chain,
        kp = x$kp_chain)
}


plot.vm_posterior_mod <- function(x, ...) {
  plot_circbayes_univariate(x, ...)
}


#' Posterior of the von Mises distribution.
#'
#' @param th Circular observations, either \code{numeric} in radians, or
#'   \code{circular}.
#' @param prior Numeric of length 3. Prior parameters for conjugate prior of the
#'   von Mises distribution. The order is \eqn{\mu_0, R_0, c}.
#' @param niter Number of iterations to perform MCMC for.
#' @param ... Further arguments passed to \code{circglmbayes::circGLM}.
#'
#' @return
#' @export
#'
#' @examples
#' vm_posterior(rvm(30, 2, 5))
#'
vm_posterior <- function(th, prior = c(0, 0, 0), niter = 1000, ...) {

  th <- as.circrad(th)

  # Run intercept-only von Mises regression model
  res <- circglmbayes::circGLM(th = th, conj_prior = prior,
                               Q = niter, ...)

  coef_vmpost <- coef(res)
  rownames(coef_vmpost) <- c("mu", "kappa")


  vmpost_res <- list(coef        = coef_vmpost,
                     mu_estimate = res$b0_meandir,
                     kp_mean     = res$kp_mean,
                     kp_mode     = res$kp_mode,
                     kp_propacc  = res$kp_propacc,
                     mu_chain    = res$b0_chain,
                     kp_chain    = res$kp_chain,
                     lppd        = res$lppd,
                     data        = res$data_th,
                     AIC_Bayes   = res$AIC_Bayes,
                     p_DIC       = res$p_DIC,
                     p_DIC_alt   = res$p_DIC_alt,
                     DIC         = res$DIC,
                     DIC_alt     = res$DIC_alt,
                     p_WAIC1     = res$p_WAIC1,
                     p_WAIC2     = res$p_WAIC2,
                     WAIC1       = res$WAIC1,
                     WAIC2       = res$WAIC2,
                     SavedIts    = res$SavedIts,
                     TotalIts    = res$TotalIts,
                     thin        = res$thin,
                     burnin      = res$burnin,
                     timeTaken   = res$TimeTaken
  )

  class(vmpost_res) <- c("vm_posterior_mod", class(vmpost_res))

  vmpost_res
}
