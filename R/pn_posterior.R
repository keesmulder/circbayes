#' Posterior of the Projected Normal distribution.
#'
#' @param th Circular observations, either \code{numeric} in radians, or
#'   \code{circular}.
#' @param niter Number of iterations to perform MCMC for.
#' @param ... Further arguments passed to \code{bpnreg::bpnr}.
#' @param thin Integer; Factor of thinning. Setting \code{thin = 1} provides no
#'   thinning.
#' @param burnin Integer; Number of iterations to burn.
#'
#' @return Object of type \code{pn_posterior_mod}.
#' @export
#'
#' @examples
#' pn_posterior(rprojnorm(30, 2, 5))
#'
pn_posterior <- function(th, niter = 1000, thin = 1, burnin = 0, ...) {

  df <- data.frame(th = as.circrad(th))

  # Run intercept-only Projected Normal regression model
  res <- bpnreg::bpnr(pred.I = th ~ 1, data = df, pred.II = th ~ 1,
                      its = niter, n.lag = thin, burn = burnin, ...)

  coef_pnpost <- bpnreg::coef_lin(res)
  rownames(coef_pnpost) <- c("mu1", "mu2")
  res$coef <- coef_pnpost
  res$data <- th
  res$estimates <- coef_pnpost[, 2L]
  res$log_posterior <- log_posterior_pn

  class(res) <- c("pn_posterior_mod", class(res))

  res
}

#' Random generation for the Projected Normal distribution.
#'
#' @param n Number of values to sample.
#' @param mu1 Mean vector component 1.
#' @param mu2 Mean vector component 2.
#'
#'
#'
#' @return Numeric vector of \code{n} samples from the Projected Normal disttribution,
#'   in radians.
#' @export
#'
#' @importFrom stats rnorm
#'
#' @examples
#' rprojnorm(40, 3, 2)
#'
rprojnorm <- function(n, mu1 = 1, mu2 = 1) {
  muvec <- cbind(mu1, mu2)
  th <- atan2(rnorm(n, muvec[, 2], 1), rnorm(n, muvec[, 1], 1))
  circrad(force_neg_pi_pi(th))
}

#' @param x Angle in radians for which to evaluate the probability density.
#' @param log Logical; whether to return the log probability density.
#'
#' @importFrom stats pnorm
#' @importFrom stats dnorm
#' @describeIn rprojnorm Probability density function.
dprojnorm <- Vectorize(function(x, mu1 = 1, mu2 = 1, log = FALSE) {
  muvec <- c(mu1, mu2)
  u    <- c(cos(x), sin(x))
  utmu <- t(u) %*% muvec
  logp <- log(1 + utmu * pnorm(utmu) / dnorm(utmu)) -
    log(2*pi) -
    (muvec[1]^2 + muvec[2]^2) / 2

  if (log) {
    return(logp)
  } else {
    return(exp(logp))
  }
})


#' @export
print.pn_posterior_mod <- function(x, digits = 3, ...) {
  print(round(coef(x), digits))
}


#' @export
coef.pn_posterior_mod <- coefficients.pn_posterior_mod <- function(object, ...) {
  coef_mat <- object$coef
  colnames(coef_mat) <- c("mean", "mode", "se", "2.5%", "97.5%")
  coef_mat
}


posterior_samples.pn_posterior_mod <- function(object, ...) {
  cbind(mu1 = as.numeric(object$B1),
        mu2 = as.numeric(object$B2))
}

#' @export
plot.pn_posterior_mod <- function(x, ...) {
  plot_circbayes_univariate(x, pdf_fun = dprojnorm, ...)
}


# Log posterior of pn.
log_posterior_pn <- function(muvec, data) {
  sum(dprojnorm(data, muvec[1], muvec[2], log = TRUE))
}


#' @export
marg_lik.pn_posterior_mod <- function(x, ...) {

  sam <- posterior_samples(x)

  lb <- c(-Inf, -Inf)
  ub <- c(Inf, Inf)

  names(lb) <- colnames(sam)
  names(ub) <- colnames(sam)

  bsobj <- bridgesampling::bridge_sampler(data = x$data,
                                          samples = as.matrix(sam),
                                          param_types = c("real", "real"),
                                          log_posterior = log_posterior_pn,
                                          lb = lb, ub = ub,
                                          silent = TRUE,
                                          ...)

  bridgesampling::logml(bsobj)
}




#' @export
inf_crit.pn_posterior_mod <- function(x, ...) {
  ics <- x$model.fit
  if (all(vapply(ics, length, FUN.VALUE = 0) == 1)) {
    ic_df <- t(data.frame(ics))
    colnames(ic_df) <- "value"
    return(ic_df)
  } else {
    return(ics)
  }
}




