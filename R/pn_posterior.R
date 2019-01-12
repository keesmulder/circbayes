
#' Random generation for the Projected Normal distribution.
#'
#' @param n Number of values to sample.
#' @param mu Mean direction.
#' @param kp Concentration parameter.
#'
#' @return Numeric vector of \code{n} samples from the Projected Normal disttribution,
#'   in radians.
#' @export
#'
#' @examples
#' rpn(40, 3, 2)
#'
rprojnorm <- function(n, muvec = c(1, 1)) {
  if (is.vector(muvec)) muvec <- t(muvec)
  th <- atan2(rnorm(n, muvec[, 2], 1), rnorm(n, muvec[, 1], 1))
  circrad(force_neg_pi_pi(th))
}


# dprojnorm not found in package.


print.pn_posterior_mod <- function(x, digits = 3, ...) {
  print(round(coef(x), digits))
}


coef.pn_posterior_mod <- coefficients.pn_posterior_mod <- function(x, ...) {
  x$coef
}


posterior_samples.pn_posterior_mod <- function(x) {
  cbind(mu1 = x$B1,
        mu2 = x$B2)
}

#
# plot.pn_posterior_mod <- function(x, ...) {
#   plot_circbayes_univariate(x, ...)
# }




# This is the fall-back generic. If this doesn't work, a custom method must be
# written.
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


#' Posterior of the Projected Normal distribution.
#'
#' @param th Circular observations, either \code{numeric} in radians, or
#'   \code{circular}.
#' @param niter Number of iterations to perform MCMC for.
#' @param ... Further arguments passed to \code{bpnreg::bpnr}.
#'
#' @return
#' @export
#'
#' @examples
#' pn_posterior(rpn(30, 2, 5))
#'
pn_posterior <- function(th, niter = 1000, thin = 1, ...) {

  df <- data.frame(th = as.circrad(th))

  # Run intercept-only Projected Normal regression model
  res <- bpnreg::bpnr(pred.I = th ~ 1, data = df, pred.II = th ~ 1,
                      its = niter, n.lag = thin, ...)

  coef_pnpost <- bpnreg::coef_lin(res)
  rownames(coef_pnpost) <- c("mu1", "mu2")
  res[["coef"]] <- coef_pnpost

  class(res) <- c("pn_posterior_mod", class(res))

  res
}
