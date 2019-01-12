
#' Random generation for the Inverse Batschelet distribution.
#'
#' @param n Number of values to sample.
#' @param mu Mean direction.
#' @param kp Concentration parameter.
#' @param lam Lambda, peakedness parameter, between -1 and 1. Positive values
#'   give a peaked density, while negative values give flat-topped densities. .
#'
#' @return Numeric vector of \code{n} samples from the Inverse Batschelet
#'   distribution, in radians.
#' @export
#'
#' @examples
#' hist(rinvbat(1000, .3, 2, .8), breaks = 100)
#'
rinvbat <- function(n, mu = 0, kp = 1, lam = 0) {
  circrad(flexcircmix::rinvbat(n, mu, kp, lam))
}

#' @rdname rinvbat Density function for Inverse Batschelet.
dinvbat <- function(x, mu = 0, kp = 1, lam = 0, log = FALSE) {
  flexcircmix::dinvbat(x, mu, kp, lam, log)
}

#' @rdname rinvbat Density function for Power Batschelet.
dpowbat <- function(x, mu = 0, kp = 1, lam = 0, log = FALSE) {
  flexcircmix::dpowbat(x, mu, kp, lam, log)
}


print.bat_posterior_mod <- function(x, digits = 3, ...) {
  print(round(coef(x), digits))
}


coef.bat_posterior_mod <- coefficients.bat_posterior_mod <- function(x, ...) {
  coef_mat <- x$mcmc_summary[-4, ]
  rownames(coef_mat) <- c("mu", "kp", "lam", "circ_variance", "circ_sd")

  coef_mat
}


posterior_samples.bat_posterior_mod <- function(x) {
  mat <- x$mcmc_sample[, 1:3]
  colnames(mat) <- c("mu", "kp", "lam")

  mat
}


plot.bat_posterior_mod <- function(x, ...) {
  if (x$bat_type == "inverse") {
    pdf_fun <- dinvbat
  } else if (x$bat_type == "power") {
    pdf_fun <- dpowbat
  }

  plot_circbayes_univariate(x, pdf_fun = pdf_fun, ...)
}


#' Posterior of the Power or Inverse Batschelet distribution.
#'
#' @param th Circular observations, either \code{numeric} in radians, or
#'   \code{circular}.
#' @param niter Number of iterations to perform MCMC for.
#' @param ... Further arguments passed to \code{circglmbayes::fitbatmix}.
#'
#' @return
#' @export
#'
#' @examples
#' bat_posterior(rvm(30, 2, 5))
#'
bat_posterior <- function(th,
                         bat_type = "power",
                         mu_logprior_fun  = function(mu)  0,
                         kp_logprior_fun  = function(kp)  0,
                         lam_logprior_fun = function(lam) 0,
                         niter = 1000, ...) {

  th <- as.circrad(th)

  # Run single component Power or Inverse Batschelet mixture model.
  res <- flexcircmix::fitbatmix(x = th, method = "bayes", Q = niter, n_comp = 1,
                                bat_type = bat_type,
                                mu_logprior_fun  = mu_logprior_fun,
                                kp_logprior_fun  = kp_logprior_fun,
                                lam_logprior_fun = lam_logprior_fun, ...)

  coef_batpost <- coef(res)

  batpost_res <- c(list(coef = coef_batpost),
                   res,
                   list(data = res$x))

  class(batpost_res) <- c("bat_posterior_mod", class(batpost_res))

  batpost_res
}
