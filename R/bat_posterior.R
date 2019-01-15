#' Posterior of the Power or Inverse Batschelet distribution.
#'
#' @param th Circular observations, either \code{numeric} in radians, or
#'   \code{circular}.
#' @param niter Number of iterations to perform MCMC for.
#' @param ... Further arguments passed to \code{circglmbayes::fitbatmix}.
#'
#' @return Object of type \code{bat_posterior_mod}.
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

  dbat_fun <- ifelse(bat_type == "inverse", dinvbat, dpowbat)

  log_posterior <- function(pvec, data) {

    mu   <- pvec[1]
    kp   <- pvec[2]
    lam  <- pvec[3]

    ll_part    <- sum(dbat_fun(data, mu, kp, lam, log = TRUE))
    prior_part <- sum(c(vapply(mu,   mu_logprior_fun,  0),
                        vapply(kp,   kp_logprior_fun,  0),
                        vapply(lam,  lam_logprior_fun, 0)))

    ll_part + prior_part
  }

  # Set the environment of the log posterior function.
  log_post_env <- new.env()
  log_post_env$dbat_fun         <- dbat_fun
  log_post_env$mu_logprior_fun  <- mu_logprior_fun
  log_post_env$kp_logprior_fun  <- kp_logprior_fun
  log_post_env$lam_logprior_fun <- lam_logprior_fun
  environment(log_posterior) <- log_post_env


  batpost_res <- c(list(coef = coef_batpost),
                   res,
                   list(data = res$x))

  batpost_res$log_posterior <- log_posterior

  class(batpost_res) <- c("bat_posterior_mod", class(res))

  batpost_res
}


#' Inverse Batschelet distribution
#'
#' Random generation and probability for the Inverse Batschelet Distribution.
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

#' @describeIn rinvbat Density function for Inverse Batschelet.
#' @export
dinvbat <- function(x, mu = 0, kp = 1, lam = 0, log = FALSE) {
  flexcircmix::dinvbat(x, mu, kp, lam, log)
}

#' @describeIn rinvbat Density function for Power Batschelet.
#' @export
dpowbat <- function(x, mu = 0, kp = 1, lam = 0, log = FALSE) {
  flexcircmix::dpowbat(x, mu, kp, lam, log)
}


#' @export
print.bat_posterior_mod <- function(x, digits = 3, ...) {
  print(round(coef(x), digits))
}


#' @export
coef.bat_posterior_mod <- coefficients.bat_posterior_mod <- function(x, ...) {
  coef_mat <- x$mcmc_summary[-4, ]
  rownames(coef_mat) <- c("mu", "kp", "lam", "circ_variance", "circ_sd")
  coef_mat
}


#' @export
posterior_samples.bat_posterior_mod <- function(x) {
  mat <- x$mcmc_sample[, 1:3]
  colnames(mat) <- c("mu", "kp", "lam")

  mat
}


#' @export
plot.bat_posterior_mod <- function(x, ...) {
  if (x$bat_type == "inverse") {
    pdf_fun <- dinvbat
  } else if (x$bat_type == "power") {
    pdf_fun <- dpowbat
  }

  plot_circbayes_univariate(x, pdf_fun = pdf_fun, ...)
}


#' @export
marg_lik.bat_posterior_mod <- function(x, ...) {

  sam <- posterior_samples(x)

  lb <- c(-2*pi, 0, -1)
  ub <- c(2*pi, Inf, 1)

  names(lb) <- colnames(sam)
  names(ub) <- colnames(sam)

  bsobj <- bridgesampling::bridge_sampler(data = x$data,
                                          samples = as.matrix(sam),
                                          param_types = c("circular", "real", "real"),
                                          log_posterior = x$log_posterior,
                                          lb = lb, ub = ub,
                                          silent = TRUE,
                                          ...)

  bridgesampling::logml(bsobj)
}


#' @export
inf_crit.bat_posterior_mod <- function(x, ...) {
  x$ic_mat
}


