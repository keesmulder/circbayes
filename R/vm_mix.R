#' Random generation for the Inverse Batschelet mixture distribution.
#'
#' @param n Number of values to sample.
#' @param mus Mean directions.
#' @param kps Concentration parameters.
#' @param lams Lambdas, peakedness parameter, between -1 and 1. Positive values
#'   give a peaked density, while negative values give flat-topped densities. .
#' @param alphs Mixture weights.
#'
#' @return Numeric vector of \code{n} samples from the Inverse Batschelet
#'   distribution, in radians.
#' @export
#'
#' @examples
#' hist(rvmmix(1000), breaks = 100)
#'
rvmmix <- function(n, mus = -1:1, kps = c(50, 25, 10), alphs = c(.2, .2, .6)) {
  circrad(flexcircmix::rinvbatmix(n, mus, kps,
                                  lams = rep(0, length(mus)), alphs))
}


print.vm_mix_mod <- function(x, digits = 3, ...) {
  print(round(coef(x), digits))
}


coef.vm_mix_mod <- coefficients.vm_mix_mod <- function(x, ...) {
  coef_mat <- x$mcmc_summary
  coef_mat
}


posterior_samples.vm_mix_mod <- function(x) {
  mat <- x$mcmc_sample[, 1:(x$n_components * 4)]
  mat
}


plot.vm_mix_mod <- function(x, ...) {
  mu_names    <- x$mu_names
  kp_names    <- x$kp_names
  lam_names   <- x$lam_names
  alph_names  <- x$alph_names

  if (x$bat_type == "inverse") {
    pdf_fun <- function(x, params) flexcircmix::dbatmix(x, dbat_fun = dinvbat,
                                                        mus   = params[mu_names  ],
                                                        kps   = params[kp_names  ],
                                                        lams  = params[lam_names ],
                                                        alphs = params[alph_names])
  } else if (x$bat_type == "power") {
    pdf_fun <- function(x, params) flexcircmix::dbatmix(x, dbat_fun = dpowbat,
                                                        mus   = params[mu_names  ],
                                                        kps   = params[kp_names  ],
                                                        lams  = params[lam_names ],
                                                        alphs = params[alph_names])
  }

  plot_circbayes_univariate(x, pdf_fun = pdf_fun, ...)
}




#' @importFrom bridgesampling bridge_sampler
bridge_sampler.vm_mix_mod <- function(samples, ...) {

    if (samples$method != "bayes") {
      stop("Bridge sampling is only possible for method =='bayes'.")
    }

    n_comp <- samples$n_components

    sam <- samples$mcmc_sample[, 1:(4*n_comp)]

    lb <- rep(c(-2*pi, 0, -1, 0), each = n_comp)
    ub <- rep(c(2*pi, Inf, 1, 1), each = n_comp)

    names(lb) <- colnames(sam)
    names(ub) <- colnames(sam)

    bridgesampling::bridge_sampler(data = samples$x,
                                   samples = as.matrix(sam),
                                   param_types = rep(c("circular", "real", "real", "simplex"), each = n_comp),
                                   log_posterior = samples$log_posterior,
                                   lb = lb, ub = ub,
                                   silent = TRUE, ...)
}

marg_lik.vm_mix_mod <- function(x, ...) {

  bsobj <- bridgesampling::bridge_sampler(samples = x,
                                          ...)

  bridgesampling::logml(bsobj)
}


inf_crit.vm_mix_mod <- function(x, ...) {
  x$ic_mat
}


#' Posterior of mixture of Von Mises distributions.
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
#' vm_mix(rvm(30, 2, 5))
#'
vm_mix <- function(th,
                    bat_type = "power",
                    n_comp = 3,
                    mu_logprior_fun  = function(mu)  0,
                    kp_logprior_fun  = function(kp)  0,
                    lam_logprior_fun = function(lam) 0,
                    alph_prior_param = rep(1, n_comp),
                    fixed_pmat       = matrix(NA, n_comp, 4),
                    niter = 1000, ...) {

  th <- as.circrad(th)

  # Make sure that lam is fixed to zero so we run von Mises mixture model.
  if (ncol(fixed_pmat) == 3) {
    fixed_pmat <- cbind(fixed_pmat[, 1:2, drop = FALSE],
                        0,
                        fixed_pmat[, 3, drop = FALSE])
  } else if (ncol(fixed_pmat) == 4) {
    fixed_pmat[, 3] <- 0
  } else {
    stop(paste("Fixed parameter matrix fixed_pmat of wrong dimension:",
               dim(fixed_pmat)))
  }


  # Run  Von Mises mixture model.
  res <- flexcircmix::fitbatmix(x = th, method = "bayes", Q = niter,
                                n_comp = n_comp,
                                bat_type = bat_type,
                                fixed_pmat = fixed_pmat,
                                mu_logprior_fun  = mu_logprior_fun,
                                kp_logprior_fun  = kp_logprior_fun,
                                lam_logprior_fun = lam_logprior_fun, ...)

  coef_vmmix <- coef(res)

  # dbat_fun <- ifelse(bat_type == "inverse", dinvbat, dpowbat)



  vmmix_res <- c(list(coef = coef_vmmix),
                   res,
                   list(data = res$x))

  vmmix_res$mu_names   <- paste0("mu_", seq(n_comp))
  vmmix_res$kp_names   <- paste0("kp_", seq(n_comp))
  vmmix_res$lam_names  <- paste0("lam_", seq(n_comp))
  vmmix_res$alph_names <- paste0("alph_", seq(n_comp))

  class(vmmix_res) <- c("vm_mix_mod", class(res))

  vmmix_res
}
