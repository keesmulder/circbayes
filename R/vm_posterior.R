#' Posterior of the von Mises distribution.
#'
#' @param th Circular observations, either \code{numeric} in radians, or
#'   \code{circular}.
#' @param prior Numeric of length 3. Prior parameters for conjugate prior of the
#'   von Mises distribution. The order is \eqn{\mu_0, R_0, c}.
#' @param niter Number of iterations to perform MCMC for.
#' @param ... Further arguments passed to \code{circglmbayes::circGLM}.
#'
#' @return Object of type \code{vm_posterior_mod}.
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


  post_params <- conj_posterior_vm(th, prior)

  vmpost_res <- list(coef          = coef_vmpost,
                     mu_estimate   = res$b0_meandir,
                     kp_mean       = res$kp_mean,
                     kp_mode       = res$kp_mode,
                     kp_propacc    = res$kp_propacc,
                     mu_chain      = res$b0_chain,
                     kp_chain      = res$kp_chain,
                     lppd          = res$lppd,
                     data          = res$data_th,
                     prior         = prior,
                     post_params   = post_params,
                     log_posterior = log_posterior_vm,
                     estimates     = c(res$b0_meandir, res$kp_mode),
                     AIC_Bayes     = res$AIC_Bayes,
                     p_DIC         = res$p_DIC,
                     p_DIC_alt     = res$p_DIC_alt,
                     DIC           = res$DIC,
                     DIC_alt       = res$DIC_alt,
                     p_WAIC1       = res$p_WAIC1,
                     p_WAIC2       = res$p_WAIC2,
                     WAIC1         = res$WAIC1,
                     WAIC2         = res$WAIC2,
                     SavedIts      = res$SavedIts,
                     TotalIts      = res$TotalIts,
                     thin          = res$thin,
                     burnin        = res$burnin,
                     timeTaken     = res$TimeTaken
  )




  class(vmpost_res) <- c("vm_posterior_mod", class(vmpost_res))

  vmpost_res
}




#' Von Mises distribution
#'
#' Random generation and probability density function for the von Mises
#' distribution. For further von Mises function, see
#' \code{\link[circular:vonMises]{pvonmises}} and \code{\link[circular:vonMises]{qvonmises}} in
#' package \pkg{circular}.
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


#' @export
#' @describeIn rvm Probability density function.
dvm <- function(x, mu = 0, kp = 1, log = FALSE) {
  logp <- kp * cos(x - mu) - log(2 * pi) - logBesselI(kp, 0)

  if (log) {
    return(logp)
  } else {
    return(exp(logp))
  }
}

conj_posterior_vm <- function(th, prior = c(0, 0, 0)) {
  mu_0 <- prior[1]
  R_0  <- prior[2]
  n_0  <- prior[3]

  C_n <- R_0 * cos(mu_0) + sum(cos(th))
  S_n <- R_0 * sin(mu_0) + sum(sin(th))

  mu_n <- atan2(S_n, C_n)
  R_n <- sqrt(C_n^2 + S_n^2)
  n_n <- length(th) + n_0

  return(c(mu_n, R_n, n_n))
}


# For this log posterior, the data are the summary statistics.
log_posterior_vm <- function(params, data) {
  data[2] * params[2] * cos(params[1] - data[1]) -
    data[3] * (logBesselI(params[2]) + log(2*pi))
}





#' @export
#' @describeIn vm_posterior Print function.
print.vm_posterior_mod <- function(x, digits = 3, ...) {
  print(round(coef(x), digits))
}


#' @importFrom stats coef
#' @export
#' @describeIn vm_posterior Posterior summary of parameters.
coef.vm_posterior_mod <- coefficients.vm_posterior_mod <- function(x, ...) {
  coef_mat <- x$coef
  colnames(coef_mat) <- c("estimate", "se", "2.5%", "97.5%")
  coef_mat
}


#' @export
#' @describeIn vm_posterior Posterior samples (only model parameters, not derived).
posterior_samples.vm_posterior_mod <- function(x) {
  sam <- cbind(x$mu_chain,
               x$kp_chain)
  colnames(sam) <- c("mu", "kappa")
  sam
}

#' @export
#' @describeIn vm_posterior Compute marginal likelihood.
marg_lik.vm_posterior_mod <- function(x, method = "integrate", ...) {

  if (method == "integrate") {

    post_param <- conj_posterior_vm(x$data, x$prior)

    R_n <- post_param[2]
    m   <- post_param[3]

    marg_post_vm <- function(kp) {
      exp((1 - m) * log(2*pi) + logBesselI(R_n * kp, 0) - m * logBesselI(kp, 0))
    }

    return(c(log_marg_lik = log(integrate(marg_post_vm, 0, Inf)$value)))

  } else if (method == "bridgesampling") {


    sam <- posterior_samples(x)

    lb <- c(-2*pi, 0)
    ub <- c(2*pi, Inf)

    names(lb) <- colnames(sam)
    names(ub) <- colnames(sam)

    bsobj <- bridgesampling::bridge_sampler(data = x$post_params,
                                            samples = as.matrix(sam),
                                            param_types = c("circular", "real"),
                                            log_posterior = log_posterior_vm,
                                            lb = lb, ub = ub,
                                            silent = TRUE, ...)
    return(c(log_marg_lik = bridgesampling::logml(bsobj)))
  } else {
    stop(paste("Method", method, "not found."))
  }
}



#' @export
#' @describeIn vm_posterior Plot function.
plot.vm_posterior_mod <- function(x, ...) {
  plot_circbayes_univariate(x, ...)
}


