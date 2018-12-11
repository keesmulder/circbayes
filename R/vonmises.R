
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




print.vonmises_mcmc <- function(x, digits = 3, ...) {
  print(round(coef(x), digits))
}

coef.vonmises_mcmc <- coefficients.vonmises_mcmc <- function(x, ...) {
  x$coef
}

plot.vonmises_mcmc <- function(x, ...) {
  params <- cbind(t(x$coef[, 1]), 0, 1)

  colnames(params) <- c("mu_1", "kp_1", "lam_1", "alph_1")
  flexcircmix::plot_batmix_sample(x$data, param = params)
}

von_mises_posterior <- function(th, ...) {

  th <- as.circrad(th)

  # Run intercept-only von Mises regression model
  res <- circglmbayes::circGLM(th = th, ...)

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

  class(vmpost_res) <- c("vonmises_mcmc", class(vmpost_res))

  vmpost_res
}
