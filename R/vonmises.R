
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



plot.vonmises_mcmc <- function(x,
                               polar_coord = TRUE,
                               add_data    = TRUE,
                               add_fit     = TRUE,
                               add_samples = 0,
                               add_ci      = FALSE,
                               bins        = 90,
                               r = 1, ymax = NA,
                               start = pi/2, direction = -1,
                               ...) {

  # Basic histogram without samples.
  p <- ggplot2::ggplot(data.frame(x = as.circrad(x$data)))

  if (add_data) {
    p <- p +
      ggplot2::geom_histogram(
        mapping = ggplot2::aes_string(x = "x", y = "..density.."),
        fill = grDevices::rgb(.65, .65, .85, .3), col = "white",
        boundary = -pi, binwidth = 2 * pi / bins)
  }



  if (add_samples > 0) {
    param_mat <- cbind(mu = x$mu_chain,
                       kp = x$kp_chain)
    p <- p + geom_mcmc_fun_sample(dvm,
                                  param_mat = param_mat,
                                  alpha = .1,
                                  n_funs = add_samples)
  }


  if (add_ci) {
    param_mat <- cbind(mu = x$mu_chain,
                       kp = x$kp_chain)
    p <- p + geom_mcmc_ci_sample(dvm,
                                 param_mat = param_mat,
                                 linetype = "dashed")
  }


  if (add_fit) {
    # Add pdf of posterior estimates
    p <- p + ggplot2::stat_function(fun = dvm,
                                    args = list(mu = coef(x)[1, 1],
                                                kp =  coef(x)[2, 1]),
                                    size = 1)
  }


  if (polar_coord) {
    p <- p + gg_circular_elems(r, ymax, start, direction) + gg_inside_labels(limits = c(-pi, pi), ...)
  } else {
    p <- p + ggplot2::coord_cartesian(...)
  }

  return(p)
}


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
