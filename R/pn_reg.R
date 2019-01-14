



print.pn_reg_mod <- function(x, ...) {
  NextMethod()
}


#' @importFrom bpnreg coef_lin
coef_lin.pn_reg_mod <- function(object) {
  # list(lin_I  = object$lin.coef.I,
  #      lin_II = object$lin.coef.II)
  NextMethod()
}

#' @importFrom bpnreg coef_circ
coef_circ.pn_reg_mod <- function(object, ...) {
  # object$circ.coef
  NextMethod()
}


coef.pn_reg_mod <- coefficients.pn_reg_mod <- function(object, ...) {
  list(linear = coef_lin(object), circular = coef_circ(object))
}



predict.pn_reg_mod <- function(object, newdata, ...) {
  NextMethod()
}



posterior_samples.pn_reg_mod <- function(x) {
  B1 <- x$B1
  B2 <- x$B2
  colnames(B1) <- paste0(colnames(B1), "_I")
  colnames(B2) <- paste0(colnames(B2), "_II")
  cbind(B1, B2)
}





plot.pn_reg_mod <- function(x, pred_name, ...) {
  # If no predictor name is chosen, pick the first.
  if (missing(pred_name)) pred_name <- x$parnames[2L]

  x_bar <- colMeans(x$mm$XI)
  pred_fun <- Vectorize(function(newx, params) {
    n_par <- length(params)
    B1    <- params[1L:(n_par/2L)]
    B2    <- params[(1L + (n_par/2L)):n_par]
    x_bar[pred_name] <- newx
    mu1 <- t(x_bar) %*% B1
    mu2 <- t(x_bar) %*% B2
    atan2(mu2, mu1)
  }, vectorize.args = "newx")

  pred_params <- c(paste0(names(x$estimates_B1), "_I"),
                   paste0(names(x$estimates_B2), "_II"))

  plot_circbayes_regression(x, pred_name = pred_name,
                            pred_fun = pred_fun,
                            fit_params = c(x$estimates_B1, x$estimates_B2),
                            pred_params = pred_params, ...)
}

# Log posterior of pn.
log_posterior_pn_reg <- function(params, data) {
  sum(dprojnorm(data, muvec[1], muvec[2], log = TRUE))
}

#
# marg_lik.pn_reg_mod <- function(x, ...) {
#
#   sam <- posterior_samples(x)
#
#   n_par <- ncol(sam)
#
#   lb <- c(-2*pi, 0,  rep(-Inf, n_par - 2))
#   ub <- c(2*pi, Inf, rep( Inf, n_par - 2))
#
#   nms <- colnames(sam)
#   names(lb) <- nms
#   names(ub) <- nms
#
#   delta_idx <-  nms %in% x$delta_names
#
#   partypes <- c("circular", "real", rep("real", n_par - 2))
#   partypes[delta_idx] <- "circular"
#   lb[delta_idx] <- -2*pi
#   ub[delta_idx] <-  2*pi
#
#   bsobj <- bridgesampling::bridge_sampler(data = x$data,
#                                           samples = as.matrix(sam),
#                                           param_types = partypes,
#                                           log_posterior = x$log_posterior,
#                                           lb = lb, ub = ub,
#                                           silent = TRUE,
#                                           ...)
#
#   bridgesampling::logml(bsobj)
# }
#
# predict_function_pars <- (XI, B1, B2) {
#   YI <- object$mm$XI %*% t(object$B1)
#   YII <- object$mm$XII %*% t(object$B2)
#   theta <- atan2(YII, YI)%%(2 * pi)
#   return(theta)
# }


inf_crit.pn_reg_mod <- function(x, ...) {
  ics <- x$model.fit
  if (all(vapply(ics, length, FUN.VALUE = 0) == 1)) {
    ic_df <- t(data.frame(ics))
    colnames(ic_df) <- "value"
    return(ic_df)
  } else {
    return(ics)
  }
}

#' Bayesian inference for Projected Normal regression.
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
#'
pn_reg <- function(formula,
                   data,
                   niter = 1000,
                   thin = 1,
                   burnin = 0,
                   ...) {

  if (is.matrix(data)) data <- data.frame(data)

  # Run intercept-only Projected Normal regression model
  res <- bpnreg::bpnr(pred.I = formula, data = data,
                      its = niter, n.lag = thin, burn = burnin, ...)

  class(res) <- c("pn_reg_mod", class(res))

  res$coef <- coef(res)
  res$data <- data
  res$parnames <- colnames(res$B1)
  res$data_th <- res$theta
  res$data_X  <- res$mm$XI
  res$estimates_B1 <- res$lin.coef.I[, 2]
  res$estimates_B2 <- res$lin.coef.II[, 2]

  # # Log posterior of pn_reg.
  # log_posterior_pn_reg <- function(params, data) {
  #   predfun <- predict_function_pars()
  #   mus <- predfun(data)
  #   sum(dvm(data[, th_name] - mus, mu = 0, kp = params['Kappa'], log = TRUE))
  # }
#
#   # Set the environment of the log posterior function.
#   log_post_env             <- new.env()
#   log_post_env$r           <- r
#   log_post_env$beta_names  <- beta_names
#   log_post_env$delta_names <- delta_names
#   log_post_env$th_name     <- th_name
#   environment(log_posterior_pn_reg) <- log_post_env
#
#   res$log_posterior <- log_posterior_pn_reg

  class(res) <- c("pn_reg_mod", class(res))

  res
}
