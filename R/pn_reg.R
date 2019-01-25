
#' Bayesian inference for Projected Normal regression.
#'
#' @param formula an object of class "\code{\link{formula}}" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param data A data frame or matrix containing the circular observations and
#'   predictors.
#' @param niter Number of iterations to perform MCMC for.
#' @param thin Integer; Factor of thinning. Setting \code{thin = 1} provides no
#'   thinning.
#' @param burnin Integer; Number of iterations to burn.
#' @param ... Further arguments passed to \code{circglmbayes::circGLM}.
#'
#' @return Object of type \code{pn_reg_mod}.
#' @export
#'
#' @examples
#' pn_reg(th ~ ., rvm_reg(20, beta = c(.5, -.2), kp = 50))
#'
pn_reg <- function(formula,
                   data,
                   niter = 1000,
                   thin = 1,
                   burnin = 0,
                   ...) {

  if (is.matrix(data)) data <- data.frame(data)

  # Run Projected Normal regression model
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
  res$estimates <- c(res$estimates_B1, res$estimates_B2)
  res$th_name <- as.character(formula)[2]

  th_name     <- res$th_name

  # # Log posterior of pn_reg.
  log_posterior_pn_reg <- function(params, data) {
    mus <- predict_pn_given_params(params, data)

    sum(dprojnorm(data[, th_name], mu1 = mus$mu1, mu2 = mus$mu2, log = TRUE))
  }
  # Set the environment of the log posterior function.
  log_post_env             <- new.env()
  log_post_env$th_name     <- res$th_name
  environment(log_posterior_pn_reg) <- log_post_env
  res$log_posterior <- log_posterior_pn_reg

  class(res) <- c("pn_reg_mod", class(res))

  res
}



# Prediction function with one (pred_name) changing predictor.
one_predict_function_pn_reg <- function(x, pred_name) {

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


  return(pred_fun)
}


# Predict function with changing parameters.
predict_pn_given_params <- function(params, data) {
  n_par <- length(params)
  B1    <- params[1L:(n_par/2L), drop = FALSE]
  B2    <- params[(1L + (n_par/2L)):n_par, drop = FALSE]

  X <- as.matrix(cbind("(Intercept)" = 1, data[, names(B1)[-1]]))

  mu1 <- X %*% B1
  mu2 <- X %*% B2
  return(data.frame(mu1 = mu1, mu2 = mu2))
}


#' @export
print.pn_reg_mod <- function(x, ...) {
  NextMethod()
}


#' @importFrom bpnreg coef_lin
#' @export
coef_lin.pn_reg_mod <- function(object) {
  coef_mat <- NextMethod()
  colnames(coef_mat) <- c("mean", "mode", "se", "2.5%", "97.5%")
  coef_mat
}

#' @importFrom bpnreg coef_circ
#' @export
coef_circ.pn_reg_mod <- function(object, ...) {
  coef_mat <- NextMethod()
  if (!is.character(coef_mat)) {
    colnames(coef_mat) <- c("mean", "mode", "se", "2.5%", "97.5%")
  }
  coef_mat
}


#' @export
coef.pn_reg_mod <- coefficients.pn_reg_mod <- function(object, ...) {
  list(linear = coef_lin(object), circular = coef_circ(object))
}



#' @export
predict.pn_reg_mod <- function(object, newdata, ...) {
  NextMethod()
}



#' @export
posterior_samples.pn_reg_mod <- function(x) {
  B1 <- x$B1
  B2 <- x$B2
  colnames(B1) <- paste0(colnames(B1), "_I")
  colnames(B2) <- paste0(colnames(B2), "_II")
  cbind(B1, B2)
}




#' @export
plot.pn_reg_mod <- function(x, pred_name, ...) {
  # If no predictor name is chosen, pick the first.
  if (missing(pred_name)) pred_name <- x$parnames[2L]

  pred_fun <- one_predict_function_pn_reg(x, pred_name)

  pred_params <- c(paste0(names(x$estimates_B1), "_I"),
                   paste0(names(x$estimates_B2), "_II"))

  plot_circbayes_regression(x, pred_name = pred_name,
                            pred_fun = pred_fun,
                            fit_params = c(x$estimates_B1, x$estimates_B2),
                            pred_params = pred_params, ...)
}


#' @export
marg_lik.pn_reg_mod <- function(x, ...) {

  sam <- posterior_samples(x)
  nms <- rep(x$parnames, 2)
  colnames(sam) <- nms

  n_par <- ncol(sam)

  lb <- rep(-Inf, n_par)
  ub <- rep( Inf, n_par)

  names(lb) <- nms
  names(ub) <- nms

  partypes <- rep("real", n_par)

  bsobj <- bridgesampling::bridge_sampler(data = x$data,
                                          samples = as.matrix(sam),
                                          param_types = partypes,
                                          log_posterior = x$log_posterior,
                                          lb = lb, ub = ub,
                                          silent = TRUE,
                                          ...)

  bridgesampling::logml(bsobj)
}


#' @export
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
