#' Bayesian inference for Hierarchical (Mixed-effects) Projected Normal regression.
#'
#' @param th Circular observations, either \code{numeric} in radians, or
#'   \code{circular}.
#' @param prior Numeric of length 3. Prior parameters for conjugate prior of the
#'   von Mises distribution. The order is \eqn{\mu_0, R_0, c}.
#' @param niter Number of iterations to perform MCMC for.
#' @param ... Further arguments passed to \code{circglmbayes::circGLM}.
#'
#' @return Object of type \code{pn_me_reg_mod}.
#' @export
#'
#' @examples
#' pn_me_reg(th ~ ., rvm_reg(20, beta = c(.5, -.2), kp = 50))
#'
pn_me_reg <- function(formula,
                      data,
                      niter = 1000,
                      thin = 1,
                      burnin = 0,
                      ...) {

  if (is.matrix(data)) data <- data.frame(data)

  # Run intercept-only Hierarchical Projected Normal regression model
  res <- bpnreg::bpnme(pred.I = formula, data = data,
                      its = niter, n.lag = thin, burn = burnin, ...)

  class(res) <- c("pn_me_reg_mod", class(res))

  res$coef <- coef(res)
  res$data <- data
  res$parnames <- colnames(res$B1)
  res$data_th <- res$theta
  res$data_X  <- res$mm$XI
  res$estimates_B1 <- res$lin.coef.I[, 2]
  res$estimates_B2 <- res$lin.coef.II[, 2]
  res$estimates <- c(res$estimates_B1, res$estimates_B2)
  th_name <- as.character(formula)[2]
  res$th_name <- th_name

  # # Log posterior of pn_me_reg.
  log_posterior_pn_me_reg <- function(params, data) {
    mus <- predict_pn_given_params(params, data)

    sum(dprojnorm(data[, th_name], mu1 = mus$mu1, mu2 = mus$mu2, log = TRUE))
  }
  # Set the environment of the log posterior function.
  log_post_env             <- new.env()
  log_post_env$th_name     <- res$th_name
  environment(log_posterior_pn_me_reg) <- log_post_env
  res$log_posterior <- log_posterior_pn_me_reg

  class(res) <- c("pn_me_reg_mod", class(res))

  res
}




#' @export
print.pn_me_reg_mod <- function(x, ...) {
  NextMethod()
}


#' @importFrom bpnreg coef_lin
#' @export
coef_lin.pn_me_reg_mod <- function(object) {
  # list(lin_I  = object$lin.coef.I,
  #      lin_II = object$lin.coef.II)
  NextMethod()
}

#' @importFrom bpnreg coef_circ
#' @export
coef_circ.pn_me_reg_mod <- function(object, ...) {
  # object$circ.coef
  NextMethod()
}


#' @export
coef.pn_me_reg_mod <- coefficients.pn_me_reg_mod <- function(object, ...) {
  list(linear = coef_lin(object), circular = coef_circ(object))
}



#' @export
predict.pn_me_reg_mod <- function(object, newdata, ...) {
  NextMethod()
}



#' @export
posterior_samples.pn_me_reg_mod <- function(x) {
  B1 <- x$B.I
  B2 <- x$B.II
  Beta1 <- x$Beta.I
  Beta2 <- x$Beta.II

  list(B1, B2, Beta1, Beta2)
}



#' @export
inf_crit.pn_me_reg_mod <- function(x, ...) {
  ics <- x$model.fit
  if (all(vapply(ics, length, FUN.VALUE = 0) == 1)) {
    ic_df <- t(data.frame(ics))
    colnames(ic_df) <- "value"
    return(ic_df)
  } else {
    return(ics)
  }
}

