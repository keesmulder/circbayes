#' Bayesian inference for von Mises regression.
#'
#' @param formula an object of class "\code{\link{formula}}" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param data A data frame or matrix containing the circular observations and
#'   predictors.
#' @param prior_b0kp Numeric vector of length 3. The prior parameters of the von
#'   Mises conjugate prior for the intercept and concentration parameter.
#' @param prior_btdt Numeric vector of length 2. The prior parameters for the
#'   normal prior on the regression coefficients.
#' @param r Parameter of the link function, \eqn{g(x) = r * arctan(x)}.
#' @param niter Number of iterations to perform MCMC for.
#' @param ... Further arguments passed to \code{circglmbayes::circGLM}.
#'
#' @return Object of type \code{vm_reg_mod}.
#' @export
#'
#' @examples
#' dat <- rvm_reg(30)
#' vm_reg(th ~ ., data = dat)
#'
vm_reg <- function(formula,
                   data,
                   prior_b0kp = c(0, 0, 0),
                   prior_btdt = c(0, 1),
                   niter = 1000,
                   r = 2,
                   ...) {




  # Run von Mises regression model
  res <- circglmbayes::circGLM(formula = formula,
                               data = data,
                               conj_prior    = prior_b0kp,
                               bt_prior_musd = prior_btdt,
                               Q = niter, r = r, ...)


  res$Call <- match.call()
  res$coef <- coef(res)
  res$parnames  <- rownames(res$coef)
  res$estimates <- res$coef[, 1]
  res$data <- data

  beta_names  <- colnames(res$bt_mean)
  delta_names <- colnames(res$dt_meandir)
  th_name     <- colnames(res$data_th)

  res$beta_names  <- beta_names
  res$delta_names <- delta_names
  res$th_name     <- th_name

  # Log posterior of vm_reg.
  log_posterior_vm_reg <- function(params, data) {
    predfun <- predict_function_pars(beta_0 = params['Intercept'],
                                     beta   = params[beta_names],
                                     delta  = params[delta_names],
                                     linkfun = function(x) r * atan(x))
    mus <- predfun(data)
    sum(dvm(data[, th_name] - mus, mu = 0, kp = params['Kappa'], log = TRUE))
  }

  # Set the environment of the log posterior function.
  log_post_env             <- new.env()
  log_post_env$r           <- r
  log_post_env$beta_names  <- beta_names
  log_post_env$delta_names <- delta_names
  log_post_env$th_name     <- th_name
  environment(log_posterior_vm_reg) <- log_post_env

  res$log_posterior <- log_posterior_vm_reg

  class(res) <- c("vm_reg_mod", class(res))

  res
}

#' Random generation for the von Mises distribution.
#'
#' Generate data from the von Mises regression model.
#'
#' @param n Number of values to sample.
#' @param kp Residual concentration parameter.
#' @param beta0 Circular intercept.
#' @param beta Vector of circular continuous regression coefficients.
#' @param delta Vector of circular categorical regression coefficients
#'
#' @return Numeric vector of \code{n} samples from the von Mises disttribution,
#'   in radians.
#' @export
#'
#' @examples
#' rvm(40, 3, 2)
#'
rvm_reg <- function(n, beta0 = 0, kp = 1, beta = c(1, 2), delta = c(1, -1)) {
  dmat <- circglmbayes::generateCircGLMData(n = n, residkappa = kp,
                                            nconpred  = length(beta),
                                            ncatpred  = length(delta),
                                            truebeta0 = beta0,
                                            truebeta  = beta,
                                            truedelta = delta)
  dmat[, 'th'] <- as.circrad(dmat[, 'th'])

  dmat
}



# Prediction function for a given set of named parameters.
# beta and delta must be named to find them in newdata.
predict_function_pars <- function(beta_0 = 0, beta = NULL, delta = NULL,
                                  linkfun = function(x) 2 * atan(x)) {
  # beta  <- matrix(beta)
  # delta <- matrix(delta)

  function(newdata) {
    if (length(delta) == 0) {
      dpart <- 0
    } else {
      d <- newdata[, names(delta), drop = FALSE]
      dpart <- d %*% delta
    }

    if (length(beta) == 0) {
      xpart <- 0
    } else {
      x <- newdata[, names(beta), drop = FALSE]
      xpart <- linkfun(x %*% beta)
    }
    beta_0 + xpart + dpart
  }
}



# Helper functions giving regression lines when plotting. These are simpler,
# because regression lines assume all other x's will be 0.
single_pred_fun_beta <- function(x, params, linkfun = function(x) 2 * atan(x)) {
  params[1] + linkfun(params[2] * x)
}
single_pred_fun_delta <- function(x, params) {
  params[1] + params[2] * x
}


#' @export
print.vm_reg_mod <- function(x, ...) {
  NextMethod()
}


#' @export
coef.vm_reg_mod <- coefficients.vm_reg_mod <- function(object, ...) {
  res <- NextMethod()
  colnames(res) <- c("estimate", "se", "2.5%", "97.5%")
  res
}


#' @export
predict.vm_reg_mod <- function(object, newdata, ...) {
  NextMethod()
}




#' @export
predict_function.vm_reg_mod <- function(object,
                                        linkfun = function(x) 2 * atan(x)) {
  function(newdata) {
    if (length(object$dt_meandir) == 0) {
      dpart <- 0
    } else {
      d <- newdata[, colnames(object$dt_meandir), drop = FALSE]
      dpart <- d %*% t(object$dt_meandir)
    }

    if (length(object$bt_mean) == 0) {
      xpart <- 0
    } else {
      x <- newdata[, colnames(object$bt_mean), drop = FALSE]
      xpart <- linkfun(x %*% t(object$bt_mean))
    }
    unname(object$b0_meandir + xpart + dpart)
  }
}


#' @export
posterior_samples.vm_reg_mod <- function(object, ...) {
  n_param <- 2 + length(object$bt_mean) + length(object$dt_meandir)
  post_sam <- object$all_chains[, 1:n_param]
  colnames(post_sam) <- object$parnames
  post_sam
}


#' @export
plot.vm_reg_mod <- function(x, pred_name, ...) {

  # If no predictor name is chosen, pick the first.
  if (missing(pred_name)) pred_name <- x$parnames[3]

  # Check if the chosen predictor is delta or beta.
  if (pred_name %in% colnames(x$bt_mean)) {
    pred_fun <- single_pred_fun_beta
    par_fit  <- x$bt_mean[, pred_name]
  } else if (pred_name %in% colnames(x$dt_meandir)) {
    pred_fun <- single_pred_fun_delta
    par_fit  <- x$dt_meandir[, pred_name]
  } else {
    stop(paste("pred_name", pred_name, "not found."))
  }

  plot_circbayes_regression(x,
                            pred_name = pred_name,
                            fit_params = c(x$b0_meandir, par_fit),
                            pred_fun = pred_fun, ...)
}



#' @export
marg_lik.vm_reg_mod <- function(x, ...) {

  sam <- posterior_samples(x)

  n_par <- ncol(sam)

  lb <- c(-2*pi, 0,  rep(-Inf, n_par - 2))
  ub <- c(2*pi, Inf, rep( Inf, n_par - 2))

  nms <- colnames(sam)
  names(lb) <- nms
  names(ub) <- nms

  delta_idx <-  nms %in% x$delta_names

  partypes <- c("circular", "real", rep("real", n_par - 2))
  partypes[delta_idx] <- "circular"
  lb[delta_idx] <- -2*pi
  ub[delta_idx] <-  2*pi

  bsobj <- bridgesampling::bridge_sampler(data = x$data,
                                          samples = as.matrix(sam),
                                          param_types = partypes,
                                          log_posterior = x$log_posterior,
                                          lb = lb, ub = ub,
                                          silent = TRUE,
                                          ...)

  bridgesampling::logml(bsobj)
}




