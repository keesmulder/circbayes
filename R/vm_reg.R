
#' Random generation for the von Mises distribution.
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



print.vm_reg_mod <- function(x, ...) {
  NextMethod()
}


coef.vm_reg_mod <- coefficients.vm_reg_mod <- function(x, ...) {
  NextMethod()
}


predict.vm_reg_mod <- function(object, newdata, ...) {
  NextMethod()
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


posterior_samples.vm_reg_mod <- function(x) {
  n_param <- 2 + length(x$bt_mean) + length(x$dt_meandir)
  post_sam <- x$all_chains[, 1:n_param]
  colnames(post_sam) <- x$parnames
  post_sam
}


plot.vm_reg_mod <- function(x, ...) {
  plot_circbayes_regression(x, ...)
}



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




#' Posterior of the von Mises distribution.
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
#' vm_reg(rvm(30, 2, 5))
#'
vm_reg <- function(formula,
                   data,
                   conj_prior = c(0, 0, 0),
                   beta_prior = c(0, 1),
                   niter = 1000,
                   r = 2,
                   ...) {




  # Run von Mises regression model
  res <- circglmbayes::circGLM(formula = formula,
                               data = data,
                               conj_prior    = conj_prior,
                               bt_prior_musd = beta_prior,
                               Q = niter, r = r, ...)


  res$Call <- match.call()
  res$coef <- coef(res)
  res$parnames <- rownames(res$coef)
  res$data <- data

  beta_names  <- colnames(mod$bt_mean)
  delta_names <- colnames(mod$dt_meandir)
  th_name     <- colnames(mod$data_th)

  res$beta_names  <- beta_names
  res$delta_names <- delta_names
  res$th_name     <- th_name

  # Log posterior of vm_reg.
  log_posterior_vm_reg <- function(params, data) {
    nms <- names(params)
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
