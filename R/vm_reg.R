
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


predict_function.vm_reg_mod <- function(object,
                                        linkfun = function(x) 2 * atan(x)) {
  function(newdata) {
    if (length(object$dt_meandir) == 0) {
      dpart <- 0
    } else {
      d <- newdata[, colnames(object$dt_meandir)]
      dpart <- d %*% t(object$dt_meandir)
    }

    if (length(object$bt_mean) == 0) {
      xpart <- 0
    } else {
      x <- newdata[, colnames(object$bt_mean)]
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
                   ...) {


  # Run von Mises regression model
  res <- circglmbayes::circGLM(formula = formula,
                               data = data,
                               conj_prior    = conj_prior,
                               bt_prior_musd = beta_prior,
                               Q = niter, ...)

  res$coef <- coef(res)
  res$parnames <- rownames(res$coef)
  res$data <- data

  class(res) <- c("vm_reg_mod", class(res))

  res
}
