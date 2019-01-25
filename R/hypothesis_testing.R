#' Compute the marginal likelihood of the circular uniform distribution.
#'
#' As this is simply \code{-n * log(2 * pi)}, only the length of \code{th} is
#' used.
#'
#' @param th Vector or data frame with circular observations.
#'
#' @return Numeric.
#' @export
#'
#' @examples
#' marg_lik_circ_unif(rvm(20))
#'
marg_lik_circ_unif <- function(th) {
  if (is.null(dim(th))) {
    return(c(log_marg_lik = -length(th) * log(2*pi)))
  } else {
    return(c(log_marg_lik = -nrow(th)   * log(2*pi)))
  }
}


#' Print function for Bayesian hypothesis testing.
#'
#' @param x Object of type \code{bht_comparison}.
#' @param digits Number of digits to print.
#' @param ... Further arguments.
#'
#' @export
#' @describeIn bht_compare Print method.
#'
#' @examples
#' th <- rvm(100, 3, .2)
#'
#' vm_mod <- vm_posterior(th, prior = c(0, 0, 1))
#'
#' cu_ml <- marg_lik_circ_unif(th)
#' vm_ml <- marg_lik(vm_mod)
#'
#' bht_compare(uniform = cu_ml, von_mises = vm_ml)
print.bht_comparison <- function(x, digits = 3, ...) {
  nmstr <- paste(x$names, collapse = ", ")

  cat("Bayesian Hypothesis Test\n")
  cat(paste("    Comparing", length(x$names), "models:", nmstr))
  cat("\n\n[Log Marginal Likelihood]\n")
  print(x$log_marg_lik_vec)
  cat("\n[Posterior Model Probabilities]\n")
  print(x$post_mod_prob)
  cat("\n[Pairwise log Bayes Factors]\n")
  print(x$log_bf_mat)
}



#' Bayesian hypothesis tests from marginal likelihoods.
#'
#' @param ... (Named) list of models.
#'
#' @return Object of type \code{'bht_comparison'}.
#' @export
#'
#' @examples
#' # Test circular uniformity versus von mises.
#' th     <- rvm(30, 0, 1)
#' vm_mod <- vm_posterior(th)
#'
#' bht_compare(uniform  = marg_lik_circ_unif(th),
#'             vonmises = marg_lik(vm_mod))
#'
bht_compare <- function(...) {
  models <- list(...)

  nms <- names(models)
  if (is.null(nms)) {
    nms <- paste("Model", seq_along(models))
  } else if (any(nms == "")) {
    which_empty <- nms == ""
    base_names <- paste("Model", seq_along(models))
    nms[which_empty] <- base_names[which_empty]
  }


  if (any(vapply(models, length, 0) > 1)) {
    stop(paste("Some model log marginal likelihood are too long. "))
  }

  # Gather marginal likelihoods.
  lmlvec        <- unlist(models)
  names(lmlvec) <- nms
  mlvec         <- exp(lmlvec)

  # Compute pairwise Bayes factors.
  lbf_mat <- outer(lmlvec, lmlvec, function(x, y) x - y)
  names(dimnames(lbf_mat)) <- c("Support for:   ", "Versus: ")
  bf_mat  <- exp(lbf_mat)

  # Compute posterior model probabilities.
  mlsum  <- sum(mlvec)
  logpmp <- lmlvec - log(mlsum)
  pmp    <- exp(logpmp)

  out <- list(log_marg_lik_vec  = lmlvec,
              log_bf_mat        = lbf_mat,
              log_post_mod_prob = logpmp,
              marg_lik_vec      = mlvec,
              bf_mat            = bf_mat,
              post_mod_prob     = pmp,
              names             = nms)

  class(out) <- c("bht_comparison", class(out))
  out
}
