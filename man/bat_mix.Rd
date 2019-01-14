% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bat_mix.R
\name{bat_mix}
\alias{bat_mix}
\title{Posterior of mixture of Power or Inverse Batschelet distributions.}
\usage{
bat_mix(th, bat_type = "power", n_comp = 3,
  mu_logprior_fun = function(mu) 0, kp_logprior_fun = function(kp) 0,
  lam_logprior_fun = function(lam) 0, alph_prior_param = rep(1,
  n_comp), fixed_pmat = matrix(NA, n_comp, 4), niter = 1000, ...)
}
\arguments{
\item{th}{Circular observations, either \code{numeric} in radians, or
\code{circular}.}

\item{niter}{Number of iterations to perform MCMC for.}

\item{...}{Further arguments passed to \code{circglmbayes::fitbatmix}.}
}
\description{
Posterior of mixture of Power or Inverse Batschelet distributions.
}
\examples{
bat_mix(rvm(30, 2, 5))

}