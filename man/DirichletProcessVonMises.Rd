% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/von_mises_dpm_functions.R
\name{DirichletProcessVonMises}
\alias{DirichletProcessVonMises}
\title{Create a Dirichlet Mixture of Von Mises distributions}
\usage{
DirichletProcessVonMises(y, g0Priors = c(0, 0, 1), alphaPriors = c(2,
  4), priorMeanMethod = "integrate", n_samp = 3)
}
\arguments{
\item{y}{Data}

\item{g0Priors}{Base Distribution Priors \eqn{\gamma = (\mu_0, R_0, n_0)}}

\item{alphaPriors}{Alpha prior parameters. See \code{\link{UpdateAlpha}}.}

\item{priorMeanMethod}{Method for marginalization of prior mean. See
\code{\link{vonMisesMixtureCreate}}.}

\item{n_samp}{Number of Gibbs samples before we assume draws from posterior
are i.i.d. See \code{\link{vonMisesMixtureCreate}}.}
}
\value{
Dirichlet process object
}
\description{
This is the constructor function to produce a \code{dirichletprocess} object
with a Von Mises mixture kernel with unknown mean and unknown concentration
kappa. The base measure is conjugate to the posterior distribution.
}
\details{
The base measure is \eqn{G_0 (\mu, \kappa \mid \gamma) = I_0(R \kappa)^{- n_0}
\exp(R_0 \kappa \cos(\mu - \mu_0))}.
}
\examples{
dp <- DirichletProcessVonMises(rvm(10, 2, 5))
dp
}
