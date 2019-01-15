#' Create a Dirichlet Mixture of Von Mises distributions
#'
#' This is the constructor function to produce a \code{dirichletprocess} object
#' with a Von Mises mixture kernel with unknown mean and unknown concentration
#' kappa. The base measure is conjugate to the posterior distribution.
#'
#'
#' The base measure is \eqn{G_0 (\mu, \kappa \mid \gamma) = I_0(R \kappa)^{- n_0}
#' \exp(R_0 \kappa \cos(\mu - \mu_0))}.
#'
#'
#' @param y Data
#' @param g0Priors Base Distribution Priors \eqn{\gamma = (\mu_0, R_0, n_0)}
#' @param alphaPriors Alpha prior parameters. See \code{\link{UpdateAlpha}}.
#' @param priorMeanMethod Method for marginalization of prior mean. See
#'   \code{\link{vonMisesMixtureCreate}}.
#' @param n_samp Number of Gibbs samples before we assume draws from posterior
#'   are i.i.d. See \code{\link{vonMisesMixtureCreate}}.
#'
#' @return Dirichlet process object
#' @export
#'
#' @examples
#' dp <- DirichletProcessVonMises(rvm(10, 2, 5))
#' dp
DirichletProcessVonMises <- function(y,
                                     g0Priors = c(0, 0, 1),
                                     alphaPriors = c(2, 4),
                                     priorMeanMethod = "integrate",
                                     n_samp = 3) {

  mdobj <- vonMisesMixtureCreate(g0Priors, priorMeanMethod, n_samp)
  dpobj <- dirichletprocess::DirichletProcessCreate(y, mdobj, alphaPriors)
  dpobj <- dirichletprocess::Initialise(dpobj)
  return(dpobj)
}


#'Create a von Mises mixing distribution
#'
#'@param priorParameters Prior parameters for the base measure which are, in
#'  order, (mu_0, R_0, c). The prior mean can be set to \code{NA} to be unknown.
#'  It is then dealt with using a method selected by \code{priorMeanMethod}.
#'@param priorMeanMethod Character; Method to deal with with the prior mean
#'  mu_0, if it is given not given but set to \code{NA}. If
#'  \code{"datadependent"}, the prior is assumed to be uninformative for the
#'  mean direction, but the prior resultant length \eqn{R_0} is used 'as is',
#'  that is the posterior resultant length is \eqn{R_n = R + R_0}. This
#'  implictly assumes that the prior mean is the data mean direction
#'  \eqn{\bar\theta}, which is why it is called `data dependent`. If
#'  \code{"integrate"}, the posterior parameters are computed averaged over a
#'  uniform distribution on the prior mean. If \code{"sample"}, a prior mean is
#'  sampled uniformly at each iteration.
#'@param n_samp Number of samples to generate from the Gibbs sampler before
#'  accepting the sample as an i.i.d. sample from the posterior. Higher values
#'  mean we can be more certain that the sampler works correctly, at the cost of
#'  computational time. The sampler mixes very fast, but different values can be
#'  attempted by means of sensitivity. As a general recommendation, 2 is only
#'  approximately correct, 3 is close, and 4 performs almost as good as 100 or
#'  more.
#'
#'@return Mixing distribution object
#'@export
vonMisesMixtureCreate <- function(priorParameters,
                                  priorMeanMethod = "integrate",
                                  n_samp = 3) {
  mdobj <- dirichletprocess::MixingDistribution("vonmises",
                                                priorParameters,
                                                "conjugate")

  mdobj$n_samp <- n_samp

  # If the prior mean direction mu_0 should be treated as unknown, add the
  # method to deal with this to the mixing distribution object.
  mdobj$noPriorMean <- is.na(priorParameters[1])
  if (mdobj$noPriorMean) {
    mdobj$priorMeanSample     <- priorMeanMethod == "sample"
    mdobj$priorMeanDataDep    <- priorMeanMethod == "datadependent"
    mdobj$priorMeanIntegrate  <- priorMeanMethod == "integrate"

    if (!priorMeanMethod  %in%  c("sample", "datadependent", "integrate")) {
      stop(paste("Unknown method for marginalizing out the prior mean ",
                 "direction. Select 'datadependent', sample', or 'integrate'."))
    }
  }

  return(mdobj)
}


# Helper function, log of a bessel function.
logBesselI <- function(x, nu = 0) log(besselI(x, nu, expon.scaled = TRUE)) + x

# von Mises
dvm_vec <- Vectorize(function(x, mu, kp) {
  logpdf <- kp * cos(x - mu) - log(2*pi) - logBesselI(kp, 0)
  return(exp(logpdf))
})



dbesselexp2 <- function(kp, mu, mu_n, R_n, n_n) {
  eta <- n_n
  g <- -R_n * cos(mu - mu_n) / n_n
  flexcircmix::dbesselexp(kp, eta, g)
}
rbesselexp2 <- function(n, mu, mu_n, R_n, n_n) {
  eta <- n_n
  g <- -R_n * cos(mu - mu_n) / n_n
  flexcircmix::rbesselexp(n, eta, g)
}


#' @importFrom dirichletprocess PosteriorParameters
#' @importFrom stats runif
PosteriorParameters.vonmises <- function(mdobj, x) {
  priorParameters <- mdobj$priorParameters

  # If the mean direction is given, compute the posterior parameters as usual.
  # Else, if mu_0 is NA, we want to marginalize over it.
  if (!mdobj$noPriorMean) {
    mu_0 <- priorParameters[1]

  } else if (mdobj$priorMeanSample) {

    # If we wish to sample it, sample a uniform prior mean and continue with the
    # usual computation.
    mu_0 <- runif(1, 0, 2*pi)

  } else if (mdobj$priorMeanDataDep) {
    R_0  <- priorParameters[2]
    n_0  <- priorParameters[3]

    # If data dependence,
    C <- sum(cos(x))
    S <- sum(sin(x))
    # In this case, mu_n is the mean direction of the data.
    mu_n <- atan2(S, C)
    R <- sqrt(C^2 + S^2)

    # In this case, the posterior resultant length is simply the sum of the two resultant lengths.
    R_n <-  (R_0 + R)

    n_n <- n_0 + length(x)
    PosteriorParameters <- matrix(c(mu_n, R_n, n_n), ncol = 3)
    return(PosteriorParameters)

  } else if (mdobj$priorMeanIntegrate) {
      # When we don't sample the mean direction, we wish to marginalize by
      # taking the expectation over the uniform distribution on mu_0 for R_n.
      R_0  <- priorParameters[2]
      n_0  <- priorParameters[3]

      # Approximate the correct R_n with the complete elliptic integral of the
      # second kind.
      C <- sum(cos(x))
      S <- sum(sin(x))
      R <- sqrt(C^2 + S^2)
      R_n <- (2/pi) * (R_0 + R) * pracma::ellipke(4 * R_0 * R / (R_0 + R)^2)$e

      n_n <- n_0 + length(x)

      # In this case, mu_n is the mean direction of the data.
      mu_n <- atan2(S, C)
      PosteriorParameters <- matrix(c(mu_n, R_n, n_n), ncol = 3)
      return(PosteriorParameters)
  }

  # If we don't marginalize or have sampled mu_0, compute the posterior
  # parameters in the usual way for conjugate von Mises models.
  R_0  <- priorParameters[2]
  n_0  <- priorParameters[3]

  n_n <- n_0 + length(x)
  C_n <- sum(cos(x)) + R_0 * cos(mu_0)
  S_n <- sum(sin(x)) + R_0 * sin(mu_0)

  R_n  <- sqrt(C_n^2 + S_n^2)
  mu_n <- atan2(S_n, C_n)

  PosteriorParameters <- matrix(c(mu_n, R_n, n_n), ncol = 3)

  return(PosteriorParameters)
}



#' @importFrom dirichletprocess Likelihood
Likelihood.vonmises <- function(mdobj, x, theta) dvm_vec(x, theta[[1]], theta[[2]])


one_vm_post_draw <- function(mun, rn, m, nsamp = 3) {
  # Random starting value.
  mu <- runif(1, -pi, pi)

  for (i in 1:nsamp) {
    kp <- rbesselexp2(1, mu, mun, rn, m)
    mu <- rvm(1, mun, rn * kp)
  }
  c(mu, kp)
}


#' @importFrom dirichletprocess PriorDraw
PriorDraw.vonmises <- function(mdobj, n = 1) {

  priorParameters <- mdobj$priorParameters
  R_0  <- priorParameters[2]
  n_0  <- priorParameters[3]

  if (n_0 < 0 | R_0 < -1) stop("Prior parameters out of bounds.")

  # If mu_0 is NA, sample uniform prior mean mu_0.
  if (mdobj$noPriorMean) {

    mu_0 <- runif(n, 0, 2*pi)
    mukp <- vapply(mu_0, function(mu0i) {
      one_vm_post_draw(mu0i, R_0, n_0, nsamp = mdobj$n_samp)
      }, FUN.VALUE = c(0, 0))

  } else {
    # Standard algorithm with given mu_0.
    mu_0 <- priorParameters[1]
    mukp <- replicate(n, one_vm_post_draw(mu_0, R_0, n_0, nsamp = mdobj$n_samp))
  }

  theta <- list(mu = array(mukp[1, ] %% (2*pi), dim = c(1, 1, n)),
                kp = array(mukp[2, ], dim = c(1, 1, n)))
  return(theta)
}


#' @importFrom dirichletprocess PosteriorDraw
PosteriorDraw.vonmises <- function(mdobj, x, n = 1) {

  # If mu_0 is NA, and priorMeanSample is TRUE, sample uniform prior mean mu_0.
  if (mdobj$noPriorMean && mdobj$priorMeanSample) {

    PosteriorParameters_calc <- replicate(n, dirichletprocess::PosteriorParameters(mdobj, x))

    mu_n <- as.matrix(PosteriorParameters_calc[1, , ])[1, ]
    R_n  <- as.matrix(PosteriorParameters_calc[1, , ])[2, ]
    n_n  <- as.matrix(PosteriorParameters_calc[1, , ])[3, 1]

    mukp <- vapply(1:n, function(i) {
      one_vm_post_draw(mu_n[i], R_n[i], n_n, nsamp = mdobj$n_samp)
    }, FUN.VALUE = c(0, 0))

  } else {
    # Standard algorithm with given mu_0 or marginalized without sampling.
    PosteriorParameters_calc <- dirichletprocess::PosteriorParameters(mdobj, x)

    mu_n <- PosteriorParameters_calc[1]
    R_n  <- PosteriorParameters_calc[2]
    n_n  <- PosteriorParameters_calc[3]

    if (!isTRUE(n_n >= 1)) n_n <- 1
    if (!isTRUE(R_n > -1)) R_n <- -1

    mukp <- replicate(n, one_vm_post_draw(mu_n, R_n, n_n, nsamp = mdobj$n_samp))
  }

  theta <- list(mu = array(mukp[1, ] %% (2*pi), dim = c(1, 1, n)),
                kp = array(mukp[2, ], dim = c(1, 1, n)))
  return(theta)
}



# Integral over mu and kp of exp(R * kp * cos(mu)) / (besselI(kp, 0)^n)
#' @importFrom stats integrate
vmbesselexp_nc <- function(R, n) {

  # mu is already integrated out analytically here. The function is
  # exponentiated at the very end for numerical stability.
  nc_fun <- function(kp) {
    exp(logBesselI(R * kp, 0) - n * logBesselI(kp, 0))
  }

  integrate(nc_fun, 0, Inf)$value
}


#' @importFrom dirichletprocess Predictive
Predictive.vonmises <- function(mdobj, x) {

  # If uninformative prior, only do this once because the predictive will be the
  # same for all data points.
  # if (mdobj$priorParameters[2] == 0) x <- x[1]

  predictiveArray <- numeric(length(x))

  R_0   <- mdobj$priorParameters[2]
  n_0   <- mdobj$priorParameters[3]

  # Normalizing constant of G_0
  nc <- vmbesselexp_nc(R_0, n_0) * (2 * pi)^(1 - n_0)

  # Term from the integral over the von Mises times the rest of G_0
  log_nc2 <- -n_0 * log(2 * pi)

  for (i in seq_along(x)) {

    post_param <- PosteriorParameters.vonmises(mdobj, x[i])
    R_p <- post_param[2]
    n_p <- post_param[3]

    # The posterior predictive density with the mean direction integrated out.
    predictiveArray[i] <- vmbesselexp_nc(R_p, n_p)
  }

  return(predictiveArray * exp(log_nc2) / nc)
}
