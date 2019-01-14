
empty_thm <- list(ggplot2::theme_minimal(),
                  ggplot2::theme(legend.position = 'none',
                                 panel.grid = ggplot2::element_blank(),
                                 panel.background = ggplot2::element_blank(),
                                 panel.grid.minor.x = ggplot2::element_blank(),
                                 axis.title.y = ggplot2::element_blank(),
                                 axis.ticks.y = ggplot2::element_blank(),
                                 axis.text.y  = ggplot2::element_blank(),
                                 axis.title.x  = ggplot2::element_blank()
                  ))

# Function that contains the logic to make a plot circular
gg_circular_elems <- function(r = 1, ymax = NA, start = pi/2, direction = -1) {
  list(
    ggplot2::coord_polar(start = start, direction = direction),
    ggplot2::ylim(-r, ymax),
    ggplot2::geom_hline(yintercept = 0, col = "gray20"),
    empty_thm
  )
}


breaks_circular <- function(units = "degrees", nticks = 4,
                            digits = 0, limits = c(0, 2 * pi),
                            positive_labels = TRUE,
                            scale_function = ggplot2::scale_x_continuous, ...) {

  if (round(abs(limits[1] - limits[2]) - 2*pi, 3) != 0) {
    stop("Limits must be a range of length 2*pi.")
  }

  units_categ <- units %in% c("texpi", "texnegpi", "cardinal", "compass")

  if (units_categ & nticks != 4) {
    warning(paste0("For `units = ", units, "`, setting `nticks` to 4."))
    nticks <- 4
  }

  if (units_categ) {
    if (units == "texpi") {
      brks   <- seq(0, 2 * pi, length.out = 5)
      limits <- c(0, 2 * pi)
    } else if (units == "texnegpi") {
      brks   <- seq(-pi, pi, length.out = 5)
      limits <- c(-pi, pi)
    } else {
      # These are the options for cardinal or compass directions.
      brks   <- seq(-pi, 3*pi, length.out = 9)
    }
  } else {
    brks <- seq(limits[1], limits[2], length.out = nticks + 1)[-(nticks + 1)]
  }

  # If desired, set absolute values for the labels.
  orig_brks <- brks
  if (positive_labels) brks <- brks %% (2*pi)

  conv_brks <- switch(
    units,
    radians  = round(brks, digits),
    degrees  = round(brks * 180 / pi, digits),
    hours    = round(brks * 12 / pi, digits),
    texpi    = c("$0$", "$\\frac{\\pi}{2}$", "$\\pi$",
                 "$\\frac{3\\pi}{2}$", "$2\\pi$"),
    texnegpi = c("$-\\pi$", "$-\\frac{\\pi}{2}$", "$0$",
                 "$\\frac{\\pi}{2}$", "$\\pi$"),
    cardinal = c("Right", "Up", "Left", "Down")[c(3:4, 1:4, 1:3)],
    compass  = c("East", "North", "West", "South")[c(3:4, 1:4, 1:3)])

  list(breaks = orig_brks,
       labels = conv_brks,
       limits = limits)

}

#' Circular ggplot scales
#'
#' Extensions of \code{scale_x_continuous} and \code{scale_y_continuous} that
#' are useful for circular axes.
#'
#' Behind the screens, radians will be used, but the plot will display
#' transformed values.
#'
#' The continuous transformations are \code{"radians"}, \code{"degrees"} and
#' \code{"hours"}. For these, \code{nticks} and \code{digits} can be set.
#'
#' The categorical transformations are \code{"cardinal"}, which prints character
#' labels of the directions (left, right, up, down), and \code{"compass"}, which
#' prints compass directions (North, South, East, West). Both of these assume
#' the circular data starts on the right and moves counterclockwise. If not,
#' appropriate transformations must first be taken, or the labels will be
#' non-sensical.
#'
#' Two additional labels are \code{"texpi"} and \code{"texnegpi"}, which print
#' nice labels for TeX to interpret starting at 0 and \code{-pi} respectively.
#' This is useful if TikZ is used.
#'
#' @param units The units to display on the axis. See `Details` for the options.
#' @param nticks The number of ticks to display. Only relevant for continous
#'   scales.
#' @param digits Numeric; The number of digits for continuous scales.
#' @param limits Numeric vector; The limits of the plot. Must two numbers that
#'   are  \code{2*pi} apart.
#' @param scale_function The scale function from ggplot to use. Either
#'   \code{scale_x_continuous} or \code{scale_y_continuous}. For convenience
#'   this option can be circumvented by directly using \code{scale_x_circular}
#'   or \code{scale_y_circular}.
#' @param ... Additional arguments to be passed to  \code{scale_x_continuous} or
#'   \code{scale_y_continuous}.
#'
#' @return An object of type \code{ScaleContinuousPosition} that can be added to
#'   any existing \code{ggplot}.
#' @export
#'
#' @examples
#' p <- plot_batmixfit(params = cbind(mu = c(-pi/2, 0, pi/2, pi), kp = 4,
#'                               lam = c(-.9, .2, .8, 0), alph = .25))
#'
#' p + scale_x_circular(units = "compass")
#' p + scale_x_circular(units = "compass", limits = c(-1, 2*pi - 1))
#'
#' p + scale_x_circular(units = "cardinal")
#'
#' p + scale_x_circular(units = "texpi")
#' p + scale_x_circular(units = "texnegpi")
#'
#' p + scale_x_circular(units = "hours", nticks = 24)
#'
scale_circular <- function(units = "degrees", nticks = 4,
                           digits = 0, limits = c(0, 2 * pi),
                           positive_labels = TRUE,
                           scale_function = ggplot2::scale_x_continuous, ...) {

  brk_list <- breaks_circular(units = units, nticks = nticks,
                              digits = digits, limits = limits,
                              positive_labels = positive_labels)

  scale_function(breaks = brk_list$breaks,
                 labels = brk_list$labels,
                 limits = brk_list$limits,
                 ...)
}

#' @export
#' @describeIn scale_circular
scale_x_circular <- function(...) {
  scale_circular(scale_function = ggplot2::scale_x_continuous, ...)
}

#' @export
#' @describeIn scale_circular
scale_y_circular <- function(...) {
  scale_circular(scale_function = ggplot2::scale_y_continuous, ...)
}



# Function that contains the logic to make a plot clockular
gg_inside_labels <- function(units = "degrees", nticks = 4,
                             digits = 0, limits = c(0, 2 * pi),
                             positive_labels = TRUE,
                             r = 1, labdist = .12, ymax = NA, zoom = 1) {

  brk_list <- breaks_circular(units = units, nticks = nticks,
                              digits = digits, limits = limits,
                              positive_labels = positive_labels)

  pos <- brk_list$breaks
  nms <- brk_list$labels

  list(ggplot2::xlim(limits[1], limits[2]),
       ggplot2::geom_text(data = data.frame(x = pos, y = -labdist * r, label = nms),
                          ggplot2::aes_string(x = 'x', y = 'y', label = 'label')),
       ggplot2::geom_segment(data = data.frame(x = pos, xend = pos,
                                               y = 0, yend = -.03 * r),
                             ggplot2::aes_string(x = 'x', y = 'y', xend = 'xend', yend = 'yend')),
       ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                      plot.margin = ggplot2::unit(c(1,1,1,1) * -zoom, "cm")))
}





plot_circbayes_univariate <- function(x,
                                      pdf_fun     = dvm,
                                      polar_coord = TRUE,
                                      add_data    = TRUE,
                                      add_fit     = TRUE,
                                      n_samples   = 0,
                                      add_ci      = FALSE,
                                      bins        = 90,
                                      r = 3,
                                      ymax = NA,
                                      qpts        = 100,
                                      start       = pi/2,
                                      direction   = -1,
                                      ...) {

  # Basic histogram without samples.
  p <- ggplot2::ggplot(data.frame(x = as.circrad(x$data)))


  # If the function is not of fun(x, params) form, refactor the function so it is.
  nm_formals <- names(formals(pdf_fun)[-1])
  if (!("params" %in% nm_formals)) {
    pdf_fun <- param_version_of_fun(pdf_fun)
  }


  if (add_data) {
    p <- p +
      ggplot2::geom_histogram(
        mapping = ggplot2::aes_string(x = "x", y = "..density.."),
        fill = grDevices::rgb(.65, .65, .85, .3), col = "white",
        boundary = -pi, binwidth = 2 * pi / bins)
  }

  if (n_samples > 0) {
    param_mat <- posterior_samples(x)
    p <- p + geom_mcmc_fun_sample(pdf_fun,
                                  param_mat = param_mat,
                                  alpha = .1,
                                  n_funs = min(nrow(param_mat), n_samples))
  }

  if (add_ci) {
    param_mat <- posterior_samples(x)
    p <- p + geom_mcmc_ci_sample(pdf_fun,
                                 param_mat = param_mat,
                                 qpts = min(qpts, nrow(param_mat)),
                                 linetype = "dashed")
  }

  if (add_fit) {
    # Add pdf of posterior estimates
    p <- p + ggplot2::stat_function(fun = pdf_fun,
                                    args = list(params = coef(x)[, 1]),
                                    size = 1)
  }

  if (polar_coord) {
    p <- p + gg_circular_elems(r, ymax, start, direction) + gg_inside_labels(limits = c(-pi, pi), ...)
  } else {
    p <- p + ggplot2::coord_cartesian(...)
  }

  return(p)
}





plot_circbayes_dpm <- function(x,
                               polar_coord = TRUE,
                               add_data    = TRUE,
                               n_samples   = 0,
                               add_ci      = TRUE,
                               bins        = 90,
                               r = 3,
                               ymax = NA,
                               add_median  = TRUE,
                               qpts        = 100,
                               start       = pi/2,
                               direction   = -1,
                               ...) {

  # pdf fun of DPM models.
  pdf_fun <- function(xpt, params) {
    ind <- sample(1:x$niter, 1)
    dirichletprocess::PosteriorFunction(x, ind)(xpt)
  }


  # Basic histogram without samples.
  p <- ggplot2::ggplot(data.frame(x = as.circrad(x$data)))

  if (add_data) {
    p <- p +
      ggplot2::geom_histogram(
        mapping = ggplot2::aes_string(x = "x", y = "..density.."),
        fill = grDevices::rgb(.65, .65, .85, .3), col = "white",
        boundary = -pi, binwidth = 2 * pi / bins)
  }

  if (n_samples > 0) {
    p <- p + geom_mcmc_fun_sample(pdf_fun,
                                  param_mat = matrix(NA, nrow = n_samples),
                                  alpha = .1,
                                  n_funs = n_samples)
  }

  if (add_ci) {
    p <- p + geom_mcmc_ci_sample(pdf_fun,
                                 param_mat = matrix(NA, nrow = qpts),
                                 qpts = min(qpts, x$niter),
                                 add_median = add_median,
                                 linetype = "dashed")
  }

  if (polar_coord) {
    p <- p + gg_circular_elems(r, ymax, start, direction) + gg_inside_labels(limits = c(-pi, pi), ...)
  } else {
    p <- p + ggplot2::coord_cartesian(...)
  }

  return(p)
}



plot_circbayes_regression <- function(x,
                                      pred_name,
                                      fit_params,
                                      pred_fun,
                                      add_data      = TRUE,
                                      add_fit       = TRUE,
                                      n_samples     = 0,
                                      alpha_samples = .3,
                                      add_ci        = FALSE,
                                      qpts          = 100,
                                      pred_params   = c("Intercept", pred_name),
                                      ...) {






  # Basic histogram without samples.
  p <- ggplot2::ggplot(data.frame(th = as.circrad(x$data_th), x = x$data_X[, pred_name]),
                       mapping = ggplot2::aes_string(x = "x", y = "th"))

  if (add_data) {
    p <- p +
      ggplot2::geom_point()
  }


  # Add samples
  if (n_samples > 0) {
    param_mat <- posterior_samples(x)[, pred_params]
    p <- p + geom_mcmc_fun_sample(pred_fun,
                                  param_mat = param_mat,
                                  alpha = alpha_samples,
                                  n_funs = min(nrow(param_mat), n_samples))
  }


  if (add_ci) {
    param_mat <- posterior_samples(x)[, pred_params]
    p <- p + geom_mcmc_ci_sample(pred_fun,
                                 param_mat = param_mat,
                                 qpts = min(qpts, nrow(param_mat)),
                                 linetype = "dashed")
  }


  if (add_fit) {
    p <- p + ggplot2::stat_function(fun = pred_fun,
                                    args = list(params = fit_params),
                                    size = 1)
  }

  return(p)
}







#'
#'
#'
#'
#'
#'
#'
#'
#'
#' #' Plot a mixture of Batschelet type distributions.
#' #'
#' #' @param x Optional data vector of angles in radians to plot a histogram of.
#' #' @param params A matrix of parameters.
#' #' @param dbat_fun A function; the Batschelet function to use. Defaults to the
#' #'   Inverse Batschelet Function.
#' #' @param bins Integer; The number of bins to use in the optional histogram.
#' #' @param hist_alpha Numeric; The alpha value of the histogram of the
#' #'   data.
#' #' @param res Integer; The number of points at which to evaluate the density
#' #'   function.
#' #'
#' #' @return A ggplot.
#' #' @export
#' #'
#' #' @examples
#' #' plot_batmixfit(params = cbind(mu = c(-pi/2, 0, pi/2, pi), kp = 4,
#' #'                               lam = c(-.9, .2, .8, 0), alph = .25))
#' #'
#' plot_batmixfit <- function(x, params, dbat_fun = dpowbat, bins = 100, res = 400,
#'                            hist_alpha = .3) {
#'
#'
#'   # Initialize plot.
#'   if (missing(x)) {
#'     p <- ggplot2::ggplot(data.frame(x = c(-pi, pi)))
#'   } else {
#'     p <- ggplot2::ggplot(data.frame(x = force_neg_pi_pi(x)))
#'   }
#'
#'   p <- p +
#'     ggplot2::xlim(-pi, pi) +
#'     ggplot2::ylim(c(0, NA)) +
#'     ggplot2::coord_cartesian(expand = TRUE) +
#'     ggplot2::theme_bw()
#'
#'
#'   # If we don't have data given as well, return only the pdfs. Otherwise, add a
#'   # histogram.
#'   if (!missing(x)) {
#'     p <- p +
#'       ggplot2::geom_histogram(
#'         mapping = ggplot2::aes_string(x = "x", y = "..density.."),
#'         fill = grDevices::rgb(.65, .65, .85, hist_alpha), col = "white",
#'         boundary = -pi, binwidth = 2*pi / bins)
#'   }
#'
#'
#'   # Add the mixture density.
#'   p <- p +
#'     ggplot2::stat_function(fun = dbatmix_pmat,
#'                            args = list(pmat = params, dbat_fun = dbat_fun),
#'                            n = res,
#'                            colour = grDevices::rgb(.35, .35, .35, .8),
#'                            lwd = 1.2)
#'
#'
#'   # Add the separate densities.
#'   n_comp <- nrow(params)
#'
#'   # Force evaluation of the functions parameters, because otherwise lazy
#'   # evaluation will only cause us to plot the last function.
#'   funlist <- lapply(1:n_comp, function(compi) {
#'     function(x) params[compi, 'alph'] * dbat_fun(x,
#'                                                  mu = params[compi, 'mu'],
#'                                                  kp = params[compi, 'kp'],
#'                                                  lam = params[compi, 'lam'])})
#'
#'   if (n_comp < 10) {
#'     palette <- RColorBrewer::brewer.pal(max(n_comp, 3), "Set1")
#'   } else {
#'     palette <- RColorBrewer::brewer.pal(n_comp, "RdYlBu")
#'   }
#'
#'   # Next, add each separate pdf.
#'   for (compi in 1:n_comp) {
#'     p <- p + ggplot2::stat_function(fun = funlist[[compi]],
#'                                     col = palette[compi],
#'                                     lwd = .8, n = res)
#'   }
#'   p
#' }
#'
#'
#' #' plot_movMF_as_batmix
#' #'
#' #' Plot a result from movMF using the Batschelet mixture plotting functions.
#' #'
#' #' @param m A movMF results object.
#' #' @param ... Additional arguments to be passed to \code{plot_batmixfit}.
#' #'
#' #' @return A ggplot.
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #'   movMF_model <- movMF(cbind(runif(100, -1, 1), runif(100, -1, 1), 2), k = 4)
#' #'   plot_movMF_as_batmix(movMF_model)
#' #' }
#' #'
#' plot_movMF_as_batmix <- function(m, ...) {
#'   pmat <- invbatmix_pmat_from_movMF(m)
#'   plot_batmixfit(params = pmat, ...)
#' }
#'
#'
#'
#'
#' #' Plot a Batschelet-type mixture model
#' #'
#' #' @param x A \code{batmixmod} object.
#' #' @param ... Additional arguments to be passed to \code{plot_batmixfit}.
#' #'
#' #' @return A \code{ggplot}.
#' #' @export
#' #'
#' #' @examples
#' #' x <- rinvbatmix(50)
#' #' plot(fitbatmix(x, method = "EM"))
#' #'
#' plot.batmixmod <- function(x, ...) {
#'   plot_batmixfit(x$x, params = x$estimates, ...)
#' }
#'
#'
#'
#' #' Plot a sample of Batschelet mixture parameter sets.
#' #'
#' #' @param x An optional dataset of angles to be plotted as a histogram.
#' #' @param param A matrix of parameter sets.
#' #' @param dbat_fun A function; The pdf of the chosen Batschelet distribution.
#' #' @param bins The number of bins to draw in the histogram.
#' #' @param res Number of points at which to evaluate the functions.
#' #' @param orderColor Logical; If \code{TRUE}, plotted pdfs get darker and more
#' #'   red as they were sampled later.
#' #' @param plot_n Integer; the number of parameter rows to sample from the matrix
#' #'   \code{param}. This is intended for MCMC for example, where we can take a
#' #'   subsample of the parameter matrix to plot for speed.
#' #' @param hist_alpha Numeric; The alpha value of the histogram of the
#' #'   data.
#' #' @param dens_darkness Numeric; Higher numbers result in less transparent
#' #'   densities plotted.
#' #'
#' #' @return A ggplot.
#' #' @export
#' #'
#' #' @examples
#' #' x <- rinvbatmix(50)
#' #' mod <- fitbatmix(x, method = "bayes",  Q = 10)
#' #'
#' #' plot_batmix_sample(x, mod$mcmc_sample, dens_darkness = 5)
#' #'
#' plot_batmix_sample <- function(x, param, dbat_fun = dinvbat,
#'                                plot_n = nrow(param),
#'                                hist_alpha = .3, dens_darkness = 20,
#'                                bins = 100, res = 400, orderColor = FALSE) {
#'
#'   # Change to matrix if needed.
#'   if (is.vector(param)) param <- t(param)
#'
#'   # Initialize plot.
#'   if (missing(x)) {
#'     p <- ggplot2::ggplot(data.frame(x = c(-pi, pi)))
#'   } else {
#'     p <- ggplot2::ggplot(data.frame(x))
#'   }
#'
#'   p <- p +
#'     ggplot2::xlim(-pi, pi) +
#'     ggplot2::ylim(c(0, NA)) +
#'     ggplot2::coord_cartesian(expand = TRUE) +
#'     ggplot2::theme_bw()
#'
#'
#'   # If we don't have data given as well, return only the pdfs. Otherwise, add a
#'   # histogram.
#'   if (!missing(x)) {
#'     p <- p +
#'       ggplot2::geom_histogram(mapping = ggplot2::aes_string(x = "x",
#'                                                             y = "..density.."),
#'                               fill = grDevices::rgb(.65, .65, .85, hist_alpha),
#'                               col = "white",
#'                               boundary = -pi, binwidth = 2*pi / bins)
#'
#'     # If there are no parameters given, just return the histogram.
#'     if (identical(param, NA)) return(p)
#'   }
#'
#'
#'   # Remove some rows if we don't plot every row of param.
#'   param <- param[round(seq(1, nrow(param),
#'                            length.out = plot_n)), , drop = FALSE]
#'
#'   mu_mat   <- param[, grep("mu_[0-9]",   colnames(param)), drop = FALSE]
#'   kp_mat   <- param[, grep("kp_[0-9]",   colnames(param)), drop = FALSE]
#'   lam_mat  <- param[, grep("lam_[0-9]",  colnames(param)), drop = FALSE]
#'   alph_mat <- param[, grep("alph_[0-9]", colnames(param)), drop = FALSE]
#'
#'   n_comp <- ncol(mu_mat)
#'
#'   if (orderColor) ordseq <- seq(0, 1, 0.6/plot_n)
#'
#'
#'   for (i in 1:plot_n) {
#'     suppressWarnings(
#'       p <- p + ggplot2::stat_function(
#'         fun = dbatmix,
#'         args = list(mus      = mu_mat[i, ],
#'                     kps      = kp_mat[i, ],
#'                     lams     = lam_mat[i, ],
#'                     alphs    = alph_mat[i, ],
#'                     dbat_fun = dbat_fun),
#'         col = grDevices::rgb(ifelse(orderColor, ordseq[i], 0.2),
#'                              0.2, 0.2, min(1, dens_darkness/plot_n)),
#'         n = res)
#'     )
#'   }
#'   p
#' }
#'
#'
