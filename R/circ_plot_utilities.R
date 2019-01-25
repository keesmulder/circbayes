
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
                            digits = 0, limits = c(-pi, pi),
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

  conv_brks <- switch(units,
    radians    = round(brks, digits),
    degrees    = round(brks * 180 / pi, digits),
    hours      = round(brks * 12 / pi, digits),
    months_abb = month.abb[(floor(brks * 6 / pi + .001) %% 12) + 1],
    months     = month.name[(floor(brks * 6 / pi + .001) %% 12) + 1],
    texpi      = c("$0$", "$\\frac{\\pi}{2}$", "$\\pi$",
                 "$\\frac{3\\pi}{2}$", "$2\\pi$"),
    texnegpi   = c("$-\\pi$", "$-\\frac{\\pi}{2}$", "$0$",
                 "$\\frac{\\pi}{2}$", "$\\pi$"),
    cardinal   = c("Right", "Up", "Left", "Down")[c(3:4, 1:4, 1:3)],
    compass   = c("East", "North", "West", "South")[c(3:4, 1:4, 1:3)])

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
#' The input data of the plot on which a \code{scale_circular} is placed,
#' therefore, must be given in radians, in the range provided in the option
#' \code{limits}.
#'
#' The continuous transformations are \code{"radians"} and \code{"degrees"}, for
#' which \code{digits} can be set.
#'
#' The temporal transformations are \code{"months"}, \code{"months_abb"}
#' (abbreviated), and \code{"hours"}.  For both of these types of
#' transformations, \code{nticks}.
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
#' @param positive_labels Logical; whether to only use positive values for the
#'   labels.
#'
#' @return An object of type \code{ScaleContinuousPosition} that can be added to
#'   any existing \code{ggplot}.
#' @export
#'
#' @examples
#' dat <- rvm(100)
#' p <- plot(vm_posterior(dat), polar_coord = FALSE)
#'
#' p + scale_x_circular(units = "compass")
#'
#' p + scale_x_circular(units = "cardinal")
#'
#' p + scale_x_circular(units = "texnegpi")
#'
#' p + scale_x_circular(units = "hours", nticks = 24)
#'
#' dat <- data.frame(th = rvm(100) + pi)
#'
#' p + scale_x_circular(units = "months", nticks = 6, limits = c(0, 2*pi))
#' p + scale_x_circular(units = "months_abb", nticks = 12, limits = c(0, 2*pi))
#'
scale_circular <- function(units = "degrees", nticks = 4,
                           digits = 0, limits = c(-pi, pi),
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
#' @describeIn scale_circular If circular data is on the x-axis.
scale_x_circular <- function(...) {
  scale_circular(scale_function = ggplot2::scale_x_continuous, ...)
}

#' @export
#' @describeIn scale_circular If circular data is on the y-axis.
scale_y_circular <- function(...) {
  scale_circular(scale_function = ggplot2::scale_y_continuous, ...)
}



# Function that contains the logic to make a plot clockular
gg_inside_labels <- function(units = "degrees", nticks = 4,
                             digits = 0, limits = c(0, 2 * pi),
                             positive_labels = TRUE,
                             r = 1, labdist = .20, ymax = NA, zoom = 1) {

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
                                      r = 1,
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
                                    args = list(params = coef(x, derived = FALSE)[, 1]),
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
                               r = 1,
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
                                      n_x_eval      = 360,
                                      alpha_samples = .3,
                                      add_ci        = FALSE,
                                      qpts          = 100,
                                      pred_params   = c("Intercept", pred_name),
                                      ...) {

  # Basic histogram without samples.
  xdat <- x$data_X[, pred_name]
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
                                 x_grid = seq(min(xdat), max(xdat),
                                              length.out = n_x_eval),
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





