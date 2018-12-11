# Make a version of a function that collects all arguments in a params arg.
param_version_of_fun <- function(fun) {
  nm_formals <- names(formals(fun)[-1])
  return(
    function(x, params) {
      names(params) <- nm_formals[1:length(params)]
      do.call(fun, args = c(list(x = x), as.list(params)))
    }
  )
}


# Return sample of pdfs on x grid
geom_mcmc_fun_sample <- function(fun, param_mat, n_funs = 100,
                                 col = grDevices::rgb(0.5, 0.5, 0.57, .25), ...) {
  n_param <- nrow(param_mat)

  # If the pdf is not of fun(x, params) form, refactor the function so it is.
  nm_formals <- names(formals(fun)[-1])
  if (!("params" %in% nm_formals)) {
    fun <- param_version_of_fun(fun)
  }

  # Validation.
  if (n_funs > n_param) stop("Can not add more plots than samples.")
  if (n_funs > 500) warning("Plotting many function lines might take a long time.")

  idx <- sample(1:n_param, n_funs)

  apply(param_mat[idx, , drop = FALSE], 1, function(x) {
    return(ggplot2::stat_function(fun = fun, args = list(params = x),
                                  col = col, ...))
  })
}



# Return ci on x grid
geom_mcmc_ci_sample <- function(fun, param_mat,
                                x_grid = seq(-pi, pi, length.out = 360),
                                qpts = 100, ci_size = .95,
                                ...) {

  # If the pdf is not of fun(x, params) form, refactor the function so it is.
  nm_formals <- names(formals(fun)[-1])
  if (!("params" %in% nm_formals)) {
    fun <- param_version_of_fun(fun)
  }

  n_param <- nrow(param_mat)

  idx <- sample(1:n_param, qpts)

  # Matrix with as qpts columns with as rows the result of the function for each
  # point on the xgrid.
  prob_mat <- apply(param_mat[idx, , drop = FALSE], 1, function(x) {
    fun(x_grid, params = x)
  })

  # Quantile probabilities
  probs <- c((1 - ci_size)/2,  1 - (1 - ci_size)/2)

  q_mat           <- t(apply(prob_mat, 1, quantile, probs = probs, na.rm = TRUE))
  colnames(q_mat) <- paste0("CI_", gsub("%", "", colnames(q_mat)))
  probstr         <- colnames(q_mat)
  q_df            <- data.frame(cbind(x_grid = x_grid, q_mat))

  return(list(
    ggplot2::geom_line(data = q_df, mapping = ggplot2::aes_string(x = "x_grid", y = probstr[1]), ...),
    ggplot2::geom_line(data = q_df, mapping = ggplot2::aes_string(x = "x_grid", y = probstr[2]), ...)
  ))
}


