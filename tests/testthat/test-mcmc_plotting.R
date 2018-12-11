context("Plotting functions for mcmc output")

test_that("Sample works", {


  # Test data
  dat <- data.frame(x = rvm(100))
  sam <- von_mises_posterior(dat)
  param_mat <- cbind(sam$mu_chain, sam$kp_chain)


  p <- ggplot2::ggplot(dat) +
    ggplot2::geom_histogram(ggplot2::aes_string(x = "x", y = "..density..")) +
    geom_mcmc_fun_sample(dvm, param_mat)

  expect_is(p, "gg")

})


test_that("CI works", {


  # Test data
  dat <- data.frame(x = rvm(100))
  sam <- von_mises_posterior(dat)
  param_mat <- cbind(sam$mu_chain, sam$kp_chain)

  p <- ggplot2::ggplot(dat) +
    ggplot2::geom_histogram(ggplot2::aes_string(x = "x", y = "..density..")) +
    geom_mcmc_ci_sample(dvm, param_mat)

  expect_is(p, "gg")

})



context("Circular plotting")
