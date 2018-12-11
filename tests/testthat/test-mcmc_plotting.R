context("Plotting functions for mcmc output")


# Test data
dat       <- data.frame(x = rvm(100) %% (2*pi))
sam       <- von_mises_posterior(dat, Q = 100)
param_mat <- cbind(sam$mu_chain, sam$kp_chain)


test_that("Sample works", {

  p <- ggplot2::ggplot(dat) +
    ggplot2::geom_histogram(ggplot2::aes_string(x = "x", y = "..density..")) +
    geom_mcmc_fun_sample(dvm, param_mat)

  expect_is(p, "gg")

})


test_that("CI works", {

  p <- ggplot2::ggplot(dat) +
    ggplot2::geom_histogram(ggplot2::aes_string(x = "x", y = "..density..")) +
    geom_mcmc_ci_sample(dvm, param_mat)

  expect_is(p, "gg")

})



context("Circular plotting")

test_that("Circular plotting works", {

  p <- ggplot2::ggplot(dat) +
    ggplot2::geom_histogram(ggplot2::aes_string(x = "x", y = "..density.."), boundary = 0) +
    geom_mcmc_fun_sample(dvm, param_mat) +
    gg_circular_elems() +
    gg_inside_labels(limits = c(-pi, pi))

  expect_is(p, "gg")

})