context("Plotting functions for mcmc output")


# Test data
dat       <- data.frame(x = rvm(100) %% (2*pi) - pi)
sam       <- vm_posterior(dat, niter = 100)
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
    geom_mcmc_fun_sample(dvm, param_mat)

  p2 <- p +
    gg_circular_elems()

  p3 <- p +
    gg_circular_elems() +
    gg_inside_labels(limits = c(-pi, pi))

  p4 <- p +
    gg_circular_elems() +
    gg_inside_labels(units = "hours", nticks = 24, limits = c(-pi, pi))

  p5 <- p +
    gg_circular_elems() +
    gg_inside_labels(units = "radians", digits = 2,
                     nticks = 8, limits = c(-pi, pi))

  expect_is(p , "gg")
  expect_is(p2, "gg")
  expect_is(p3, "gg")
  expect_is(p4, "gg")
  expect_is(p5, "gg")

})


context("Circular scales")

test_that("Circular scales work", {

  dat       <- data.frame(x = rvm(100) %% (2*pi) - pi)
  sam       <- vm_posterior(dat, niter = 100)

  p <- plot(sam, polar_coord = FALSE, r = 1) + ggplot2::theme_bw()


  expect_warning(print(ggplot2::ggplot(dat)  +
    ggplot2::geom_histogram(ggplot2::aes_string(x = "x", y = "..density.."),
                            boundary = 0, bins = 10) +
    scale_x_circular()))
})










