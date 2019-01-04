context("Von Mises distribution")

test_that("Von Mises functions work", {
  expect_length(rvm(10, 5, 2), 10)

  expect_equal(logBesselI(3, 0), log(besselI(3, 0)))

  expect_equal(integrate(dvm, 0, 2*pi)$value, 1)

  th <- rvm(100, 5, 6)

  vm_post <- von_mises_posterior(th)

  expect_is(vm_post, "vonmises_mcmc")
  expect_is(plot(vm_post), "gg")
  expect_is(coef(vm_post), "matrix")


  expect_is(plot(vm_post), "gg")
  expect_is(plot(vm_post, add_ci = TRUE), "gg")
  expect_is(plot(vm_post, add_samples = 10, polar_coord = FALSE), "gg")
  expect_is(plot(vm_post, add_fit = FALSE, add_data = FALSE), "gg")

  # Clock data
  expect_is(plot(vm_post, add_fit = FALSE, add_data = TRUE,
                 start = 0, direction = 1, units = "hours"), "gg")


  # Check prior usage
  vm_post <- von_mises_posterior(th, prior = c(2, 10, 15))

  expect_is(vm_post, "vonmises_mcmc")
  expect_is(plot(vm_post), "gg")
  expect_is(coef(vm_post), "matrix")

})




test_that("Von Mises plotting works", {

  th <- rvm(100, 5, 6)

  vm_post <- von_mises_posterior(th)

  expect_is(plot(vm_post, polar_coord = FALSE), "gg")
  expect_is(plot(vm_post, add_ci = TRUE), "gg")
  plot(vm_post, add_samples = TRUE)

})