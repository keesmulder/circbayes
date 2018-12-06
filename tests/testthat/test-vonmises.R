context("vonmises")

test_that("Von Mises functions work", {
  expect_length(rvm(10, 5, 2), 10)

  expect_equal(logBesselI(3, 0), log(besselI(3, 0)))

  expect_equal(integrate(dvm, 0, 2*pi)$value, 1)

  th <- rvm(100, 5, 6)

  vm_post <- von_mises_posterior(th)

  expect_is(vm_post, "vonmises_mcmc")
  expect_is(plot(vm_post), "gg")
  expect_is(coef(vm_post), "matrix")


})
