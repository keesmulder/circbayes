context("Von Mises")


test_that("Von Mises functions", {
  expect_equal(logBesselI(3, 0), log(besselI(3, 0)))
  expect_equal(integrate(dvm, 0, 2*pi)$value, 1)
})

th <- rvm(20, 5, 6)

test_that("Random generation", {
  expect_length(th, 20)
  expect_is(th, "numeric")
})

mod  <- vm_posterior(th, niter = 10)
mod2 <- vm_posterior(th, niter = 10, prior = c(2, 10, 15))

test_that("Posterior sampling", {

  expect_is(mod,        "vm_posterior_mod")
  expect_is(plot(mod),  "gg")
  expect_is(coef(mod),  "matrix")

  expect_is(mod2,       "vm_posterior_mod")
  expect_is(plot(mod2), "gg")
  expect_is(coef(mod2), "matrix")
})


test_that("Plotting", {

  expect_is(plot(mod),                                        "gg")
  expect_is(plot(mod, add_ci = TRUE),                         "gg")
  expect_is(plot(mod, n_samples = 10, polar_coord = FALSE),   "gg")
  expect_is(plot(mod, add_fit = FALSE, add_data = FALSE),     "gg")

  # Clock data
  expect_is(plot(mod, add_fit = FALSE, add_data = TRUE,
                 start = 0, direction = 1, units = "hours"),  "gg")
})


