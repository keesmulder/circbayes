context("Von Mises Dirichlet Process Mixture Model")

set.seed(36)

th <- rvmmix(50)

test_that("Random generation", {
  expect_length(th, 50)
  expect_is(th, "numeric")
})

qpts <- 3

mod  <- vm_dpm(th, niter = 40)
mod2 <- vm_dpm(th, niter = 10, g0Priors = c(2, 5, 10), alphaPriors = c(10, 10))

test_that("Posterior sampling", {

  expect_is(mod,                  "vm_dpm_mod")
  expect_is(plot(mod, qpts = qpts),  "gg")

  expect_is(mod2,                 "vm_dpm_mod")
  expect_is(plot(mod2, qpts = qpts), "gg")
})



test_that("Plotting", {

  expect_is(plot(mod, qpts = qpts),                                        "gg")
  expect_is(plot(mod, qpts = qpts, add_ci = TRUE),                         "gg")
  expect_is(plot(mod, qpts = qpts, n_samples = 10, polar_coord = FALSE),   "gg")
  expect_is(plot(mod, qpts = qpts, add_data = FALSE),                      "gg")

  # Clock data
  expect_is(plot(mod, qpts = qpts, add_data = TRUE,
                 start = 0, direction = 1, units = "hours"),  "gg")
})

