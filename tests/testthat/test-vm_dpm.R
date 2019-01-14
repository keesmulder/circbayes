context("Von Mises Dirichlet Process Mixture Model")

set.seed(36)

th <- rvmmix(50)

test_that("Random generation", {
  expect_length(th, 50)
  expect_is(th, "numeric")
})


mod  <- vm_dpm(th, niter = 40)
mod2 <- vm_dpm(th, niter = 10,
                      mu_logprior_fun  = function(mu)  0,
                      kp_logprior_fun  = function(kp)  dgamma(kp, 2, 0.2, log = TRUE),
                      lam_logprior_fun = function(lam) 0)

test_that("Posterior sampling", {

  expect_is(mod,        "vm_dpm_mod")
  expect_is(plot(mod),  "gg")
  expect_is(coef(mod),  "matrix")

  expect_is(mod2,       "vm_dpm_mod")
  expect_is(plot(mod2), "gg")
  expect_is(coef(mod2), "matrix")
})


test_that("Information criteria", {
  expect_error(inf_crit(mod), NA)
})

test_that("Hypothesis testing", {

  expect_is(marg_lik(mod), "numeric")
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

