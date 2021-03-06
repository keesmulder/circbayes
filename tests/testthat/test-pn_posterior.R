context("Projected Normal")

set.seed(36)


th <- rprojnorm(100, 4, 2)

test_that("Random generation", {
  expect_length(th, 100)
  expect_is(th, "numeric")

  skip("Dev test")
  curve(dprojnorm, -pi, pi)
  hist(rprojnorm(100000), breaks = 1000)
})

mod  <- pn_posterior(th, niter = 50)
mod2 <- pn_posterior(th, niter = 10, thin = 2)

test_that("Posterior sampling", {

  expect_is(mod,        "pn_posterior_mod")
  expect_is(plot(mod),  "gg")
  expect_is(coef(mod),  "matrix")

  expect_is(mod2,       "pn_posterior_mod")
  expect_is(plot(mod2), "gg")
  expect_is(coef(mod2), "matrix")

})



test_that("Information criteria", {
  expect_error(inf_crit(mod), NA)
})

test_that("Hypothesis testing", {
  expect_is(mod$log_posterior(mod$estimates, data = th), "numeric")
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
