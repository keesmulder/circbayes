context("Projected Normal Regression")

th_df <- data.frame(rvm_reg(20, beta = c(.5, -.2), kp = 50))

test_that("Random generation", {
  expect_equal(nrow(th_df), 20)
  expect_is(th_df, "data.frame")
})

mod  <- pn_reg(th ~ ., data = th_df, burnin = 0, niter = 50)
mod2 <- pn_reg(th ~ .,
               data = as.matrix(th_df),
               burnin = 10,
               niter = 20)


test_that("Posterior sampling", {

  expect_is(mod,        "pn_reg_mod")
  expect_is(plot(mod),  "gg")
  expect_is(coef(mod),  "list")

  expect_is(mod2,       "pn_reg_mod")
  expect_is(plot(mod2), "gg")
  expect_is(coef(mod2), "list")
})


test_that("Predict", {
  expect_is(predict(mod, newdata = as.matrix(th_df)), "matrix")
})



test_that("Information criteria", {
  expect_error(inf_crit(mod), NA)
})

test_that("Hypothesis testing", {
  expect_is(mod$log_posterior(params = mod$estimates, data = th_df), "numeric")
  expect_is(marg_lik(mod), "numeric")
})





test_that("Plotting", {

  expect_is(plot(mod),                                    "gg")
  expect_is(plot(mod, add_ci = TRUE),                     "gg")
  expect_is(plot(mod, n_samples = 10),                    "gg")
  expect_is(plot(mod, add_fit = FALSE, add_data = FALSE), "gg")

})

test_that("Regression plotting choices", {

  expect_is(plot(mod, pred_name = 'c1'),                      "gg")
  expect_error(plot(mod,  pred_name = 'nonexisting'))
  expect_is(plot(mod, n_samples = 10),   "gg")
  expect_is(plot(mod, add_fit = FALSE, add_data = FALSE),     "gg")

})



