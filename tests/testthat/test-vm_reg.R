context("Von Mises Regression")

set.seed(11)

th_df <- rvm_reg(20, beta = .5, kp = 50)

test_that("Random generation", {
  expect_equal(nrow(th_df), 20)
  expect_is(th_df, "matrix")
})

mod  <- vm_reg(th ~ ., data = th_df, burnin = 0, niter = 100)
mod2 <- vm_reg(th ~ .,
               data = th_df,
               burnin = 0,
               niter = 20,
               prior_b0kp = c(2, 10, 15),
               prior_btdt = c(0, .3))


test_that("Posterior sampling", {

  expect_is(mod,        "vm_reg_mod")
  expect_is(plot(mod),  "gg")
  expect_is(coef(mod),  "matrix")

  expect_is(mod2,       "vm_reg_mod")
  expect_is(plot(mod2), "gg")
  expect_is(coef(mod2), "matrix")
})


tdf <- data.frame(th_df, c_fac1 = rep(factor(1:2), each = 10), c_fac2 = rep(factor(1:2), 10))

test_that("Regression tests", {
  mod <- vm_reg(th ~ c_fac1, data = tdf, burnin = 10, niter = 20)
  expect_is(mod, "vm_reg_mod")
  expect_is(vm_reg(th ~ 1,      data = tdf, burnin = 10, niter = 20), "vm_reg_mod")
  expect_is(vm_reg(th ~ .^2,    data = tdf, burnin = 10, niter = 20), "vm_reg_mod")
})



test_that("Predict", {
  expect_is(predict_function(mod)(newdata = as.matrix(th_df)), "matrix")
  expect_is(predict_function(mod)(newdata =
                                    as.matrix(rvm_reg(20, beta = .5, kp = 50))),
            "matrix")
})



test_that("Information criteria", {
  expect_error(inf_crit(mod), NA)
})

test_that("Hypothesis testing", {
  expect_is(mod$log_posterior(mod$estimates, data = th_df), "numeric")
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



