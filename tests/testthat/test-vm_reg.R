context("Von Mises Regression")

th_df <- rvm_reg(20, beta = .5, kp = 50)

test_that("Random generation", {
  expect_equal(nrow(th_df), 20)
  expect_is(th_df, "matrix")
})

mod  <- vm_reg(th ~ ., data = th_df, burnin = 0, niter = 20)
mod2 <- vm_reg(th ~ .,
               data = th_df,
               burnin = 0,
               niter = 20,
               conj_prior = c(2, 10, 15),
               beta_prior = c(0, .3))


test_that("Posterior sampling", {

  expect_is(mod,        "vm_reg_mod")
  expect_is(plot(mod),  "gg")
  expect_is(coef(mod),  "matrix")

  expect_is(mod2,       "vm_reg_mod")
  expect_is(plot(mod2), "gg")
  expect_is(coef(mod2), "matrix")
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



