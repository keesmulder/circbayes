context("Hierarchical Projected Normal Regression")

th_df <- data.frame(rvm_reg(20, beta = c(.5, -.2), kp = 50))

th_df$c1 <- 1:5

test_that("Random generation", {
  expect_equal(nrow(th_df), 20)
  expect_is(th_df, "data.frame")
})
#
# mod  <- pn_me_reg(th ~ l1 + l2 + (1 | c1) + (1 | c2), data = th_df, burnin = 0, niter = 50)
# mod2 <- pn_me_reg(th ~ l1 + l2 + (1 | c1 + c2),
#                data = th_df,
#                burnin = 10,
#                niter = 20)
#
#
# test_that("Posterior sampling", {
#
#   expect_is(mod,        "pn_me_reg_mod")
#   # expect_is(plot(mod),  "gg")
#   expect_is(coef(mod),  "list")
#
#   expect_is(mod2,       "pn_me_reg_mod")
#   # expect_is(plot(mod2), "gg")
#   expect_is(coef(mod2), "list")
# })
#
#
# test_that("Predict", {
#   expect_is(predict(mod, newdata = as.matrix(th_df)), "matrix")
# })
#



