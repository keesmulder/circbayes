context("Hypothesis Testing")

test_that("BHT compare works", {
  th     <- rvm(30, 0, 2)
  vm_mod <- vm_posterior(th)
  comp   <- bht_compare(uniform  = marg_lik_circ_unif(th),
                        von_mises = marg_lik(vm_mod))
  expect_is(comp, "bht_comparison")


})
