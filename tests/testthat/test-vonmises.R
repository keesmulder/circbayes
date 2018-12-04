context("vonmises")

test_that("Von Mises functions work", {
  expect_length(rvm(10, 5, 2), 10)
})
