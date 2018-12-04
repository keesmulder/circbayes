context("datahandling")

test_that("From Package 'Circular'", {

  rad <- circular::circular(rnorm(100))
  # Mean direction differs from linear mean
  expect_true(mean(rad) != mean(as.numeric(rad)))
  circrad <- as.circrad(rad)

  hours <- circular::circular(0:18, units = "hours")
  hours_cr <- as.circrad(hours)
  # Check if as.circrad conversion works.
  expect_equal(as.numeric(hours_cr), force_neg_pi_pi(as.numeric(hours) * 2 * pi / 24))


  deg <- circular::rvonmises(10, circular::circular(0), 10, control.circular = list(units = "degrees"))
  deg_cr <- as.circrad(deg)
  # Check if as.circrad conversion works.
  expect_equal(as.numeric(deg_cr), force_neg_pi_pi(as.numeric(deg) * pi / 180))


  cr <- circrad(c(0, 3))

  expect_s3_class(circrad(0), "circrad")
  expect_s3_class(as.circrad(0), "circrad")
  expect_true(is.circrad(cr))

  expect_is(mean(cr), "circrad")
  expect_is(sd(cr),   "numeric")
  expect_is(var(cr),  "numeric")
  expect_is(resultant_length(cr),  "numeric")

  cr <- circrad(c(pi/2, -pi/2))

  mean(cr)

})
