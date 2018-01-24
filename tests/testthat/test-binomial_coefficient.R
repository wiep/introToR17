context("binomial_coefficient")

test_that("valid input", {
  expect_equal(1, binomial_coefficient(0, 0))
  expect_equal(2, binomial_coefficient(2, 1))
  expect_equal(3003, binomial_coefficient(15, 5))
  expect_equal(1, binomial_coefficient(15, 0))
  expect_equal(1, binomial_coefficient(15, 15))
})

test_that("invalid input", {
  expect_error(binomial_coefficient(0, -1))
  expect_error(binomial_coefficient(-1, 0))
  expect_error(binomial_coefficient(2, 1.1))
  expect_error(binomial_coefficient(1.1, 1))
  expect_error(binomial_coefficient(1, 2))
  expect_error(binomial_coefficient(1:2, 0))
  expect_error(binomial_coefficient(2, 0:2))
})
