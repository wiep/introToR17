context("is_palindrom")

test_that("palindrom", {
  expect_true(is_palindrom(""))
  expect_true(is_palindrom("a"))
  expect_true(is_palindrom("12321"))
  expect_true(is_palindrom("123321"))
  expect_false(is_palindrom("12"))
  expect_false(is_palindrom("112"))
})

test_that("invalid input", {
  expect_error(is_palindrom(c("1", "2")))
  expect_error(is_palindrom(1))
})
