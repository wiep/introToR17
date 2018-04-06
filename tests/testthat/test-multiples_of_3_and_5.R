context("multiples of 3 and 5")

test_that("x is string", {
  expect_that(multiples(x = "hello"), throws_error())
})
