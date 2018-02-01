context("permutation")

test_that("vector of size 1", {
  expect_equal(as.matrix(1), permute_vector(1))
})

test_that("vector of size 2", {
  permutations <- permute_vector(1:2)
  expect_true(all(1:2 == permutations[,1]) || all(1:2 == permutations[,2]))
  expect_true(all(2:1 == permutations[,1]) || all(2:1 == permutations[,2]))
})

