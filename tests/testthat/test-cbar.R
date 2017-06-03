context("cbar")

test_that("cbar", {
  .data <- iris[, 1:4]
  pre_period <- 1:100
  post_period <- 101:150
  apply_standardized = T
  res <- cbar(.data, pre_period, post_period, apply_standardized)
  expect_true(inherits(res, "cbar"))
})


