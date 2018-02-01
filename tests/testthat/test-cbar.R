context("cbar")

test_that("cbar", {
  .data <- iris[, 1:4]
  datetime <- seq(from = Sys.time(), length.out = nrow(.data), by = "mins")
  .data <- cbind(datetime = datetime, .data)

  pre_period <- 1:100
  post_period <- 101:150
  res <- cbar(.data, pre_period, post_period)
  expect_true(inherits(res, "cbar"))

  res <- cbar(.data, pre_period, post_period, apply_standardized = F)
  expect_true(inherits(res, "cbar"))

  expect_error(cbar(.data, -3:-1, -100:-20))
  expect_error(cbar(.data, 1:100000, 1:10000))
})


