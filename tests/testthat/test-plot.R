context("plot")

test_that("cbar_plot", {
  .data <- iris[, 1:4]
  datetime <- seq(from = Sys.time(), length.out = nrow(.data), by = "mins")
  .data <- cbind(datetime = datetime, .data)

  pre_period <- 1:100
  post_period <- 101:150
  apply_standardized = T
  .cbar <- cbar(.data, pre_period, post_period, apply_standardized)

  .plot <- cbar_plot(.cbar)
  expect_true(inherits(.plot, c("gg", "ggplot")))
})
