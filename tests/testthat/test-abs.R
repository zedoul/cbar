context("abs")

.data <- iris[, 1:4]
datetime <- seq(from = Sys.time(), length.out = nrow(.data), by = "mins")
.data <- cbind(datetime = datetime, .data)

ref_period <- 1:100
mea_period <- 101:150
.cbar <- cbar(.data, ref_period, mea_period)

test_that("visual abstraction", {
  .plot <- plot_ts(.cbar)
  expect_true(inherits(.plot, c("gg", "ggplot")))
})

test_that("analytical abstraction", {
  res <- summarise_pred_error(.cbar)
  expect_true(inherits(res, "data.frame"))
  res <- summarise_anomaly(.cbar)
  expect_true(inherits(res, "data.frame"))
  res <- summarise_session(.cbar)
  expect_true(inherits(res, "data.frame"))
  res <- summarise_incprob(.cbar)
  expect_true(inherits(res, "numeric"))
})
