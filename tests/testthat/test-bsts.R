context("bsts")

test_that("bsts_spec_static", {
  .data <- iris[, 1:4]
  .spec <- bsts_spec_static(.data)
  expect_true(inherits(.spec, "cbar.model.spec"))
})

test_that("bsts_model", {
  .data <- iris[, 1:4]
  pre_period <- c(1, 100)
  post_period <- c(101, 150)

  training_data <- .data
  training_data[post_period[1]:post_period[2], 1] <- NA

  .model <- bsts_model(.data)
  expect_true(inherits(.model, "bsts"))

  names(.model)
  .model$coefficients
  .model$state.contributions[1000, 1:2, 145:150]

})
