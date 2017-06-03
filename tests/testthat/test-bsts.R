context("bsts")

test_that("bsts_spec_static", {
  .data <- iris[, 1:4]
  .spec <- bsts_spec_static(.data)
  expect_true(inherits(.spec, "cbar.model.spec"))
})

test_that("bsts_model", {
  .data <- iris[, 1:4]
  .model <- bsts_model(.data)
  expect_true(inherits(.model, "bsts"))
})
