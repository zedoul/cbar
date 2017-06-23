context("inference")

.data <- iris[, 1:4]
datetime <- seq(from = Sys.time(), length.out = nrow(.data), by = "mins")
.data <- cbind(datetime = datetime, .data)

.model <- bsts_model(.data)

test_that("posterior_means", {
 ret <- posterior_mean(.model)

  # the number of burned in MCMC samples should be niter (1000)
  expect_true(nrow(ret) < 1000)
  expect_true(ncol(ret) == nrow(iris))
})

test_that("response_trajectories", {
  ret <- response_trajectory(.model)

  # the number of burned in MCMC samples should be niter (1000) - n_burn_in
  expect_true(nrow(ret) < 1000)
  expect_true(ncol(ret) == nrow(iris))
})

test_that("point_prediction", {
  y_hat <- response_trajectory(.model)
  .posterior_mean <- posterior_mean(.model)

  ret <- point_prediction(y_hat,
                          .posterior_mean)

  # the number of burned in MCMC samples should be niter (1000) - n_burn_in (2)
  expect_true(inherits(ret, "data.frame"))
  expect_true(nrow(ret) == nrow(iris))
  expect_true(ncol(ret) == 3)
})
