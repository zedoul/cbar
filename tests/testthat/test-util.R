context("bsts")

test_that("standardized and destandardized", {
  .data <- iris[, 1]
  res <- standardized(.data)
  orig <- destandardized(res,
                         mean(.data, na.rm = T),
                         sd(.data, na.rm = T))
  expect_true(identical(.data, orig))
})


