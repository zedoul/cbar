training_data <- function(.data, post_period,
                          apply_standardized = T) {

  training_data <- .data

  std_info <- list()

  if (apply_standardized) {
    # Exclude datetime
    for (i in 2:ncol(training_data)) {
      std_info[[i]] <- list(mean = mean(training_data[, i], na.rm = T),
                            sd = sd(training_data[, i], na.rm = T))
      training_data[, i] <- standardized(training_data[, i])
    }
  }

  training_data[post_period[1]:post_period[2], 2] <- NA

  list(training_data = training_data,
       standardized_info = std_info)
}

destandard_pred <- function(.pred, .standardized_info) {
  .mean <- .standardized_info[[2]]$mean
  .sd <- .standardized_info[[2]]$sd

  for (i in 1:ncol(.pred)) {
    .pred[, i] <- destandardized(.pred[, i], .mean, .sd)
  }

  .pred
}

#' Detect contextual anomaly
#'
#' @param .data data table with datetime, y, and predictors
#' @param pre_period vector
#' @param post_period vector
#' @param ... params for \code{bsts_model}
#' @export
cbar <- function(.data, pre_period, post_period,
                 apply_standardized = T,
                 verbose = getOption("cbar.verbose"),
                 ...) {
  # TODO: Use mapping for datetime and y

  check_data(.data, pre_period, post_period)

  # Create model
  .df <- training_data(.data, post_period, apply_standardized)
  .training_data <- .df[["training_data"]]
  .standardized_info <- .df[["standardized_info"]]

  # TODO: Separate training and prediction
  # Predict counterfactual
  .model <- bsts_model(.training_data)

  # Summarise intervals and point estimates
  .pred <- inference(.model, post_period)
  if (apply_standardized) {
    .pred <- destandard_pred(.pred, .standardized_info)
  }

  .pred <- cbind(datetime = .data[, 1],
                 y = .data[, 2],
                 .pred)

  structure(list(model = .model,
                 pred = .pred),
            class = "cbar")
}
