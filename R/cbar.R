training_data <- function(.data, post_period,
                          apply_standardized = T) {

  training_data <- .data

  std_info <- list()

  if (apply_standardized) {
    for (i in 1:ncol(training_data)) {
      std_info[[i]] <- list(mean = mean(training_data[, i], na.rm = T),
                            sd = sd(training_data[, i], na.rm = T))
      training_data[, i] <- standardized(training_data[, i])
    }
  }

  training_data[post_period[1]:post_period[2], 1] <- NA

  list(training_data = training_data,
       standardized_info = std_info)
}

destandard_pred <- function(.pred, .standarized_info) {
  .mean <- .standardized_info[[1]]$mean
  .sd <- .standardized_info[[1]]$sd

  for (i in 1:ncol(.pred)) {
    .pred[, i] <- destandardized(.pred[, i], .mean, .sd)
  }

  .pred
}

#' Detect contextual anomaly
#'
#' @param .data data table
#' @param pre_period vector
#' @param post_period vector
#' @param ... params for \code{bsts_model}
#' @export
cbar <- function(.data, pre_period, post_period,
                 apply_standardized = T,
                 verbose = getOption("cbar.verbose"),
                 ...) {

  res <- check_data(.data, pre_period, post_period)

  # Create model
  .df <- training_data(.data, post_period, apply_standardized)
  .training_data <- .df[["training_data"]]
  .standardized_info <- .df[["standardized_info"]]

  # TODO: Separate training and prediction
  .model <- bsts_model(.training_data)

  # Predict counterfactual
  .pred <- inference(.model, post_period)
  if (apply_standardized) {
    .pred <- destandard_pred(.pred, .standardized_info)
  }

  structure(list(model = .model,
                 pred = .pred),
            class = "cbar")
}
