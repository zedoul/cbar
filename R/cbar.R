#' Detect contextual anomaly
#'
#' @param .data data table
#' @param pre_period vector
#' @param post_period vector
#' @param ... params for \code{bsts_model}
#' @export
cbar <- function(.data, pre_period, post_period,
                 verbose = getOption("cbar.verbose"),
                 ...) {

  res <- check_data(.data, pre_period, post_period)

  # Create model
  training_data <- .data
  training_data[post_period[1]:post_period[2], 1] <- NA

  # TODO: Separate training and prediction
  .model <- bsts_model(training_data)

  # Predict counterfactual
  .pred <- inference(.model, post_period)

  structure(list(model = .model,
                 pred = .pred),
            class = "cbar")
}
