# TDDO: Support flexible ref and mea period
prepare_data <- function(.data,
                         ref_period,
                         mea_period,
                         apply_standardized = T) {

  std_info <- list()

  if (apply_standardized) {
    # Exclude datetime
    std_info <- list(mean = mean(.data[, 2], na.rm = T),
                     sd = sd(.data[, 2], na.rm = T))

    for (i in 2:ncol(.data)) {
      .data[, i] <- standardized(.data[, i])
    }
  }

  .data[mea_period, 2] <- NA

  list(data = .data,
       std = std_info)
}

destandard_pred <- function(.pred, .std_info) {
  stopifnot(ncol(.pred) == length(c("point_pred",
                                    "lower_bound",
                                    "upper_bound")))

  if (!is.null(.std_info)) {
    .mean <- .std_info$mean
    .sd <- .std_info$sd

    for (i in 1:ncol(.pred)) {
      .pred[, i] <- destandardized(.pred[, i], .mean, .sd)
    }
  }

  .pred
}

#' Detect contextual anomaly
#'
#' In anomaly detection, especailly in telecommunication, we call it reference
#' and measurement period
#'
#' @param .data data table with datetime, y, and predictors
#' @param ref_period performance reference period
#' @param mea_period performance measurement period
#' @param ... params for \code{bsts_model}
#' @export
cbar <- function(.data,
                 ref_period,
                 mea_period,
                 apply_standardized = T,
                 verbose = getOption("cbar.verbose"),
                 ...) {
  # TODO: Check data
  # the first column should be datetime/date and second one should be y
  # check_data(.data, pre_period, post_period)
  # .data should NOT have any NA value... or... I know not

  # Create model
  ret <- prepare_data(.data,
                      ref_period, mea_period,
                      apply_standardized)

  target_data <- ret[["data"]]

  # TODO: Separate training and prediction
  # Predict counterfactual
  .model <- bsts_model(target_data, ...)

  # Summarise intervals and point estimates
  res <- inference(.model)
  if (apply_standardized) {
    res <- destandard_pred(res, ret[["std"]])
  }

  .pred <- cbind(datetime = .data[, 1],
                 session = c(rep("reference",
                                length(ref_period)),
                             rep("measurement",
                                length(mea_period))),
                 y = .data[, 2],
                 res)

  structure(list(model = .model,
                 pred = .pred),
            class = "cbar")
}
