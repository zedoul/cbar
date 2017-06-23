#' \code{cbar} package
#'
#' cbar: Contextual Bayesian Anomaly Detection in R
#'
#' See the README on
#' \href{https://github.com/zedoul/cbar}{Github}
#' @docType package
#' @name cbar
#' @importFrom magrittr %>%

# quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "session", "datetime", "y", "lower_bound",
                           "upper_bound", "point_pred"))
}

#' @importFrom stats sd
prepare_data <- function(.data,
                         ref_period,
                         mea_period,
                         apply_standardized = T) {
  # TDDO: Support flexible ref and mea period

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

#' Detect contextual anomaly with Bayesian Contextual Anomaly Detection in R
#'
#' This function generates \code{cbar} object to detect contextual anomaly and
#' to abstract analysis output.
#'
#' For the input \code{.data}, note that you should use \code{datetime} for the
#' first column name. Also, you should use \code{numeric} type for other
#' columns.
#'
#' @param .data data table with datetime, y, and predictors
#' @param ref_period performance reference period
#' @param mea_period performance measurement period
#' @param apply_standardized whether it will standardized data or not
#' @param ... params for \code{bsts_model}
#' @export
#' @examples
#' library(cbar)
#'
#' .data <- mtcars
#' rownames(.data) <- NULL
#' datetime <- seq(from = Sys.time(), length.out = nrow(.data), by = "mins")
#' .data <- cbind(datetime = datetime, .data)
#'
#' ref_session <- 1:16
#' mea_session <- 17:nrow(.data)
#'
#' obj <- cbar(.data, ref_session, mea_session)
cbar <- function(.data,
                 ref_period,
                 mea_period,
                 apply_standardized = T,
                 ...) {
  # TODO: Consider the possibility that we may use check_data function for this
  stopifnot(colnames(.data)[1] == "datetime")
  stopifnot(any(inherits(.data[, 1], "POSIXt"),
                inherits(.data[, 1], "POSIXct"),
                inherits(.data[, 1], "POSIXlt"),
                inherits(.data[, 1], "character")))
  stopifnot(all(sapply(2:ncol(.data),
                       function(i) inherits(.data[, i], "numeric"))))

  # TODO: Support flexible session names, more than reference and measurement

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

#' Print cbar object
#'
#' @param x \code{cbar} object to print
#' @param ... further arguments passed to or from other methods
#' @export
print.cbar <- function(x, ...) {
  summarise_session(x)
}
