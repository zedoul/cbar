#' analytical abstraction
#'
#' @importFrom dplyr filter
#' @export
summarise_pred_error <- function(.cbar,
                                 .session = "measurement") {
  stopifnot(inherits(.cbar, "cbar"))

  target_data <- dplyr::filter(.cbar$pred, session == .session)

  error <- abs(target_data$point_pred - target_data$y)

  data.frame(datetime = target_data$datetime,
             session = .session,
             error = error,
             mape = error / abs(target_data$y))
}

#' analytical abstraction
#'
#' @importFrom dplyr filter
#' @export
summarise_anomaly <- function(.cbar,
                              .session = "measurement") {

  stopifnot(inherits(.cbar, "cbar"))

  target_data <- dplyr::filter(.cbar$pred, session == .session)

  anomaly <- as.logical((target_data$y > target_data$upper_bound) +
                        (target_data$y < target_data$lower_bound))
  cbind(target_data, anomaly = anomaly)
}

#' analytical abstraction
#'
#' @export
summarise_sessions <- function(.cbar,
                              .sessions = NULL) {

  stopifnot(inherits(.cbar, "cbar"))

  if (is.null(.sessions)) {
    .sessions <- as.character(unique(.cbar$pred$session))
  }

  ret <- data.frame(session = as.character(),
                    n_anomaly = as.numeric(),
                    n_total = as.numeric(),
                    rate = as.numeric())

  for (.session in .sessions) {
    target_data <- summarise_anomaly(.cbar, .session)
    target_data <- dplyr::filter(target_data, session == .session)

    n_anomaly <- sum(target_data$anomaly)
    n_total <- nrow(target_data)

    res <- data.frame(session = .session,
                      n_anomaly = n_anomaly,
                      n_total = n_total,
                      rate = n_anomaly / n_total)
    ret <- rbind(ret, res)
  }

  ret
}

# analytical abstraction
coef_samples <- function(.cbar) {
  stopifnot(inherits(.cbar, "cbar"))
  coef_mat <- .cbar$model$coefficients
  col_names <- colnames(coef_mat)
  coefs <- list()
  for (i in 1:ncol(coef_mat)) {
    coefs[[col_names[i]]] <- coef_mat[, i]
  }
  coefs
}

# analytical abstraction
coef_estimate <- function(.cbar) {
  stopifnot(inherits(.cbar, "cbar"))
  .vars <- coef_samples(.cbar)

  coefs <- c()
  for (var_name in names(.vars)) {
    val <- median(.vars[[var_name]])
    coefs <- c(coefs, val)
  }
  names(coefs) <- names(.vars)
  coefs
}

#' analytical abstraction
#'
#' @importFrom dplyr filter
#' @export
summarise_incprob <- function(.cbar, threshold = .1) {
  stopifnot(inherits(.cbar, "cbar"))
  coefs <- coef_estimate(.cbar)
  coefs <- coefs / max(coefs)
  coefs[coefs > threshold]
}
