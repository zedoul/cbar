#' Summarise prediction error of model
#'
#' This function uses absolute difference and mean absolute percentage error
#' for summarising prediction errors
#'
#' @param .cbar \code{cbar} object
#' @param .session names of sessions, which is NULL by default
#' @importFrom dplyr filter
#' @return \code{data.frame} with prediction errors
#' @export
summarise_pred_error <- function(.cbar,
                                 .session = "measurement") {
  stopifnot(inherits(.cbar, "cbar"))

  target_data <- dplyr::filter(.cbar$pred, session == .session)

  error <- abs(target_data$point_pred - target_data$y)

  data.frame(datetime = target_data$datetime,
             session = .session,
             diff = error,
             mape = error / abs(target_data$y))
}

#' Summarise anomaly detection result
#'
#' @param .cbar \code{cbar} object
#' @param .session names of sessions, which is NULL by default
#' @importFrom dplyr filter
#' @return \code{data.frame} that summarises input data with anomaly label
#' @export
summarise_anomaly <- function(.cbar,
                              .session = NULL) {

  stopifnot(inherits(.cbar, "cbar"))

  if (is.null(.session)) {
    .session <- as.character(unique(.cbar$pred$session))
  }

  ret <- data.frame(datetime = as.character(),
                    session = as.character(),
                    y = as.numeric(),
                    point_pred = as.numeric(),
                    lower_bound = as.numeric(),
                    upper_bound = as.numeric(),
                    anomaly = as.logical())

  for (.session_name in .session) {
    target_data <- dplyr::filter(.cbar$pred, session == .session_name)
    anomaly <- as.logical((target_data$y > target_data$upper_bound) +
                          (target_data$y < target_data$lower_bound))
    res <- cbind(target_data, anomaly = anomaly)
    ret <- rbind(ret, res)
  }

  ret
}

#' Summarise anomaly in session
#'
#' @param .cbar \code{cbar} object
#' @param .session names of sessions, which is NULL by default
#' @importFrom dplyr filter
#' @return \code{data.frame} that summarises outcome for each sessoin
#' @export
summarise_session <- function(.cbar,
                              .session = NULL) {

  stopifnot(inherits(.cbar, "cbar"))

  if (is.null(.session)) {
    .session <- as.character(unique(.cbar$pred$session))
  }

  ret <- data.frame(session = as.character(),
                    n_anomaly = as.numeric(),
                    n_total = as.numeric(),
                    rate = as.numeric())

  for (.session_name in .session) {
    target_data <- summarise_anomaly(.cbar, .session_name)
    target_data <- dplyr::filter(target_data, session == .session_name)

    n_anomaly <- sum(target_data$anomaly)
    n_total <- nrow(target_data)

    res <- data.frame(session = .session_name,
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

#' @importFrom stats median
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

#' Summarise inclusion probability of model
#'
#' @param .cbar \code{cbar} object
#' @param threshold threshold of inclusion probability, which is .1 by default
#' @return \code{vector} that summarises inclusion probabilities for each MCMC
#'   samples
#' @export
summarise_incprob <- function(.cbar, threshold = .1) {
  stopifnot(inherits(.cbar, "cbar"))
  coefs <- coef_estimate(.cbar)
  coefs <- abs(coefs)
  coefs <- coefs / max(coefs)
  coefs[coefs > threshold]
}
