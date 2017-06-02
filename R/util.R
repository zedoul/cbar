#' Standarized a vector
#'
#' @param y numeric vector
#' @export
standarized <- function(y) {
  y_mu <- mean(y, na.rm = T)
  y_sd <- sd(y, na.rm = T)
  stopifnot(!is.na(y_mu) && !is.nan(y_mu))
  stopifnot(!is.na(y_sd) && y_sd > 0)

  (y - y_mu) / y.sd
}

#' Destandarized a vector
#'
#' @param y_hat standarized numeric vector
#' @param y_mu a mean value of unstandarized vector
#' @param y_sd a standard deviation of unstandarized vector
#' @export
destandarized <- function(y_hat, y_mu, y_sd) {
  stopifnot(!is.na(y_mu) && !is.nan(y_mu))
  stopifnot(!is.na(y_sd) && y_sd > 0)

  y * y_sd + y_mu
}
