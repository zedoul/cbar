#' Generate trajectories of the response variable
#'
#' Note that \code{posterior_state_samples} returns posterior mean, whereas,
#' this one returns posterior mean + noise
#'
#' This one is used for lower and upper bounds
#'
#' @param .model \code{bsts_model}
#' @param seed a seed value. NULL by default
#' @return \code{data.frame} that contains predicted value \code{y_hat}
#' @importFrom stats rnorm
response_trajectory <- function(.model, seed = NULL) {

  # TODO: Recheck burnin rate
  n_burn_in <- bsts::SuggestBurn(0.1, .model)
  stopifnot(n_burn_in > 0)

  # Get noise
  sigma_obs <- .model$sigma.obs[-(1:n_burn_in)]

  # Get the MCMC sample from the posterior predictive mean
  .posterior_mean <- posterior_mean(.model)

  n_samples <-  nrow(.posterior_mean)

  set.seed(seed)
  .noise <- rnorm(n = prod(dim(.posterior_mean)),
                  mean = 0, sd = sigma_obs)
  .noise <- matrix(.noise,
                   nrow = n_samples)
  y_hat <- .posterior_mean + .noise
  as.data.frame(y_hat)
}

#' Generate posterior mean of the response variable
#'
#' This one is used for point prediction based one predictive posterior
#' distribution
#'
#' @param .model \code{bsts_model}
#' @return \code{vector} that contains poseterior means
posterior_mean <- function(.model) {
  stopifnot(inherits(.model, "bsts"))

  # TODO: Recheck burnin rate
  n_burn_in <- bsts::SuggestBurn(0.1, .model)

  stopifnot(n_burn_in > 0)

  # state comtributions = MCMC x (trend, regression) x time
  state_contributions <- .model$state.contributions[-(1:n_burn_in), , ,
                                                    drop = F]

  # aggregate trend and regression
  posterior_mean <- rowSums(aperm(state_contributions, c(1, 3, 2)),
                            dims = 2)
  posterior_mean
}

#' Get point prediction from posterior means and response trajectories
#'
#' @param y_hat response trajectories
#' @param .posterior_mean posterior mean values
#' @param alpha alpha
#' @return \code{data.frame} for predicted values
#' @importFrom stats quantile
point_prediction <- function(y_hat,
                             .posterior_mean,
                             alpha = .05) {
  point_pred_mean <- colMeans(.posterior_mean)

  prob_lower <- alpha / 2
  prob_upper <- 1 - prob_lower
  lower_bound <- as.numeric(t(apply(y_hat, 2, quantile,
                                    prob_lower)))
  upper_bound <- as.numeric(t(apply(y_hat, 2, quantile,
                                    prob_upper)))

  data.frame(point_pred = point_pred_mean,
             lower_bound = lower_bound,
             upper_bound = upper_bound)
}

#' Infer from predictive posetrior prediction of bsts model
#'
#' @param .model \code{bsts} model
#' @param alpha percentile for anomaly
#' @param seed a seed value. NULL by default
#' @return \code{data.frame} with observations and predictions
inference <- function(.model,
                      alpha = .05,
                      seed = NULL) {
  # Compute conterfactual
  y_hat <- response_trajectory(.model, seed)
  .posterior_mean <- posterior_mean(.model)

  point_prediction(y_hat, .posterior_mean, alpha)
}
