#' Generate trajectories of the response variable
#'
#' Note that \code{posterior_state_samples} returns posterior mean, whereas,
#' this one returns posterior mean + noise
#'
#' This one is used for lower and upper bounds
#'
#' @param .model bsts model
response_trajectory <- function(.model) {

  # TODO: Recheck burnin rate
  n_burn_in <- bsts::SuggestBurn(0.1, .model)
  stopifnot(n_burn_in > 0)

  # Get noise
  sigma_obs <- .model$sigma.obs[-(1:n_burn_in)]

  # Get the MCMC sample from the posterior predictive mean
  .posterior_mean <- posterior_mean(.model)

  n_samples <-  nrow(.posterior_mean)
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
#' @param .model bsts model
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
#
#' @param yhat response trajectories
#' @param .posetrior_mean posterior mean values
#' @param alpha alpha
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

#' Get inference from predictive posetrior prediction of bsts model
#'
#' @export
inference <- function(.model,
                      alpha = .05) {
  # Compute conterfactual
  y_hat <- response_trajectory(.model)
  .posterior_mean <- posterior_mean(.model)

  point_prediction(y_hat, .posterior_mean, alpha)
}
