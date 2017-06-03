#' Create bsts model
#'
#' @param .data time-series data to be trained
#' @param spec bsts model specification
#' @importFrom bsts bsts
#' @export
bsts_model <- function(.data, spec = NULL) {
  if (is.null(spec)) {
    spec <- bsts_spec_static(.data)
  }
  stopifnot(inherits(spec, "cbar.model.spec"))
  class(spec) <- "list"
  do.call(bsts::bsts, spec)
}

#' Specify bsts model for static linear regression
#'
#' @param .data time-series data to be trained
#' @importFrom bsts AddLocalLevel BstsOptions
#' @importFrom Boom SdPrior
#' @export
bsts_spec_static <- function(.data,
                             sigma_guess = NULL,
                             upper_limit = NULL,
                             sd_prior_sample_size = 32,
                             expected_model_size = 3,
                             expected_r2 = 0.8,
                             prior_df = 50,
                             niter = 1000,
                             model_options = NULL,
                             ...) {
  y <- .data[, 1]

  if (is.null(sigma_guess)) {
    sigma_guess <- 0.01 * sd(y, na.rm = TRUE)
  }
  stopifnot(sigma_guess > 0)

  if (is.null(upper_limit)) {
    upper_limit <- sd(y, na.rm = T)
  }
  stopifnot(upper_limit > 0)

  if (is.null(model_options)) {
    model_options <- bsts::BstsOptions(save.prediction.errors = TRUE)
  }
  stopifnot(inherits(model_options, "BstsOptions"))

  sd_prior <- Boom::SdPrior(sigma.guess = sigma_guess,
                      upper.limit = upper_limit,
                      sample.size = sd_prior_sample_size)
  ss <- bsts::AddLocalLevel(list(), y, sigma.prior = sd_prior)

  structure(list(formula = paste0(names(.data)[1], sep = " ~ ."),
                 data = .data,
                 state.specification = ss,
                 expected.model.size = expected_model_size,
                 expected.r2 = expected_r2,
                 prior.df = prior_df,
                 niter = niter,
                 model.options = model_options),
            class = "cbar.model.spec")
}

#' @export
bsts_spec_dynamic <- function() {
  stop("Not implemented yet")
}
