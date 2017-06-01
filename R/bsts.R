#' @export
bsts_model <- function(.data, spec = NULL) {
  if (is.null(spec)) {
    spec <- bsts_spec_static(.data)
  }
  do.call(bsts, spec)
}

#' @export
bsts_spec_static <- function(.data, ...) {
  sd_prior <- SdPrior(sigma.guess = guess,
                      upper.limit = sdy,
                      sample.size = sample_siae)

  list(formula = paste(names(.data), sep = " ~ ."),
       data = .data,
       state.specification = ss,
       expected.model.size = xx,
       expected.r2 = xx,
       prior.df = xx,
       model.options = xx)
}

#' @export
bsts_spec_dynamic <- function() {
  stop("Not implemented yet")
}
