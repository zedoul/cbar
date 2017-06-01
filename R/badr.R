#' @param .data
#' @param pre.period
#' @param post.period
#' @param ... params for \code{bsts_model}
#' @export
badr <- function(.data, pre.period, post.period,
                 verbose = getOption("badr.verbose"),
                 ...) {

  res <- check_data(.data, pre.period, post.period)

  .model <- bsts_model(...)
  .pred <- inference(.model, post.period)

  structure(list(model = .model,
                 prediction = .pred),
            class = "badr")
}
