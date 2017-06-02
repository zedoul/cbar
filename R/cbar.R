#' Detect contextual anomaly
#'
#' @param .data data table
#' @param pre.period vector
#' @param post.period vector
#' @param ... params for \code{bsts_model}
#' @export
cbar <- function(.data, pre.period, post.period,
                 verbose = getOption("badr.verbose"),
                 ...) {

  res <- check_data(.data, pre.period, post.period)

  .model <- bsts_model(.data, ...)
  .pred <- inference(.model, post.period)

  structure(list(model = .model,
                 prediction = .pred),
            class = "badr")
}
