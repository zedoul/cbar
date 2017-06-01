#' @importFrom magrittr %>%
#' @export
inference <- function(model_bsts, y, post.period, alpha = .05) {
  check_model(model_bsts, y, post.period)
}
