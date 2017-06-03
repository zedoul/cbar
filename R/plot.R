#' Visual abstraction
#'
#' @param .cbar cbar object
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line
cbar_plot <- function(.cbar) {
  # TODO: Use S3 class function

  ggplot(.cbar$pred, aes(x = datetime)) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                fill = "SlateGray2") +
    geom_line(aes(y = y),
              colour = "black", size = .8, linetype = "solid") +
    geom_line(aes(y = point_pred),
              colour = "darkblue", size = .8, linetype = "dashed")
}
