#' Print time-series plot
#'
#' @param .cbar cbar object
#' @param x_label a label for x-axis
#' @param y_label a label for y-ayis
#' @param seq_by increment of the sequence, which is NULL by default
#' @return \code{ggplot} object
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_vline xlab ylab
#'             scale_x_continuous theme element_text
#' @export
plot_ts <- function(.cbar, x_label = "", y_label = "", seq_by = NULL) {
  stopifnot(inherits(.cbar, "cbar"))
  target_data <- .cbar$pred
  time_label <- as.character(target_data[, "datetime"])
  target_data[, "datetime"] <- 1:nrow(target_data)

  # TODO: Use S3 class function
  if (is.null(seq_by)) {
    n_row <- nrow(.cbar$pred)
    seq_by <- ifelse(n_row > 50,
                     round(n_row/50), 1)
  }
  brks <- seq(1, n_row, seq_by)
  time_label <- time_label[brks]
  xintercept <- dplyr::filter(target_data, session == "measurement") %>%
                  .[1, "datetime"] - 1

  ggplot(target_data, aes(x = datetime)) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                fill = "PaleVioletRed") +
    geom_line(aes(y = y),
              colour = "black", size = .8, linetype = "solid") +
    geom_line(aes(y = point_pred),
              colour = "DarkRed", size = .8, linetype = "dashed") +
    geom_vline(xintercept = xintercept,
               colour = "black", size = 1, linetype = "solid") +
    xlab(x_label) + ylab(y_label) +
    scale_x_continuous(labels = time_label,
                       breaks = brks) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                     vjust = 0.5, size = 8))
}

# reserved
plot_error <- function(.cbar) {
#  pivot = as.numeric(input$choosetr2) - as.numeric(input$choosetr1) + 1
#  point.effect <- point.effect[pivot : length(point.effect)]
#  boxplot(as.matrix(point.effect), xlab="resource", ylab="estimation error (%)") #, ylim=c(0,1.6))
}

# reversed: Variable selection (each)
plot_var <- function(.cbar,
                     period = c("reference", "measurement")) {
}

# reserved: Variable selection (compare)
plot_var_comp <- function(.cbar) {
}
