#' Print time-series plot
#'
#' @param target_data data frame
#' @param x_label a label for x-axis
#' @param y_label a label for y-ayis
#' @param seq_by increment of the sequence, which is NULL by default
#' @return \code{ggplot} object
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_vline xlab ylab
#'             scale_x_continuous theme element_text
#' @export
plot_ts_ <- function(target_data, x_label = "", y_label = "", seq_by = NULL) {
  time_label <- as.character(target_data[, "datetime"])
  target_data[, "datetime"] <- 1:nrow(target_data)

  # TODO: Use S3 class function
  if (is.null(seq_by)) {
    n_row <- nrow(target_data)
    seq_by <- ifelse(n_row > 50,
                     round(n_row / 50), 1)
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
    theme(axis.text.x = element_text(angle = 50, hjust = 1,
                                     vjust = 1, size = 12))
}

#' Print time-series plot
#'
#' @param .cbar cbar object
#' @param x_label a label for x-axis
#' @param y_label a label for y-ayis
#' @param seq_by increment of the sequence, which is NULL by default
#' @return \code{ggplot} object
#' @export
plot_ts <- function(.cbar, x_label = "", y_label = "", seq_by = NULL) {
  stopifnot(inherits(.cbar, "cbar"))
  target_data <- .cbar$pred
  plot_ts_(target_data, x_label, y_label, seq_by)
}

#' Print estimation error plot
#'
#' @param .error error data frame
#' @param xlab a label for x-axis
#' @param ylab a label for y-ayis
#' @param method diff
#' @param ... params for boxplot
#' @importFrom graphics boxplot
#' @export
plot_error_ <- function(.error,
                       xlab = "",
                       ylab = "Estimation error",
                       method = "diff",
                       ...) {
  boxplot(.error[, method],
          xlab = xlab, ylab = ylab, ...)
}


#' Print estimation error plot
#'
#' @param .cbar cbar object
#' @param xlab a label for x-axis
#' @param ylab a label for y-ayis
#' @param method diff
#' @param ... params for boxplot
#' @return \code{boxplot} object
#' @export
plot_error <- function(.cbar,
                       xlab = "",
                       ylab = "Estimation error",
                       method = "diff",
                       ...) {
  .error <- summarise_pred_error(.cbar)

  plot_error_(.error[, method], xlab, ylab, method, ...)
}

#' Print inclusion probablity plot
#'
#' @param .incprob data frame
#' @param threshold a threhold for inclusion probablity
#' @param horiz horiz
#' @param cex.names cex.names
#' @param xlab xlab
#' @param las las
#' @param ... params for barplot
#' @return \code{boxplot} object
#' @importFrom graphics barplot
#' @export
plot_incprob_ <- function(.incprob,
                         threshold = .1,
                         horiz = T,
                         cex.names = .5,
                         xlab = "Inclusion probability (%)",
                         las = 1,
                         ...) {
  barplot(sort(.incprob) * 100,
          horiz = horiz, cex.names = cex.names,
          xlab = xlab, las = las, ...)
}


#' Print inclusion probablity plot
#'
#' @param .cbar cbar object
#' @param threshold a threhold for inclusion probablity
#' @param horiz horiz
#' @param cex.names cex.names
#' @param xlab xlab
#' @param las las
#' @param ... params for barplot
#' @return \code{boxplot} object
#' @export
plot_incprob <- function(.cbar,
                         threshold = .1,
                         horiz = T,
                         cex.names = .5,
                         xlab = "Inclusion probability (%)",
                         las = 1,
                         ...) {
  .incprob <- summarise_incprob(.cbar, threshold)
  plot_incprob_(.incprob, threshold, horiz, cex.names, xlab, las, ...)
}

# reserved: Variable selection (compare)
plot_incprob_comp <- function(.cbar) {
}
