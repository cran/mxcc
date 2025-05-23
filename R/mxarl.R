#-----------------------------------------------------------------------------#
#                                                                             #
#R Package for Maxwell Control Charts                             #
#                                                                             #
#  Written by: Zahid Khan, Zsolt T. Kosztyan                                  #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kosztyan.zsolt@gtk.uni-pannon.hu                               #
#                                                                             #
# Last modified: February 2025                                                  #
#-----------------------------------------------------------------------------#

#' @export
#  Plot ARL curves for V or VSQ chart
mxarl <- function(n = 1,
                  delta = seq(1, 3, length.out = 100),
                  alpha = 0.0027,
                  type = "V") {

  arl_v <- function(n, delta) {
    a <- (1 / delta) * qgamma(alpha / 2, shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    b <- (1 / delta) * qgamma((1 - (alpha / 2)), shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    c <- pgamma(a, shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    d <- pgamma(b, shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    pw <- 1 + c - d
    arl <- 1 / pw
    return(arl)
  }

  arl_vsq <- function(n, delta) {
    df <- 3 * n
    a <- delta * qchisq((1 - (alpha / 2)), df = df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    b <- delta * qchisq(alpha / 2, df = df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    c <- pchisq(a, df = df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    d <- pchisq(b, df = df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    beta <- c - d
    pw <- 1 - beta
    arl <- 1 / pw
    return(arl)
  }

  if (type == "V") {
    arl_data <- sapply(n, function(n) {
      sapply(delta, function(delta) arl_v(n, delta))
    })
  } else if (type == "VSQ") {
    arl_data <- sapply(n, function(n) {
      sapply(delta, function(delta) arl_vsq(n, delta))
    })
  } else {
    stop("Invalid chart_type. Please choose 'V' or 'VSQ'.")
  }

  if (is.vector(arl_data)) {
    arl_data <- matrix(arl_data, ncol = 1)
  }

  line_types <- c("solid", "dashed", "dotted", "dotdash")

  plot(delta, arl_data[, 1], type = "l", lty = line_types[1], lwd = 2, xlab = "Shift constant", ylab = "ARL", main = "")

  if (length(n) > 1) {
    for (i in 2:length(n)) {
      lines(delta, arl_data[, i], lty = line_types[i %% length(line_types) + 1], lwd = 2)
    }
  }

  if (length(n) > 1) {
    legend("topright", legend = paste("n =", n), lty = line_types, lwd = 2)
  }
}
