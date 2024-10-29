#-----------------------------------------------------------------------------#
#                                                                             #
#R Package for Maxwell Control Charts                             #
#                                                                             #
#  Written by: Zahid Khan, Zsolt T. Kosztyan                                  #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kosztyan.zsolt@gtk.uni-pannon.hu                               #
#                                                                             #
# Last modified: October 2024                                                  #
#-----------------------------------------------------------------------------#

#' @export
# Power Computation of V Chart and VSQ Chart
mxp <- function(n = 1, alpha = 0.0027, delta = 1, type = "V") {
  if (delta < 1 || delta > 6) {
    stop("Delta should be between 1 and 6, inclusive.")
  }

  if (type == "V") {
    a <- (1 / delta) * qgamma(alpha / 2, shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    b <- (1 / delta) * qgamma((1 - (alpha / 2)), shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    c <- pgamma(a, shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    d <- pgamma(b, shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    power <- 1 + c - d
    return(power)

  } else if (type == "VSQ") {
    df <- 3 * n
    a <- delta * qchi((1 - (alpha / 2)), df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    b <- delta * qchi(alpha / 2, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    c <- pchi(a, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    d <- pchi(b, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    beta <- c - d
    power <- 1 - beta
    return(power)
  } else {
    stop("Invalid chart type. Use 'V' or 'VSQ'.")
  }
}
