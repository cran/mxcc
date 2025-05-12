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
#' @import chi
#' @import stats
#' @export
# k -Coefficients Determination for V Chart and VSQ Chart
mxk <- function(n = 1, alpha = 0.0027, type = "V") {

  # V-chart
  if (type == "V") {
    L1 <- (2 / (3 * n)) * qgamma(alpha / 2, shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    L2 <- (2 / (3 * n)) * qgamma((1 - alpha / 2), shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    return(list(L1 = L1, L2 = L2))

    # VSQ chart
  } else if (type == "VSQ") {
    t <- (sqrt(2) / sqrt(3 * n)) * (gamma((3 * n + 1) / 2) / gamma(3 * n / 2))
    a <- 1 / sqrt(3 * n)
    P1 <- a * qchi(alpha / 2, df = 3 * n, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    P2 <- a * qchi((1 - (alpha / 2)), df = 3 * n, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    P3 <- (a/t) * qchi(alpha / 2, df = 3 * n, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    P4 <- (a/t) * qchi((1 - (alpha / 2)), df = 3 * n, ncp = 0, lower.tail = TRUE, log.p = FALSE)

    return(list(P1 = P1, P2 = P2,P3 = P3,P4 = P4))
  } else {
    stop("Invalid chart type. Use 'V' or 'VSQ'.")
  }
}
