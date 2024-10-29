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
# Run length distribution characteristics of V Chart and VSQ Chart
mxrl<- function(n = 1, alpha = 0.0027, delta = 1, type = "V") {
  if (delta < 1 || delta > 6) {
    stop("Delta should be between 1 and 6, inclusive.")
  }

  if (type == "V") {
    a <- (1 / delta) * qgamma(alpha / 2, shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    b <- (1 / delta) * qgamma((1 - (alpha / 2)), shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    c <- pgamma(a, shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    d <- pgamma(b, shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
    power <- 1 + c - d
    ARL <- 1 / power
    SDRL <- sd(rgeom(100000, power))
    MRL <- median(rgeom(100000, power))

    result <- list(ARL = round(ARL, 4), SDRL = round(SDRL, 4), MRL = round(MRL, 4))
    return(result)

  } else if (type == "VSQ") {
    df <- 3 * n
    a <- delta * qchi((1 - (alpha / 2)), df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    b <- delta * qchi(alpha / 2, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    c <- pchi(a, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    d <- pchi(b, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    beta <- c - d
    power <- 1 - beta
    ARL <- 1 / power
    SDRL <- sd(rgeom(100000, power))
    MRL <- median(rgeom(100000, power))

    result <- list(ARL = round(ARL, 4), SDRL = round(SDRL, 4), MRL = round(MRL, 4))
    return(result)
  } else {
    stop("Invalid chart type. Use 'V' or 'VSQ'.")
  }
}
