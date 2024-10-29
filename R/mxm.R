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
#' @import chi
#' @import stats
#' @export
# L-sigma multiplier for V chart and VSQ chart
mxm <- function(n = 1, alpha = 0.0027, type = "V") {
  if (type == "V") {
        L <- qgamma(alpha, shape = (3 * n) / 2, rate = 1, lower.tail = TRUE, log.p = FALSE)
  } else if (type == "VSQ") {
       L <- qchi(alpha, df = 3 * n, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  } else {
    stop("Invalid chart type. Please choose 'V' or 'VSQ'.")
  }

  return(L)
}

