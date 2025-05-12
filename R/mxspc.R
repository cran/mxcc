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
#' @import shotGroups
#' @import stats
#' @export
#  V control chart and VSQ control chart for simulated data
mxspc <- function(m = 25, n = 4, alpha = 0.0027, sigma, limit = "PCL", chart = "V", summary = FALSE) {
  if (missing(sigma)) {
    stop("You must provide a value for 'sigma'.")
  }

  x <- rMaxwell(n * m, sigma)
  a <- array(x, dim = c(m, n))
  v <- numeric(m)

  if (chart == "V") {
    for (i in 1:m) {
      y <- a[i, ]
      v[i] <- (sum(y^2)) / (3 * n)
    }
  } else if (chart == "VSQ") {
    for (i in 1:m) {
      y <- a[i, ]
      v[i] <- sqrt((sum(y^2)) / (3 * n))
    }
  } else {
    stop("Invalid chart. Please choose either 'V' or 'VSQ'.")
  }

  if (chart == "V") {
    if (limit == "PCL") {
      LCL <- mxk(n, alpha, type = "V")$L1 * sigma^2
      UCL <- mxk(n, alpha, type = "V")$L2 * sigma^2
      CL <- sigma^2
    } else if (limit == "KCL") {
      LCL <- (1 - mxm(n, alpha, type = "V") * sqrt(2 / (3 * n))) * sigma^2
      UCL <- (1 + mxm(n, alpha, type = "V") * sqrt(2 / (3 * n))) * sigma^2
      CL <- sigma^2
    }
  } else if (chart == "VSQ") {
    t <- (sqrt(2) / sqrt(3 * n)) * (gamma((3 * n + 1) / 2) / gamma(3 * n / 2))
    if (limit == "PCL") {
      LCL <- mxk(n, alpha, type = "VSQ")$P1 * sigma
      UCL <- mxk(n, alpha, type = "VSQ")$P2 * sigma
      CL <- t * sigma
    } else if (limit == "KCL") {
      LCL <- (t - mxm(n, alpha, type = "VSQ") * sqrt(1 - t^2)) * sigma
      UCL <- (t + mxm(n, alpha, type = "VSQ") * sqrt(1 - t^2)) * sigma
      CL <- t * sigma
    }
  }

  output <- list(v = v, a = a, LCL = LCL, CL = CL, UCL = UCL, m = m, n = n, sigma = sigma, limit = limit, chart = chart)

  class(output) <- "mxspc"

  if (summary) {
    cat("Summary of Control Chart Parameters:\n")
    cat("Subgroup Number (m):", m, "\n")
    cat("Sample Size (n):", n, "\n")
    cat(ifelse(limit == "PCL", "Lower Probability Limit (LPL):", "Lower Control Limit (LCL):"), round(LCL, 4), "\n")
    cat("Central Line (CL):", round(CL, 4), "\n")
    cat(ifelse(limit == "PCL", "Upper Probability Limit (UPL):", "Upper Control Limit (UCL):"), round(UCL, 4), "\n")
    cat("Sigma value used:", sigma, "\n")
    cat("Limit Type:", limit, "\n")
    cat("Chart Type:", chart, "\n")
  }

  return(invisible(output))
}
