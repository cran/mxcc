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
#  V control chart and VSQ control chart for real data process
mxrpc <- function(data, alpha = 0.0027, limit = "PCL", chart = "V", summary = FALSE) {

  if (!is.data.frame(data)) {
    stop("The input 'data' must be a data frame.")
  }

  m <- nrow(data)
  n <- ncol(data)

  v <- numeric(m)

    if (chart == "V") {
    for (i in 1:m) {
      y <- as.numeric(data[i, ])  # Get the row data
      v[i] <- (sum(y^2)) / (3 * n)
    }
  } else if (chart == "VSQ") {
    for (i in 1:m) {
      y <- as.numeric(data[i, ])  # Get the row data
      v[i] <- sqrt((sum(y^2)) / (3 * n))
    }
  } else {
    stop("Invalid chart. Please choose either 'V' or 'VSQ'.")
  }

  sig <- mean(v)


  if (chart == "V") {
    if (limit == "PCL") {
      LCL <- mxk(n, alpha, type = "V")$L1 * sig
      UCL <- mxk(n, alpha, type = "V")$L2 * sig
      CL <- sig
    } else if (limit == "KCL") {
      LCL <- (1 - mxm(n, alpha, type = "V") * sqrt(2 / (3 * n))) * sig
      UCL <- (1 + mxm(n, alpha, type = "V") * sqrt(2 / (3 * n))) * sig
      CL <- sig
    }
  } else if (chart == "VSQ") {
    t <- (sqrt(2) / sqrt(3 * n)) * (gamma((3 * n + 1) / 2) / gamma(3 * n / 2))
    if (limit == "PCL") {
      LCL <- mxk(n, alpha, type = "VSQ")$P3 * (sig)
      UCL <- mxk(n, alpha, type = "VSQ")$P4 * (sig)
      CL <- sig
    } else if (limit == "KCL") {
      LCL <- (1 - (mxm(n, alpha, type = "VSQ") / t) * sqrt(1 - t^2)) * sig
      UCL <- (1 + (mxm(n, alpha, type = "VSQ") / t) * sqrt(1 - t^2)) * sig
      CL <- sig
    }
  }
  output<- list(v=v, data=data,LCL = LCL, CL = CL, UCL = UCL, m = m, n = n, sig = sig, limit = limit, chart = chart)
  class(output) <- "mxrpc"

  if (summary) {
    cat("Summary of Control Chart Parameters:\n")
    cat("Subgroup Number (m):", m, "\n")
    cat("Sample Size (n):", n, "\n")
    cat(ifelse(limit == "PCL", "Lower Probability Limit (LPL):", "Lower Control Limit (LCL):"), round(LCL, 4), "\n")
    cat("Central Line (CL):", round(CL, 4), "\n")
    cat(ifelse(limit == "PCL", "Upper Probability Limit (UPL):", "Upper Control Limit (UCL):"), round(UCL, 4), "\n")
    cat("Estimated Sigma value:", sig, "\n")
    cat("Limit Type:", limit, "\n")
    cat("Chart Type:", chart, "\n")
  }

  return(invisible(output))
}

