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
#A Brief Print of Control Chart Parameters for real (mxrpc) and simulated  (mxspc) data

# print method for summary.mxrpc and  summary.mxspc objects

#' @export
print <- function(x, ...) {
  UseMethod("print", x)
}


#' @export
print.summary.mxrpc <- function(x, ...) {
  cat("Summary of Real Data Control Chart (mxrpc)\n")
  cat("----------------------------------------------------\n")
  print(as.data.frame(x[1:7]))
  cat("\nPlotting Statistic Summary:\n")
  print(x$plotting.statistic.summary)
  cat("\nData Summary:\n")
  print(x$data.summary)
  invisible(x)
}


#' @export
print.summary.mxspc <- function(x, ...) {
  cat("Summary of Simulated Data Control Chart (mxspc)\n")
  cat("----------------------------------------------------\n")
  print(as.data.frame(x[1:7]))
  cat("\nPlotting Statistic Summary:\n")
  print(x$plotting.statistic.summary)
  cat("\nSimulated Data Summary:\n")
  print(x$simulated.data.summary)
  invisible(x)
}

#' @export
print.mxrpc <- function(x, ...) {
  cat("Summary of Control Chart Parameters:\n")
  cat("Subgroup Number (m):", x$m, "\n")
  cat("Sample Size (n):", x$n, "\n")
  cat(ifelse(x$limit == "PCL", "Lower Probability Limit (LPL):", "Lower Control Limit (LCL):"), round(x$LCL, 4), "\n")
  cat("Central Line (CL):", round(x$CL, 4), "\n")
  cat(ifelse(x$limit == "PCL", "Upper Probability Limit (UPL):", "Upper Control Limit (UCL):"), round(x$UCL, 4), "\n")
  cat("Estimated Sigma value:", round(x$sig, 4), "\n")
  cat("Limit Type:", x$limit, "\n")
  cat("Chart Type:", x$chart, "\n")
  cat("\nSummary Statistics for Plotting Statistic (V values):\n")
  print(summary(x$v))
  cat("\nSummary Statistics for Real Data:\n")
  print(summary(x$data))
}

#' @export
print.mxspc <- function(x, ...) {
  cat("Summary of Control Chart Parameters:\n")
  cat("Subgroup Number (m):", x$m, "\n")
  cat("Sample Size (n):", x$n, "\n")
  cat(ifelse(x$limit == "PCL", "Lower Probability Limit (LPL):", "Lower Control Limit (LCL):"), round(x$LCL, 4), "\n")
  cat("Central Line (CL):", round(x$CL, 4), "\n")
  cat(ifelse(x$limit == "PCL", "Upper Probability Limit (UPL):", "Upper Control Limit (UCL):"), round(x$UCL, 4), "\n")
  cat("Estimated Sigma value:", round(x$sig, 4), "\n")
  cat("Limit Type:", x$limit, "\n")
  cat("Chart Type:", x$chart, "\n")
  cat("\nSummary Statistics for Plotting Statistic (V values):\n")
  print(summary(x$v))
  cat("\nSummary Statistics for Simulated Data (x values):\n")
  print(summary(x$a))
}



