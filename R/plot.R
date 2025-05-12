
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
#Control Chart plots for real (mxrpc) and simulated  (mxspc) data

#' @export
plot <- function(x, ...) {
  UseMethod("plot", x)
}


#' @export
plot.mxrpc <- function(x, ...) {
  m <- x$m
  n <- x$n
  v <- x$v
  LCL <- x$LCL
  CL <- x$CL
  UCL <- x$UCL
  limit <- x$limit
  chart <- x$chart

  y_label <- ifelse(chart == "V", "V", expression(paste(V[SQ])))

  # Set up plotting parameters
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mar = c(5, 5, 4, 10) + 0.1)

  # Plot the control chart
  plot(1:m, v, type = "b", pch = 20, col = "darkgreen", lwd = 2,
       ylim = c(LCL * 0.9, UCL * 1.1), xlab = "Sample Number",
       ylab = y_label, cex.axis = 1.2, cex.main = 1.5,...)

  # Adding control lines
  abline(h = LCL, col = "blue", lty = 2, lwd = 2)
  abline(h = CL, col = "blue", lty = 1, lwd = 2)
  abline(h = UCL, col = "blue", lty = 2, lwd = 2)

  # Adding legend
  legend_labels <- if (limit == "PCL") c("LPC", "CL", "UPL", "Plotting Statistic") else c("LCL", "CL", "UCL", "Plotting Statistic")
  legend("topright", inset = c(-0.45, 0), legend = legend_labels,
         col = c("blue", "blue", "blue", "darkgreen"), lty = c(2, 1, 2, 1), lwd = 2,
         pch = c(NA, NA, NA, 20), xpd = TRUE, bty = "n", cex = 0.9)
}

#' @export
plot.mxspc <- function(x, ...) {
  # Extracting data from the object
  m <- x$m
  n <- x$n
  v <- x$v
  LCL <- x$LCL
  CL <- x$CL
  UCL <- x$UCL
  limit <- x$limit
  chart <- x$chart

  y_label <- ifelse(chart == "VSQ", expression(paste(V[SQ])), "V")

  # Set up plotting parameters
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mar = c(5, 5, 4, 10) + 0.1)

  # Plot the control chart
  plot(1:m, v, type = "b", pch = 20, col = "darkred", lwd = 2,
       ylim = c(LCL * 0.9, UCL * 1.1), xlab = "Sample Number",
       ylab = y_label, cex.axis = 1.2, cex.main = 1.5,...)

  # Adding control lines
  abline(h = LCL, col = "blue", lty = 2, lwd = 2)
  abline(h = CL, col = "blue", lty = 1, lwd = 2)
  abline(h = UCL, col = "blue", lty = 2, lwd = 2)

  # Adding legend
  legend_labels <- if (limit == "PCL") c("LPC", "CL", "UPL", "Plotting Statistic") else c("LCL", "CL", "UCL", "Plotting Statistic")
  legend("topright", inset = c(-0.45, 0), legend = legend_labels,
         col = c("blue", "blue", "blue", "darkred"), lty = c(2, 1, 2, 1), lwd = 2,
         pch = c(NA, NA, NA, 20), xpd = TRUE, bty = "n", cex = 0.9)
}
