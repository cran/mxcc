\name{mxpw}
\alias{mxpw}
\title{
Power Curves Construction for V chart and VSQ chart
}
\description{
This function computes the power curves for V or VSQ control charts based on the Maxwell distribution. It allows the user to analyze the performance of these charts under different sample sizes and shifts in the process parameter.
}
\usage{
mxpw(n = 1, delta = seq(1, 3, length.out = 100), alpha = 0.0027, type = "V")
}

\arguments{
  \item{n}{
A numeric vector specifying the sample sizes. Default is \code{1}.
  }
  \item{delta}{
A numeric vector specifying the shift constants for the control chart. Default is \code{seq(1, 3, length.out = 100)}.
  }
  \item{alpha}{
A numeric value specifying the significance level (false alarm probability). Default is \code{0.0027}.
  }
  \item{type}{
A character string specifying the type of chart to be used. Can be either \code{"V"} chart or \code{"VSQ"}. Default is \code{"V"}.
  }
}
\details{
This function calculates the power curves for either the V or VSQ control charts, depending on the specified \code{type} parameter. It computes the power values for different sample sizes and shift constants. The function uses the Gamma and Chi distribution functions for the V and VSQ charts, respectively.}
\value{
The function returns a plot of power curves for the specified control chart type
}
\references{
Hossain, M.P., Omar, M.H. and Riaz, M. (2017) "New V control chart for the Maxwell distribution". Journal of Statistical Computation and Simulation, 87(3), pp.594-606. <doi:10.1080/00949655.2016.1222391>

Shah, F., Khan, Z., Aslam, M. and Kadry, S. (2021) "Statistical Development of the VSQ‐Control Chart for Extreme Data with an Application to the Carbon Fiber Industry". Mathematical Problems in Engineering, 2021(1), p.9766986. <doi:10.1155/2021/9766986>
}
\author{
Zahid Khan
}

\seealso{
\code{\link{mxp}}
}
\examples{
mxpw(n = c(5,8), alpha = 0.0027, type = "VSQ")

}
