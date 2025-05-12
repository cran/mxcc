\name{mxm}
\alias{mxm}

\title{
Determination of K-sigma limit Coefficients for V and VSQ Charts for Maxwell-Distributed Quality Characteristics
}
\description{
This function calculates the K-sigma multiplier (L)  for control chart based on the specified type: V chart or VSQ chart. The calculation is based on the sample size  and the false alarm probability.This multiplier can further be used in the construction of coefficients \code{W1}and \code{W2} for the V chart and coefficients \code{W1},\code{W2},\code{W3} and \code{W4} for the VSQ chart.
}
\usage{
mxm(n = 1, alpha = 0.0027, type = "V")
}
\arguments{
  \item{n}{
    Sample size used in the chart. Default is 1.
  }
  \item{alpha}{
    False alarm probability. Default is 0.0027.
  }
  \item{type}{
    The type of control chart. Can be "V" for V chart or "VSQ" for VSQ chart. Default is "V".
  }
}
\details{
The function computes the K-sigma multiplier for either V chart or VSQ chart based on the specified type. If the type is "V", it uses the gamma distribution with the specified parameters. If the type is "VSQ", it uses the ch distribution. The output is the L value that represent multiplier in the K-sigma control limits for the respective chart.
}
\value{
Returns the K-sigma multiplier (L) as a numeric value
}
\references{
Hossain, M.P., Omar, M.H. and Riaz, M. (2017) "New V control chart for the Maxwell distribution". Journal of Statistical Computation and Simulation, 87(3), pp.594-606. <doi:10.1080/00949655.2016.1222391>

Shah, F., Khan, Z., Aslam, M. and Kadry, S. (2021) "Statistical Development of the VSQ‐Control Chart for Extreme Data with an Application to the Carbon Fiber Industry". Mathematical Problems in Engineering, 2021(1), p.9766986. <doi:10.1155/2021/9766986>
}
\author{
Zahid Khan
}
\examples{
mxm(n = 5, alpha = 0.005, type = "V")
}
