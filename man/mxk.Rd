\name{mxk}
\alias{mxk}
\title{
Determination of Probability Limit Coefficients for V and VSQ Charts for Maxwell-Distributed Quality Characteristics
}
\description{
The function \code{mxk} calculates the coefficients for V and VSQ control charts used to monitor the scale parameter of Maxwell-distributed quality characteristics. It computes two coefficients based on the chosen chart type. For the V chart, the function returns \code{L1} and \code{L2}, while for the VSQ chart, it returns \code{P1},\code{P2},\code{P3} and \code{P4}.The coefficients \code{P3} and \code{P4} are used in case of estimated scale parameter value.These coefficients are utilized to construct a probability limits-based control chart.
}
\usage{
mxk(n = 1, alpha = 0.0027, type = "V")
}

\arguments{
 \item{n}{
    Sample size. The number of observations in each subgroup (numeric). Defaults to \code{1}.
}
  \item{alpha}{
    Probability of false alarm (Type I error). Defaults to \code{0.0027} (numeric).
}
  \item{type}{
    The type of chart. Accepts either \code{"V"} for the V chart or \code{"VSQ"} for the VSQ chart (character).
}
}
\details{
The \code{mxk} function calculates the coefficients used in V and VSQ control charts for monitoring the scale parameter of Maxwell-distributed data. The user must specify the sample size \code{n}, the probability of a false alarm \code{alpha}, and the type of chart (\code{"V"} or \code{"VSQ"}). By default, \code{n} is set to 1, and \code{alpha} is set to 0.0027.
}
\value{
Depending on the \code{type} of chart:
  \item{L1 }{Coefficient L1 for the V chart.}
  \item{L2 }{Coefficient L2 for the V chart.}
  \item{P1 }{Coefficient P1 for the VSQ chart.}
  \item{P2 }{Coefficient P2 for the VSQ chart.}
  \item{P3 }{Coefficient P3 for the VSQ chart.}
  \item{P4 }{Coefficient P4 for the VSQ chart.}
}
\references{
Hossain, M.P., Omar, M.H. and Riaz, M. (2017) "New V control chart for the Maxwell distribution". Journal of Statistical Computation and Simulation, 87(3), pp.594-606. <doi:10.1080/00949655.2016.1222391>

Shah, F., Khan, Z., Aslam, M. and Kadry, S. (2021) "Statistical Development of the VSQ‐Control Chart for Extreme Data with an Application to the Carbon Fiber Industry". Mathematical Problems in Engineering, 2021(1), p.9766986. <doi:10.1155/2021/9766986>
}
\author{
Zahid Khan
}
\examples{
mxk(n = 4, alpha = 0.0027, type = "VSQ")
}


