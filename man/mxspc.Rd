\name{mxspc}
\alias{mxspc}
\title{
V chart and VSQ chart Construction for Simulated Process Control
}
\description{
This function constructs control limits for the V and VSQ control charts based on probability limits or k-sigma limits using simulated data from the Maxwell distribution. The function allows for flexible configuration of control chart types and limit methods.
}
\usage{
mxspc(m = 25, n = 4, alpha = 0.0027, sigma,
limit = "PCL", chart = "V", summary = FALSE)
}

\arguments{
   \item{m}{
The number of subgroups or samples. Default is \code{m = 25}.
}
  \item{n}{
The size of each sample or subgroup. Default is \code{n = 4}.
}
  \item{alpha}{
The false alarm probability for control limit calculation. Default is \code{alpha = 0.0027}.
}
  \item{sigma}{
The scale parameter of the Maxwell distribution, which must be provided by the user.
}

  \item{limit}{
The type of control limit to be used: either "PCL" for probability limit control chart or "KLC" for k-sigma limit control chart. Default is \code{limit = "PCL"}.
}
  \item{chart}{
The type of control chart to construct: either "V" for V chart or "VSQ" for VSQ chart. Default is \code{chart = "V"}.
}
  \item{summary}{
Logical value indicating whether to display a summary of control chart parameters. Default is \code{summary = FALSE}.
}
}
\details{
The function simulates data from the Maxwell distribution using the provided scale parameter (\code{sigma}) and calculates control limits and statistics for the specified control chart type (V or VSQ). It allows for choosing between probability limit control charts and k-sigma control charts. A control chart is plotted based on the calculated limits and plotting statistics.
}
\value{
A list of control chart parameters is returned invisibly, which includes:
  \item{LCL }{The lower control limit (or probability limit).}
  \item{CL }{The center line of the control chart.}
  \item{UCL }{The upper control limit (or probability limit).}
  \item{m }{The number of subgroups.}
  \item{n }{The sample size for each subgroup.}
  \item{sigma }{The provided scale parameter for the Maxwell distribution.}
  \item{limit }{The type of limit used ("PCL" or "KLC").}
  \item{chart }{The type of control chart ("V" or "VSQ").}
}
\references{
Hossain, M.P., Omar, M.H. and Riaz, M. (2017) "New V control chart for the Maxwell distribution". Journal of Statistical Computation and Simulation, 87(3), pp.594-606. <doi:10.1080/00949655.2016.1222391>

Shah, F., Khan, Z., Aslam, M. and Kadry, S. (2021) "Statistical Development of the VSQ‐Control Chart for Extreme Data with an Application to the Carbon Fiber Industry". Mathematical Problems in Engineering, 2021(1), p.9766986. <doi:10.1155/2021/9766986>
}
\author{
Zahid Khan
}


\seealso{
\code{\link{mxp}},\code{\link{mxk}}
}
\examples{
mxspc(m = 30, n = 4, alpha = 0.0027, sigma = 1777.86,
limit="PCL", summary = TRUE, chart = "V")
}
