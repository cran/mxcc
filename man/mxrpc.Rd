\name{mxrpc}
\alias{mxrpc}

\title{
V chart and VSQ chart Construction for Real Process Control
}
\description{
The \code{mxrpc} function constructs control limits for the V and VSQ control charts using real data. It allows the user to specify the value of \code{alpha}, the type of control limit (Probability Limit Control Chart or K-Sigma Control Chart), and the type of control chart (V or VSQ). The function provides a summary of control chart parameters and plots the corresponding control chart.
}
\usage{
mxrpc(data, alpha = 0.0027, limit = "PCL", chart = "V", summary = FALSE)
}

\arguments{
  \item{data}{
A data frame containing real-world observations for which the control charts will be constructed.
}
  \item{alpha}{
The false alarm probability for control limit calculation. Default is \code{alpha = 0.0027}.
}
  \item{limit}{
The type of control limit to be used: either "PCL" for Probability Limit Control Chart or "KCL" for K-Sigma Control Chart. Default is \code{limit = "PCL"}.
}
  \item{chart}{
The type of control chart to construct: either "V" for V chart or "VSQ" for VSQ chart. Default is \code{chart = "V"}.
}
  \item{summary}{
Logical value indicating whether to display a summary of control chart parameters. Default is \code{summary = FALSE}.
}
}
\details{
This function takes a real data set and generates control charts (V or VSQ) based on the specified control limit type (PCL or KCL). The function outputs both a graphical representation of the control chart and a summary of the control chart parameters, including control limits, central line, and the values used for constructing the chart.
}
\value{
The function outputs a graph of the selected control chart (V or VSQ), and if \code{summary = TRUE}, it prints a summary of the control chart parameters including the lower control limit (LCL), center line (CL), and upper control limit (UCL).
}
\references{
Hossain, M.P., Omar, M.H. and Riaz, M. (2017) "New V control chart for the Maxwell distribution". Journal of Statistical Computation and Simulation, 87(3), pp.594-606. <doi:10.1080/00949655.2016.1222391>

Shah, F., Khan, Z., Aslam, M. and Kadry, S. (2021) "Statistical Development of the VSQ‐Control Chart for Extreme Data with an Application to the Carbon Fiber Industry". Mathematical Problems in Engineering, 2021(1), p.9766986. <doi:10.1155/2021/9766986>
}
\author{
Zahid Khan
}

\seealso{
\code{\link{mxspc}}
}
\examples{
mxrpc(data=strength_data, alpha = 0.0027, limit = "PCL", chart = "VSQ", summary = TRUE)
}
