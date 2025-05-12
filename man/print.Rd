\name{print}
\alias{print}
\alias{print.mxrpc}
\alias{print.mxspc}
\alias{print.summary.mxrpc}
\alias{print.summary.mxspc}


\title{
Print Method for Control Chart Objects and Their Summaries
}
\description{
Print methods to display key parameters and summaries for control charts generated using \code{mxrpc} and \code{mxspc}, as well as their corresponding summary objects.
}
\usage{
\method{print}{mxrpc}(x, ...)
\method{print}{mxspc}(x, ...)
\method{print}{summary.mxrpc}(x, ...)
\method{print}{summary.mxspc}(x, ...)
}

\arguments{
 \item{x}{An object of class \code{mxrpc}, \code{mxspc}, \code{summary.mxrpc}, or \code{summary.mxspc}.}
  \item{...}{Additional arguments (currently not used).}
}
\details{
These print methods provide structured output for:
  \itemize{
    \item Control chart parameters: subgroup size, sample size, control limits (LCL, CL, UCL or PCL), estimated sigma, limit type, and chart type.
    \item Summary statistics for the plotting statistic (e.g., V values).
    \item Summary of either real data (for \code{mxrpc}) or simulated data (for \code{mxspc}).
    \item For summary objects (\code{summary.mxrpc}, \code{summary.mxspc}), control chart parameters and statistical summaries are displayed as formatted data frames.
  }
}
\value{
 These functions return their input invisibly after printing the relevant summaries to the console.
}

\references{
Hossain, M.P., Omar, M.H. and Riaz, M. (2017) "New V control chart for the Maxwell distribution". Journal of Statistical Computation and Simulation, 87(3), pp.594-606. <doi:10.1080/00949655.2016.1222391>

Shah, F., Khan, Z., Aslam, M. and Kadry, S. (2021) "Statistical Development of the VSQ‐Control Chart for Extreme Data with an Application to the Carbon Fiber Industry". Mathematical Problems in Engineering, 2021(1), p.9766986. <doi:10.1155/2021/9766986>
}
\author{
Zahid Khan, Zsolt T. Kosztyan
}

\examples{
t1 <- mxspc(m = 20, n = 5, alpha = 0.004, sigma = 0.5, limit = "KCL")
print(t1)
print(summary(t1))
}
