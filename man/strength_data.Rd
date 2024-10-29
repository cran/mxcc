\name{strength_data}
\alias{strength_data}
\docType{data}
\title{
Strength Data of Carbon Fiber
}
\description{
This dataset contains the strength measurements of carbon fiber tested under tension at various gauge levels. The data is used to construct control charts for monitoring the scale parameter of the Maxwell distribution in the carbon fiber industry.
}
\usage{data("strength_data")}
\format{
  A data frame with 12 subgroups each with 5 observations.
}
\details{
he dataset consists of 12 subgroups, each containing 5 measurements of carbon fiber strength. These measurements are used to compute the V-statistic, which is then applied to control charts for monitoring the Maxwell distribution's scale parameter. The data was originally reported by Badar and Priest (1982) and is slightly modified for statistical analysis.
}
\source{
Badar, M. G., & Priest, A. M. (1982). Statistical aspects of fiber and bundle strength in hybrid composites. In "Progress in Science and Engineering Composites ICCM-IV, Tokyo", pp. 1129-1136.
}
\references{
Shah, F., Khan, Z., Aslam, M. and Kadry, S. (2021) "Statistical Development of the VSQ‐Control Chart for Extreme Data with an Application to the Carbon Fiber Industry". Mathematical Problems in Engineering, 2021(1), p.9766986. <doi:10.1155/2021/9766986>

}
\examples{
data("strength_data")
strength_data
}
\keyword{datasets}
