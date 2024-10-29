\name{failure_time}
\alias{failure_time}
\docType{data}
\title{
Failure Time of Vertical Boring Machine
}
\description{
This dataset contains the failure times (in hours) of a vertical boring machine, used to illustrate the control chart for monitoring the Maxwell distribution parameter.The data was originally reported by Krishna and Malik (2012).
}

\usage{data("failure_time")}
\format{
  A data frame consisted of 8 sample batches each wiht 4 observations.

}
\details{
The failure times in this dataset are organized into 8 subgroups, each containing 4 observations. The failure times are measured in hours. These data are used to construct control charts for monitoring the scale parameter of the Maxwell distribution.
}
\source{
 Krishna, H. and Malik, M. (2012) "Reliability estimation in Maxwell distribution with progressively Type-II censored data". Journal of Statistical Computation and Simulation, 82(4), pp.623–641. <doi:10.1080/00949655.2010.550291>

}
\references{
Hossain, M.P., Omar, M.H. and Riaz, M. (2017) "New V control chart for the Maxwell distribution". Journal of Statistical Computation and Simulation, 87(3), pp.594-606. <doi:10.1080/00949655.2016.1222391>
}
\examples{
data("failure_time")
failure_time
}
\keyword{datasets}
