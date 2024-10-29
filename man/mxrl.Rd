\name{mxrl}
\alias{mxrl}
\title{
Characteristics of Run Length Distribution for V Chart and VSQ Control Chart
}
\description{
The \code{mxrl} function computes key characteristics of the run length distribution for V and VSQ control charts. It calculates the Average Run Length (ARL), Standard Deviation of the Run Length (SDRL), and Median Run Length (MRL), based on the provided sample size, significance level, shift constant, and control chart type.
}
\usage{
mxrl(n = 1, alpha = 0.0027, delta = 1, type = "V")
}
\arguments{
 \item{n}{
     The sample size for each subgroup (Integer). Default is \code{n = 1}.
  }
  \item{alpha}{
    Probability of false alarm (type I error) for the control chart(numeric). Default is \code{alpha = 0.0027}.
  }
  \item{delta}{
     The shift constant representing the magnitude of the shift to detect(numeric). Default is \code{delta = 1}.
  }
  \item{type}{
     Specifies the type of control chart to be used. Options are \code{"V"}(character) for the V chart and \code{"VSQ"} (character) for the VSQ chart. This argument is required.
  }
}
\details{
This function computes the characteristics of the run length distribution for either the V chart or the VSQ chart. The run length distribution is an essential metric in control chart analysis as it quantifies the performance of the control chart in detecting shifts in the process. The ARL is the expected number of samples before an out-of-control signal, SDRL is the standard deviation of the run length, and MRL is the median run length.
}
\value{
A list with the following components:
  \item{ARL}{The Average Run Length (ARL).}
  \item{SDRL}{The Standard Deviation of the Run Length (SDRL).}
  \item{MRL}{The Median Run Length (MRL).}
}
\references{
Hossain, M.P., Omar, M.H. and Riaz, M. (2017) "New V control chart for the Maxwell distribution". Journal of Statistical Computation and Simulation, 87(3), pp.594-606. <doi:10.1080/00949655.2016.1222391>

Shah, F., Khan, Z., Aslam, M. and Kadry, S. (2021) "Statistical Development of the VSQ‐Control Chart for Extreme Data with an Application to the Carbon Fiber Industry". Mathematical Problems in Engineering, 2021(1), p.9766986. <doi:10.1155/2021/9766986>
}
\author{
Zahid Khan
}

\examples{
mxrl(n = 2, alpha = 0.005, delta = 1, type = "VSQ")

}
