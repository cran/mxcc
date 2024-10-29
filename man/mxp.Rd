\name{mxp}
\alias{mxp}
\title{
Power Computation of V Chart and VSQ Chart for Maxwell-Distributed Quality Characteristics
}
\description{
The \code{mxp} function calculates the power of V chart and VSQ control chart for monitoring the Maxwell scale parameter. It computes the probability of detecting a shift in the process, depending on the specified sample size, significance level, and the shift magnitude
}
\usage{
mxp(n = 1, alpha = 0.0027, delta = 1, type)
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
The function calculates the power of a control chart based on the provided sample size (\code{n}), false alarm probability (\code{alpha}), and shift constant (\code{delta}). The chart type, either \code{"V"} or \code{"VSQ"}, determines which chart is used for the calculations. Power is a critical metric that evaluates the sensitivity of the control chart to detecting process shifts, allowing users to monitor for deviations from the expected process behavior.
}
\value{
A numeric value representing the power of the control chart to detect the process shift.
}
\references{
Hossain, M.P., Omar, M.H. and Riaz, M. (2017) "New V control chart for the Maxwell distribution". Journal of Statistical Computation and Simulation, 87(3), pp.594-606. <doi:10.1080/00949655.2016.1222391>

Shah, F., Khan, Z., Aslam, M. and Kadry, S. (2021) "Statistical Development of the VSQ‐Control Chart for Extreme Data with an Application to the Carbon Fiber Industry". Mathematical Problems in Engineering, 2021(1), p.9766986. <doi:10.1155/2021/9766986>
}
\author{
Zahid Khan
}

\examples{
result <- mxp(n = 5, alpha = 0.0027, delta = 2, type = "V")
print(result)
}

