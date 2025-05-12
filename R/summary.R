#-----------------------------------------------------------------------------#
#                                                                             #
#R Package for Maxwell Control Charts                             #
#                                                                             #
#  Written by: Zahid Khan, Zsolt T. Kosztyan                                  #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kosztyan.zsolt@gtk.uni-pannon.hu                               #
#                                                                             #
# Last modified: February 2025                                                  #
#-----------------------------------------------------------------------------#
#A Brief Summary of Control Chart Parameters for real (mxrpc) and simulated  (mxspc) data
#' @export
summary <- function(object, ...) {
  UseMethod("summary", object)
}

#' @export
summary.mxrpc <- function(object, ...) {
  out <- list(
    subgroup.size = object$m,
    sample.size = object$n,
    lower.limit = round(object$LCL, 4),
    central.limit = round(object$CL, 4),
    upper.limit = round(object$UCL, 4),
    sigma = round(object$sig, 4),
    limit.type = object$limit,
    chart.type = object$chart,
    plotting.statistic.summary = summary(object$v),
    data.summary = summary(object$data)
  )
  class(out) <- "summary.mxrpc"
  return(out)
}

#' @export
summary.mxspc <- function(object, ...) {
  out <- list(
    subgroup.size = object$m,
    sample.size = object$n,
    lower.limit = round(object$LCL, 4),
    central.limit = round(object$CL, 4),
    upper.limit = round(object$UCL, 4),
    sigma = round(object$sig, 4),
    limit.type = object$limit,
    chart.type = object$chart,
    plotting.statistic.summary = summary(object$v),
    simulated.data.summary = summary(object$a)
  )
  class(out) <- "summary.mxspc"
  return(out)
}

