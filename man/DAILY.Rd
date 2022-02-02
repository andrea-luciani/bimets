\name{DAILY}
\alias{DAILY}


\title{
Daily Time Series (Dis)Aggregation
}
\description{
This function returns a daily disaggregated time series, by using as input an annual, semiannual, quarterly or monthly time series.
}
\usage{
DAILY(x = NULL, fun = NULL, avoidCompliance = FALSE, ...)
}
\arguments{
  \item{x}{
Input time series, that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{fun}{ 

\bold{NULL}: (default) the output value of each daily observation is set equal to the value of the input observation the date belongs to (i.e. duplicated values over the period)\cr
\bold{INTERP_END}: the value of the input time series in a period is copied into the last day of the output time series that lies in the same period. Other values are calculated by linear interpolation.\cr
\bold{INTERP_CENTER}: the value of the input time series in a period is copied into the median day of the output time series that lies in the same period. Other values are calculated by linear interpolation. \cr
\bold{INTERP_BEGIN}: the value of the input time series in a period is copied into the first day of the output time series that lies in the same period. Other values are calculated by linear interpolation. \cr

}
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See  \code{\link[=is.bimets]{is.bimets}}
}
 \item{...}{
Backward compatibility.
}
}


\value{
This function returns a daily BIMETS time series.

}

\seealso{
\code{\link[=YEARLY]{YEARLY}}\cr
\code{\link[=SEMIANNUAL]{SEMIANNUAL}}\cr
\code{\link[=QUARTERLY]{QUARTERLY}}\cr
\code{\link[=MONTHLY]{MONTHLY}}\cr
}
\examples{

	#TS quarterly
	ts1<-TSERIES((1:2),START=c(2000,1),FREQ='Q')
	TABIT(DAILY(ts1,fun='INTERP_CENTER'))	
	
	#TS monthly
	ts1<-TSERIES((1:4),START=c(2000,1),FREQ=12)
	TABIT(DAILY(ts1))	
	
}
