\name{TSDELTA}
\alias{TSDELTA}
\alias{DELTA}

\title{
Time Series Lag Differences (Delta)
}
\description{
This function returns the \code{O}-order, \code{L}-lag differences of the input time series.
}
\usage{
TSDELTA(x = NULL, L = 1, O = 1, avoidCompliance = FALSE, ...)
}

\arguments{
 \item{x}{
Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{L}{
Lag.
}
  \item{O}{
Order of the difference.
}
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
   \item{...}{
Backward compatibility.
}
}
  


\value{
This function returns a BIMETS time series.
}


\seealso{
\code{\link[=DELTAP]{TSDELTAP}}\cr
\code{\link[=TSDELTALOG]{TSDELTALOG}}\cr
\code{\link[=TSLAG]{TSLAG}}\cr
\code{\link[=MOVAVG]{MOVAVG}}\cr
\code{\link[=INTS]{INTS}}\cr
\code{\link[=CUMSUM]{CUMSUM}}\cr
}
\examples{

	#random TS
	n<-10
	xArr<-rnorm(n)
	ts1<-TSERIES(xArr,START=c(2000,1),FREQ='A')
	TABIT(ts1,TSDELTA(ts1,1,1),TSDELTA(ts1,1,2),TSDELTA(ts1,1,3))
}
