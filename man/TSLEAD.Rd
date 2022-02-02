\name{TSLEAD}
\alias{TSLEAD}


\title{
Lead Time Series
}
\description{
This function leads the input time series  by the specified number of time periods.
}
\usage{
TSLEAD(x = NULL, L = 1, avoidCompliance = FALSE, ...)
}

\arguments{
  \item{x}{
Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{L}{
Lead. Must be an integer, positive or negative.
}
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}}
   \item{...}{
Backward compatibility.
}
}
  


\value{
This function returns a BIMETS time series built by leading the input time series.
}

\seealso{
\code{\link[=TSJOIN]{TSJOIN}}\cr
\code{\link[=TSEXTEND]{TSEXTEND}}\cr
\code{\link[=TSMERGE]{TSMERGE}}\cr
\code{\link[=MOVAVG]{MOVAVG}}\cr
\code{\link[=GETYEARPERIOD]{GETYEARPERIOD}}\cr
\code{\link[=TSLAG]{TSLAG}}\cr
}


\examples{
	
	#DEFINE TS
	n<-10
	
	ts1<-TSERIES(n:1,START=c(2000,1),FREQ=1)
	ts1[5]<-NA
	
	#print TSLEAD
	TABIT(ts1,TSLEAD(ts1,5))
}
