\name{GETRANGE}
\alias{GETRANGE}


\title{
Time Series Common Range
}
\description{
Given a time series list, this function returns intersection or union of time series' ranges.}

\usage{
GETRANGE( x=list(), 
          type='INNER', 
          avoidCompliance=FALSE,  
          ...)
}

\arguments{
  \item{x}{
Input list, having elements as time series of class \code{ts} or \code{xts}. This argument can be also a single time series.
}
  \item{type}{ If \code{type='INNER'} then this funtion will return the intersection (if not \code{NULL}) of input time series' ranges. If \code{type='OUTER'} then this funtion will return the union of input time series' ranges.
}
   
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
   \item{...}{
Backward compatibility.
}
}
  


\value{
This function returns the common range of the input time series as a 4-integer array built by \code{c(START_Y,START_P,END_Y,END_P)}. If \code{type='INNER'} and there is no intersection of time series' ranges, then this function will return a \code{NULL}.  
}

\seealso{
\code{\link[=TSJOIN]{TSJOIN}}\cr
\code{\link[=TSEXTEND]{TSEXTEND}}\cr
\code{\link[=TSMERGE]{TSMERGE}}\cr
\code{\link[=MOVAVG]{MOVAVG}}\cr
\code{\link[=GETYEARPERIOD]{GETYEARPERIOD}}\cr
\code{\link[=CUMSUM]{CUMSUM}}\cr
}


\examples{
		
	#create ts
	ts1=TSERIES((1:40), START=c(2000,1), FREQ=4)
	ts2=TSERIES((1:40), START=c(2001,1), FREQ=4)
	ts3=TSERIES((1:40), START=c(2002,1), FREQ=4)
	
	myList=list(ts1,ts2,ts3)
	
	print(GETRANGE(myList))
	print(GETRANGE(myList,type='OUTER'))
}
