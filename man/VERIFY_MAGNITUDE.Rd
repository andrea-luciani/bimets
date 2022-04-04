\name{VERIFY_MAGNITUDE}
\alias{VERIFY_MAGNITUDE}


\title{
Time Series Magnitude Test
}
\description{
Given a time series list, this function returns list indices that refer to time serie having a magnitude greater than the user provided value. Magnitude \code{M} is defined as \code{M = sqrt(sum(X_i^2))}, given \code{X_i} the time series values. Missing values will be discarded with a warning message.

This function can be useful in comparing list of time series, e.g. differences between historical and simulated endogenous variables of an econometric model.}

\usage{
VERIFY_MAGNITUDE(x=list(), 
                magnitude=10e-7, 
                verbose=TRUE, 
                ...)
}

\arguments{
  \item{x}{
Input list, having elements as time series of class \code{ts} or \code{xts}.
}
  \item{magnitude}{A positive number that is the maximum magnitude allowed.
}
   
  \item{verbose}{
If \code{TRUE}, output messages will be printed out.
}
   \item{...}{
Backward compatibility.
}
}
  


\value{
This function returns the list indices related to time series that have a magnitude grater than the \code{magnitude} user argument.
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
	ts1=TSERIES((1:10)*0.1, START=c(2000,1), FREQ=1)
	ts2=TSERIES((1:10)*0.01, START=c(2001,1), FREQ=4)
	ts3=TSERIES(c((1:10)*0.001,NA), START=c(2002,1), FREQ=12)
	
	myList=list(t1=ts1,t2=ts2,t3=ts3)
	
	print(VERIFY_MAGNITUDE(myList,magnitude=0.1))
}
