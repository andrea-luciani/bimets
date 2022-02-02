\name{TSDELTAP}
\alias{TSDELTAP}
\alias{DELTAP}


\title{
Time Series Percentage Lag Differences (Delta Percentage)
}
\description{
This function returns the \code{O}-order, \code{L}-lag percentage differences of the input time series.
If the input time series frequency is a multiple of the \code{L} lag argument, then it is possible to set the argument \code{ANNUALIZE=TRUE} in order to have the percent changes returned at annual rates, i.e. raised to the power of \code{frequency/L}.
}
\usage{
TSDELTAP(x = NULL, L = 1, ANNUALIZE = FALSE, avoidCompliance = FALSE, ...)
}

\arguments{
 \item{x}{
Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{L}{
Lag.
}
  \item{ANNUALIZE}{
If TRUE the percent changes are returned as annual rates, i.e. raised to the power of frequency/L
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
\code{\link[=DELTA]{TSDELTA}}\cr
\code{\link[=TSLAG]{TSLAG}}\cr
\code{\link[=MOVAVG]{MOVAVG}}\cr
\code{\link[=INDEXNUM]{INDEXNUM}}\cr
}

\examples{
	
	#TS Q
	n<-10;
	ts1<-TSERIES(n:0,START=c(2000,1),FREQ='Q')
	TABIT(ts1,TSDELTAP(ts1,1))
	
	#TS 366
	ts1<-TSERIES(seq(1,length=10,by=-0.001),START=c(2000,1),FREQ=366)
	TABIT(ts1,TSDELTAP(ts1,1,ANNUALIZE=TRUE))
	
	}
