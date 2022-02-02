\name{TSDELTALOG}
\alias{TSDELTALOG}

\title{
Time Series Lag Logarithmic Differences 
}
\description{
This function returns the \code{L}-lag logarithmic differences of the input time series.
}
\usage{
TSDELTALOG(x = NULL, L = 1,  avoidCompliance = FALSE, ...)
}

\arguments{
 \item{x}{
Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{L}{
Lag.
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
\code{\link[=TSLAG]{TSLAG}}\cr
\code{\link[=MOVAVG]{MOVAVG}}\cr
\code{\link[=INTS]{INTS}}\cr
\code{\link[=CUMSUM]{CUMSUM}}\cr
}

\examples{

	#random TS
	n=10
	ts1=TSERIES(1:n,START=c(2000,1),FREQ='A')
	TABIT(ts1,TSDELTALOG(ts1,1))
}
