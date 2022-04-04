\name{MOVTOT}
\alias{MOVTOT}
\alias{MTOT}
\alias{MOVSUM}
\alias{MSUM}


\title{
Moving Sum
}
\description{
This function returns the moving sum of the elements of the input array or the input time series. The result is an object of the same class of the input, and its elements are the moving sum of length \code{L} of the input values. If the input is a time series, the \code{DIRECTION} of the moving sum, i.e backward, forward or centered, can be provided. \code{MTOT} and \code{MSUM} are alias for \code{MOVTOT} and \code{MOVSUM}
}
\usage{
MOVSUM(x = NULL, L = NULL, DIRECTION = NULL, avoidCompliance = FALSE, ...)
MOVTOT(x = NULL, L = NULL, DIRECTION = NULL, avoidCompliance = FALSE, ...)
}

\arguments{
  \item{x}{
Input numerical array or time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{L}{
Length of the sum. It must be a positive integer.
}
  \item{DIRECTION}{if \code{x} is a time series, given \code{y} as output and \code{x} as input: \cr
\bold{AHEAD}: the output observation value in index \code{n} will be \cr \code{y[n] = sum(x[n],x[n+1],...,x[n+L-1])}.\cr
\bold{CENTER}: the output observation value in index \code{n} will be \cr \code{y[n] = sum(x[n-trunc(L/2)],...x[n],x[n+1],...,x[n+trunc(L/2)])}.\cr
\bold{NULL} o \bold{BACK}: (default) the output observation value in index \cr \code{n} will be \code{y[n] = sum(x[n+1-L],...,x[n-1],x[n])}.\cr 
}
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
     \item{...}{
Backward compatibility.
}
}

\value{
This function returns an object of the same class of the input, i.e. an array or a BIMETS time series.
}

\seealso{
\code{\link[=TSDELTA]{TSDELTA}}\cr
\code{\link[=TSLAG]{TSLAG}}\cr
\code{\link[=TSPROJECT]{TSPROJECT}}\cr
\code{\link[=TSEXTEND]{TSEXTEND}}\cr
\code{\link[=TSLEAD]{TSLEAD}}\cr
\code{\link[=CUMSUM]{CUMSUM}}\cr
\code{\link[=INDEXNUM]{INDEXNUM}}\cr
\code{\link[=VERIFY_MAGNITUDE]{VERIFY_MAGNITUDE}}\cr
\code{\link[=GETRANGE]{GETRANGE}}\cr
}


\examples{
  
	#input data	
	inputArray<-c(1,2,3,4,NA,1,2,3,4,5)
	
	#array lag 3
	out_movtot<-MOVSUM(inputArray,3)
	print(out_movtot)

	#ts lag 4 centered with missings	
	ts1<-TSERIES(inputArray,START=c(2000,1),FREQ='A')
	out_movtot<-MOVSUM(ts1,4,'CENTER')
	TABIT(out_movtot)
	
	#ts daily	
	ts1<-TSERIES(inputArray,START=c(2000,1),FREQ='D')
	out_movtot<-MSUM(ts1,3)
	TABIT(ts1,out_movtot)
	
	
}
