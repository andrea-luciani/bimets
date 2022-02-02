\name{CUMPROD}
\alias{CUMPROD}



\title{
Cumulative Product
}
\description{
This function returns the cumulative product of the elements of the input array or time series. The result is an object of the same class of the input, and its elements are the cumulative product of the current and the previous elements of the input. \cr If the input is a time series, users can provide the argument \code{TSRANGE} in order to project the input time series before the cumulative product.
}
\usage{
CUMPROD(x=NULL, TSRANGE=NULL, avoidCompliance=FALSE, ...)
}

\arguments{
  \item{x}{
Input numerical array or time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
\item{TSRANGE}{
Optional date range of operations that process the input time series. \code{TSRANGE} must be specified as an array composed by starting year, starting period, ending year and ending period, i.e. \code{TSRANGE=c(START_Y,START_P,END_Y,END_P)}. The projection into the time interval specified in \code{TSRANGE} takes place before the cumulative product.
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
\code{\link[=TSPROJECT]{TSPROJECT}}\cr
\code{\link[=TSMERGE]{MOVAVG}}\cr
\code{\link[=TSDELTA]{TSDELTA}}\cr
\code{\link[=TSLAG]{TSLAG}}\cr
\code{\link[=TSPROJECT]{TSPROJECT}}\cr
\code{\link[=TSEXTEND]{TSEXTEND}}\cr
\code{\link[=TSLEAD]{TSLEAD}}\cr
\code{\link[=INDEXNUM]{INDEXNUM}}\cr
}


\examples{
	
	#create ts
	ts1=TSERIES(INTS(1,10),START=c(2000,1),FREQ='M')
	out_CUMPROD=CUMPROD(ts1)
	TABIT(out_CUMPROD)
	    
	out_CUMPROD=CUMPROD(ts1,TSRANGE=c(2000,4,2001,1))	
	TABIT(ts1,out_CUMPROD)
  
	#define an array
	arr1=c(INTS(1,5),INTS(-1,-5))
	out_CUMPROD=CUMPROD(arr1)
	print(out_CUMPROD)
	
	
}
