\name{CUMSUM}
\alias{CUMSUM}
\alias{CUMULO}



\title{
Cumulative Sum
}
\description{
This function returns the cumulative sum of the elements of the input array or time series. The result is an object of the same class of the input, and its elements are the cumulative sum of the current and the previous elements of the input. \cr If the input is a time series, users can provide the argument \code{TSRANGE} in order to project the input time series before the cumulative sum. \cr \code{CUMULO} is an alias form \code{CUMSUM} with the argument \code{MODE='YEARLY'}.

}
\usage{
CUMSUM(x=NULL, TSRANGE=NULL, MODE=NULL, avoidCompliance=FALSE,...)
CUMULO(x=NULL, TSRANGE=NULL, avoidCompliance=FALSE,...)
}

\arguments{
  \item{x}{
Input numerical array or time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
\item{TSRANGE}{
Optional date range of operations that process time series. \code{TSRANGE} must be specified as an array composed by starting year, starting period, ending year and ending period,\cr i.e. \code{TSRANGE=c(START_YEAR,START_PERIOD,END_YEAR,END_PERIOD)}. The projection into the time interval specified in \code{TSRANGE} takes place before the cumulative sum.
}
\item{MODE}{
When selecting \code{MODE='YEARLY'} or \code{MODE='MONTHLY'} the sum is reset to zero when a new year or a new month starts.
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
\code{\link[=CUMPROD]{CUMPROD}}\cr
}


\examples{
	
	#create ts
	ts1<-TSERIES(INTS(1,30),START=c(2000,1),FREQ='M')
	out_CUMSUM<-CUMSUM(ts1)
	TABIT(out_CUMSUM)
	
	    
	out_CUMSUM<-CUMSUM(ts1,TSRANGE=c(2000,4,2001,7))	
	out_CUMSUM_Y<-CUMSUM(ts1,TSRANGE=c(2000,4,2001,7),MODE='YEARLY')
	TABIT(ts1,out_CUMSUM,out_CUMSUM_Y)
	
	
  
	#define an array
	arr1<-c(INTS(1,10),INTS(-1,-10))
	out_CUMSUM<-CUMSUM(arr1)
	print(out_CUMSUM)
	
	#print...1  3  6 10 15 21 ... 27 19 10  0
  
	
}
