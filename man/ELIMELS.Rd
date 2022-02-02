\name{ELIMELS}
\alias{ELIMELS}


\title{
Eliminate Elements from Arrays or Time Series
}
\description{
This function eliminates the selected elements from the input array or the input time series.
}
\usage{
ELIMELS(x=NULL, idx=NULL, avoidCompliance=FALSE, ...)
}

\arguments{
  \item{x}{
Input numerical array or time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}. 
}
  \item{idx}{
Numerical array built with the indices of selected elements to be removed from the input. If the input is a time series the index must be provided as a sequence of numbers \code{IDX=YEAR+PERIOD/FREQ} with \code{YEAR} and \code{PERIOD} the year and the period to be removed (see example).

}
\item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
 \item{...}{
Backward compatibility.
}
}
  


\value{
This function returns an array with the kept elements from the input array or the input time series.
}

\seealso{

\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=GETYEARPERIOD]{GETYEARPERIOD}}\cr
\code{\link[=LOCS]{LOCS}}\cr
\code{\link[=NAMELIST]{NAMELIST}}\cr
\code{\link[=INTS]{INTS}}\cr
\code{\link[=TSINFO]{TSINFO}}\cr
\code{\link[=TSLOOK]{TSLOOK}}\cr
\code{\link[=TABIT]{TABIT}}\cr
\code{\link[=NOELS]{NOELS}}\cr
}


\examples{
	
	
	print(ELIMELS(INTS(1,10),c(1,4,5))) 
	#print 2 3 6 7 8 9 10
	
	print(ELIMELS(TSERIES(1:10,START=c(2000,1),FREQ=4),c(2000.25,2000.75))) 
	#print 2  4  5  6  7  8  9 10 
	
				
}






