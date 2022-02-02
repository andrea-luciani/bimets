\name{TSLOOK}
\alias{TSLOOK}

\title{
Lookup a Time Series
}
\description{
This function returns the time range and the frequency of an input time series.
}
\usage{
TSLOOK(x=NULL, avoidCompliance=FALSE, ...)
}

\arguments{
\item{x}{
Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}

 
\item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}.

}


  \item{...}{
Backward compatibility.
}
  
}

\value{
This function returns a list of numeric arrays built with the following elements:\cr
\code{STARTY} will contain the value of the starting year\cr
\code{STARTP} will contain the value of the starting period\cr
\code{ENDY} will contain the value of the ending year\cr
\code{ENDP} will contain the value of the ending period\cr
\code{FREQ} will contain the value of the time series frequency\cr
}

\seealso{
\code{\link[=NOELS]{NOELS}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=fromBIMETStoXTS]{fromBIMETStoXTS}}\cr
\code{\link[=fromBIMETStoTS]{fromBIMETStoTS}}\cr
\code{\link[=GETYEARPERIOD]{GETYEARPERIOD}}\cr
\code{\link[=INTS]{INTS}}\cr
\code{\link[=TSINFO]{TSINFO}}\cr

}


\examples{
	
	#create series
	ts1=TSERIES(INTS(1,10),START=c(2000,1),FREQ=12)
	
	ts1Look=TSLOOK(ts1)
  	
	print(ts1Look$STARTY) #print...2000 
	print(ts1Look$STARTP) #print...1
	print(ts1Look$ENDY) #print...2000
	print(ts1Look$ENDP) #print...10
	print(ts1Look$FREQ) #print...12
	
}





















