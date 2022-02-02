\name{INDEXNUM}
\alias{INDEXNUM}


\title{
Rebase a Time Series
}
\description{
This function rebases an input time series to the value of 100 in the year selected by the \code{BASEYEAR} argument. If the input time series frequency is greater than one, the initial reference is set to the average value of the input time series observations that lie in the \code{BASEYEAR}.
}
\usage{
INDEXNUM(x=NULL, BASEYEAR=NULL, avoidCompliance=FALSE, ...)
}

\arguments{
  \item{x}{
Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{BASEYEAR}{
Rebasing year.
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
\code{\link[=TSJOIN]{TSJOIN}}\cr
\code{\link[=TSEXTEND]{TSEXTEND}}\cr
\code{\link[=TSMERGE]{TSMERGE}}\cr
\code{\link[=MOVAVG]{MOVAVG}}\cr
\code{\link[=GETYEARPERIOD]{GETYEARPERIOD}}\cr
\code{\link[=CUMSUM]{CUMSUM}}\cr
}


\examples{
		
	#create yearly ts
	n<-20
	ts1<-TSERIES(1:n,START=c(2000,1),FREQ=1)
	TABIT(ts1, INDEXNUM(ts1,2005))
	
	#quarterly
	ts1<-TSERIES(1:n,START=c(2000,1),FREQ=4)
	ts1[5]<-NA
	TABIT(ts1, INDEXNUM(ts1,2000))
}
