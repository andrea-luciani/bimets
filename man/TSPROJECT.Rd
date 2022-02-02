\name{TSPROJECT}
\alias{TSPROJECT}


\title{
Project a Time series
}
\description{
This function projects the input time series into a time interval. The output class can be either a time series (default) or a one-dimensional array if the argument \code{ARRAY=TRUE}.
}
\usage{
TSPROJECT(x=NULL, TSRANGE=NULL, ARRAY=FALSE, 
          EXTEND=FALSE, avoidCompliance=FALSE,...)
}

\arguments{
  \item{x}{
Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{TSRANGE}{
Date range of data projection. \code{TSRANGE} must be specified as a numerical array composed by starting year, starting period, ending year and ending period of projection, \cr i.e. \code{TSRANGE=c(START_YEAR,START_PERIOD,END_YEAR,END_PERIOD)}. 
}
   \item{ARRAY}{
If \code{TRUE} this function will return a numerical array built with observation values that lie in the specified time range. If \code{FALSE} (default) the output will be a time series.
}

\item{EXTEND}{
If \code{TRUE} and in the case that the input time series does not overlap with the provided \code{TSRANGE}, the output time series will be extended over the \code{TSRANGE} by inserting missing values \code{NA} into the new observations.
}

  \item{avoidCompliance}{
  If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}

}
     \item{...}{
Backward compatibility.
}
}

\value{
This function returns a BIMETS time series, or a numerical array if the argument \code{ARRAY=TRUE}, built by projecting the input time series into the provided \code{TSRANGE}.
}

\seealso{
\code{\link[=TSJOIN]{TSJOIN}}\cr
\code{\link[=TSEXTEND]{TSEXTEND}}\cr
\code{\link[=TSMERGE]{TSMERGE}}\cr
\code{\link[=MOVAVG]{MOVAVG}}\cr
\code{\link[=GETYEARPERIOD]{GETYEARPERIOD}}\cr
\code{\link[=CUMSUM]{CUMSUM}}\cr
\code{\link[=TSTRIM]{TSTRIM}}\cr
}


\examples{
		
	#create yearly ts
	ts1=TSERIES((1:10),START=c(2000,1),FREQ=1)
  	
	print(TSPROJECT(ts1,TSRANGE=c(2002,1,2005,1)))	#print projected ts from 2002 to 2005
	
	print(TSPROJECT(ts1,TSRANGE=c(2001,1,2004,1),ARRAY=TRUE)) #print c(2,3,4,5)
	
	print(TSPROJECT(ts1,TSRANGE=c(1998,1,2002,1),EXTEND=TRUE,ARRAY=TRUE)) 
	#print c(NA,NA,1,2,3)
	
}
