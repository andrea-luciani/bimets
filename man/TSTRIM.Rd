\name{TSTRIM}
\alias{TSTRIM}


\title{
Trim a Time Series
}
\description{
This function removes trailing or leading missing values \code{NA} from the input array or the input time series. Users can provide the value to be removed other than \code{NA} missing vallue by using the argument \code{VALUE}. 
}
\usage{
TSTRIM(x=NULL, VALUE=NA, TRAIL=TRUE, LEAD=TRUE, avoidCompliance=FALSE, ...)

}

\arguments{
 \item{x}{
Input numerical array or time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}

\item{VALUE}{
Target value to be removed. Default to missing value \code{NA}.
}
\item{TRAIL}{
If \code{TRUE} this function will remove trailing target values from the input time series.
}
\item{LEAD}{
If \code{TRUE} this function will remove leading target values from the input time series.
}

\item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}

}
     \item{...}{
Backward compatibility.
}
}

\value{
This function returns an object of the same class of the input, i.e. an array or a BIMETS time series, built by removing leading and trailing user-defined values.
}

\seealso{
\code{\link[=TSLAG]{TSLAG}}\cr
\code{\link[=TSJOIN]{TSJOIN}}\cr
\code{\link[=TSMERGE]{TSMERGE}}\cr
\code{\link[=TSPROJECT]{TSPROJECT}}\cr
\code{\link[=CUMSUM]{CUMSUM}}\cr
\code{\link[=INDEXNUM]{INDEXNUM}}\cr
}
\examples{
	
	#TS A
	n=10
	ts1=TSERIES(c(NA,1:n,NA),START=c(2000,1),FREQ='A')
	TABIT(ts1,TSTRIM(ts1))
		
	ts1=TSERIES(c(NA,1:n,NA),START=c(2000,1),FREQ='A')
	TABIT(ts1,TSTRIM(ts1,TRAIL=FALSE))
	
	ts1=TSERIES(c(NA,1:n,NA),START=c(2000,1),FREQ='A')
	TABIT(ts1,TSTRIM(ts1,LEAD=FALSE))
	
	ts1=TSERIES(c(0,0,NA,1:n,NA,0),START=c(2000,1),FREQ='A')
	TABIT(ts1,TSTRIM(ts1,0))
	
}
