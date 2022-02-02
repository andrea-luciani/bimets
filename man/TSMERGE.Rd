\name{TSMERGE}
\alias{TSMERGE}


\title{
Merge Time Series
}
\description{

This function merges and concatenates two or more time series of the same frequency. The output time series will be defined over the union of dates for which the input time series are defined, from the earliest starting date to the latest ending date. \cr\cr
For each period, the output value will be set equal to the first non-missing value found in the input time series list by using the order of the arguments. If
all the input time series are missing at a period, then the output time series will be set to the missing value \code{NA} in the same period. Note that if the input time series' date spans do not intersect, \code{TSMERGE(X1,X2,...,XN)} returns a simple concatenation of \code{X1, X2,..., XN}.\cr\cr By defining the argument \code{fun}, the value of the output time series can also be computed as a function of the values of the input time series in the same period (see example).
}
\usage{
TSMERGE(..., fun = NULL, MV = FALSE, avoidCompliance = FALSE)
}

\arguments{
  
\item{...}{
Input list of time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  
\item{fun}{ By defining the argument \code{fun}, the value of the output time series in a period can be computed as a function of the input time series values in the same period. \cr \code{fun} can assume the following string values:\cr
\bold{AVE}: the value of the output time series in a period will be set equal to the average of all input time series values in the same period. \cr
\bold{SUM}: the value of the output time series in a period will be set equal to the sum of all input time series values in the same period. \cr
\bold{MAX}: the value of the output time series in a period will be set equal to the maximum of all input time series values in the same period. \cr
\bold{MIN}: the value of the output time series in a period will be set equal to the minimum of all input time series values in the same period. \cr
}
  \item{MV}{
If \code{FALSE}, the function defined in the argument \code{fun} will skip any missing values found in the input time series.
}
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}

}
}

\value{
This function returns a BIMETS time series built by merging two or more input time series.
}

\seealso{
\code{\link[=TSJOIN]{TSJOIN}}\cr
\code{\link[=TSEXTEND]{TSEXTEND}}\cr
\code{\link[=TSLAG]{TSLAG}}\cr
\code{\link[=TSPROJECT]{TSPROJECT}}\cr
\code{\link[=TSLEAD]{TSLEAD}}\cr
\code{\link[=TSTRIM]{TSTRIM}}\cr
}

\examples{

	#DEFINE TS
	n<-10
	ts1<-TSERIES(n:1,START=c(1995,1),FREQ=1)
	ts2<-TSERIES(n:1,START=c(2000,1),FREQ=1)
	
	ts2[5]<-NA
	ts1[10]<-NA
	
	#print TSMERGE
	TABIT(ts1,ts2,TSMERGE(ts1,ts2,fun='SUM',MV=TRUE))
	
	#TS D
	n<-20
	ts1<-TSERIES(n:1,START=c(1999,360),FREQ='D')
	ts2<-TSERIES(n:1,START=c(2000,1),FREQ='D')
	
	ts2[5]<-NA
	ts1[10]<-NA
	
	#print TSMERGE
	TABIT(ts1,ts2,TSMERGE(ts1,ts2,fun='SUM',MV=TRUE))
}
