\name{QUARTERLY}
\alias{QUARTERLY}


\title{
Quarterly (Dis)Aggregation
}
\description{
This function returns a quarterly (dis)aggregated time series, using as input an annual, semiannual, monthly or daily time series.
}
\usage{
QUARTERLY(x = NULL, fun = NULL, avoidCompliance = FALSE, ...)
}
\arguments{
  \item{x}{
  Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
  }
   \item{fun}{ Only for daily or monthly input time series:\cr 
\bold{STOCK}: the value of the input time series in the last observation of a quarter is assigned to the same quarter of the output time series.\cr
\bold{NSTOCK}: the value of the input time series in the last non-missing observation of a quarter is assigned to the same quarter of the output time series.\cr
\bold{SUM}: the sum of input observations in a quarter is assigned to the same quarter of the output time series.\cr
\bold{NSUM}:  the sum of input non-missing observations in a quarter is assigned to the same quarter of the output time series. \cr
\bold{AVE}: the average of input observations in a quarter is assigned to the same quarter of the output time series. \cr
\bold{NAVE}: the average of input non-missing observations in a quarter is assigned to the same quarter of the output time series.\cr \cr
Only for semiannual or annual input time series:\cr
\bold{NULL}: (default) the output value of each quarterly observation is set equal to the value of the input observation the quarter belongs to (i.e. duplicated values over the period)\cr
\bold{INTERP_END}: the value of the input time series in a period is copied into the last quarter of the output time series that lies in the same period. Other values are calculated by linear interpolation.\cr
\bold{INTERP_CENTER}: the value of the input time series in a period is copied into the median quarter of the output time series that lies in the same period. Other values are calculated by linear interpolation. \cr
\bold{INTERP_BEGIN}: the value of the input time series in a period is copied into the first quarter of the output time series that lies in the same period. Other values are calculated by linear interpolation.
}
\item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
   \item{...}{
Backward compatibility.
}
}


\value{
This function returns a quarterly BIMETS time series.
}

\seealso{
\code{\link[=YEARLY]{YEARLY}}\cr
\code{\link[=SEMIANNUAL]{SEMIANNUAL}}\cr
\code{\link[=MONTHLY]{MONTHLY}}\cr
\code{\link[=DAILY]{DAILY}}\cr
}

\examples{

	#TS YEARLY TO QUARTERLY
	n<-10
	ts1<-TSERIES(1:n,START=c(2000,1),FREQ=1)
	ts1[5]<-NA
	TABIT(QUARTERLY(ts1,fun='INTERP_CENTER'));
	
	#TS DAILY TO QUARTERLY
	n<-600
	ts1<-TSERIES(1:n,START=c(2000,1),FREQ='D')
	ts1[25]<-NA
	TABIT(QUARTERLY(ts1,fun='SUM'))
}
