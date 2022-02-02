\name{MONTHLY}
\alias{MONTHLY}


\title{
Monthly Time Series (Dis)Aggregation
}
\description{
This function returns a monthly (dis)aggregated time series, by using as input an annual, semiannual, quarterly or daily time series.
}
\usage{
MONTHLY(x = NULL, fun = NULL, avoidCompliance = FALSE, ...)
}
\arguments{
  \item{x}{
  Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
  }
  \item{fun}{ Only for daily input time series:\cr 
\bold{STOCK}: the value of the input time series in the last observation of a month is assigned to the same month of the output time series.\cr
\bold{NSTOCK}: the value of the input time series in the last non-missing observation of a month is assigned to the same month of the output time series.\cr
\bold{SUM}: the sum of input observations in a month is assigned to the same month of the output time series.\cr
\bold{NSUM}:  the sum of input non-missing observations in a month is assigned to the same month of the output time series. \cr
\bold{AVE}: the average of input observations in a month is assigned to the same month of the output time series. \cr
\bold{NAVE}: the average of input non-missing observations in a month is assigned to the same month of the output time series.\cr \cr
Only for quarterly, semiannual or annual input time series:\cr
\bold{NULL}: (default) the output value of each monthly observation is set equal to the value of the input observation the month belongs to (i.e. duplicated values over the period)\cr
\bold{INTERP_END}: the value of the input time series in a period is copied into the last month of the output time series that lies in the same period. Other values are calculated by linear interpolation.\cr
\bold{INTERP_CENTER}: the value of the input time series in a period is copied into the median month of the output time series that lies in the same period. Other values are calculated by linear interpolation. \cr
\bold{INTERP_BEGIN}: the value of the input time series in a period is copied into the first month of the output time series that lies in the same period. Other values are calculated by linear interpolation.
}
\item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}}
   \item{...}{
Backward compatibility.
}
}

\value{
This function returns a monthly BIMETS time series.
}

\seealso{
\code{\link[=YEARLY]{YEARLY}}\cr
\code{\link[=SEMIANNUAL]{SEMIANNUAL}}\cr
\code{\link[=QUARTERLY]{QUARTERLY}}\cr
\code{\link[=DAILY]{DAILY}}\cr
}
\examples{

	#TS FREQ 2 SEMIANNUAL TO MONTHLY
	ts1<-TSERIES((1:10),START=c(2000,1),FREQ=2)
	TABIT(MONTHLY(ts1,fun='INTERP_CENTER'))
	
	#TS DAILY TO MONTHLY
	ts1<-TSERIES((1:366),START=c(2000,1),FREQ='D')
	TABIT(MONTHLY(ts1,fun='STOCK'))
	
	
	
}
