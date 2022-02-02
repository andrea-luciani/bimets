\name{SEMIANNUAL}
\alias{SEMIANNUAL}


\title{
Semiannual (Dis)Aggregation
}
\description{
This function returns a semi-annual (dis)aggregated time series, by using as input an annual, quarterly, monthly or daily time series.
}
\usage{
SEMIANNUAL(x = NULL, fun = NULL, avoidCompliance = FALSE, ...)
}
\arguments{
  \item{x}{
  Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
  }
   \item{fun}{ Only for daily or monthly or quarterly input time series:\cr 
\bold{STOCK}: the value of the input time series in the last observation of a half-year is assigned to the same half-year of the output time series.\cr
\bold{NSTOCK}: the value of the input time series in the last non-missing observation of a half-year is assigned to the same half-year of the output time series.\cr
\bold{SUM}: the sum of input observations in a half-year is assigned to the same half-year of the output time series.\cr
\bold{NSUM}:  the sum of input non-missing observations in a half-year is assigned to the same half-year of the output time series. \cr
\bold{AVE}: the average of input observations in a half-year is assigned to the same half-year of the output time series. \cr
\bold{NAVE}: the average of input non-missing observations in a half-year is assigned to the same half-year of the output time series.\cr \cr
Only for annual input time series:\cr
\bold{NULL}: (default) the output value of each half-year observation is set equal to the value of the input observation the half-year belongs to (i.e. duplicated values over the period)\cr
\bold{INTERP_END}: the value of the input time series in a period is copied into the last half-year of the output time series that lies in the same period. Other values are calculated by linear interpolation.\cr
\bold{INTERP_CENTER}: the value of the input time series in a period is copied into the median half-year of the output time series that lies in the same period. Other values are calculated by linear interpolation. \cr
\bold{INTERP_BEGIN}: the value of the input time series in a period is copied into the first half-year of the output time series that lies in the same period. Other values are calculated by linear interpolation.
}
\item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
   \item{...}{
Backward compatibility.
}
}

\value{
This function returns a semi-annual BIMETS time series.
}

\seealso{
\code{\link[=ANNUAL]{ANNUAL}}\cr
\code{\link[=QUARTERLY]{QUARTERLY}}\cr
\code{\link[=MONTHLY]{MONTHLY}}\cr
\code{\link[=DAILY]{DAILY}}\cr
}
\examples{

	#TS QUARTERLY TO SEMIANNUAL
	n<-14
	xArr<-(n:0)
	ts1<-TSERIES(xArr,START=c(2000,1),FREQ='Q')
	print(SEMIANNUAL(ts1,fun='NAVE'))
	
	#TS ANNUAL TO SEMIANNUAL
	ts1<-TSERIES((1:10),START=c(2000,1),FREQ=1)
	print(SEMIANNUAL(ts1,fun='INTERP_END'))
}
