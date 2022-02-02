\name{ANNUAL}
\alias{ANNUAL}
\alias{YEARLY}

\title{
Annual Time Series (Dis)Aggregation
}

\description{
This function returns a yearly aggregated time series, by using as input a semiannual, quarterly, monthly or daily time series.
}
\usage{
ANNUAL(x = NULL, fun = NULL, avoidCompliance = FALSE, ...)
YEARLY(x = NULL, fun = NULL, avoidCompliance = FALSE, ...)
}
\arguments{
  \item{x}{
Input time series, that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
\item{fun}{ 
\bold{STOCK}: the output value of a year is equal to the value of the input time series in the last period of the same year \cr
\bold{NSTOCK}: the output value of a year is equal to the value of the input time series in the last non-missing \code{NA} period of the same year \cr
\bold{SUM}: the output value of a year is equal to the sum of all the observations of the input time series in the same year \cr
\bold{NSUM}: the output value of a year is equal to the sum of all the non-missing \code{NA} observations of the input time series in the same year \cr
\bold{AVE}: the output value of a year is equal to the average of all the observations of the input time series in the same year \cr
\bold{NAVE}: the output value of a year is equal to the average of all the non-missing \code{NA} observations of the input time series in the same year \cr
}
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
 \item{...}{
Backward compatibility.
}
}
\value{
This function returns a yearly BIMETS time series.
}

\seealso{
\code{\link[=SEMIANNUAL]{SEMIANNUAL}}\cr
\code{\link[=QUARTERLY]{QUARTERLY}}\cr
\code{\link[=MONTHLY]{MONTHLY}}\cr
\code{\link[=DAILY]{DAILY}}\cr
}
\examples{
	
	#TS DAILY TO ANNUAL
	n=366
	ts1=TIMESERIES(0:n,START=c(2000,1),FREQ='D')
	ts1[10]=NA
	TABIT(ANNUAL(ts1,fun='NAVE'))
	
	#TS DAILY TO ANNUAL
	n=36
	ts1=TIMESERIES(0:n,START=c(2000,1),FREQ='M')
	ts1[10]=NA
	TABIT(YEARLY(ts1,fun='SUM'))
}	


