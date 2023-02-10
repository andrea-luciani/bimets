\name{as.bimets}
\alias{as.bimets}

\title{
Convert a Time Series to BIMETS
}
\description{

This function tries to convert a time series of class \code{ts()} or \code{xts()} into a BIMETS time series that satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}\cr

All the information in the input time series will be preserved.\cr\cr Input time series must be of class \code{ts()} or \code{xts()}, and will be converted in the BIMETS class-type specified in the global option \code{BIMETS_CONF_CCT} (see \code{\link[=bimetsConf]{BIMETS configuration}}). 
\cr\cr If the input time series has a temporal discontinuity (i.e. a missing pair Date-Value in the case of \code{xts()} time series) then the missing pair Date-Value is inserted in the output time series with a  missing value \code{NA}, or with the value provided in the \code{FILLVALUE} argument. \cr\cr If \code{BIMETS_CONF_CCT='XTS'}, in the case of a monthly input time series the \code{.indexCLASS} is converted to the class \code{yearmon()}; in the case of a quarterly input time series the \code{.indexCLASS} is converted to \code{yearqtr()}; in the case of other input frequency the \code{.indexCLASS} is converted to \code{Date()}. If \code{BIMETS_CONF_CCT='XTS'} the dates of all the output observations are set accordingly to the BIMETS global option \code{BIMETS_CONF_DIP}, i.e. the first or the last dates in the period (see \code{\link[=bimetsConf]{BIMETS configuration}}). \cr\cr
If the input time series has multiple observations in the same date, e.g. an \code{xts()} with a two or more observations in the same date, the duplication is removed and the output time series will contain only the later observation (see example).
\cr\cr If the input time series is multivariate, the output time series will contain only the first column of the input matrix of data (where the matrix of data is the matrix built by binding input time series values as columns). \cr\cr If the input time series is a daily time series of class \code{xts()} and the global option \code{BIMETS_CONF_CCT='TS'} then the 366th period of the output time series in each non-bissextile year will have the value of the 365th period in the same year (duplicated value).
}
\usage{
as.bimets(x=NULL, FILLVALUE=NA, VERBOSE=FALSE, ...)
}

\arguments{
  \item{x}{
Input time series of class \code{ts()} or \code{xts()}. 
}
  \item{FILLVALUE}{
Value inserted in the output time series in the case of temporal discontinuity. Defaults to missing NA.
 }
  \item{VERBOSE}{
If \code{TRUE}, a verbose description of inserted and/or removed observations, if any, will be shown.
}
   \item{...}{
Backward compatibility.
}
}

\value{
This function returns a BIMETS time series (see also \code{\link[=bimetsConf]{BIMETS configuration}})
.

}


\seealso{
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
\code{\link[=fromBIMETStoXTS]{fromBIMETStoXTS}}\cr
\code{\link[=fromBIMETStoTS]{fromBIMETStoTS}}\cr
}

\examples{

	#xts series with dates equal to the first date in the period, 
	#and some missing observations
	#first...set option and work with xts
	setBIMETSconf('BIMETS_CONF_CCT','XTS')
	#create xts
	xt<-xts(1:10,order.by=seq(as.Date('2000-01-01'),len=10,by='year'))
	#remove some data
	xt<-xt[-5]
	xt<-xt[-3] 
	#convert to bimets
	xtBimets<-as.bimets(xt)
	#print before and after...
	print(xt)
	print(xtBimets)
	
	#ts bivariate series into xts
	setBIMETSconf('BIMETS_CONF_CCT','XTS')
	ts<-ts(matrix(c(1,2,3,4,5,6),nrow=3,ncol=2),start=c(2000,1),frequency=1)
	print(ts)
	xtsBimets<-as.bimets(ts)
	print(xtsBimets)
	
	#reset defaults
	setBIMETSconf('BIMETS_CONF_DIP','LAST')
	setBIMETSconf('BIMETS_CONF_CCT','TS')
	
	#xts quarterly with irregular dates and missings data 
	xt<-xts(1:10,order.by=seq(as.Date('2000-01-03'),len=10,by='3 months'))
	#remove some data
	xt<-xt[-5]
	xt<-xt[-3] 
	#convert
	tsBimets<-as.bimets(xt)
	#print before and after
	print(xt)
	print(tsBimets)
	
	#xts daily with duplicated observations and missing data
	xt<-xts(1:11,order.by=c(as.Date('2000-01-01'),
	                     seq(as.Date('2000-01-01'),
	                     len=10,by='day')))
	xt<-xt[-5]
	xt<-xt[-3] 
	#convert
	tsBimets<-as.bimets(xt)
	#print before and after
	print(xt)
	print(tsBimets)
	
	#verbose and fillvalue
	xt<-xts(1:11,order.by=c(as.Date('2000-01-01'),
                       seq(as.Date('2000-01-01'),
                       len=10,by='day')))
xt<-xt[-5]
xt<-xt[-3]
as.bimets(xt,FILLVALUE=99.99,VERBOSE=TRUE)

}

