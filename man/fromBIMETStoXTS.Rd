\name{fromBIMETStoXTS}
\alias{fromBIMETStoXTS}

\title{
Convert BIMETS to XTS
}
\description{
This function transforms a BIMETS compliant time series (as defined in \code{\link[=is.bimets]{is.bimets}}) 
 into a time series of class \code{xts()}.\cr The core XTS function as.xts() does not satisfy all the compliance control check requirements, so it has been extended. If the output time series will have an \code{.indexClass} of type \code{Date()}, i.e. neither monthly nor quarterly, the output dates will be chosen accordingly to the BIMETS option \code{BIMETS_CONF_DIP}: if this option is set to \code{LAST} (default), the output \code{xts()} time series will have the date of the period set equal to the last day in the same period, e.g. 31 December for yearly time series, 30 June for semiannual, etc.; if the BIMETS option \code{BIMETS_CONF_DIP} is set to \code{FIRST}, the output \code{xts()} time series will have the date of the period set equal to the first day in the same period, e.g. 1 January for yearly time series, 1 July for semiannual time series on the second period, etc. \cr In the case of quarterly time series the \code{.indexClass=yearqtr}; \cr
in the case of monthly time series the \code{.indexClass=yearmon}.\cr Attributes and description of the input time series will be copied to the output time series (see \code{\link[=TSERIES]{TIMESERIES}})
}
\usage{
fromBIMETStoXTS(x = NULL, ...)
}

\arguments{
  \item{x}{
Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{...}{
Backward compatibility.
}
}

\value{
This function returns a time series of class \code{xts()} that has the same observations of the input BIMETS time series.
}

\seealso{
\code{\link[=fromBIMETStoTS]{fromBIMETStoTS}}\cr
\code{\link[=as.bimets]{as.bimets}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
\code{\link[=BIMETS2CSV]{BIMETS2CSV}}\cr
\code{\link[=CSV2BIMETS]{CSV2BIMETS}}\cr
}
\examples{
	
	#create yearly time series
	ts<-TSERIES(1:10,START=c(2000,1),FREQ='A')
	print(is.xts(ts))#FALSE
	#convert to xts
	xts<-fromBIMETStoXTS(ts)
	print(is.xts(xts))#TRUE
	print(xts)
	
	#create monthly time series
	ts<-TSERIES(1:10,START=c(2000,1),FREQ='M')
	print(is.xts(ts))#FALSE
	#convert to xts
	xts<-fromBIMETStoXTS(ts)
	print(is.xts(xts))#TRUE
	print(xts)
	
	#create daily time series
	ts<-TSERIES(1:10,START=c(2000,1),FREQ='D')
	print(is.xts(ts))#FALSE
	#convert to xts
	xts<-fromBIMETStoXTS(ts)
	print(is.xts(xts))#TRUE
	print(xts)
	
	#create yearly time series with first date on period
	setBIMETSconf('BIMETS_CONF_DIP','FIRST')
	ts<-TSERIES(1:10,START=c(2000,1),FREQ='A')
	print(is.xts(ts))#FALSE
	#convert to xts
	xts=fromBIMETStoXTS(ts)
	print(is.xts(xts))#TRUE
	print(xts)#dates on Jan 1
	
	#reset default
	setBIMETSconf('BIMETS_CONF_DIP','LAST')
	
	
}
