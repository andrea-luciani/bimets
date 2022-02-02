\name{fromXTStoTS}
\alias{fromXTStoTS}

\title{
Convert XTS to TS
}
\description{
This function transforms a BIMETS compliant \code{xts()} time series (as defined in \code{\link[=is.bimets]{is.bimets}}) into a time series of class \code{ts()}. The core R function \code{as.ts()} does not satisfy all the requirements of the compliance control check, so it has been extended. Attributes and description of the input time series will be copied to the output time series (see \code{\link[=TSERIES]{TIMESERIES}}).
}
\usage{
fromXTStoTS(x = NULL, avoidCompliance = FALSE, ...)
}

\arguments{
  \item{x}{
Input \code{xts()} time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}. 
}
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}.
}
  \item{...}{
Backward compatibility.
}
}

\value{
This function returns a time series of class \code{ts()} that has the same observations of the input \code{xts()} time series.

}

\seealso{
\code{\link[=fromTStoXTS]{fromTStoXTS}}\cr
\code{\link[=as.bimets]{as.bimets}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
}

\examples{

	#day and month names can change depending on locale
	Sys.setlocale('LC_ALL','C')
	Sys.setlocale('LC_TIME','C')
  
	#set configuration BIMETS_CONF_DIP on FIRST
	setBIMETSconf('BIMETS_CONF_DIP','FIRST')
	
	#set configuration BIMETS_CONF_CCT on XTS
	setBIMETSconf('BIMETS_CONF_CCT','XTS')
	
	#semiannual with Date()
	n=10
	xArr=rnorm(n)
	dateArr=seq(as.Date('2000/07/01'),by='6 months',length=n) 
	dataF=data.frame(dateArr,xArr)
	xt=xts(dataF[,2],order.by=dataF[,1])
	print(fromXTStoTS(xt))
	
	#set configuration BIMETS_CONF_DIP on LAST
	setBIMETSconf('BIMETS_CONF_DIP','LAST')
	
	#annual with Date()
	n=10
	xArr=rnorm(n)
	dateArr=seq(as.Date('2000/12/31'),by='year',length=n)
	dataF=data.frame(dateArr,xArr)
	xt=xts(dataF[,2],order.by=dataF[,1])
	print(fromXTStoTS(xt))
	
	#quarterly with yearqtr()
	n=10
	xArr=rnorm(n+1)
	dateArr=as.yearqtr('2000 Q2') + 0:n/4
	dataF=data.frame(dateArr,xArr)
	xt=xts(dataF[,2],order.by=dataF[,1])
	print(fromXTStoTS(xt))
	
	#monthly with yearmon()
	n=10
	xArr=rnorm(n+1)
	dateArr=as.yearmon('Jul 2000') + 0:n/12
	dataF=data.frame(dateArr,xArr)
	xt=xts(dataF[,2],order.by=dataF[,1])
	print(fromXTStoTS(xt))
	
	#daily with Date()
	n=10
	xArr=rnorm(n)
	dateArr=seq(as.Date('2000/07/14'),by='day',length=n) 
	dataF=data.frame(dateArr,xArr)
	xt=xts(dataF[,2],order.by=dataF[,1])
	print(fromXTStoTS(xt))
	
	#restore defaults
	setBIMETSconf('BIMETS_CONF_DIP','LAST')
	setBIMETSconf('BIMETS_CONF_CCT','TS')
	
	
}
