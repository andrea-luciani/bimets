\name{fromBIMETStoTS}
\alias{fromBIMETStoTS}

\title{
Convert BIMETS to TS
}
\description{
This function transforms a BIMETS compliant time series (as defined in \code{\link[=is.bimets]{is.bimets}}) into a time series of class \code{ts()}. The core R function as.ts() does not satisfy all the compliance control check requirements, so it has been extended. Attributes and description of the input time series will be copied to the output time series (see \code{\link[=TSERIES]{TIMESERIES}}).
}
\usage{
fromBIMETStoTS(x = NULL, ...)
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
This function returns a time series of class \code{ts()} that has the same observations of the input BIMETS time series.
}

\seealso{
\code{\link[=fromBIMETStoXTS]{fromBIMETStoXTS}}\cr
\code{\link[=as.bimets]{as.bimets}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
\code{\link[=BIMETS2CSV]{BIMETS2CSV}}\cr
\code{\link[=CSV2BIMETS]{CSV2BIMETS}}\cr
}

\examples{	
	
	#work with XTS
	setBIMETSconf('BIMETS_CONF_CCT','XTS')
	
	#create yearly time series
	xts<-TSERIES(1:10,START=c(2000,1),FREQ='A')
	print(is.ts(xts))#FALSE
	#convert to ts
	ts<-fromBIMETStoTS(xts)
	print(is.ts(ts))#TRUE
	print(ts)
	
	#create monthly time series
	xts<-TSERIES(1:10,START=c(2000,1),FREQ='M')
	print(is.ts(xts))#FALSE
	#convert to ts
	ts<-fromBIMETStoTS(xts)
	print(is.ts(ts))#TRUE
	print(ts)
	
	#create daily time series
	xts<-TSERIES(1:10,START=c(2000,1),FREQ='D')
	print(is.ts(xts))#FALSE
	#convert to ts
	ts<-fromBIMETStoTS(xts)
	print(is.ts(ts))#TRUE
	print(ts)
	

	
	#reset default	
	setBIMETSconf('BIMETS_CONF_CCT','TS')
}
