\name{fromTStoXTS}
\alias{fromTStoXTS}

\title{
Convert TS to XTS
}
\description{
This function transforms a BIMETS compliant \code{ts} time series (as defined in \code{\link[=is.bimets]{is.bimets}}) into a time series of class \code{xts()}.\cr The core XTS function \code{as.xts()} 
 does not satisfy all the requirements of the compliance control check, so it has been extended.  If the output time series has an \code{.indexClass} of type \code{Date()}, i.e. neither monthly nor quarterly, the output dates are chosen accordingly to the BIMETS option \code{BIMETS_CONF_DIP}: if this option is set to \code{LAST} (default), the output \code{xts()} time series will have the date of the period set equal to the last day in the same period, e.g. 31 December for annual time series, 30 June for semiannual, etc.; if BIMETS option \code{BIMETS_CONF_DIP} is set to \code{FIRST}, the output \code{xts()} time series will have the date of the period set equal to the first day in the same period, e.g. 1 January for annual time series, 1 July for semiannual time series on the second period, etc. \cr In the case of quarterly time series the \code{.indexClass=yearqtr}; \cr
in the case of monthly time series the \code{.indexClass=yearmon}.\cr Attributes and description of the input time series will be copied to the output time series (see \code{\link[=TSERIES]{TIMESERIES}})
}
\usage{
fromTStoXTS(x = NULL, avoidCompliance = FALSE, ...)
}

\arguments{
  \item{x}{
Input \code{ts} time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}. 
}
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}.
}
  \item{...}{
Backward compatibility.
}
}

\value{
This function returns a time series of class \code{xts()} that has the same observations of the input \code{ts} time series.
}

\seealso{
\code{\link[=fromXTStoTS]{fromXTStoTS}}\cr
\code{\link[=as.bimets]{as.bimets}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
}
\examples{
	
	#day and month names can change depending on locale
	Sys.setlocale('LC_ALL','C')
	Sys.setlocale('LC_TIME','C')
  
	#BIMETS_CONF_DIP default on LAST
	print('annual')
	t=ts(1:20,start=c(2005,2),frequency=1)
	ts=fromTStoXTS(t)
	print(t);print(ts) #...dates on 31 Dec
	
	print('semiannual')
	t=ts(1:20,start=c(2005,2),frequency=2)
	ts=fromTStoXTS(t)
	print(t);print(ts) #...dates on 31 Dec/30 Jun
	
	#set configuration BIMETS_CONF_DIP on FIRST
	setBIMETSconf('BIMETS_CONF_DIP','FIRST')
	
	print('annual')
	t=ts(1:20,start=c(2005,2),frequency=1)
	ts=fromTStoXTS(t)
	print(t);print(ts) #...dates on 1 Jan
	
	print('semiannual')
	t=ts(1:20,start=c(2005,2),frequency=2)
	ts=fromTStoXTS(t)
	print(t);print(ts) #...dates on 1 Jan/1 Jul
	
	print('quarterly')
	t=ts(1:20,start=c(2004,3),frequency=4)
	ts=fromTStoXTS(t)
	print(t);print(ts)
	
	print('monthly')
	t=ts(1:20,start=c(2003,5),frequency=12)
	ts=fromTStoXTS(t)
	print(t);print(ts)
	
	
	print('daily')
	t=ts(1:20,start=c(2003,125),frequency=366)
	ts=fromTStoXTS(t)
	print(t);print(ts)
	
}
