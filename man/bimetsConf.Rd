\name{bimetsConf}
\alias{setBIMETSconf}
\alias{getBIMETSconf}
\alias{BIMETS configuration}
\alias{bimetsConf}

\title{
BIMETS Global Options Configuration
}
\description{
BIMETS package depends on some options in order to transform and to present time series data. These options can be read or changed by using the functions:\cr \code{setBIMETSconf(opt,value)} and \code{getBIMETSconf(opt)} 
}
\usage{
setBIMETSconf(opt=NULL, value=NULL, suppressOutput=FALSE)
getBIMETSconf(opt=NULL)
}

\arguments{
  \item{opt}{
Name of the BIMETS option. Available option names are:\cr \cr 
\bold{BIMETS_CONF_DIP}: Date In Period. Users can associate to each observation in a time series the first or the last date in the period, i.e.  1 January or 31 December in the case of an annual time series, 1 January/1 July or 30 June/31 December in the case of a semiannual time series, 1 January/31 January in the case of an monthly time series in January, etc. The assignments by date to time series (e.g. \code{ts['2000-01-01']=value}) need to be coherent to the value of this global option. Accepted values are:\cr
LAST: (default) each observation has associated the last date of the period, e.g. 31 Mar for a quarterly time series, 31 January for a monthly time series in the first period, etc.\cr
FIRST: each observation has associated the first date of the period, e.g. 1 Jan for a quarterly time series, 1 Feb for a monthly time series in the second period, etc.
\cr \cr
\bold{BIMETS_CONF_CCT}: Constructor Class Type. The package supports \code{ts()} and \code{xts()} time series as input arguments; furthermore the user can select the base class of a BIMETS time series, i.e. the class used when a time series is created with \code{TIMESERIES()} or converted to BIMETS with \code{as.bimets()}. This is a global option; Users can locally override the selection of the output class by using the \code{class='TS'} or \code{class='XTS'} argument in the \code{TIMESERIES()} function. The option \code{BIMETS_CONF_CCT} can be assigned to the following values:\cr
TS: (default) the time series constructor \code{TIMESERIES()} and the conversion function \code{as.bimets()} return an object of class \code{ts()}\cr
XTS: the time series constructor \code{TIMESERIES()} and the conversion function \code{as.bimets()} return an object of class \code{xts()}.
\cr \cr
Please note that BIMETS package performs better with BIMETS_CONF_CCT='TS'\cr\cr
\bold{BIMETS_CONF_NOC}: NO Compliance test. If this option is set \code{TRUE} then the compliance control check on input time series, i.e. \code{\link[=is.bimets]{is.bimets()}}, will be globally disabled. The default is set to \code{FALSE}. The compliance check on input time series should generally be active, otherwise a malformed input time series can produce unwanted results in operations. 
 \cr
}
 \item{value}{ 
The value to be assigned to the BIMETS option.
}
 \item{suppressOutput}{ 
If \code{TRUE}, the output messages will be disabled.} 
}

\value{
This function set or read global BIMETS options, and return a NULL value.
}

\seealso{
\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=as.bimets]{as.bimets}}\cr
\code{\link[=fromBIMETStoXTS]{fromBIMETStoXTS}}\cr
\code{\link[=fromBIMETStoTS]{fromBIMETStoTS}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
}


\examples{
	
	#default BIMETS_CONF_DIP is LAST
	#create ts
	ts1=TSERIES((1:10),START=c(2000,1),FREQ=1)
	
	#transform to xts and print
	xt1=fromBIMETStoXTS(ts1)
	print(xt1)  #....dates as of 31 Dec
	
	#set configuration BIMETS_CONF_DIP to FIRST
	setBIMETSconf('BIMETS_CONF_DIP','FIRST')
	
	#create ts
	ts1=TSERIES((1:10),START=c(2000,1),FREQ=1)
	
	#transform to xts and print
	xt1=fromBIMETStoXTS(ts1)
	print(xt1)  #....dates as of 1 Jan
	
	#set configuration BIMETS_CONF_DIP to LAST
	setBIMETSconf('BIMETS_CONF_DIP','LAST')
	
	#work on XTS
	setBIMETSconf('BIMETS_CONF_CCT','XTS')
	
	#check compliance of xt1 and fail...	
	is.bimets(xt1) #... FALSE
	
	#set configuration BIMETS_CONF_DIP to FIRST
	setBIMETSconf('BIMETS_CONF_DIP','FIRST')
	
	#check compliance of xt1 and ok...	
	is.bimets(xt1) #... TRUE
	
	print(getBIMETSconf('BIMETS_CONF_DIP')) # ... returns FIRST		
	print(getBIMETSconf('BIMETS_CONF_CCT')) # ... returns XTS
	
	print(is.xts(TSERIES(1:10,START=c(2000,1),FREQ=1))) #...print TRUE
	print(is.ts(TSERIES(1:10,START=c(2000,1),FREQ=1,class='TS'))) #...print TRUE
	
	
	
	#NOC
	setBIMETSconf('BIMETS_CONF_CCT','XTS')
	is.bimets(xts()) #FALSE
	setBIMETSconf('BIMETS_CONF_NOC',TRUE) 
	is.bimets(xts()) #TRUE
	
	
	#...back to default
	setBIMETSconf('BIMETS_CONF_DIP','LAST')
	setBIMETSconf('BIMETS_CONF_CCT','TS')
	setBIMETSconf('BIMETS_CONF_NOC',FALSE)
	
	
}
