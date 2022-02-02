\name{is.bimets}
\alias{is.bimets}

\title{
Check the Compliance of a Time Series
}
\description{
This function checks the compliance of the input time series that must verify the following BIMETS requirements:\cr\cr
- the input time series must be of the class defined in \code{BIMETS_CONF_CCT} (see \code{\link[=bimetsConf]{BIMETS configuration}}) \cr\cr
- If \code{BIMETS_CONF_CCT='TS'} the input time series must be of class \code{ts}, univariate, with at least one observation and with a frequency \code{f=1, 2, 3, 4, 12, 24, 36, 53 or 366} per year.\cr\cr
- if \code{BIMETS_CONF_CCT='XTS'} the input time series must be of class \code{xts}, univariate, with at least one observation and with a frequency \code{f=1, 2, 3, 4, 12, 24, 36, 53 or 366} per year; the input time series must also be stricty regular, i.e. without any temporal discontinuity, and must have an \code{.indexClass} of type \code{yearmon()} for monthly time series, of type \code{yearqtr()} for quarterly time series and of type \code{Date()} for any other frequency. If configuration option \code{BIMETS_CONF_DIP='LAST'}, i.e. the default value, the provided observation dates of the input \code{xts()} time series must be the last dates in the period, e.g. Dec. 31 for annual time series, Jun. 30 for the first period in a semiannual time series, etc.; If configuration option \code{BIMETS_CONF_DIP='FIRST'} the provided observation dates of the input \code{xts()} time series must be the first dates in the period, e.g. Jan. 1 for an annual time series, Jul. 1 for the second period in a semiannual time series, etc.; \cr \cr BIMETS package functions return time series that are compliant to the above requirements.\cr\cr The compliance check can be locally disabled by using the function argument \code{avoidCompliance=TRUE}, that is available in almost all package functions. The compliance check of a BIMETS generated time series can be avoided and disabling the control check can speed up the execution time. This is suggested when users concatenate several call to the package functions, e.g. the compliance check of the \code{ts2} time series in the following example can be avoided: \cr \code{ts2=TSLAG(ts1);ts3=TSDELTA(ts2,avoidCompliance=TRUE);}.\cr\cr Time series must lie in the year range 1800-2199: in this range the conversion between a date and the related year-period (and vice versa) has been hardcoded in order to speedup the execution time.\cr\cr
If the compliance check is disabled, i.e. \code{avoidCompliance=TRUE} and the input time series does not verify all the above requirements, the package functions can have an erroneous behavior. Should any doubt arise, we suggest to call the package functions using the default arguments; we also suggest to create time series object by using the command \code{\link[=TSERIES]{TIMESERIES}}.\cr

}
\usage{
is.bimets(x = NULL, suppressErrors=TRUE, ...)
}

\arguments{
  \item{x}{
Input time series. 
}
  \item{suppressErrors}{
If \code{suppressErrors=TRUE} the function returns a logical value \code{TRUE/FALSE} whenever the input time series is BIMETS compliant. If \code{suppressErrors=FALSE} the function will throw an error if the input time series is not BIMETS compliant.
}

 \item{...}{
Backward compatibility.
}

}

\value{
This function returns a logical value \code{TRUE/FALSE} whenever the input time series is compliant to the above BIMETS requirements. If the test fails and \code{suppressErrors=FALSE} this function will throw an error.
}


\seealso{
\code{\link[=as.bimets]{as.bimets}}\cr
\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
\code{\link[=fromBIMETStoTS]{fromBIMETStoTS}}\cr
\code{\link[=fromBIMETStoXTS]{fromBIMETStoXTS}}\cr
}
\examples{

  #day and month names can change depending on locale
  Sys.setlocale('LC_ALL','C')
  Sys.setlocale('LC_TIME','C')
  
  #set day in period to last 
  setBIMETSconf('BIMETS_CONF_DIP','LAST')
  
  #set constructor class type
  setBIMETSconf('BIMETS_CONF_CCT','XTS')
  
  #create an xts
  xt=TIMESERIES(1:10,START=c(2000,1),FREQ='A')
  
  print(xt); #...dates are at Dec 31
  
  print(is.bimets(xt)) #...TRUE
  
  #change setting
  setBIMETSconf('BIMETS_CONF_DIP','FIRST')
  
  print(is.bimets(xt)) #...FALSE
  
  #set constructor class type
  setBIMETSconf('BIMETS_CONF_CCT','TS')
  
  #bivariate ts
  tsBiv=ts(matrix(c(1,2,3,4,5,6),nrow=3,ncol=2),start=c(2000,1),frequency=1)
  
  print(is.bimets(tsBiv)) #...FALSE
  
  #...error
  tryCatch({is.bimets(tsBiv,suppressError=FALSE)},
  	error=function(e){cat(e$message)});try({is.bimets(tsBiv,suppressError=FALSE)}) 
  
  #ts year 
  n=10
  xArr=rnorm(n)
  t=ts(data=xArr,start=c(2000,1),frequency=1)
  cat('is compliant?',is.bimets(t),'\n')
  
  #ts semestral 
  n=10
  xArr=rnorm(n)
  t=ts(data=xArr,start=c(2000,1),frequency=2)
  cat('is compliant?',is.bimets(t),'\n')
  
  #set configuration BIMETS_CONF_DIP on FIRST
  setBIMETSconf('BIMETS_CONF_DIP','FIRST')
  
  #work with XTS
  setBIMETSconf('BIMETS_CONF_CCT','XTS')
  
  #xts annual with dates
  n=10
  xArr=rnorm(n)
  dateArr=seq(as.Date('2000/01/01'),by='year',length=n)
  dataF=data.frame(dateArr,xArr)
  xt=xts(dataF[,2],order.by=dataF[,1])
  cat('is compliant?',is.bimets(xt),'\n')
  
  
  #xts daily
  n=10
  xArr=rnorm(n)
  dateArr=seq(as.Date('2000/01/01'),by='day',length=n)
  dataF=data.frame(dateArr,xArr)
  xt=xts(dataF[,2],order.by=dataF[,1])
  cat('is compliant?',is.bimets(xt),'\n')
  
  #xts monthly with dates
  n=10
  xArr=rnorm(n)
  dateArr=seq(as.Date('2000/01/01'),by='month',length=n)
  dataF=data.frame(dateArr,xArr)
  xt=xts(dataF[,2],order.by=dataF[,1])
  cat('monthly with dates is compliant? ',is.bimets(xt),'\n') #...false
  
  
  #xts monthly with yearmon
  n=10
  xArr=rnorm(n+1)
  dateArr=as.yearmon('Jan 2001')+0:n/12
  dataF=data.frame(dateArr,xArr)
  xt=xts(dataF[,2],order.by=dataF[,1])
  cat('monthly with yearmon is compliant? ',is.bimets(xt),'\n') #...true
  
  
  #restore defaults
  setBIMETSconf('BIMETS_CONF_CCT','TS')
  setBIMETSconf('BIMETS_CONF_DIP','LAST')

}
