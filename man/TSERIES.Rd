\name{TSERIES}
\alias{TSERIES}
\alias{TIMESERIES}


\title{
Create a Time Series
}
\description{
This function returns a time series that is compliant with BIMETS compliance control defined in \code{\link[=is.bimets]{is.bimets}}. Users can provide observation values, frequency, and the starting period. Moreover, users can provide metadata information that will be stored into the time series object as attributes.\cr\cr \code{TIMESERIES} is an alias for \code{TSERIES}.
}
\usage{

TIMESERIES(..., START = c(2000,1), FREQ = 1, 
           SOURCE = NULL, TITLE = NULL, UNITS = NULL, SCALEFAC = 0, 
           class=NULL, avoidCompliance = FALSE)
           
TSERIES(..., START = c(2000,1), FREQ = 1, 
        SOURCE = NULL, TITLE = NULL, UNITS = NULL, SCALEFAC = 0, 
        class=NULL, avoidCompliance = FALSE)

}

\arguments{
  \item{...}{
  List of values to be inserted into the output time series. This function accepts
arguments of class \code{ts()} and \code{xts()}, that must be BIMETS compliant as defined in \code{\link[=is.bimets]{is.bimets}}. It also accepts numerical arrays. Please note that for daily and weekly time series, as in the default R time series class \code{ts()}, this function will insert provided input values always filling up to the 366th period in the daily case and up to the 53rd period in the weekly case. (see example)
}
  \item{START}{
This argument defines the start of the time series. Starting period can be specified as \code{c(YEAR,PERIOD)}, or as \code{Date()}, or as \code{yearmon()} if the frequency \code{FREQ=12}, or as \code{yearqtr()} if the frequency \code{FREQ=4}. Please note that the time series must lie in the year range 1800-2199: conversion between date and year-period has been optimized and hard-coded for all frequencies within this time range.
}
 \item{FREQ}{
The frequency of the time series. Frequency can be \cr \code{FREQ=1, 2, 3, 4, 12, 24, 36, 53, or 366}. Frequency can also be defined by using the char \code{'A'} or \code{'Y'} for annual/yearly, \code{'S'} for semiannual, \code{'Q'} for quarterly, \code{'M'} for monthly, \code{'W'} for weekly, and \code{'D'} for daily time series.
}


  \item{SOURCE}{
Set the metadata string that represents the source of the data. Metadata will be lost if the current time series is transformed by any function that changes its values.
}

  \item{TITLE}{
Set the metadata string that represents the description of the data. Metadata will be lost if the current time series is transformed by any function that changes its values.
}

  \item{UNITS}{
Set the metadata string that represents the unit of measure of the data. Metadata will be lost if the current time series is transformed by any function that changes its values.
}
  
  \item{SCALEFAC}{
Set the numerical value that represents the scale factor of the data. Users may eventually want to use this value in code. Metadata will be lost if the current time series is transformed by any function that changes its values.
}

\item{class}{
If \code{class='XTS'} this function will return a time series based on the \code{xts()} class. If \code{class='TS'} this function will return a time series based on the \code{ts()} class. If \code{class=NULL} (default) the output base class will be the one defined in the global BIMETS option \code{'BIMETS_CONF_CCT'} (see \code{\link[=bimetsConf]{BIMETS configuration}}). Please note that package functions only accept time series belonging to the same class as the one defined in the global option \code{'BIMETS_CONF_CCT'}. Users can change any global option directly in the code.

Please note that BIMETS package performs better with \code{class='TS'} or more generally with BIMETS_CONF_CCT='TS'
} 

   \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}}
}
  


\value{
This function returns a BIMETS time series that is compliant with the BIMETS compliance control defined in \code{\link[=is.bimets]{is.bimets}}. 
}

\seealso{
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=as.bimets]{as.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
\code{\link[=fromBIMETStoTS]{fromBIMETStoTS}}\cr
\code{\link[=fromBIMETStoXTS]{fromBIMETStoXTS}}\cr
\code{\link[=NOELS]{NOELS}}\cr
\code{\link[=TSDATES]{TSDATES}}\cr
\code{\link[=INTS]{INTS}}\cr
\code{\link[=TABIT]{TABIT}}\cr
}


\examples{
  
  #day and month names can change depending on locale
  Sys.setlocale('LC_ALL','C')
  Sys.setlocale('LC_TIME','C')
  
  #create a simple R ts
  n<-10
  ts1<-ts((1:n),start=c(2000,1),frequency=1)	
  
  #create a bimets ts annual
  #having the following values: 5, (all data in ts1), NA, 8
  #starting from Jan 2020, and having custom metadata
  out_tseries<-TIMESERIES(5,ts1,NA,8,START=c(2020,1),FREQ=1,
  SOURCE='mySource',TITLE='myTitle',UNITS='myUnits',SCALEFAC=2)
  
  #print out
  TABIT(out_tseries)
  
  #use Date() as start date
  TABIT(TIMESERIES(1:10,START=as.Date('2000-01-01'),FREQ='A'))
  TABIT(TIMESERIES(1:10,START=as.Date('2000-01-01'),FREQ='Y'))
  TABIT(TIMESERIES(1:10,START=as.Date('2000-07-08'),FREQ='D'))
  TABIT(TIMESERIES(1:10,START=as.Date('2018-01-01'),FREQ='W'))
  
  #use yearmon()/yearqtr() as start date
  TABIT(TIMESERIES(1:10,START=as.yearmon('Mar 2001'),FREQ='M'))
  TABIT(TIMESERIES(1:10,START=as.yearqtr('2000 Q3'),FREQ='Q'))
  
  #create ts monthly with metadata
  out_tseries<-TIMESERIES(5,ts1,NA,8,START=c(2020,1),FREQ='M', 
  SOURCE='mySource',TITLE='myTitle',UNITS='myUnits',SCALEFAC=2)
  
  #print out
  TABIT(out_tseries)
  
  #create daily 
  out_tseries<-TSERIES(5,ts1,NA,8,START=c(2000,1),FREQ=366, 
  SOURCE='mySource',TITLE='myTitle',UNITS='myUnits',SCALEFAC=2)
  
  #print out
  TABIT(out_tseries)
  
  #insert values skipping 366 in non-bissextile
  myLength<-400
  myValues<-1:myLength
  myDates<-as.Date('2001-01-01')+0:(myLength-1)
  ts<-as.bimets(xts(myValues,order.by = myDates))
  TABIT(ts) #366 observation will be a duplicated of 365, see as.bimets() help
	
}
