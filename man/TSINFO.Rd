\name{TSINFO}
\alias{TSINFO}


\title{
Get Time Series Info
}
\description{
This function returns detailed information about the input time series list. The requested information is defined in the argument \code{MODE}.
}
\usage{
TSINFO(..., MODE = NULL, avoidCompliance=FALSE)
}

\arguments{
\item{...}{
Input time series list. Each time series must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
\item{MODE}{
Select the information to be retrieved from the list of time series. \code{MODE} can be set to:\cr
\bold{STARTY}: the output will be a numerical array built with the starting year of each time series in the input list.\cr
\bold{ENDY}:  the output will be a numerical array built with the ending year of each time series in the input list.\cr
\bold{STARTP}:  the output will be a numerical array built with the starting period of each time series in the input list.\cr 
\bold{ENDP}:  the output will be a numerical array built with the ending period of each time series in the input list.\cr
\bold{START}:  the output will be a numerical array built with the value \cr \code{x=START_YEAR+START_PERIOD/FREQ} calculated on each time series in the input list, where \code{FREQ} is the time series frequency.\cr
\bold{END}:  the output will be a numerical array built with the value \cr \code{x=END_YEAR+END_PERIOD/FREQ} calculated on each time series in the input list.\cr
\bold{START2}:  the output will be a numerical matrix. For each time series in the input list the output matrix will have a \code{row=c(START_YEAR,START_PERIOD)} with related values.\cr
\bold{END2}:  the output will be a numerical matrix. For each time series in the input list the output matrix will have a \code{row=c(END_YEAR,END_PERIOD)} with related values.\cr
\bold{FREQ}:  the output will be a numerical array built with the frequency of each time series in the input list.\cr
\bold{FACTOR}: the output will be a numerical array built with the SCALEFAC metadata value of each time series in the input list. \cr
\bold{UNITS}: the output will be a string array built with the UNITS metadata string of each time series in the input list.\cr
\bold{TITLE}:  the output will be a string array built with the TITLE metadata string of each time series in the input list.\cr
\bold{SOURCE}: the output will be a string array built with the SOURCE metadata string of each time series in the input list. \cr
}

 
\item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}}
}
  


\value{
This function returns an array built of the requested information about the input time series list. In the case of \code{MODE=START2} or \code{MODE=END2} the output will be of class \code{matrix()}.
}

\seealso{

\code{\link[=NOELS]{NOELS}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=fromBIMETStoTS]{fromBIMETStoTS}}\cr
\code{\link[=fromBIMETStoXTS]{fromBIMETStoXTS}}\cr
\code{\link[=GETYEARPERIOD]{GETYEARPERIOD}}\cr
\code{\link[=INTS]{INTS}}\cr
\code{\link[=TSLOOK]{TSLOOK}}\cr
\code{\link[=TABIT]{TABIT}}\cr
}


\examples{
	
	#create ts
	ts1<-TIMESERIES(INTS(1,10),START=c(2004,2),FREQ=2,
	UNITS='myUnits',TITLE='myTitle',SOURCE='mySource')
	ts2<-TIMESERIES(INTS(1,20),START=c(2006,3),FREQ=4,SCALEFAC=1)
	ts3<-TIMESERIES(INTS(1,30),START=c(2008,7),FREQ=12)
	
	print(TSINFO(ts1,ts2,ts3,MODE='STARTY')) #print ... c(2004,2006,2008)
	print(TSINFO(ts1,ts2,ts3,MODE='ENDP')) #print ... c(1,2,12)
	print(TSINFO(ts1,ts2,ts3,MODE='FREQ')) #print ... c(2,4,12)
	
	print(TSINFO(ts1,ts2,ts3,MODE='START2')) 
	#print ... 
	#[,1] [,2]
	#[1,] 2004    2
	#[2,] 2006    3
	#[3,] 2008    7
	
	print(TSINFO(ts1,ts2,ts3,MODE='END')) #print ... c(2009.5, 2011.5, 2011.0)
	print(TSINFO(ts1,ts2,ts3,MODE='FACTOR')) #print ...  c(0,1,0)
	print(TSINFO(ts1,ts2,ts3,MODE='UNITS')) #print ...  c('myUnits','','')
	
}





















