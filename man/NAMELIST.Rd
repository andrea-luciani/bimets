\name{NAMELIST}
\alias{NAMELIST}


\title{
Named List of Time Series
}
\description{
In the case of strings input, this function returns a string array built with the input strings. In the case of time series input, this function returns a list built with the input time series; the output list names will be the variable names passed as arguments. 
}
\usage{
NAMELIST(...)
}

\arguments{
  \item{...}{
List of strings or list of time series. In the case of a list of strings, if an input string is not eligible to be a variable name, e.g. a string composed only with numbers, or with special characters, a warning will be thrown and a message will describe the required change made to the input string in order to make it eligible to be a variable name (see example). 
}

  
}

\value{
In the case of strings as input, this function returns a string array built with the input strings. In the case of time series as input, this function returns a list built with the input time series; the output list names will be the variable names passed as arguments. 
}

\seealso{
\code{\link[=NOELS]{NOELS}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=TSERIES]{TSERIES}}\cr
\code{\link[=GETYEARPERIOD]{GETYEARPERIOD}}\cr
\code{\link[=LOCS]{LOCS}}\cr
}


\examples{
	
	#NAMELIST with time series...
	
	ts1<-TSERIES(1:10,START=c(2000,1),FREQ=12)
	ts2<-TSERIES(10:20,START=c(2002,5),FREQ=12)
	myNameList<-NAMELIST(ts1,ts2)
	print(myNameList)
	
	#prints a list with $ts1 and $ts2 elements 
	#please note that names are 'ts1' and 'ts2'...	
	#$ts1
	#     Jan Feb Mar Apr May Jun Jul Aug Sep Oct
	#2000   1   2   3   4   5   6   7   8   9  10
	
	#$ts2
	#     Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
	#2002                  10  11  12  13  14  15  16  17
	#2003  18  19  20
	
	
	
	#define strange namelist
	#print warnings... '' converted in 'X', '9' converted in 'X9'
	myNAMELIST<-NAMELIST('pp','oo','ii','','9');
	
	print(myNAMELIST)
	
}
