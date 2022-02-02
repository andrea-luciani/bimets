\name{TSEXTEND}
\alias{TSEXTEND}
\alias{EXTEND}  
 
\title{
Extend Time Series
}
\description{
This function extends the time series definition range by using the directives specified in the \code{EXTMODE} and \code{FACTOR} arguments. 
}
\usage{
TSEXTEND(x = NULL, BACKTO = NULL, UPTO = NULL, EXTMODE = "GROWTH",  
       FACTOR = NA, avoidCompliance = FALSE, ...)
}

\arguments{
 \item{x}{
Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{BACKTO}{
Define the new start of the time series, which must be provided as \code{c(YEAR,PERIOD)}. It is possible to convert a \code{Date()}, or a \code{yearmon()}, or a \code{yearqtr()} to the related \code{c(YEAR,PERIOD)} by using the functions \code{\link[=date2yp]{date2yp}}, \code{\link[=ym2yp]{ym2yp}}, and  \code{\link[=yq2yp]{yq2yp}}.
}
  \item{UPTO}{
Define the new end of the time series, which must be provided as \code{c(YEAR,PERIOD)}. It is possible to convert a \code{Date()}, or a \code{yearmon()}, or a \code{yearqtr()} to the related \code{c(YEAR,PERIOD)} by using the functions \code{\link[=date2yp]{date2yp}}, \code{\link[=ym2yp]{ym2yp}}, and  \code{\link[=yq2yp]{yq2yp}}.
}
  \item{EXTMODE}{ It must be one of the following:\cr
\bold{MISSING}: extend the time series using missings values \code{NA}. \cr
\bold{ZERO}: extend the time series by using 0 (zero) values. \cr
\bold{CONSTANT}: extend the time series by using the closest non-missing observation. \cr
\bold{MEAN4}: extend the time series by using the mean of the closest four non-missing observations. \cr
\bold{LINEAR}: extend the time series by using the same increment of the closest couple of observations. \cr
\bold{QUADRATIC}: extend the time series by using the same quadratic increment of the closest eight observations.\cr 					
\bold{GROWTH}: extends the time series by using the closest growth rate.\cr
\bold{GROWTH4}: extend the time series by using the factor \cr \code{
               r=(mean(x[-1:-4])/mean(x[-5:-8]))**(1/4)}  \cr
\bold{MYCONST}: extend the time series by using the value defined in \code{FACTOR}  \cr
\bold{MYRATE}: extend the time series by using the increment defined in \code{FACTOR}   \cr

}
  \item{FACTOR}{
User-defined value used by some options of the \code{EXTMODE} argument.
}
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
   \item{...}{
Backward compatibility.
}
}
  


\value{
This function returns a BIMETS time series built by extending the input time series.
}

\seealso{
\code{\link[=TSLAG]{TSLAG}}\cr
\code{\link[=TSJOIN]{TSJOIN}}\cr
\code{\link[=TSMERGE]{TSMERGE}}\cr
\code{\link[=TSPROJECT]{TSPROJECT}}\cr
\code{\link[=CUMSUM]{CUMSUM}}\cr
\code{\link[=INDEXNUM]{INDEXNUM}}\cr
\code{\link[=TSTRIM]{TSTRIM}}\cr
}
\examples{
	
	
	n<-10;
	ts1<-TIMESERIES(1:n,START=c(2000,1),FREQ='A')
	ts2<-TSEXTEND(ts1,BACKTO=c(1990,1),UPTO=c(2020,1),EXTMODE='GROWTH4')
	TABIT(ts1,ts2)
		
	xArr<-c(0.5,5.6,4.8,3.8,7.3,9.9,7.8,3.7,8.2,10)
	ts1<-TIMESERIES(xArr,START=c(2000,1),FREQ='A')
	ts2<-TSEXTEND(ts1,BACKTO=c(1990,1),UPTO=c(2020,1),EXTMODE='QUADRATIC')
	TABIT(ts1,ts2)
	
	xArr<-(1:n)
	dateArr<-seq(as.Date('2000/12/31'),by='year',length=n)
	dataF<-data.frame(dateArr,xArr)
	ts1<-TIMESERIES(xArr,START=c(2000,1),FREQ='A')
	ts2<-TSEXTEND(ts1,BACKTO=c(1990,1),UPTO=c(2020,1),EXTMODE='MYRATE',FACTOR=2.5)
	TABIT(ts1,ts2)
}
