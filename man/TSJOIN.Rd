\name{TSJOIN}
\alias{TSJOIN}


\title{
Join Time Series
}
\description{
This function returns the join of the two input time series. If the first time series overlaps the second time series, output data is taken from the first time series up to the second time series's 
starting date, the remainder of the data being taken from the second time series. \cr A different joining period can be specified by using the \code{JPRD} argument.\cr The two time series must have the same frequency.
}
\usage{
TSJOIN(x = NULL, y = NULL, JPRD = NULL, ALLOWGAP = FALSE, 
       WARN = FALSE, avoidCompliance = FALSE, ...)
}

\arguments{
  \item{x}{
First input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{y}{
Second input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{JPRD}{
This argument defines a joining period other than the starting period of the second time series.
It must be defined as \code{JPRD=c(YEAR,PERIOD)} and must lie in the time range of the second time series. Users can convert a \code{Date()}, or a \code{yearmon()} or a \code{yearqtr()} to a \code{c(YEAR,PERIOD)} by using \code{\link[=date2yp]{date2yp}}, \code{\link[=ym2yp]{ym2yp}}, \code{\link[=yq2yp]{yq2yp}}.
}
  \item{ALLOWGAP}{
if \code{TRUE}, the possible gap between the two time series is filled with missing values \code{NA}, otherwise, if the two time ranges do not overlap, an error will be thrown.
}
  \item{WARN}{
Print a warning message if the two time series do not overlap or if the first time series starts after the \code{JPRD}. The warning is shown only if \code{ALLOWGAP=TRUE}.
}
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}}
   \item{...}{
Backward compatibility.
}
}
  


\value{
This function returns a BIMETS time series that is built by joining the two input time series.
}

\seealso{
\code{\link[=TSLAG]{TSLAG}}\cr
\code{\link[=TSEXTEND]{TSEXTEND}}\cr
\code{\link[=TSMERGE]{TSMERGE}}\cr
\code{\link[=TSPROJECT]{TSPROJECT}}\cr
}
\examples{
	
  #day and month names can change depending on locale
  Sys.setlocale('LC_ALL','C')
  Sys.setlocale('LC_TIME','C')
  
  #TS
  ts1<-TSERIES((1:10),START=c(1985,1),FREQ=1)
  ts2<-TSERIES((1:10),START=c(2000,1),FREQ=1)
  TABIT(ts1,ts2,TSJOIN(ts1,ts2,ALLOWGAP=TRUE))
  
  #XTS
  setBIMETSconf('BIMETS_CONF_CCT','XTS')
  n<-10
  xArr<-(0:n)
  dateArr<-as.yearqtr('1997 Q1')+ 0:n/4
  dataF<-data.frame(dateArr,xArr)
  ts1<-xts(dataF[,2],order.by=dataF[,1])
  dateArr<-as.yearqtr('2000 Q1')+ 0:n/4
  dataF<-data.frame(dateArr,xArr)
  ts2<-xts(dataF[,2],order.by=dataF[,1])
  TABIT(ts1,ts2,TSJOIN(ts1,ts2,ALLOWGAP=TRUE,JPRD=yq2yp(as.yearqtr("2001 Q3"))))
  
  #restore default
  setBIMETSconf('BIMETS_CONF_CCT','TS')
	
}
