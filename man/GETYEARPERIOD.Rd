\name{GETYEARPERIOD}
\alias{GETYEARPERIOD}
\alias{TSDATES}

\title{
Get Time Series Year-Period
}
\description{
This function returns a two-element list (or a two-columns matrix in the case of \code{JOIN=TRUE}) built with of the years and the periods of the input time series observations. Users can provide the output list names.
}
\usage{
GETYEARPERIOD(x=NULL, YEARS='YEAR', PERIODS='PRD', JOIN=FALSE, avoidCompliance=FALSE, ...)
TSDATES(x=NULL, YEARS='YEAR', PERIODS='PRD',  JOIN=FALSE, avoidCompliance=FALSE, ...)
}

\arguments{
  \item{x}{
Input time series, that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}
}
  \item{YEARS}{
Argument of type string that will be the output list name for the array of observation years.
}
  \item{PERIODS}{
Argument of type string that will be the output list name for the array of observation periods.
}
\item{JOIN}{If \code{TRUE}, the output will be a matrix having each row built with the year and the period of the related observation.}
\item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
   \item{...}{
Backward compatibility.
}
}

\value{
This function returns an object of class \code{list()}. If \code{JOIN=TRUE}, this function returns a matrix.
}

\seealso{
\code{\link[=NOELS]{NOELS}}\cr
\code{\link[=TSERIES]{TSERIES}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=TSLEAD]{TSLEAD}}\cr
\code{\link[=TSINFO]{TSINFO}}\cr
\code{\link[=TSLOOK]{TSLOOK}}\cr
\code{\link[=TABIT]{TABIT}}\cr
\code{\link[=ELIMELS]{ELIMELS}}\cr
}


\examples{
  
  #create quarterly ts
  n<-20
  ts1<-TSERIES((n:1),START=c(2000,1),FREQ=4)
  myYP<-GETYEARPERIOD(ts1)
  print(myYP$YEAR) #print 2000 2000 ... 
  print(myYP$PRD) #print 1 2 3 4 1 2 ...
  
  #create monthly ts
  ts1<-TSERIES((n:1),START=c(2000,1),FREQ='M')
  myYP<-GETYEARPERIOD(ts1)
  print(myYP$YEAR) #print 2000 2000 ... 
  print(myYP$PRD) #print 1 2 3 4 5 6 7 ...
  
  #create yearly ts
  ts1<-TSERIES((1:n),START=c(2000,1),FREQ=1)
  myYP<-GETYEARPERIOD(ts1,YEARS='YEARSS', PERIODS='PRDSS')
  print(myYP$YEARSS) #print 2000 2001 2002 ... 
  print(myYP$PRDSS) #print 1 1 1 1.....
  
  #JOIN=TRUE
  ts1<-TSERIES((n:1),START=c(2000,1),FREQ='M')
  myYP<-GETYEARPERIOD(ts1,JOIN=TRUE)
  print(myYP) #print 2000 2000 ... 
  #      [,1] [,2]
  #[1,] 2000    1
  #[2,] 2000    2
  #[3,] 2000    3
  #...	
} 
