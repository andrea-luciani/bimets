\name{frequency}
\alias{frequency}
\alias{frequency.xts}

\title{
Frequency of a Time Series
}
\description{
This function returns the frequency of a time series. In the case of a sparse \code{xts()} time series, and in other cases, the R functions \code{xts::periodicity()} and \code{frequency()} do not return BIMETS compliant values. We suggest to use this function when a BIMETS time series frequency is requested.
}
\usage{
\method{frequency}{xts}(x,...)
}

\arguments{
  \item{x}{
Input time series.
}

  \item{...}{
Backward compatibility.
}

}

\value{
This function returns the integer value stored in the attribute \code{.bimetsFreq} of the input time series, if any. Otherwise the frequency will be calculated by using the shortest time difference between two observations, while accounting for daysaving and bissextile years. 
}

\seealso{
\code{\link[=normalizeYP]{normalizeYP}}\cr
\code{\link[=NUMPERIOD]{NUMPERIOD}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
}

\examples{
		
	#build a sparse xts()
	xArr=rnorm(13)
	dateArr=seq(as.Date('2000/01/01'),by='6 months',length=10)
	dateArr2=seq(as.Date('2010/01/01'),by='3 months',length=3)
	
	#strange array of dates here below...
	dateArr3=c(dateArr,dateArr2)
	dataF=data.frame(dateArr3,xArr)
	xt=xts(dataF[,2],order.by=dataF[,1])
	
	#get bimets calculated frequency
	cat(frequency(xt)) #print 4... without bimets R returns 1
	
	#...legacy periodicity()
	periodicity(xt)
	
	
}
