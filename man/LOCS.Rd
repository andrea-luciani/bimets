\name{LOCS}
\alias{LOCS}


\title{
Select Time Series Indices
}
\description{
This function returns the indices of the input \code{TRUE} elements. The input can be either an array or a time series. The result is usually used as a structured index to produce a new array.

}
\usage{
LOCS(x=NULL, options='ALL', ...)
}

\arguments{
  \item{x}{
  
This function accepts as input a boolean array or a boolean time series, often as the result of a logic comparison between an expression and a numerical array or a numerical time series: \cr e.g. \code{LOCS(c(a,b,c,...)>=k); LOCS(ts==j);} \cr
                 \code{LOCS(ts<expr); LOCS(is.na(ts)); etc...}
}
  \item{options}{A selection option can refine the result:\cr
  \bold{ALL}: (default) all the \code{TRUE} indices will be returned in the output.\cr
  \bold{UNIQUE}: return the index of the unique \code{TRUE} result; if there are multiple \code{TRUE} results then an error will be thrown.\cr
  \bold{FIRST}: return the first \code{TRUE} result.\cr
  \bold{LAST}: return the last \code{TRUE} result.\cr
}

  \item{...}{
Backward compatibility.
}
 
}

\value{
This function returns a numerical array built with the indices of the values that are \code{TRUE} in the input boolean array or in the input boolean time series.
}

\seealso{
\code{\link[=NOELS]{NOELS}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=TSERIES]{TSERIES}}\cr
\code{\link[=GETYEARPERIOD]{GETYEARPERIOD}}\cr
\code{\link[=NOELS]{NOELS}}\cr
\code{\link[=NAMELIST]{NAMELIST}}\cr
\code{\link[=INTS]{INTS}}\cr
\code{\link[=TSINFO]{TSINFO}}\cr
\code{\link[=TABIT]{TABIT}}\cr
\code{\link[=ELIMELS]{ELIMELS}}\cr
}


\examples{

	#create ts
	n<-10
	ts1<-TSERIES((1:n),START=c(2000,1),FREQ=1)
	print(LOCS(ts1>7,options='FIRST')) #print 8
	
	#generate error: print LOCS(): input has more than one TRUE element.
	tryCatch({print(LOCS(ts1>=3,options='UNIQUE'));},error=function(e){print(e$message);})
	
	print(LOCS(is.na(c(1,2,NA,4,5,6,7,NA,NA)))) #print c(3,8,9)
}
