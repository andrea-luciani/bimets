\name{NOELS}
\alias{NOELS}


\title{
Count Elements
}
\description{
This function returns a numerical array built with the length of each input argument. Input arguments can be numerical or time series. Input arguments can also be string variables. 
}
\usage{
NOELS(...)
}

\arguments{
  \item{...}{
List of input arguments. This function accepts input of class \code{numerical}, \code{character}, or BIMETS time series. \code{character} arguments are exclusive: if an argument is of class \code{character}, all other arguments must be of class \code{character}.
}

  
}

\value{
This function returns an array of class \code{c()}.
}

\seealso{
\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=TSDATES]{TSDATES}}\cr
\code{\link[=LOCS]{LOCS}}\cr
\code{\link[=NAMELIST]{NAMELIST}}\cr
\code{\link[=INTS]{INTS}}\cr
\code{\link[=TSINFO]{TSINFO}}\cr
\code{\link[=TSLOOK]{TSLOOK}}\cr
\code{\link[=TABIT]{TABIT}}\cr
}


\examples{

	out_NOELS=NOELS(c(1,2,3,4),c(5,6,7))
	print(out_NOELS) #print c(4,3)
	
	out_NOELS=NOELS(TSERIES(c(1,2,3,4),START=c(2000,1),FREQ=1),c(5,6,7))
	print(out_NOELS) #print c(4,3)
	
	out_NOELS=NOELS('aaa','bb')
	print(out_NOELS) #print c(3,2)
	
}
