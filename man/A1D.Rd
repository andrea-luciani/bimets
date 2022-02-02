\name{A1D}
\alias{A1D}


\title{
A1D
}
\description{
This function returns the array built with input argment values. Input can be time series, numerical arrays or strings.
}
\usage{
A1D(..., length = NULL, avoidCompliance = FALSE)
}

\arguments{
  \item{...}{
Input argument list.
This function accepts strings, time series, objects of class \code{numeric} or \code{logical}. Input time series must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}
}
  \item{length}{
Length of output array, that must be greater than the sum of each input argument size: if the length of the output array is provided, than the output array will be eventually filled with zeros. 
}
  \item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
  
}
}
\value{
This function returns an array of the same class of the input.
}

\seealso{
\code{\link[=NOELS]{NOELS}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=TSDATES]{TSDATES}}\cr
\code{\link[=LOCS]{LOCS}}\cr
\code{\link[=NAMELIST]{NAMELIST}}\cr
\code{\link[=INTS]{INTS}}\cr
\code{\link[=TSINFO]{TSINFO}}\cr
\code{\link[=TSLOOK]{TSLOOK}}\cr
\code{\link[=TABIT]{TABIT}}\cr
\code{\link[=ELIMELS]{ELIMELS}}\cr
\code{\link[=INDEXNUM]{INDEXNUM}}\cr
}


\examples{
	
	n=10;
	#create ts
	ts1=TSERIES(rnorm(n),START=c(2000,1),FREQ=1)

	#create A1D() array with scalars, ts, and NA
	out_a1d=A1D(length=25, ts1, 1, -8.9, NA)
	print(out_a1d)
	
	#same example no length specified
	out_a1d=A1D(ts1, 1, -8.9, NA)
	print(out_a1d)
	
	#strings example
	out_a1d=A1D(length=5,'aa','bb','ccc')
	print(out_a1d)
}

