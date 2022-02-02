\name{normalizeYP}
\alias{normalizeYP}

\title{
Normalize Year-Period Array
}
\description{
This function normalizes a numerical array \code{c(YEAR,PERIOD)}, given a frequency \code{f} and \code{PERIOD>=f}.\cr e.g. \code{normalizeYP(c(2000,15),12) = c(2001,3)}
}
\usage{
normalizeYP(x = NULL, f = NULL)
}

\arguments{
  \item{x}{
Input numerical array \code{c(YEAR,PERIOD)}
}
  \item{f}{
Frequency of normalization. Must be a positive integer.
}
}

\value{
This function returns a numerical array \code{c(YEAR,PERIOD)}
}

\seealso{
\code{\link[=NUMPERIOD]{NUMPERIOD}}\cr
\code{\link[=frequency]{frequency}}\cr
}
\examples{
	
	#c(2,13) and frequency=4 => c(5,1)
	print(normalizeYP(c(2,13),4))
	
}
