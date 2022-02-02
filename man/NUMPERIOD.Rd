\name{NUMPERIOD}
\alias{NUMPERIOD}

\title{
Distance Between Two Year-Periods
}
\description{
This function returns the number of time periods that lie between the provided starting period \code{x1=c(YEAR1,PRD1)} and the provided ending period \code{x2=c(YEAR2,PRD2)}, given a frequency \code{f}. 
}
\usage{
NUMPERIOD(x1, x2, f = NULL)
}

\arguments{
  \item{x1}{
Starting period specified as a numerical array \code{c(YEAR,PRD)}
}
  \item{x2}{
Ending period specified as a numerical array \code{c(YEAR,PRD)}
}
  \item{f}{
Frequency over the year. Must be a positive integer.
}
}

\value{
This function returns an integer of class \code{numeric}.
}


\seealso{
\code{\link[=normalizeYP]{normalizeYP}}\cr
\code{\link[=frequency]{frequency}}\cr
\code{\link[=GETDATE]{GETDATE}}\cr
\code{\link[=LOCS]{LOCS}}\cr
\code{\link[=NAMELIST]{NAMELIST}}\cr
}


\examples{

	# f=5, c(3,4) - c(2,3) = 6 periods
	print(NUMPERIOD(c(2,3),c(3,4),5))
}
