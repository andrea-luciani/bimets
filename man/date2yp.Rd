\name{date2yp}
\alias{date2yp}

\title{
Date to Year-Period Conversion
}
\description{
This function converts an object of class \code{Date()} to an array \code{c(YEAR,PERIOD)}, where \code{YEAR} and \code{PERIOD} are the year-period the input \code{Date()} belongs to, given an input frequency.
}
\usage{
date2yp(x = NULL, f = 1)
}

\arguments{
  \item{x}{
Input of class \code{Date()}.  
}
  \item{f}{
Positive integer. Valid values are: 1, 2, 3, 4, 12, 24, 36, 53, 366
}
}

\value{
This function returns a two-dimensional array \code{c(YEAR,PERIOD)}.
}


\seealso{
\code{\link[=yq2yp]{yq2yp}}\cr
\code{\link[=ym2yp]{ym2yp}}\cr
\code{\link[=GETDATE]{GETDATE}}\cr
\code{\link[=INTS]{INTS}}\cr
\code{\link[=TABIT]{TABIT}}\cr
}
\examples{	
print(date2yp(as.Date('2001/06/30'),2)) #2001,1
print(date2yp(as.Date('2002/03/23'),1)) #2002,1
print(date2yp(as.Date('2003/07/01'),366)) #2003,182
print(date2yp(as.Date('2004/09/13'),2)) #2004,2
print(date2yp(as.Date('2004/01/13'),12)) #2004,1	
print(date2yp(as.Date('2004/07/24'),4)) #2004,3
print(date2yp(c(as.Date('1900-01-01'),as.Date('1944-12-01'),
as.Date('1964-06-12'),as.Date('1923-03-01'),
as.Date('1943-12-05')),f=366)) #...
}
