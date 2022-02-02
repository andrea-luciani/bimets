\name{yq2yp}
\alias{yq2yp}

\title{
yearqtr to Year-Period Conversion
}
\description{
This function transforms an input variable of class \code{yearqtr()} into an equivalent two-dimensional numerical array of type \code{c(YEAR,PERIOD)}.
}
\usage{
yq2yp(x = NULL)
}

\arguments{
  \item{x}{
Input of class  \code{yearmon()}
}

}

\value{
This function returns a two-dimensional numerical array of type \code{c(YEAR,PERIOD)}.
}


\seealso{
\code{\link[=date2yp]{date2yp}}\cr
\code{\link[=ym2yp]{ym2yp}}\cr
\code{\link[=GETDATE]{GETDATE}}\cr
}


\examples{
	
  #day and month names can change depending on locale
  Sys.setlocale('LC_ALL','C')
  Sys.setlocale('LC_TIME','C')
  
  print(yq2yp(as.yearqtr('2001 Q3'))); #print c(2001,3)
  print(yq2yp(c(as.yearqtr('2000 Q2'),as.yearqtr('1987 Q4'),as.yearqtr('2003 Q1'))))
}
