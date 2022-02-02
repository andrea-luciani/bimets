\name{ym2yp}
\alias{ym2yp}

\title{
yearmon to Year-Period Conversion
}
\description{
This function transforms an input variable of class \code{yearmon()} into an equivalent two-dimensional numerical array of type \code{c(YEAR,PERIOD)}.
}

\usage{
ym2yp(x = NULL)
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
\code{\link[=yq2yp]{yq2yp}}\cr
\code{\link[=GETDATE]{GETDATE}}\cr
}


\examples{
  
  #day and month names can change depending on locale
  Sys.setlocale('LC_ALL','C')
  Sys.setlocale('LC_TIME','C')
  
  print(ym2yp(as.yearmon("Dec 2013"))); #print c(2013,12)
  print(ym2yp(c(as.yearmon('Jan 2000'),as.yearmon('Dec 1987'),
        as.yearmon('Jan 2003'),as.yearmon('Mar 2012'))))
}
