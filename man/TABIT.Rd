\name{TABIT}
\alias{TABIT}



\title{
Print Time Series Data
}
\description{
This function prints, in a human-readable format, the content of a list of time series. Time series can be subsetted with the projection argument \code{TSRANGE}.
}
\usage{
TABIT(..., 
      TSRANGE=NULL, 
      digits=getOption('digits'),
      avoidCompliance=FALSE)
}

\arguments{
\item{...}{ 

Input list of time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}.
}
  \item{TSRANGE}{
The optional date range of data presentation. \code{TSRANGE} must be specified as a numerical array composed by starting year, staring period, ending year and ending period of projection, \cr i.e. \code{TSRANGE=c(START_YEAR,START_PERIOD,END_YEAR,END_PERIOD)}. 
}
  \item{digits}{
  Controls the number of digits to print out. Valid values are 1 to 22 with a default of 7.
  }
\item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
  
}

\value{
This function prints out time series data and returns a \code{NULL} value.
}

\seealso{
\code{\link[=TSPROJECT]{TSPROJECT}}\cr
\code{\link[=TSMERGE]{MOVAVG}}\cr
\code{\link[=TSDELTA]{TSDELTA}}\cr
\code{\link[=TSLAG]{TSLAG}}\cr
\code{\link[=TSPROJECT]{TSPROJECT}}\cr
\code{\link[=TSEXTEND]{TSEXTEND}}\cr
\code{\link[=TSLEAD]{TSLEAD}}\cr
}



\examples{

#create monthly series
ts1<-TSERIES(INTS(1,15),START=c(2000,1),FREQ=12)
ts2<-TSERIES(INTS(1,15),START=c(2001,1),FREQ=12)
ts3<-TSERIES(rnorm(15),START=c(2002,1),FREQ=12)
ts4<-TSERIES(rep(NA,15),START=c(2001,4),FREQ=12)

TABIT(ts1,ts2,ts3,ts4)
# print...
#
#      Date, Prd., ts1            , ts2            , ts3            , ts4            
#
#  Jan 2000, 1   ,  1             ,                ,                ,                
#  Feb 2000, 2   ,  2             ,                ,                ,                
#  Mar 2000, 3   ,  3             ,                ,                ,                
#  ...
#  Dec 2000, 12  ,  12            ,                ,                ,                
#  Jan 2001, 1   ,  13            ,  1             ,                ,                
#  Feb 2001, 2   ,  14            ,  2             ,                ,                
#  ...
#  Dec 2001, 12  ,                ,  12            ,                ,  NA            
#  Jan 2002, 1   ,                ,  13            , -1.419782      ,  NA            
#  Feb 2002, 2   ,                ,  14            , -1.070188      ,  NA            
#  Mar 2002, 3   ,                ,  15            ,  0.889571      ,  NA            
#  Apr 2002, 4   ,                ,                ,  0.9583392     ,  NA            
#  ...
#  Feb 2003, 2   ,                ,                , -0.3444237     ,                
#  Mar 2003, 3   ,                ,                , -0.3073225     ,                


#create quarterly series, set TSRANGE then print with 3 digits

ts1<-TSERIES(INTS(1,15),START=c(2000,1),FREQ=4)
ts2<-TSERIES(INTS(1,15),START=c(2001,1),FREQ=4)
ts3<-TSERIES(rnorm(15),START=c(2002,1),FREQ=4)
ts4<-TSERIES(rep(NA,15),START=c(2001,4),FREQ=4)

TABIT(ts1,ts2,ts3,ts4,TSRANGE=c(1991,3,2003,2),digits=3)

#print...
#	
#      Date, Prd., ts1        , ts2        , ts3        , ts4        
#
#   2000 Q1, 1   ,  1         ,            ,            ,            
#   2000 Q2, 2   ,  2         ,            ,            ,            
#   2000 Q3, 3   ,  3         ,            ,            ,            
#   2000 Q4, 4   ,  4         ,            ,            ,            
#   2001 Q1, 1   ,  5         ,  1         ,            ,            
#   2001 Q2, 2   ,  6         ,  2         ,            ,            
#   2001 Q3, 3   ,  7         ,  3         ,            ,            
#   2001 Q4, 4   ,  8         ,  4         ,            ,  NA        
#   2002 Q1, 1   ,  9         ,  5         ,  0.729     ,  NA        
#   2002 Q2, 2   ,  10        ,  6         ,  0.923     ,  NA        
#   2002 Q3, 3   ,  11        ,  7         , -0.81      ,  NA        
#   2002 Q4, 4   ,  12        ,  8         , -0.0748    ,  NA        
#   2003 Q1, 1   ,  13        ,  9         ,  0.248     ,  NA        
#   2003 Q2, 2   ,  14        ,  10        , -0.347     ,  NA   


#create daily series and set TSRANGE

ts1<-TSERIES(INTS(1,25),START=c(2000,1),FREQ=366)
ts2<-TSERIES(INTS(1,25),START=c(2000,10),FREQ=366)
ts3<-TSERIES(rnorm(25),START=c(2000,20),FREQ=366)
ts4<-TSERIES(rep(NA,25),START=c(2000,30),FREQ=366)

TABIT(ts1,ts2,ts3,ts4,TSRANGE=c(2000,5,2000,35))

#...print data

}
