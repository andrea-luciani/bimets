\name{idxOver}
\alias{BIMETS indexing}
\alias{idxOver}

\title{
BIMETS Time Series Indexing
}
\description{
Bimets package extends the way users can access and modify time series data.
\cr \cr
\bold{SELECTING BY YEAR-PERIOD}: Users can select observations by providing the related year and period . Selection and modification of data require the double square bracket syntax, \cr e.g. \code{ts[[year,period]] <- value}, given \code{year} and \code{period} as positive integers. \cr Users can also assign an array of values to the input time series, starting from the \code{[[year,period]]} provided, \cr i.e. \code{ts[[year,period]] <- c(value1,value2,...,valueN)}: in this case the input time series will be eventually extended in order to sequentially insert all values of the provided array \code{c(value1,value2,...,valueN)} starting from the provided \code{[[year,period]]} (see example). \cr \cr
Users can select observations also by using a single bi-dimensional argument, e.g. \code{ts[[start]]}, and by using two bi-dimensional arguments \code{ts[[start,end]]}, given \code{start <- c(year1,period1); end <- c(year2,period2)}: in this case all observation in \cr \code{TSRANGE=c(year1,period1,year2,period2)} will be returned. \cr Assignments by using two-dimensional arguments are also allowed, e.g. \code{ts[[start]] <- value; ts[[start,end]] <- value; ts[[start,end]] <- c(value1,...,valueN)} (see example)\cr \cr
\bold{SELECTING BY DATE}: users can select a single observation by date by using the syntax \code{ts['Date']} and  multiple observations by using \code{ts['StartDate/EndDate']} or \cr \code{ts['StartDate'+(0:n)/f]}, given \code{f} as frequency, and \code{n} as observations count. \cr Data modification follows the same syntax: \cr \code{ts['Date'] <- value, ts['Date/Date'] <- c(value1,value2,...,valueN)}, etc. Users can also provide the string representing only the year of selection, or the year and the month of selection. For quarterly and monthly time series it is possible to select dates by using instances of class \code{yearmon()} and \code{yearqtr()} (See example).\cr \cr
\bold{SELECTING BY INDICES}: (core R) Users can select observations by simply providing the array of requested indices, \cr e.g. \code{ts[c(idx1,idx2,...,idxN)]} while reading and \cr \code{ts[c(idx1,idx2,...,idxN)] <- c(value1,value2,...,valueN)} while modifying time series data.
}


\value{No return value.}


\seealso{
\code{\link[=GETDATE]{GETDATE}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
\code{\link[=date2yp]{date2yp}}\cr
\code{\link[=yq2yp]{yq2yp}}\cr
\code{\link[=ym2yp]{ym2yp}}\cr
\code{\link[=as.bimets]{as.bimets}}\cr
\code{\link[=is.bimets]{is.bimets}}\cr
\code{\link[=LOCS]{LOCS}}\cr
\code{\link[=NAMELIST]{NAMELIST}}\cr
\code{\link[=TABIT]{TABIT}}\cr
\code{\link[=ELIMELS]{ELIMELS}}\cr
\code{\link[=BIMETS2CSV]{BIMETS2CSV}}\cr
\code{\link[=CSV2BIMETS]{CSV2BIMETS}}\cr
}

\examples{

  #day and month names can change depending on locale
  Sys.setlocale('LC_ALL','C')
  Sys.setlocale('LC_TIME','C')
	
  #monthly
  #-------------------------------
  print('MONTHLY GET by DATE')
  
  n <- 25
  
  #create ts
  ts1 <- TIMESERIES((0:n),START=c(2000,1),FREQ=12) 
  
  print(ts1['2001-01']) #get Jan 2001
  print(ts1[as.yearmon('Jan 2001')]) #get Jan 2001
  print(ts1['2000-09/2001-01']) #get data from Sep 2000 to Jan 2001
  print(ts1['2000-09/']) #get data from Sep 2000 
  print(ts1['/2001-01']) #get data till Jan 2001
  print(ts1['2001'])  #gat all data in year 2001
  
  #get 3 consecutive months starting from Jan 2001
  print(ts1[as.yearmon('Jan 2001')+ 0:2/12]) 
  
  
  print(ts1[c(2,4,5)]) #get observation number 2,4 and 5
  
  print('MONTHLY GET by YEAR-PERIOD')

  print(ts1[[2000,5]]) #get year 2000 period 5
  
  #get year 2010 period 1 (out of range)
  tryCatch({print(ts1[[2010,1]])},error=function(e){cat(e$message)})
  
  print(ts1[[2002,2]]) #get year 2002 period 2
  
  start <- c(2001,2)
  end <- c(2001,4)
  
  print(ts1[[start]]) #get year 2001 period 2
  print(ts1[[start,end]]) #get from year-period 2001-2 to 2001-4
  
  print('MONTHLY SET by DATE')

  ts1['2000-08'] <- 9.9 #assign to Aug 2000
  ts1[as.yearmon('Feb 2001')] <- 8.8 #assign to Feb 2001
  
  #assign 8.8 on Feb 2001 and give warning
  ts1[as.yearmon('Feb 2001')]=c(8.8,7.7) 
  
  #assign same value to all observation in range Sep 2000 - Jan 2001
  ts1['2000-09/2001-01'] <- 11.11
  
  #assign repeatedly the two values to each observation starting from Sep 2001 
  ts1['2001-09/'] <- c(1.1,2.2) 
  print(ts1)
  
  print('MONTHLY SET by YEAR-PERIOD')

  ts1[[2000,5]] <- NA #set year 2000 period 5
  
  #assign an array starting from year 2002 period 2 (extend time series)
  ts1[[2002,2]] <- c(-1,-2,-3,-4,-5) 
  TABIT(ts1)
  
  #set by bi-dimensional arrays
  ts1 <- TIMESERIES((0:n),START=c(2000,1),FREQ=12) 
  ts1[[c(2000,12)]] <- NA
  start <- c(2001,2)
  end <- c(2001,4)
  ts1[[start,end]] <- 0 #assign single value
  TABIT(ts1)
  ts1[[start,end]] <- c(-2,-4,-6) #assign multiple values
  TABIT(ts1)
  
  
  #quarterly
  #-------------------------------
  print('QUARTERLY GET by DATE')
  
  #create ts
  ts1 <- TSERIES((0:n),START=c(2000,1),FREQ=4)
    
  print(ts1[as.yearqtr('2001 Q1')]) #get 2001 Q1
  print(ts1['2001']) #get all data in year 2001
  
  #get 4 consecutive quarters starting from 2002 Q2
  print(ts1[as.yearqtr('2002 Q2')+ 0:3/4])  
  
  print(ts1['2003/']) #gat all data from 2003 Q1 
  
  print('QUARTERLY GET by YEAR-PERIOD')
  
  print(ts1[[2002,4]]) #get year 2002 period 4
  
  start <- c(2001,2)
  end <- c(2001,4)
  
  print(ts1[[start]]) #get year 2001 period 2
  print(ts1[[start,end]]) #get from year-period 2001-2 to 2001-4
      
  print('QUARTERLY SET by DATE')
  
  ts1[as.yearqtr('2001 Q1')] <- 7.7 #assign to 2001 Q1
  ts1['2002'] <- NA #assign to all observations of 2002
  
  #assign to 3 quaters starting from 2003 Q2
  ts1[as.yearqtr('2003 Q2')+ 0:2/4] <- 0 
  
  ts1['2004/'] <- -1 #assign to all observations starting from 2004
  TABIT(ts1)
  
  print('QUARTERLY SET by YEAR-PERIOD')
  
  ts1[[2005,4]] <- c(1,2,3) #assign array starting from year 2005 period 4
  TABIT(ts1)
  
  #set by bi-dimensional arrays
  ts1 <- TIMESERIES((0:n),START=c(2000,1),FREQ=4) 
  ts1[[c(2000,12)]] <- NA
  start <- c(2001,2)
  end <- c(2001,4)
  ts1[[start,end]] <- 0 #assign single value
  TABIT(ts1)
  ts1[[start,end]] <- c(-2,-4,-6) #assign multiple values
  TABIT(ts1)
  
  #yearly 
  #-------------------------------
  print('YEARLY GET by DATE')
  
  #create ts
  ts1 <- TSERIES((1:n),START=c(2000,1),FREQ=1)
  
  print(ts1['2002-12-31']) #get 2002 data
  print(ts1['2002'])  #get 2002 data
  print(ts1['2000/2004'])  #get data from 2000 to 2004
  print(ts1['2005/']) #get data starting from 2005
  
  print('YEARLY GET by YEAR-PERIOD')
  
  print(ts1[[2005,1]]) #get year 2005
  #get year 2032 (out of range)
  tryCatch({print(ts1[[2032,1]])},error=function(e){cat(e$message)})
  
  start <- c(2001,1)
  end <- c(2002,1)
  
  print(ts1[[start]]) #get year 2001 
  print(ts1[[start,end]]) #get from year 2001 to 2002
  
  
  print('YEARLY SET by DATE')
  
  ts1['2004'] <- NA #assign to 2004
  ts1['2007/'] <- 0.0 #assign starting from 2007
  ts1['2000/2002'] <- -1 #assign in range 2000/2002
  TABIT(ts1)
 
  print('YEARLY SET by YEAR-PERIOD')
 
  ts1[[2005,1]] <- NA #assign to 2005
  ts1[[2014,1]] <- c(-1,-2,-3) #assign array starting from 2014 (extend series)
  TABIT(ts1)
  
  #set by bi-dimensional arrays
  ts1 <- TIMESERIES((0:n),START=c(2000,1),FREQ=1) 
  ts1[[c(2000,12)]] <- NA
  start <- c(2001,1)
  end <- c(2002,1)
  ts1[[start,end]] <- 0 #assign single value
  TABIT(ts1)
  ts1[[start,end]] <- c(-2,-4) #assign multiple values
  TABIT(ts1)
  
  #daily 
  #-------------------------------
  print('DAILY GET by DATE')
  
  #create ts
  ts1 <- TSERIES((1:n),START=c(2000,1),FREQ='D')
  
  print(ts1['2000-01-12']) #get Jan 12, 2000 data
  
  print('DAILY GET by YEAR-PERIOD')
  
  print(ts1[[2000,14]]) #get year 2000 period 14
  #get year 2032 (out of range)
  tryCatch({print(ts1[[2032,1]])},error=function(e){cat(e$message)})
  
  print('DAILY SET by DATE')
  
  ts1['2000-01-15'] <- NA #assign to Jan 15, 2000
  TABIT(ts1)
  
  print('DAILY SET by YEAR-PERIOD')
  
  ts1[[2000,3]] <- NA #assign to Jan 3, 2000
  
  #assign array starting from 2000 period 35 (extend series)
  ts1[[2000,35]] <- c(-1,-2,-3)
  TABIT(ts1)  
   

}
