\name{GETDATE}
\alias{GETDATE}


\title{
Retrieve Dates of Time Series
}
\description{
This function returns the dates array of selected observations, in the requested print format. Dates will be provided accordingly to the BIMETS configuration option \code{BIMETS_CONF_DIP} \cr (see \code{\link[=bimetsConf]{BIMETS configuration}} )
}
\usage{
GETDATE(x=NULL, index=NULL, format='\%Y-\%m-\%d', avoidCompliance=FALSE, ...)
}

\arguments{
  \item{x}{
Input time series that must satisfy the compliance control check defined in \code{\link[=is.bimets]{is.bimets}}. 
}
  \item{index}{
Index of observations to be selected. The output dates will be the dates of the selected observations. If \code{index=NULL} this function will retrieve all available dates in the input time series.
}
  \item{format}{
Output print format, provided as a paste of the following codes:\cr
\bold{\%Y}: 4 digits year\cr
\bold{\%y}: 2 digits year\cr
\bold{\%j}: period in the year for daily time series\cr
\bold{\%q}: quarter index, available only if also \%y or \%Y have been requested\cr
\bold{\%m}: 2 digits month\cr
\bold{\%b}: 3 digits month\cr
\bold{\%B}: full month name\cr
\bold{\%d}: 2 digits day\cr
\bold{\%a}: 3 letters weekday\cr
\bold{\%A}: full weekday name\cr


}
\item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
 \item{...}{
Backward compatibility.
}
}
  


\value{
This function returns the dates array of selected observations, in the requested print format. 
}

\seealso{
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=yq2yp]{yq2yp}}\cr
\code{\link[=ym2yp]{ym2yp}}\cr
\code{\link[=date2yp]{date2yp}}\cr
\code{\link[=LOCS]{LOCS}}\cr
\code{\link[=NAMELIST]{NAMELIST}}\cr
\code{\link[=TSLOOK]{TSLOOK}}\cr
\code{\link[=TABIT]{TABIT}}\cr
\code{\link[=ELIMELS]{ELIMELS}}\cr
}


\examples{
  
#day and month names can change depending on locale
Sys.setlocale('LC_ALL','C')
Sys.setlocale('LC_TIME','C')

#work on xts
setBIMETSconf('BIMETS_CONF_CCT','XTS')

#XTS yearly
n=10
xArr=(n:1)
dateArr=seq(as.Date('2000-12-31'),by='year',length=n)
dataF=data.frame(dateArr,xArr)
ts1=xts(dataF[,2],order.by=dataF[,1])
ts1[5]=NA
print(GETDATE(ts1,5)) #...print 2004-12-31
print(GETDATE(ts1,5,'\%A \%d \%b \%Y')) #print... Friday 31 Dec 2004
print(GETDATE(ts1)) #print... "2000-12-31" "2001-12-31" ... "2009-12-31"

#XTS quarterly
n=15
xArr=(n:0)
dateArr=as.yearqtr('2000 Q1')+0:n/4
dataF=data.frame(dateArr,xArr)
ts1=xts(dataF[,2],order.by=dataF[,1])	
print(GETDATE(ts1,9,'\%b \%Y')) #print...Mar 2002	

#XTS monthly
#set configuration BIMETS_CONF_DIP to FIRST
setBIMETSconf('BIMETS_CONF_DIP','FIRST')
n=15
xArr=(n:0)
dateArr=as.yearmon('Jan 2000')+0:n/12
dataF=data.frame(dateArr,xArr)
ts1=xts(dataF[,2],order.by=dataF[,1])	
print(GETDATE(ts1,9,'\%b \%Y')) #print...Sep 2000	

#set configuration BIMETS_CONF_DIP to LAST
setBIMETSconf('BIMETS_CONF_DIP','LAST')

#2000 is bissextile... 
print(GETDATE(ts1,2)) #print... 2000-02-29

#quarter...
print(GETDATE(ts1,5,'\%Y Q\%q')) #print... 2000 Q2

#restore default
setBIMETSconf('BIMETS_CONF_CCT','TS')
				
}






