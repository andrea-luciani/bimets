\name{CSV2BIMETS}
\alias{CSV2BIMETS}


\title{
CSV2BIMETS
}
\description{
This function transforms an input CSV (Comma-Separated Values) file into a \code{bimets}-compliant time series list (according to the specifications defined in the \code{\link[=is.bimets]{is.bimets}} help page). 

\bold{Heterogeneous frequency}

If time series to be imported have different frequencies, the CSV file must consist of as many pairs of columns as time series to be imported. 

For each time series, there must be a column for dates and a subsequent column for observation values. 

The column header of the date column must contain the description of the related time series.

If users want to force a specific frequency for the output time series, the header of the observation column might contain the desired frequency value in the format \code{FREQ_f, f=1,2,3,4,6,12,53,366, etc.} (see the \code{freqHeaderPrefix} argument); if the CSV does not contain a specific frequency in the observation column header, the frequency will be automatically calculated (see examples). 

Any observation that can't be converted into a numerical value is interpreted as a missing value \code{NA}. 

\bold{Homogeneous frequency}

If all input time series have the same frequency, the date column can be unique and must correspond to the first column in the CSV file (see the \code{mergedList} argument); in this case, users can provide a desired output frequency in the date column header (in the same format as described above), otherwise the frequency will be automatically calculated (see examples). Following columns must contain observation values having the time series description in the column header (see examples).

Any observation that can't be converted into a numerical value is interpreted as a missing value \code{NA}. 

\bold{Locale configuration}

Users can change current locale (e.g., month names) by using the base \code{R} command \code{\link[=locales]{locales}}, e.g.:

\code{Sys.setlocale('LC_TIME','en_US.UTF8')}, 

\code{Sys.setlocale('LC_TIME','fr_FR.UTF8')}, 

\code{Sys.setlocale('LC_TIME','it_IT.UTF8')}, 

... etc  (see examples).

\bold{Metadata specification}

The first line of text in the CSV file might contain metadata information about the cell separator used in the CSV file, e.g. \code{"sep=,"}

This metadata information is often inserted to allow the CSV file to be directly opened in MS Excel(R). The \code{CSV2BIMETS} function is compatible with this metadata schema. In this case, the \code{cellSeparator} argument must be defined accordingly to the specified infile separator.

}
\usage{
CSV2BIMETS( 
  file=NULL, 
  cellSeparator=',', 
  decimalSeparator='.', 
  mergedList=FALSE, 
  dateFormat='\%Y/\%m/\%d', 
  skipLines=NULL, 
  freqHeaderPrefix='FREQ_',
  ... )

}

\arguments{
  
\item{file}{ 
CSV file path.
}
\item{cellSeparator}{
Delimiter that separates individual data fields (i.e., cells) in the CSV file. Default to \code{","}
} 
\item{decimalSeparator}{
The character in the CSV file that separates the integer part of a number from its fractional part.  Default to \code{"."}
} 
\item{skipLines}{
An integer number that represents the count of the CSV lines of text to be skipped when importing data, e.g. see the \code{title} arguments in the \code{\link[=BIMETS2CSV]{BIMETS2CSV}} function.
} 

\item{mergedList}{
It is possible to import time series from two different types of CSV files. 

If \code{mergedList=FALSE}, the input CSV file must contain, for each time series to be imported, a couple of adjacent columns, i.e, a date column and an observation value column. Thus, it is possible to import time series that have different frequencies. The date column header must contain the time series description. On the other hand, users might indicate, in the observation header, a specific frequency for the related output time series, by using the \code{freqHeaderPrefix} argument, e.g., \code{FREQ_12}, \code{FREQ_4}, etc. If the observation header does not contain the freqHeaderPrefix tag, the output time series frequency will be automatically calculated by the code (see examples).

If \code{mergedList=TRUE}, only a single date column is allowed, and it must be the first column in the CSV file, followed by as many columns as the time series to be imported. Users might indicate, in the data header, the frequency for all the output time series, by using the \code{freqHeaderPrefix} argument as stated above. The observation column headers must contain time series descriptions (see examples).
} 
\item{dateFormat}{
The date format to be used when converting a date string from the CSV file into a \code{Date()} variable. Default to : \code{"\%Y/\%m/\%d"}. In the \code{\link[=GETDATE]{GETDATE}} help page all compatible tags are listed, e.g. \code{\%Y, \%m, \%d, etc.}.
} 
\item{freqHeaderPrefix}{
The tag prefix in the CSV file to be used when imposing a specific frequency on the related output time series. Default to: \code{FREQ_}. The prefix must be followed by the numeric frequency value, e.g. \code{FREQ_4}, \code{FREQ_12}, etc. (see examples).
}
\item{...}{Backward compatibility.}
}

\value{
This function produces a time series \code{list()} built with time series data stored in a CSV file. List names, i.e., \code{names(list))}, will be a copy of time series descriptions in the CSV file headers.
}

\seealso{
\code{\link[=BIMETS2CSV]{BIMETS2CSV}}\cr
\code{\link[=date2yp]{date2yp}}\cr
\code{\link[=GETDATE]{GETDATE}}\cr
\code{\link[=TABIT]{TABIT}}\cr
}

\examples{
\donttest{

#define file path
filePath <- tempfile(fileext = ".csv")

#Heterogeneous frequency -----------------------------

#create time series
ts1 <- TSERIES(1:10+0.000057,START=c(2000,1),FREQ=1)
ts2 <- TSERIES(1:11,START=c(2001,2),FREQ=12)
ts3 <- TSERIES(c(1:3,NA,9:19)+0.0023,START=c(2001,3),FREQ=4)

#create time series list
myList <- list(
  myTitle1=ts1,
  myTitle2=ts2,
  myTitle3=ts3)

#export time series to csv then import back from csv

#export
BIMETS2CSV(
  myList,
  cellSeparator=';',
  decimalSeparator=',',
  dateFormat='\%Y\%m\%d',
  filePath=filePath,
  overWrite=TRUE)

#import from csv
outList <- CSV2BIMETS(
  filePath,
  cellSeparator=';',
  decimalSeparator=',',
  dateFormat='\%Y\%m\%d')

#compare input and output time series
for (idx in paste0('myTitle',1:3)) TABIT(myList[[idx]],outList[[idx]])

#Homogeneous frequency -----------------------------

#re-define time series, same frequency, different time range
ts1 <- TSERIES(1:10+0.000057,START=c(2000,1),FREQ=12)
ts2 <- TSERIES(1:11,START=c(2001,2),FREQ=12)
ts3 <- TSERIES(c(1:3,NA,9:19)+0.0023,START=c(2001,3),FREQ=12)

#create time series list
myList <- list(
  myTitle1=ts1,
  myTitle2=ts2,
  myTitle3=ts3)

#export time series to csv then import from csv

#export with BIMETS2CSV and mergeList=TRUE 
#note: argument in CSV2BIMETS is "mergedList"
BIMETS2CSV(
  myList,
  mergeList=TRUE, 
  cellSeparator=';',
  decimalSeparator=',',
  dateFormat='\%Y\%m\%d',
  filePath=filePath,
  overWrite=TRUE)
	
outList <- CSV2BIMETS(filePath,
  mergedList=TRUE,
  cellSeparator=';',
  decimalSeparator=',',
  dateFormat='\%Y\%m\%d')

#compare input and output time series
for (idx in paste0('myTitle',1:3)) TABIT(myList[[idx]],outList[[idx]])

#Impose user frequency -----------------------------

#create a monthly CSV file to be imported as a quarterly, i.e., FREQ_4
cat(c(
"FREQ_4,myTitle1,myTitle2",
"2001/01/31,NA,NA",
"2001/02/28,1,NA",
"2001/03/31,2,1.0023",
"2001/04/30,3,2.0023",
"2001/05/31,4,3.0023",
"2001/06/30,5,NA",
"2001/07/31,6,9.0023",
"2001/08/31,7,10.0023",
"2001/09/30,8,11.0023",
"2001/10/31,9,12.0023",
"2001/11/30,10,13.0023",
"2001/12/31,11,14.0023"
),
sep='\n',
file=filePath)

#import CSV
outList <- CSV2BIMETS(
  filePath,
  mergedList=TRUE)

#print quarterly series
TABIT(outList$myTitle1,outList$myTitle2)

#Automatic frequency retrieval -----------------------------

#create a quarterly CSV file with no frequency indication
cat(c(
"DATE,myTitle1,myTitle2",
"2001/03/31,2,1.0023",
"2001/06/30,5,NA",
"2001/09/30,8,11.0023",
"2001/12/31,11,14.0023"
),
sep='\n',
file=filePath)

#import CSV
outList <- CSV2BIMETS(
  filePath,
  mergedList=TRUE)

#print quarterly series
TABIT(outList$myTitle1,outList$myTitle2)

#Change locale -----------------------------

#set language to french
Sys.setlocale('LC_TIME','fr_FR.UTF8')

#export with BIMETS2CSV, full month names, and mergeList=TRUE 
BIMETS2CSV(
  myList,
  mergeList=TRUE, 
  dateFormat='\%Y \%B \%d',
  filePath=filePath,
  overWrite=TRUE)

#print file with french month names
cat(readLines(file(filePath)),sep='\n')

#read back file
outList <- CSV2BIMETS(
  filePath, 
  dateFormat='\%Y \%B \%d',
  mergedList=TRUE)
	
#Custom TSRANGE, Title, missingString -----------------------------

#set language to US english
Sys.setlocale('LC_TIME','en_US.UTF8')

#define ts
ts1 <- TSERIES(1:10+0.000057,START=c(2000,1),FREQ=12)

#insert missing values
ts1[[c(2000,5),c(2000,7)]] <- NA

#set custom ts description
attributes(ts1)$MyDescription <- 'My Long Description'

#export to csv
BIMETS2CSV(
  ts1, 
  dateFormat='\%Y \%B \%d',
  filePath=filePath,
  overWrite=TRUE,
  attributeOfNames='MyDescription',
  missingString='.',
  freqHeaderPrefix='MYFREQ_',
  title="CSV TITLE")
  
#print file with custom settings
cat(readLines(file(filePath)),sep='\n')
  
#reimport in R
outList <- CSV2BIMETS(
  filePath,
  dateFormat='\%Y \%B \%d',
  filePath=filePath,
  freqHeaderPrefix='MYFREQ_',
  skipLines=1)

#print ts
TABIT(outList[['My Long Description']])

}
}