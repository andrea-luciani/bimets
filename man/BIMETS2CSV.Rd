\name{BIMETS2CSV}
\alias{BIMETS2CSV}

  
\title{
BIMETS2CSV
}

\description{
This function transforms a time series list into a CSV (Comma-Separated Values) file built with the ordered sequence of pairs, i.e., dates and observation values, copied from the input time series.
The CSV file can then be easily manipulated by external spreadsheet programs or other econometric applications (e.g.,  MS Excel (R), Python, STATA(R), SAS(R), etc. ).

\bold{Heterogeneous frequency}

If the time series to be exported have different frequencies, the CSV file will consist of as many pairs of columns as time series to be exported. 

For each time series, a column for dates and a subsequent column for observation values will be created. 
The column header of the date column will contain the description of the related time series (see \code{attributeOfNames} argument).

The header of the observation column will contain the related time series frequency value in the format \code{FREQ_f, f=1,2,3,4,6,12,53,366, etc.} (see the \code{freqHeaderPrefix} argument and examples). 

Any missing observation \code{NA} will be printed in the CSV file as the string defined in the \code{missingString} argument.

\bold{Homogeneous frequency}

If all input time series have the same frequency, and \code{mergeList=TRUE}, the date column will be unique and will correspond to the first column in the CSV file (see the \code{mergeList} argument); in this case, the global frequency value will be printed in the date column header (in the same format as described before); other columns will contain observation values having the time series description in the column header (see \code{attributeOfNames} argument and examples).

Any missing observation \code{NA} will be printed in the CSV file as the string defined in the \code{missingString} argument.

\bold{Locale configuration}

Users can change current locale (e.g., month names) by using the base \code{R} command \code{\link[=locales]{locales}}, e.g.:

\code{Sys.setlocale('LC_TIME','en_US.UTF8')}, 

\code{Sys.setlocale('LC_TIME','fr_FR.UTF8')}, 

\code{Sys.setlocale('LC_TIME','it_IT.UTF8')}, 

... etc  (see examples).

\bold{Metadata specification}

The first line of text in the CSV file will contain metadata information about the cell separator, e.g. \code{"sep=,"}

This metadata information is often inserted to allow the CSV file to be directly opened in MS Excel(R).

}

\usage{
BIMETS2CSV( 
  input=NULL, 
  cellSeparator=',', 
  decimalSeparator='.', 
  filePath=NULL, 
  overWrite=FALSE,
  append=FALSE, 
  mergeList=FALSE, 
  TSRANGE=NULL,  
  attributeOfNames=NULL, 
  dateFormat='\%Y/\%m/\%d', 
  missingString='NA', 
  plainTable=FALSE, 
  title=NULL, 
  freqHeaderPrefix='FREQ_', 
  avoidCompliance=FALSE,
  ...)
}

\arguments{
  
\item{input}{ 
Input time series list, or a single input time series. Time series must be compliant with the specifications defined in the \code{\link[=is.bimets]{is.bimets}} help page.
}
\item{cellSeparator}{
Delimiter that separates individual data fields (i.e., cells) in the CSV file. Default to ","
} 
\item{decimalSeparator}{
The character in the CSV file that separates the integer part of a number from its fractional part. Default to "."
} 
\item{filePath}{
Path of the output CSV file.
} 
\item{overWrite}{
If the CSV file in the \code{filePath} already exists, and \code{overWrite=FALSE}, an error will be thrown; If \code{overWrite=TRUE}, the output CSV file will be overwritten.
} 
\item{append}{
If the CSV file in the \code{filePath} already exists, and \code{append=TRUE}, the new time series data will be appended to the existing CSV file. 
} 

\item{mergeList}{
It is possible to export time series into two different types of CSV files.

If \code{mergeList=FALSE}, the output CSV file will contain, for each time series to be exported, a couple of adjacent columns, i.e, a date column and an observation value column. Thus, it is possible to export time series that have different frequencies. The date column header will contain the time series description (see \code{attributeOfNames} argument). On the other hand, the observation header, will contain the frequency for the related time series, econded using the \code{freqHeaderPrefix} argument, e.g., \code{FREQ_12}, \code{FREQ_4}, etc. (see examples).

If \code{mergeList=TRUE}, and all input time series have the same frequency, only a single date column will be exported, and it will be the first column in the CSV file, followed by as many columns as the time series to be exportd. The data header will contain the frequency for all the output time series, using the \code{freqHeaderPrefix} argument as stated above. The observation column headers will contain time series descriptions (see \code{attributeOfNames} argument and examples).
} 
\item{TSRANGE}{
If \code{mergeList=TRUE}, the 4-element integer array argument \cr
\code{TSRANGE=c(startY,startP,endY,endP)} allows users to select observations to be exported.
} 
\item{attributeOfNames}{
Both description and frequency for each input time series are exported in column headers. In particular, the time series description is copied from \code{input} list attributes, giving priority to:

- \code{input} list names, i.e., \code{names(input)};

- the time series attribute whose name is specified in this argument \code{attributeOfNames};

- the time series attribute \code{Title};

- the input variable name, in the case of a single input time series;

- the string "UNKNOWN SERIES";

} 
\item{dateFormat}{
The date format to be used when converting an observation \code{Date()} variable into the string to be exported into the CSV. Default to : \code{"\%Y/\%m/\%d"}. In the the \code{\link[=GETDATE]{GETDATE}} help page all compatible tags are listed, e.g. \code{\%Y, \%m, \%d}, etc...

} 
\item{missingString}{
Time series missing values \code{NA} will be exported into the CSV file as the string defined in this in argument \code{missingString}. Default to \code{NA}.
} 

\item{plainTable}{
If \code{plainTable=TRUE}, the following arguments will be definitively set to:

\code{mergeList=TRUE}

\code{cellSeparator=;}

\code{decimalSeparator=.}

\code{dateFormat=\%d/\%m/\%Y}

Date column header will be set to \code{"DATE"}

} 

\item{title}{
Users can insert, into the output CSV file, a header line containing the text passed as a string to the argument \code{title}. If \code{title} is defined as a string array, for each array's element, a new line will be inserted into the CSV file header.
}
\item{freqHeaderPrefix}{
The tag prefix in the CSV file used when exporting a frequency value on the related time series header. Default to: \code{FREQ_}. The prefix will be followed by the numeric frequency value, e.g. \code{FREQ_4}, \code{FREQ_12}, etc. (see examples).
}
\item{avoidCompliance}{
If \code{TRUE}, compliance control check of input time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}.
}
\item{...}{Backward compatibility.}
}

\value{
This function produces a CSV file, in the provided \code{filePath}, built with time series observations, ordered by date, and copied from the input time series list.
}

\seealso{
\code{\link[=CSV2BIMETS]{CSV2BIMETS}}\cr
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
