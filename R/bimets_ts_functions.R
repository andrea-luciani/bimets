#############################################################################
#
# Copyright (C) 2021-2031 Bank of Italy
#  
#  This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
# @description: BIMETS - Time Series FUNs 		    
#
# @authors: ANDREA.LUCIANI@bancaditalia.it, ROBERTO.STOK@bancaditalia.it - ECS - Bank of Italy
#
# @license: GPL-3 - GNU GENERAL PUBLIC LICENSE - Version 3
#
#############################################################################

#called when pkg is attached
.onAttach <- function(...) {
  
  if (! exists('bimets_366_YP2D__') ) {stop('bimets: required cache files are missing. Please reinstall BIMETS.'); } 
  
  #clear console
  packageStartupMessage("\014 ");
  
  packageStartupMessage(gsub("\\$","",'bimets is active - version 2.0.1\nFor help type \'?bimets\'\n'))
  
  #packageStartupMessage('Loading required libraries...OK'); 
  #packageStartupMessage('\nBIMETS is active.\n');
}

.onLoad <- function(...) {

  
  
  tryCatch({
    
    #suppressMessages({
    #   if (! requireNamespace('zoo',quietly=TRUE)) 
    #   {cstop('Missing zoo required library.');}
    #  if (! requireNamespace('xts',quietly=TRUE) )
    #  {cstop('Missing xts required library.');}
    #});
    
    #init default configuration day in period
    if (is.null(getOption('BIMETS_CONF_DIP'))) 
    {
      #day in the period setup
      #options('BIMETS_CONF_DIP'='FIRST');
      options('BIMETS_CONF_DIP'='LAST');
    }
    
    #init default configuration constructor class type
    if (is.null(getOption('BIMETS_CONF_CCT'))) 
    {
      #day in the period setup
      #options('BIMETS_CONF_DIP'='FIRST');
      options('BIMETS_CONF_CCT'='TS');
    }
    
    #init default configuration on compliance behaviour
    if (is.null(getOption('BIMETS_CONF_NOC'))) 
    {
      #day in the period setup
      #options('BIMETS_CONF_DIP'='FIRST');
      options('BIMETS_CONF_NOC'=FALSE);
    }
    
    
    #overload ts operators
    
    #assign xts data by year period
    registerS3method('[[<-','xts' , function(x, idx, jdx, value, drop = TRUE, avoidCompliance=FALSE) {
      
      #if numeric take in charge
      if ((!(missing(idx))) && (!(missing(jdx))) && (!(missing(value)))
          && (!(is.null(idx))) && (!(is.null(jdx))) 
          && (is.numeric(idx)) && (is.numeric(jdx))  ) 
      {
        if (is.null(value)) stop('xts[[year,period]]: attempt to assign a NULL value.');
        
        if (! avoidCompliance ) 
        {
          tryCatch({.isCompliant(x);},error=function(e){stop('xts[[year,period]]: x - ',e$message);});      
        }
        
        #convert to TS  
        tmpTS=fromXTStoTS(x,avoidCompliance=TRUE);
        
        tryCatch({
          #try to assign values
          tmpTS[[idx,jdx,avoidCompliance=TRUE]]=value;
        },error=function(e){stop('xts[[year,period]]: ',e$message);});
        
        return(fromTStoXTS(tmpTS,avoidCompliance=TRUE));
      }
      
      return(NextMethod());
      
    });
    
    #assign ts data by year period
    registerS3method('[[<-','ts' , function(x, idx, jdx, value, drop = TRUE, avoidCompliance=FALSE) {
      
      
      #if numeric take in charge
      if ((!(missing(idx))) && (!(missing(jdx))) && (!(missing(value)))
          && (!(is.null(idx))) && (!(is.null(jdx))) 
          && (is.numeric(idx)) && (is.numeric(jdx))  ) 
      {
        if (is.null(value)) stop('ts[[year,period]]: attempt to assign a NULL value.');
        
        if (! avoidCompliance ) 
        {
          tryCatch({.isCompliant(x);},error=function(e){stop('ts[[year,period]]: x - ',e$message);});      
        }
        
        fTS=frequency(x);
        
        tryCatch({
          outI=(1+NUMPERIOD(start(x),normalizeYP(c(idx,jdx),fTS),fTS));
        },error=function(e){stop('ts[[year,period]]: wrong syntax. ',e$message)});
        
        
        
        #if ((outI<1)|| (outI>length(x)))
        if (outI<1)
        {
          stop('ts[[year,period]]: index out of range.')
        }
        
        coreTS=coredata(x);
        
        for(tmpIdx in (1:length(value)))
        { 
          
          coreTS[outI-1+tmpIdx]=value[tmpIdx];      
        }
        
        
        outF=ts(coreTS,start=start(x),frequency=fTS)
        
        #create temp ts with input value
        return(outF);
        
        
      }
      
      return(NextMethod());
      
    });
    
    #address ts data by year period
    registerS3method('[[','ts' , function(x, idx, jdx, drop = TRUE, avoidCompliance=FALSE) {
      
      
      #if numeric take in charge
      if ((!(missing(idx))) && (!(missing(jdx))) && (!(is.null(idx))) && (!(is.null(jdx))) 
          && (is.numeric(idx)) && (is.numeric(jdx))  ) 
      {
        
        if (! avoidCompliance ) 
        {
          tryCatch({.isCompliant(x);},error=function(e){stop('ts[[year,period]]: x - ',e$message);});      
        }
        
        tryCatch({
          outI=(1+NUMPERIOD(start(x),normalizeYP(c(idx,jdx),frequency(x)),frequency(x)));
        },error=function(e){stop('ts[[year,period]]: wrong syntax. ',e$message)});
        
        if ((outI<1)|| (outI>length(x)))
        {
          stop('ts[[year,period]]: index out of range.')
        }
        
        tryCatch({
          return(x[outI]);  
        },error=function(e){stop('ts[[year,period]]: unknown error. ',e$message)});
        
        return(NULL);
      }
      
      return(NextMethod());
    });
    
    #address ts data by date
    registerS3method('[','ts' , function (x, idx, jdx, drop = TRUE, avoidCompliance=FALSE) {
      
      #print(idx);
      
      #if not numeric take in charge
      if ((!(missing(idx))) && (!(is.null(idx))) && ((is.character(idx) || class(idx)=='Date' || class(idx)=='yearmon' || class(idx)=='yearqtr'  )))
      {
        xtsT=NULL;
        
        if (! avoidCompliance ) 
        {
          tryCatch({.isCompliant(x);},error=function(e){stop('ts[date]: x - ',e$message);});      
        }
        
        #transform in xts
        tryCatch({
          xtsT=fromTStoXTS(x,avoidCompliance=TRUE);
        },error=function(e){stop('ts[date]: error in fromTStoXTS. ',e$message);});
        
        #return requested data      
        if (length(xtsT[idx])==0) stop('ts[date]: index out of bounds.');
        
        return(coredata(xtsT[idx])[,1]);  
      }
      return(NextMethod());
    });
    
    #assign ts data by date
    registerS3method('[<-','ts' , function (x, idx, jdx, value, avoidCompliance=FALSE) {
      
      #if not numeric take in charge
      if ((!(missing(idx))) && (!(is.null(idx))) && ((is.character(idx) || class(idx)=='Date' || class(idx)=='yearmon' || class(idx)=='yearqtr'  )))
      {
        tsT=NULL;
        xtsT=NULL;
        
        if (is.null(value)) stop('ts[date]: attempt to assign a NULL value.');
        
        if (! avoidCompliance ) 
        {
          tryCatch({.isCompliant(x);},error=function(e){stop('ts[date]: x - ',e$message);});      
        }
        
        #transform
        tryCatch({
          xtsT=fromTStoXTS(x,avoidCompliance=TRUE);
        },error=function(e){stop('ts[date]: error in fromTStoXTS. ',e$message);});
        
        #apply changes
        tryCatch({
          xtsT[idx]=value;
        },error=function(e){stop('ts[date]: error in assignment. ',e$message);});
        
        #return ts
        tryCatch({
          tsT=fromXTStoTS(xtsT,avoidCompliance=TRUE); 
        },error=function(e){stop('ts[date]: error in fromXTStoTS. ',e$message);});
        
        return(tsT);
        
      }
      return(NextMethod());
    });
    
    #address xts data by year period
    registerS3method('[[','xts' , function(x, idx, jdx, drop = TRUE, avoidCompliance=FALSE) {
      
      #if numeric take in charge
      if ((!(missing(idx))) && (!(missing(jdx))) && (!(is.null(idx))) && (!(is.null(jdx))) 
          && (is.numeric(idx)) && (is.numeric(jdx))  ) 
      {
        
        if (! avoidCompliance ) 
        {
          tryCatch({.isCompliant(x);},error=function(e){stop('xts[[year,period]]: x - ',e$message);});      
        }
        
        startXTS=index(x)[1];
        
        if (class( startXTS )=='yearqtr') 
          
          startXTSYP=NULL;
        
        #get xts frequency
        xtsF=frequency(x);
        
        tryCatch({
          
          if (class( startXTS )=='Date')     startXTSYP=date2yp(startXTS,xtsF);
          if (class( startXTS )=='yearqtr')  startXTSYP=yq2yp(startXTS);
          if (class( startXTS )=='yearmon')  startXTSYP=ym2yp(startXTS);
          
          if (is.null(startXTSYP)) stop('unknown xts tclass()');
          
        },error=function(e){stop('xts[[year,period]]: unable to convert start date. ',e$message)});
        
        #return(startXTSYP);
        
        tryCatch({
          outI=(1+NUMPERIOD(startXTSYP,normalizeYP(c(idx,jdx),xtsF),xtsF));
        },error=function(e){stop('xts[[year,period]]: wrong syntax. ',e$message)});
        
        if ((outI<1)|| (outI>length(x)))
        {
          stop('xts[[year,period]]: index out of range.')
        }
        
        tryCatch({
          return(coredata(x[outI])[,1]);  
        },error=function(e){stop('xts[[year,period]]: unknown error. ',e$message)});
        
        return(NULL);
      }
      
      return(NextMethod());
    });
    
    #packageStartupMessage('Overloading time series operators...OK\nBIMETS is active.');
    
  },error=function(e){stop('BIMETS not fully loaded. ',e$message)})
  
  
  
}





# core ----------------------------------

.getFunArgsNames <- function(...)
{ 
  tryCatch({
    dots = substitute(list(...))[-1];    
    return(c( sapply(dots, deparse)));
  },error=function(e){stop('getArgsNames(): ',e$message);});
  
  return(NULL);
}

#set bimets conf
setBIMETSconf <- function(opt=NULL,value=NULL,suppressOutput=FALSE)
{
  if (opt=='BIMETS_CONF_DIP') options('BIMETS_CONF_DIP'=value);  
  if (opt=='BIMETS_CONF_CCT') options('BIMETS_CONF_CCT'=value);
  if (opt=='BIMETS_CONF_NOC') options('BIMETS_CONF_NOC'=value); 
  
  if (suppressOutput==TRUE) return(invisible());
  
  return(print(paste(opt,'=',toupper(.TRIM(value)))));  
}

#get bimets conf
getBIMETSconf <- function(opt=NULL)
{
  if (is.null(opt)) return(NULL);
  return(toupper(.TRIM(getOption(opt))));  
}

#fast check
.gBc <- function()
{
  return(cat(getBIMETSconf('BIMETS_CONF_DIP'),getBIMETSconf('BIMETS_CONF_CCT'),getBIMETSconf('BIMETS_CONF_NOC'),'\n'));  
}

#test if a time series is compliant and deals with options
is.bimets <- function(x=NULL,suppressErrors=TRUE,...)
{
  outF=FALSE;
  
  #check if compliant
  tryCatch({
    .isCompliant(x);outF=TRUE;           
    },error=function(e){if (! suppressErrors) stop('is.bimets(): error. ',e$message);});
    
  return(outF);
}

#convert a compliant ts into bimets
as.bimets <- function(x=NULL,FILLVALUE=NA,VERBOSE=FALSE,...)
{
   
	outF=NULL;
	coreD=NULL;
	
	if (is.null(x)) stop('as.bimets(): cannot convert NULL time series.') 
	
	if (! (is.ts(x) || is.xts(x))) stop('as.bimets(): input needs to be instance of ts() or xts() class.') 
	
  if (! is.logical(VERBOSE) || is.na(VERBOSE)) stop('as.bimets(): VERBOSE needs to be TRUE or FALSE.')
	if (! (is.numeric(FILLVALUE) || is.na(FILLVALUE))) stop('as.bimets(): FILLVALUE needs to be numeric or missing NA.')
	
  
  tryCatch({
    isUv=.isUnivariate(x);
  },error=function(e){stop('as.bimets(): ',e$message)});
  
	if (! isUv) 
  {
    cat('as.bimets(): warning, input time series is multivariate. Only the first column of data will be returned.\n');
    coreD=coredata(x)[,1];
	} else {	
   coreD=coredata(x);
	}
	
  #deal with frequency
  freqIn=frequency(x);
  
	if (! (freqIn %in% c(1,2,3,4,12,24,36,53,366))) stop("as.bimets(): unsupported frequency on input ts().");
	
	if (is.ts(x))
	{    
		outF=TSERIES(coreD,START=start(x),FREQ=freqIn);
		
	} else { #xts
    
	  startDate=c();
    endDate=c();
    indexDates=c();
    
    if (tclass( x )=='yearqtr' || tclass( x )=='yearmon')
    {
      startDate=as.Date(start(x));
      endDate=as.Date(end(x));  
      indexDates=as.Date(index(x));
      
    } else if (tclass( x )=='Date')
    {
      startDate=start(x);
      endDate=end(x); 
      indexDates=index(x);
            
    } else stop("as.bimets(): unsupported index class.");
    
  
    #get static dates
	  tryCatch({
      
      #get YP 
	    staticYP=unique(.getStaticYP(start=startDate,end=endDate,freq=freqIn)); 
      localYP=.getStaticYP(start=indexDates,freq=freqIn);
      
      #convert to numeric YYYYPPP
      if (startDate==endDate) {
        staticYPn=staticYP[1]*1000+staticYP[2];
        localYPn=localYP[1]*1000+localYP[2];
      } else {
	      staticYPn=staticYP[,1]*1000+staticYP[,2];
	      localYPn=localYP[,1]*1000+localYP[,2];
      }
        
	  },error=function(e){stop('as.bimets(): ',e$message)});
    
    newValue=FILLVALUE;
	  tmpCD=rep(newValue,length(staticYPn));      
	  
	  #find indices
	  matchIdx=match(localYPn,staticYPn);
	     
    if (length(matchIdx)!=length(coreD)) stop('as.bimets(): unknown error.');
    
	  #
    
    if (VERBOSE)
    {
           
      #get indexdes filled with new data
      newYPs=base::setdiff(staticYPn,localYPn);
      newIndexes=match(newYPs , staticYPn);
      
      if (length(newIndexes) != length(newYPs)) stop('as.bimets(): unknown error in VERBOSE.');
      
      if (length(newIndexes)>0) 
      {
        cat('\nas.bimets(): There are new observations inserted in to the non-regular input time series:\n\n');
        
        for (idxNI in 1:length(newIndexes))
        {
          insertedY=trunc(newYPs[idxNI]/1000);
          insertedP=newYPs[idxNI] %% 1000;
          cat('index: ',newIndexes[idxNI],', YP: ',insertedY,' - ', insertedP,', ','filled with: ',newValue,'\n',sep='');
        }
        
        cat('\n');
      }
      
      
    }
    
    
	  
	  tmpCD[matchIdx]=coreD;
    
    #deal with 366 <- 365 if not bisextile
    if (freqIn==366) 
    {
      
      
      staticDates=.getStaticDates(start=date2yp(startDate,f=366),end=date2yp(endDate,f=366),freq=freqIn);
      whicNA=which(is.na(staticDates)); 
      tmpCD[whicNA]=tmpCD[whicNA-1];
      
      #add last NA if ends with 365 and not bisextile
      if (date2yp(endDate,f=366)[2]==365 && (! .isBisextile(date2yp(endDate,f=366)[2])))
      {
        tmpCD=c(tmpCD,tmpCD[length(tmpCD)]);
      }
    }
    
    #output
	  if (startDate==endDate) {
      outF=TSERIES(tmpCD,START=staticYP,FREQ=freqIn);
	  } else {
	    outF=TSERIES(tmpCD,START=staticYP[1,],FREQ=freqIn);
	  }
    
      
	}#end if xts
 
  if (! is.bimets(outF)) stop('as.bimets(): cannot convert input time series.') 
	
	#copy attributes 
	if (!(is.null(attr(x,'Source'))))   		attr(outF,'Source')   =attr(x,'Source');
	if (!(is.null(attr(x,'Title'))))    		attr(outF,'Title')    =attr(x,'Title');
	if (!(is.null(attr(x,'Units'))))    		attr(outF,'Units')    =attr(x,'Units');
	if (!(is.null(attr(x,'ScaleFac')))) 		attr(outF,'ScaleFac') =attr(x,'ScaleFac');
	if (!(is.null(attr(x,'DESCRIZIONE')))) 	attr(outF,'DESCRIZIONE') =attr(x,'DESCRIZIONE');
	if (!(is.null(attr(x,'OGGETTO')))) 			attr(outF,'OGGETTO') =attr(x,'OGGETTO');
	if (!(is.null(attr(x,'STATO')))) 				attr(outF,'STATO') =attr(x,'STATO');
	if (!(is.null(attr(x,'STATUS')))) 			attr(outF,'STATUS') =attr(x,'STATUS');
	if (!(is.null(attr(x,'ID')))) 					attr(outF,'ID') =attr(x,'ID');
	
	return(outF);
  
}

#convert dates to yp 
.getStaticYP <- function (start=NULL,end=NULL,freq=NULL)
{
  
  if (!(class(start)=='Date')) stop('.getStaticYP(): "start" must be of class Date().');
  
  if (!is.null(end)) if (!(class(end)=='Date')) stop('.getStaticYP(): "end" must be of class Date().');

  
  if (!.isCompliantF(freq)) stop('.getStaticYP(): uncompliant frequency.');
  
  startY=as.numeric(format(start,format='%Y'));
  
  
  if (any(startY<bimets::bimets_static_startYear___) || any(startY>(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1)) ) stop(paste0('.getStaticYP(): time series must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
  
  if (!is.null(end)) 
  {
    endY=as.numeric(format(end,format='%Y'));    
    if (endY<bimets::bimets_static_startYear___ || endY>(bimets::bimets_static_startYear___   + bimets::bimets_static_totalLength___ -1) ) stop(paste0('.getStaticYP(): time series must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
  }
  
  startNum=as.numeric(start)-as.numeric(as.Date(paste0(bimets::bimets_static_startYear___,'-01-01')))+1;
  
  
  if (is.null(end))
  {
    range=startNum;
  } else {
    endNum=as.numeric(end)-as.numeric(as.Date(paste0(bimets::bimets_static_startYear___,'-01-01')))+1;
    range=startNum:endNum;
  }
  
      if (freq==366)
      { 
        startP=as.numeric(format(start,format='%j'));
        
        startNum=(startY-bimets::bimets_static_startYear___)*366+startP;
        
        if (is.null(end))
        {
          range=startNum;
        } else {
          
          endP=as.numeric(format(end,format='%j'));
          endNum=(endY-bimets::bimets_static_startYear___)*366+endP;
          
          range=startNum:endNum;
        }
        return(bimets::bimets_366_D2YP__[range,]);
        
      } else if (freq==12)
      {
        return(bimets::bimets_12_D2YP__[range,]);
        
      } else if (freq==4)
      {
        return(bimets::bimets_4_D2YP__[range,]);
        
      } else if (freq==3)
      {
        return(bimets::bimets_3_D2YP__[range,]);
        
      } else if (freq==2)
      {
        return(bimets::bimets_2_D2YP__[range,]);
        
      } else if (freq==1)
      {
        return(bimets::bimets_1_D2YP__[range,]);
        
      }else if (freq==24)
      {
        return(bimets::bimets_24_D2YP__[range,]);
        
      }else if (freq==36)
      {
        return(bimets::bimets_36_D2YP__[range,]);
        
      } else if (freq==53)
      {
        return(bimets::bimets_53_D2YP__[range,]);
        
      }else stop('.getStaticYP(): unsupported frequency.')
  
  return(NA);
}

#convert yp range to string date
.getStaticDates <- function (start=NULL,end=NULL,freq=NULL)
{
  tryCatch({
    .isCompliantYP(start,freq);
    
    if (is.null(end)) {
    end=start;
    } else {
    .isCompliantYP(end,freq);
    }    
    },error=function(e){stop('.getStaticDates(): ',e$message);})
  
  if (NUMPERIOD(start,end,freq)<0) stop('.getStaticDates(): "end" is before "start".');
  
  if (start[1]<bimets::bimets_static_startYear___ || start[1]>(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1) ) 
    stop(paste0('.getStaticDates(): year must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
  if (end[1]<bimets::bimets_static_startYear___ || end[1]>(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1) ) 
    stop(paste0('.getStaticDates(): year must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
  
  if (freq==366)
  { 
    #get date from static array
    return(bimets::bimets_366_YP2D__[((start[1]-bimets::bimets_static_startYear___)*366+start[2]):((end[1]-bimets::bimets_static_startYear___)*366+end[2])]);
  } else
  if (freq==36)
  { 
    if (getBIMETSconf('BIMETS_CONF_DIP')=='LAST') {
      return(bimets::bimets_36L_YP2D__[((start[1]-bimets::bimets_static_startYear___)*36+start[2]):((end[1]-bimets::bimets_static_startYear___)*36+end[2])]);
    } else {
      return(bimets::bimets_36F_YP2D__[((start[1]-bimets::bimets_static_startYear___)*36+start[2]):((end[1]-bimets::bimets_static_startYear___)*36+end[2])]); 
    }
  } else
  if (freq==53)
  { 
    if (getBIMETSconf('BIMETS_CONF_DIP')=='LAST') {
      return(bimets::bimets_53L_YP2D__[((start[1]-bimets::bimets_static_startYear___)*53+start[2]):((end[1]-bimets::bimets_static_startYear___)*53+end[2])]);
    } else {
      return(bimets::bimets_53F_YP2D__[((start[1]-bimets::bimets_static_startYear___)*53+start[2]):((end[1]-bimets::bimets_static_startYear___)*53+end[2])]); 
    }
  } else
  if (freq==24)
  { 
    if (getBIMETSconf('BIMETS_CONF_DIP')=='LAST') {
      return(bimets::bimets_24L_YP2D__[((start[1]-bimets::bimets_static_startYear___)*24+start[2]):((end[1]-bimets::bimets_static_startYear___)*24+end[2])]);
    } else {
      return(bimets::bimets_24F_YP2D__[((start[1]-bimets::bimets_static_startYear___)*24+start[2]):((end[1]-bimets::bimets_static_startYear___)*24+end[2])]); 
    }
  } else
  if (freq==12)
  { 
    if (getBIMETSconf('BIMETS_CONF_DIP')=='LAST') {
    return(bimets::bimets_12L_YP2D__[((start[1]-bimets::bimets_static_startYear___)*12+start[2]):((end[1]-bimets::bimets_static_startYear___)*12+end[2])]);
    } else {
    return(bimets::bimets_12F_YP2D__[((start[1]-bimets::bimets_static_startYear___)*12+start[2]):((end[1]-bimets::bimets_static_startYear___)*12+end[2])]); 
    }
  } else
  if (freq==4)
  { 
    if (getBIMETSconf('BIMETS_CONF_DIP')=='LAST') {
      return(bimets::bimets_4L_YP2D__[((start[1]-bimets::bimets_static_startYear___)*4+start[2]):((end[1]-bimets::bimets_static_startYear___)*4+end[2])]);
    } else {
      return(bimets::bimets_4F_YP2D__[((start[1]-bimets::bimets_static_startYear___)*4+start[2]):((end[1]-bimets::bimets_static_startYear___)*4+end[2])]); 
    }
  } else
  if (freq==3)
  { 
    if (getBIMETSconf('BIMETS_CONF_DIP')=='LAST') {
      return(bimets::bimets_3L_YP2D__[((start[1]-bimets::bimets_static_startYear___)*3+start[2]):((end[1]-bimets::bimets_static_startYear___)*3+end[2])]);
    } else {
      return(bimets::bimets_3F_YP2D__[((start[1]-bimets::bimets_static_startYear___)*3+start[2]):((end[1]-bimets::bimets_static_startYear___)*3+end[2])]); 
    }
  } else
  if (freq==2)
  { 
    if (getBIMETSconf('BIMETS_CONF_DIP')=='LAST') {
      return(bimets::bimets_2L_YP2D__[((start[1]-bimets::bimets_static_startYear___)*2+start[2]):((end[1]-bimets::bimets_static_startYear___)*2+end[2])]);
    } else {
      return(bimets::bimets_2F_YP2D__[((start[1]-bimets::bimets_static_startYear___)*2+start[2]):((end[1]-bimets::bimets_static_startYear___)*2+end[2])]); 
    }
  } else
  if (freq==1)
  { 
    if (getBIMETSconf('BIMETS_CONF_DIP')=='LAST') {
      return(bimets::bimets_1L_YP2D__[((start[1]-bimets::bimets_static_startYear___)+start[2]):((end[1]-bimets::bimets_static_startYear___)+end[2])]);
    } else {
      return(bimets::bimets_1F_YP2D__[((start[1]-bimets::bimets_static_startYear___)+start[2]):((end[1]-bimets::bimets_static_startYear___)+end[2])]); 
    }
  } else stop(paste0('.getStaticDates(): unsupported frequency.'))
  
}
  
#convert BIMETS to XTS 
fromBIMETStoXTS <- function(x=NULL,...)
{
  if (! is.bimets(x)) stop('fromBIMETStoXTS(): input time series is not bimets compliant.');
  
  if (is.xts(x)) 
  {
    return(x);
  } else if (is.ts(x))
  {
    return(fromTStoXTS(x,avoidCompliance=TRUE));
    
  } else stop('fromBIMETStoXTS(): cannot convert time series. Unknown error.' ); 
    
}

#convert BIMETS to TS 
fromBIMETStoTS <- function(x=NULL,...)
{
  if (! is.bimets(x)) stop('fromBIMETStoTS(): input time series is not bimets compliant.');
  
  if (is.ts(x)) 
  {
    return(x);
  } else if (is.xts(x))
  {
    return(fromXTStoTS(x,avoidCompliance=TRUE));
    
  } else stop('fromBIMETStoTS(): cannot convert time series. Unknown error.' ); 
  
}

#convert TS to XTS - req: .isCompliant()
fromTStoXTS <- function(x=NULL,avoidCompliance=FALSE,...)
{
	if (is.null(x)) stop('fromTStoXTS(): input needs to be instance of ts() class.');
	
  tryCatch({
	if (! avoidCompliance ) 
		{
		 .isCompliant(x);
		}
  },error=function(e){stop('fromTStoXTS(): ',e$message);});
	
	#trivial
	if (is.xts(x)) return(x);	
	if (!is.ts(x)) stop("fromTStoXTS(): input needs to be instance of ts() class.");
	
  #getfrequency
	fTS=frequency(x);
  
  #get dates
  dateArr=GETDATE(x,avoidCompliance=TRUE);
  
  #remove NA and relative data
  naIdxs=-which(is.na(dateArr))
  if (length(naIdxs)>0) dateArr=dateArr[naIdxs];  
  tmpData=coredata(x);
  if (length(naIdxs)>0) tmpData=tmpData[naIdxs];
  
  #366 ts with length 1 and start on 366 no bisextile
  if (length(dateArr)==0) stop('fromTStoXTS(): cannot convert input time series.');
  
  #deal with index class
  if (fTS==4) 
  {
    dataF=data.frame(as.yearqtr(as.Date(dateArr)),tmpData); 
  } else if (fTS==12)
  {
    dataF=data.frame(as.yearmon(as.Date(dateArr)),tmpData); 
  } else {
    dataF=data.frame(as.Date(dateArr),tmpData); 
  }
  
  #saving hours 1970 bugs
  suppressWarnings({xt=xts(dataF[,2],order.by=dataF[,1],tzone=Sys.getenv("TZ"))});
    
	#copy attributes 
	if (!(is.null(attr(x,'Source'))))   		attr(xt,'Source')   =attr(x,'Source');
	if (!(is.null(attr(x,'Title'))))    		attr(xt,'Title')    =attr(x,'Title');
	if (!(is.null(attr(x,'Units'))))    		attr(xt,'Units')    =attr(x,'Units');
	if (!(is.null(attr(x,'ScaleFac')))) 		attr(xt,'ScaleFac') =attr(x,'ScaleFac');
  if (!(is.null(attr(x,'DESCRIZIONE')))) 	attr(xt,'DESCRIZIONE') =attr(x,'DESCRIZIONE');
  if (!(is.null(attr(x,'OGGETTO')))) 			attr(xt,'OGGETTO') =attr(x,'OGGETTO');
  if (!(is.null(attr(x,'STATO')))) 				attr(xt,'STATO') =attr(x,'STATO');
  if (!(is.null(attr(x,'STATUS')))) 			attr(xt,'STATUS') =attr(x,'STATUS');
 	if (!(is.null(attr(x,'ID')))) 					attr(xt,'ID') =attr(x,'ID');
 
 
  #set freqeuncy attrib on xts
	attr(xt,'.bimetsFreq')=fTS;
  
	return(xt);
	
	}

#convert XTS to TS - req: .isCompliant()
fromXTStoTS <- function(x=NULL,avoidCompliance=FALSE,...)
{	
	if (is.null(x)) stop('fromXTStoTS(): input needs to be instance of xts() class.');
	
	tryCatch({
	  if (! avoidCompliance ) 
	  {
	    .isCompliant(x);
	  }
	},error=function(e){stop('fromTStoXTS(): ',e$message);});
	
	if (is.ts(x)) return(x);
	if (!is.xts(x)) stop("fromXTStoTS(): input needs to be instance of xts() class.");
	
	xtsF=frequency(x);  
	startYP=c();
	endYP=c();
	tmpCD=c();
  
  #get start / end year period
  if (xtsF==4) {
    
    startYP=yq2yp(start(x));
    endYP=yq2yp(end(x));
    
  } else if (xtsF==12) {
    
    startYP=ym2yp(start(x));
    endYP=ym2yp(end(x));
    
  } else {
    
    startYP=date2yp(start(x),f=xtsF);
    endYP=date2yp(end(x),f=xtsF);    
    
  }
  
  #get static dates
  tryCatch({
    staticDates=.getStaticDates(start=startYP,end=endYP,freq=xtsF)
  },error=function(e){stop('fromXTStoTS(): ',e$message);})
  
  #get NA indexes
  whichNA=which(is.na(staticDates));
  
  #preallocate data array
  tmpCD=rep(NA,length(staticDates));
	  
  #only if daily...
  if (length(whichNA)>0) 
  {
    #set to coredata only position != whichNA
    tmpCD[-whichNA]=coredata(x)[,1];
    
    #copy 365 on 366 if not bisextile
    tmpCD[whichNA]=tmpCD[whichNA-1];
    
  } else
  {
    tmpCD=coredata(x)[,1];
  }
  
	#add a 366 if last obs is 365 and next is NA    
	if (endYP[2]==365 && is.na(.getStaticDates(start=c(endYP[1],366),freq=xtsF)))
	{
	  tmpCD=c(tmpCD,tmpCD[length(tmpCD)]);
	}
  
	xt=ts(tmpCD,start=startYP,frequency=xtsF);  	
  
	if (!(is.null(attr(x,'Source'))))       attr(xt,'Source')   =attr(x,'Source');
	if (!(is.null(attr(x,'Title'))))        attr(xt,'Title')    =attr(x,'Title');
	if (!(is.null(attr(x,'Units'))))        attr(xt,'Units')    =attr(x,'Units');
	if (!(is.null(attr(x,'ScaleFac'))))     attr(xt,'ScaleFac') =attr(x,'ScaleFac');
	if (!(is.null(attr(x,'DESCRIZIONE'))))  attr(xt,'DESCRIZIONE') =attr(x,'DESCRIZIONE');
	if (!(is.null(attr(x,'OGGETTO')))) 			attr(xt,'OGGETTO') =attr(x,'OGGETTO');
	if (!(is.null(attr(x,'STATO')))) 				attr(xt,'STATO') =attr(x,'STATO');
	if (!(is.null(attr(x,'STATUS')))) 			attr(xt,'STATUS') =attr(x,'STATUS');
	if (!(is.null(attr(x,'ID')))) 					attr(xt,'ID') =attr(x,'ID');
	
	
	return(xt);
	
}

#override frequency on xts
frequency.xts <- function(x=NULL, ...) 
{
  if (is.null(x)) stop('frequency(): NULL input');
  
  return(.xtsPeriodicity(x)$relFrequency);
  
}


  
#extract periodicity from xts (xts::periodicity dont fit requirements)
.xtsPeriodicity <- function (x=NULL) 
{
	if (is.null(x)) stop('.xtsPeriodicity(): input needs to be instance of xts() class.');
	if (!is.xts(x)) stop(".xtsPeriodicity(): input needs to be instance of xts() class.");
		
	#attribute bimetsFreq has priority
	if (is.null(attr(x,'.bimetsFreq')))
	{
  	scale=NA;value=NA;p=NA;relFrequency=NA;
  	
  	if (!(length(x)<2 )) #NA if single sample
  	{	
  	
  	#diff on index unreliable due to dependance on index class (i.e. in yearmon a diff of 1 is not a day nor really a month...)
  	#p = suppressWarnings(min(diff(.index(x))));
    tempDiff=diff(.index(x));
  	p = suppressWarnings(min(tempDiff[which(tempDiff>0)]));
  	
  	if (!(is.na(p)))
  		{
  	  
  		#scale: year as max, second as min
  		#xts::.index can include on some hw daysaving settings so there could be a 23h day and a 25h day
  		
  		if (p < 60) 									{	scale <- "second" 				; value <- 0 		;	relFrequency = 31622400;	} #60 sec
  		else if (p < 3600) 						{ scale <- "minute" 				; value <- 1 		; relFrequency = 527040;		} #60*60 = 1h
  		else if (p < 3600*23) 				{ scale <- "hour" 					; value <- 2		; relFrequency = 8740;			} #1h*24 = 1d can be daysaving!
  		else if (p < 3600*(24*6+23)) 	{ scale <- "day" 						; value <- 3		; relFrequency = 366;				} #1d*7  = 1w
  		else if (p < 3600*(24*7+23))	{ scale <- "week"						; value <- 4		; relFrequency = 53;				} #1d*8  = 10days (need end feb as minimum)
  		else if (p < 3600*(24*9+23))  { scale <- "10days"					; value <- 5		; relFrequency = 36;				} #1d*10 = 2weeks (need end feb as minimum)
  		else if (p < 3600*(24*27+23)) { scale <- "2weeks"					; value <- 6		; relFrequency = 24;				} #1d*28 = 1m (need feb as minimum)
  		else if (p < 3600*(24*88+23))	{ scale <- "month"  				; value <- 7		; relFrequency = 12;				} #28feb+31(jan or mar)+30(nov or apr)-daysaving if any! ....
  		else if (p < 3600*(24*119+23)){ scale <- "trimestral" 		; value <- 8 		; relFrequency = 4;					} #...+31(oct or may)
  		else if (p < 3600*(24*180+23)){ scale <- "quadrimestral" 	; value <- 9 		; relFrequency = 3;					} #
  		else if (p < 3600*(24*364+23)){ scale <- "semestral"  	 	; value <- 10		; relFrequency = 2;					} #365 min #days in year
  		else 	                        { scale <- "year"     	    ; value <- 11		; relFrequency = 1;					}
  		}
  	}
    else if(length(x)==1)
    {
      if (tclass( x )=='yearqtr') { scale <- "trimestral"   	; value <- 8 		; relFrequency = 4;					}
      if (tclass( x )=='yearmon') { scale <- "month"    			; value <- 7		; relFrequency = 12;				}
      if (tclass( x )=='Date')    { scale <- "day"      		  ; value <- 3		; relFrequency = 366;				}

    }
  } else
	{
    if (attr(x,'.bimetsFreq')==1)            { scale =  "year"            ; value <- 11   ; relFrequency = 1;         }
    else if (attr(x,'.bimetsFreq')==2)       { scale <- "semestral"     	; value <- 10 	; relFrequency = 2;					}
    else if (attr(x,'.bimetsFreq')==3)       { scale <- "quadrimestral"   ; value <- 9 		; relFrequency = 3;					}
    else if (attr(x,'.bimetsFreq')==4)       { scale <- "trimestral"   	  ; value <- 8 		; relFrequency = 4;					}
    else if (attr(x,'.bimetsFreq')==12)      { scale <- "month"    			  ; value <- 7		; relFrequency = 12;				}
    else if (attr(x,'.bimetsFreq')==24)      { scale <- "2weeks"      		; value <- 6		; relFrequency = 24;				}
    else if (attr(x,'.bimetsFreq')==36)      { scale <- "10days"      		; value <- 5		; relFrequency = 36;				}
    else if (attr(x,'.bimetsFreq')==53)      { scale <- "week"  					; value <- 4		; relFrequency = 53;				}
    else if (attr(x,'.bimetsFreq')==366)     { scale <- "day"   					; value <- 3		; relFrequency = 366;				} 
    else if (attr(x,'.bimetsFreq')==8740)    { scale <- "hour"   				  ; value <- 2	  ; relFrequency = 8740;			}
    else if (attr(x,'.bimetsFreq')==527040)  { scale <- "minute"   			  ; value <- 1 		; relFrequency = 527040;		}
    else if (attr(x,'.bimetsFreq')==31622400){ scale <- "second" 				  ; value <- 0 	  ;	relFrequency = 31622400;	}
    
    else stop('.xtsPeriodicity(): unsupported .bimetsFreq.');
	} 
	
	
  
	#eg. .xtsPeriodicity(xts)$scale...
	return(structure(list( scale = scale, value = value ,relFrequency = relFrequency)));
}

#true if xts/ts is univariate && length>1
.isUnivariate <- function(x=NULL)
{
	if (is.null(x)) stop('.isUnivariate(): input needs to be instance of ts() or xts() class.');
	
	if (!(is.xts(x) || is.ts(x))) stop(".isUnivariate(): input needs to be instance of xts() or ts() class.");
	
	dims=dim(x);
  
	if (is.xts(x))
	{
	
		if (length(dims)<2) stop('.isUnivariate(): misformed coredata in input xts.');
	
		#return (dims[2]==1 && dims[1]>1); #also check there are at least 2 samples
		return (dims[2]==1 && dims[1]>0); #new: also check there are at least 1 samples
	}
			
	if (is.ts(x))
	{	
		#dim null on univariate ts ....
		if (is.null(dims)) { 
      #return(length(x)>1);
		  return(length(x)>0); #new
		}
		else
		{
			if (length(dims)<2) stop('.isUnivariate(): misformed dimension in input ts.');
			
			#return (dims[2]==1 && dims[1]>1); #also check there are at least 2 samples
			return (dims[2]==1 && dims[1]>0); #new: also check there are at least 1 samples
		}
	}
}

#check if ts/xts is compliant
.isCompliant <- function(x=NULL,suppressErrors=FALSE,...)
{
  #avoid test if global option set
  if (getBIMETSconf('BIMETS_CONF_NOC')==TRUE) return(TRUE);
  
  #return FALSE instead of error
	if (suppressErrors==TRUE){
    outF=FALSE;
      
    tryCatch({.isCompliant(x);outF=TRUE;},error=function(e){});
    
    return(outF);
    
	}
  
  
  if (is.null(x)) stop('.isCompliant(): input needs to be instance of ts() or xts() class.');
  
  
  #check if class is correct with default
	if (getBIMETSconf(opt='BIMETS_CONF_CCT')=='TS' && (! is.ts(x)))
	{
	  if (! suppressErrors) 
	  {
	    stop('.isCompliant(): input needs to be instance of class ts(). BIMETS_CONF_CCT value is TS.');
	  } else {
	    return(FALSE);
	  }
	}
	
	if (getBIMETSconf(opt='BIMETS_CONF_CCT')=='XTS' && (! is.xts(x)))
	{
	  if (! suppressErrors) 
	  {
	    stop('.isCompliant(): input needs to be instance of class xts(). BIMETS_CONF_CCT value is XTS.');
	  } else {
	    return(FALSE);
	  }
	}
  
  
	
	if (is.xts(x))
	{
		return(.isCompliantXTS(x));
	}
	else if (is.ts(x))
	{
		return(.isCompliantTS(x));
	}
	else {stop('.isCompliant(): input needs to be instance of ts() or xts() class.');}
}

#backward compatibility
.isCFCompliant <- .isCompliant;

#check if ts is compliant: 
.isCompliantTS <- function(x=NULL)
{
	if (is.null(x)) stop('.isCompliantTS(): input needs to be instance of ts() class.');
	
	if (! (is.ts(x))) stop('.isCompliantTS(): input needs to be instance of ts() class.');
	
	if (!.isUnivariate(x)) stop('.isCompliantTS(): input needs to be univariate with at least one observation.');
	
  
	fTS=frequency(x);	
	if (! (fTS %in% c(1,2,3,4,12,24,36,53,366))) stop(".isCompliantTS(): uncompliant frequency in input time series.");
	
  start=start(x);
  end=end(x);
  
	#check if date in bimets range
	if (start[1]<bimets::bimets_static_startYear___ || start[1]>(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1) ) stop(paste0('.isCompliantTS(): dates must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
	if (end[1]<bimets::bimets_static_startYear___ || end[1]>(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1) ) stop(paste0('.isCompliantTS(): dates must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
	
	return(TRUE);
}

#check if xts is compliant: i.e. stricty regular on daily, monthly, quarterly, semiannual or yearly
.isCompliantXTS <- function(x=NULL)
{
	if (is.null(x)) stop('.isCompliantXTS(): input needs to be instance of xts() class.');
	
	if (!is.xts(x)) stop(".isCompliantXTS(): input needs to be instance of xts() class.");
	
	if (!.isUnivariate(x)) stop('.isCompliantXTS(): input needs to be univariate with at least one observation.');
	
	if(is.null(tclass( x )) ) stop(".isCompliantXTS(): input needs to be instance of xts() class.");
	
	if ((tclass( x )=='yearmon') || (tclass( x )=='yearqtr')) #monthly&quarterly
	{
		if ((! is.regular(x,strict=TRUE)) && (length(x)>1)) stop(paste('.isCompliantXTS(): input of class',tclass( x ),'is not strictly regular.'));
		if ((length(x)>1) && (min(diff(.index(x)))==0)) stop(".isCompliantXTS(): there are duplicated observations at position ",which.min(diff(.index(x))));
    
	   
		if (tclass( x )=='yearmon')
    {
      start=ym2yp(start(x));
		  end=ym2yp(end(x));
		}
		
		if (tclass( x )=='yearqtr')
		{
		  start=yq2yp(start(x));
		  end=yq2yp(end(x));
		}
    
    #check if date in bimets range
		if (start[1]<bimets::bimets_static_startYear___ || start[1]>(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1) ) stop(paste0('.isCompliantXTS(): dates must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
		if (end[1]<bimets::bimets_static_startYear___ || end[1]>(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1) ) stop(paste0('.isCompliantXTS(): dates must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
		
		#is.regular bug doesnt work if irregularity is regular...
		theoNumPer=NUMPERIOD(start,end,f=frequency(x))+1
		if (length(x)!=theoNumPer) stop(".isCompliantXTS(): input is not strictly regular.");
		
		
  }
	else if ((tclass( x )=='Date'))
	{ 
		fXTS=frequency(x);
    
		start=date2yp(start(x),f=fXTS);
		end=date2yp(end(x),f=fXTS);
		
		#check if date in bimets range
		if (start[1]<bimets::bimets_static_startYear___ || start[1]>(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1) ) stop(paste0('.isCompliantXTS(): dates must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
		if (end[1]<bimets::bimets_static_startYear___ || end[1]>(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1) ) stop(paste0('.isCompliantXTS(): dates must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
		
    
    if (fXTS ==12) stop('.isCompliantXTS(): uncompliant index class for monthly time series.');
  	if (fXTS ==4) stop('.isCompliantXTS(): uncompliant index class for quarterly time series.'); 

    startD=start(x);
    startYP=date2yp(startD,f=fXTS);
    endD=end(x);
		endYP=date2yp(endD,f=fXTS);
    len=length(x);
   
    tryCatch({
      referenceDates=as.Date(.getStaticDates(startYP,endYP,fXTS));
      whichNa=which(is.na(referenceDates));
      if (length(whichNa)>0) referenceDates=referenceDates[-whichNa];
      
	  },error=function(e){stop('.isCompliantXTS(): ',e$message);});
    
    suppressWarnings({
      diffArr=which(index(x)!=referenceDates)
      });
    
    if (length(diffArr>0)) stop('.isCompliantXTS(): uncompliant date on sample ',diffArr[1]);
    
	}# endif class date
	else {#not Date or yearmon or yearqtr index class
		stop('.isCompliantXTS(): uncompliant index class.')
	}
		
	return(TRUE);
}

#convert yearmon to c(year,period)
ym2yp <- function(x=NULL)
{
	if (is.null(x)) stop('ym2yp(): input needs to be instance of yearmon class.');
	outF=NA;
	
	if ( any(is.na(x)) || any(is.null(class( x ))) || !(all(class( x )=='yearmon'))) stop('ym2yp(): input needs to be instance of class yearmon.');
	
	tryCatch({
	  
	  x=as.Date(x);
	  outF=.getStaticYP(start=x,freq=12);
	  
	},error=function(e){stop('yq2yp(): ',e$message);});
  

	
	return(outF);
}

#convert yearqtr to c(year,period)
yq2yp <- function(x=NULL)
{
	if (is.null(x)) stop('yq2yp(): input needs to be instance of yearqtr class.');
	outF=NA;
	
	if ( any(is.na(x)) || any(is.null(class( x ))) || !(all(class( x )=='yearqtr'))) stop('yq2yp(): input needs to be instance of class yearqtr.');
	
	tryCatch({
	  
    x=as.Date(x);
	  outF=.getStaticYP(start=x,freq=4);
	  
	},error=function(e){stop('yq2yp(): ',e$message);});
  
  
	
	return(outF);
}

#convert date to c(year,period) - f=frequency of ts
date2yp <- function(x=NULL,f=1)
{
	
  
  if (is.null(x)) stop('date2yp(): input needs to be instance of class Date().');
	outF=NA;
	
	if (any(is.na(x)) || any(is.null(class( x ))) || !(all(class( x )=='Date'))  ) stop('date2yp(): input needs to be instance of class Date().');

  if (!(f %in% (c(1,2,4,3,12,24,36,53,366)))) stop('date2yp(): frequency f needs to be 1, 2, 3, 4, 12, 24, 36, 53 or 366.');
	
	tryCatch({
    
	outF=.getStaticYP(start=x,freq=f);
  
	},error=function(e){stop('date2yp(): ',e$message);});

	
	return(outF);
}

#normalize c(y,p) if p>f. e.g. c(2000,5) & f=4 => c(2001,1)
normalizeYP <- function(x=NULL,f=NULL)
{
	if (is.null(x)) stop('normalizeYP(): Uncompliant input. Usage: normalizeYP(c(y,p),f=frequency).');
	
	if (is.null(f)) stop('normalizeYP(): NULL frequency.');
	if (!(.isCompliantF(f))) stop('normalizeYP(): uncompliant frequency');
	
	if (!(is.vector(x) && length(x)==2))  stop('normalizeYP(): Uncompliant input. Usage: normalizeYP(c(y,p),f=frequency).');	
	if (!(is.numeric(x[1]) ) )  stop('normalizeYP(): year must be a positive integer.');
	if (!(is.numeric(x[2]) ) )  stop('normalizeYP(): period must be a positive integer.');
	if (is.na(x[1])  )  stop('normalizeYP(): year must be a positive integer.');
	if (is.na(x[2])  )  stop('normalizeYP(): period must be a positive integer.');
	if (!( x[1]>=0) )  stop('normalizeYP(): year must be a positive integer.');
	#if (!( x[2]>=0) )  stop('normalizeYP(): period must be a positive integer.');
	if (!(x[1]%%1==0))  stop('normalizeYP(): non-integer year.');
	if (!(x[2]%%1==0))  stop('normalizeYP(): non-integer period.');
	
	
  return(c(x[1]+((x[2]-1)%/%f ),((x[2]-1)%%f)+1));
	
	
}

#check if f is integer>0
.isCompliantF <- function(f=NULL)
{
	if (is.null(f)) stop('.isCompliantF(): NULL frequency.');
	if (is.na(f)) stop('.isCompliantF(): NULL frequency.');
	if (!(is.numeric(f) && f>0) ) stop('.isCompliantF(): frequency must be a positive integer.');
	if (!(f%%1==0))  stop('.isCompliantF(): non-integer frequency.');
	
	return(TRUE);
}

#check if x==c(integer>=0,integer>=0)
.isCompliantYP <- function(x=NULL,f=NULL)
{
	
	if (is.null(x)) stop('.isCompliantYP(): Uncompliant input. Usage: .isCompliantYP(c(y,p),f=frequency).');
	
	if (is.null(f)) stop('.isCompliantYP(): NULL frequency.');
	
	if (!(.isCompliantF(f))) stop('.isCompliantYP(): uncompliant frequency');
	
	if (!(is.vector(x) && length(x)==2))  stop('.isCompliantYP(): uncompliant input. Usage: .isCompliantYP(c(y,p),f=frequency)');	
	if (!(is.numeric(x[1]) ) )  stop('.isCompliantYP(): year must be a positive integer.');
	if (!(is.numeric(x[2]) ) )  stop('.isCompliantYP(): period must be a positive integer.');
	if (is.na(x[1])  )  stop('.isCompliantYP(): year must be a positive integer.');
	if (is.na(x[2])  )  stop('.isCompliantYP(): period must be a positive integer.');
	if (!( x[1]>=0) )  stop('.isCompliantYP(): year must be a positive integer.');
	if (!( x[2]>=0) )  stop('.isCompliantYP(): period must be a positive integer.');
	if (!(x[1]%%1==0))  stop('.isCompliantYP(): non-integer year.');
	if (!(x[2]%%1==0))  stop('.isCompliantYP(): non-integer period.');
	if (x[2]>f) stop('.isCompliantYP(): period greater than frequency. Consider using normalizeYP(c(y,p),f)');
	
	return(TRUE);
}

#TRUE if x1=c(x11,x12) is earlier then x2=c(x21,x22)
.isMinorPeriodYP <- function (x1,x2,f=NULL)
{
	
	if (is.null(f)) stop('.isMinorPeriodYP(): NULL frequency.');	
	
	if (!( .isCompliantYP(x1,f) && .isCompliantYP(x2,f)))	stop('.isMinorPeriodYP(): uncompliant inputs.');
	
	if (x1[1]<x2[1]) return(TRUE);
	if (x1[1] == x2[1] && x1[2] < x2[2]) return(TRUE);
	return(FALSE);
	
}

#TRUE if x1=c(x11,x12) is equal to x2=c(x21,x22)
.isEqualPeriodYP <- function (x1,x2,f=NULL)
{
	if (is.null(f)) stop('.isEqualPeriodYP(): NULL frequency.');
	
	if (!(.isCompliantF(f))) stop('.isEqualPeriodYP(): uncompliant frequency');
		
	if (!( .isCompliantYP(x1,f) && .isCompliantYP(x2,f)))	stop('.isEqualPeriodYP(): uncompliant inputs.');
	
	
	if (x1[1] == x2[1] && x1[2] == x2[2]) return(TRUE);
	return(FALSE);
	
}

#numbers of perdiods between two c(year, period)
NUMPERIOD <- function (x1,x2,f=NULL)
{
	outF=NA;
	
	if (is.null(f)) stop('NUMPERIOD(): NULL frequency.');
	if (!(.isCompliantF(f))) stop('NUMPERIOD(): uncompliant frequency');
	if (!( .isCompliantYP(x1,f) && .isCompliantYP(x2,f)))	stop('NUMPERIOD(): uncompliant input x1 or x2.');

	outF=x2[2]-x1[2]+ (x2[1]-x1[1])*f;
	
	return(outF);
	
}

#return count of Missing Values on first samples
.numberOfStartingMissing <- function(x=NULL)
{
	if (is.null(x)) stop('.numberOfStartingMissing(): input needs to be instance of ts() or xts() class.');
	if (!(is.ts(x) || is.xts(x))) stop('.numberOfStartingMissing(): input needs to be instance of ts() or xts() class.');	
	
	i=1;
	
	while (i<=length(x) && is.na(x[i])) i=i+1;
	
	return(i-1);
}

#return count of Missing Values on last samples
.numberOfEndingMissing <- function(x=NULL)
{
	if (is.null(x)) stop('.numberOfEndingMissing(): input needs to be instance of ts() or xts() class.');
	if (!(is.ts(x) || is.xts(x))) stop('.numberOfEndingMissing(): input needs to be instance of class ts() or xts()');	
	
	i=1;
	
	while (i<=length(x) && is.na(x[length(x)+1-i])) i=i+1;
	
	return(i-1);
}

#bisextile year check
.isBisextile <- function(year=NULL)
{
	if (any(is.null(year))) stop('.isBisextile(): NULL year');
	if (any(is.na(year))) stop('.isBisextile(): missing year');
	
	if (length(year)==1)
	{
		if (!(is.numeric(year) && year>0) ) stop('.isBisextile(): year must be a positive integer.');
		if (!(year%%1==0))  stop('.isBisextile(): non-integer year.');
		
		return((year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0)));  
	} else {
		if (!(is.numeric(year) ) ) stop('.isBisextile(): year must be a positive integer.');
		outF=c()
		for (idxTmp in 1:length(year))
		{
			outF=c(outF,.isBisextile(year[idxTmp]))
		}
		
		return(outF)
	}
	
	return(NULL)
}


# A1D code ----------------------------------------
#A1D support function
.A1DCompliantInput <- function(x=NULL)
{
  
  if (is.null(x)) return(FALSE);
  if (is.numeric(x)) return(TRUE);
  if (is.xts(x)) return(TRUE);
  if (is.ts(x)) return(TRUE);
  if (all(is.na(x))) return(TRUE);
  return(FALSE);
}

#A1D defines a one-dimensional array..
A1D <- function(..., length = NULL, avoidCompliance=FALSE)
{
  
  if (!(is.null(length))) 
  {
    #stop('A1D(): null length.');
    if (!(is.numeric(length) ) ) stop('A1D(): length must be an integer.');
    if (!(length%%1==0))  stop('A1D(): non-integer length.');
    if (!(length>=0))  stop('A1D(): length must be a positive integer.');
    
  }
  
  outF=c();
  
  #an input is null
  tryCatch({inputsL=list(...);},error=function(e){stop('A1D(): an input argument is NULL.')});  
  
   
  #cat('length=',length,'\n');
  #cat('list len=',length(inputsL),'\n');
  
  #no args
  if (is.null(length) && (length(inputsL)==0)) stop('A1D(): at least one parameter is required.');
  
  #no values
  if (length(inputsL)==0) 
  {
    if (length==0) return(outF);
    for (idx in (1:(length))) outF=c(outF,0); 
    return(outF);
  }
  
  
  #inputs are ts xts or numeric or na
  if (all(as.logical(lapply(inputsL,.A1DCompliantInput))))
  {
    #sum of length of inputs
    totLength=0; 
    
    for (idx in (1:length(inputsL))) 
    {
      
      #if inputs is ts/xts check if required
      if (!(avoidCompliance) && (is.ts(inputsL[[idx]]) || is.xts(inputsL[[idx]])))
      {
        tryCatch({.isCompliant(inputsL[[idx]]);},error=function(e){stop('A1D(): ',e$message);});
      }
      
      
      totLength=totLength+length(inputsL[[idx]]);
      
      
      
    }
    
    #print(totLength)
    
    if (!(is.null(length)) && (totLength>length)) stop('A1D(): too many elements are specified for given length.');
    
    #combine
    for (idx in (1:length(inputsL))) 
    {
      if ((is.ts(inputsL[[idx]])) || (is.xts(inputsL[[idx]]))) 
      {
        outF=c(outF,coredata(inputsL[[idx]]));
      } else {
        outF=c(outF,inputsL[[idx]]);
      }
    }
    
    #add tail zeros    
    if ( (!(is.null(length))) && (totLength<length)) 
    {
      for (idx in (1:(length-totLength))) outF=c(outF,0); 
    }
    
    
  } else if (all(as.logical(lapply(inputsL,is.character)))) {
    #inputs are strings
    
    if (!(is.null(length)) && (length(inputsL)>length)) stop('A1D(): too many elements are specified for given length.');
    
    #length specified and inputs are strings
    if (!(is.null(length)))
    { 
      
      for (idx in (1:length)) 
      {
        if (idx > length(inputsL)) { outF=c(outF,'');} else {outF=c(outF,inputsL[[idx]]);}
        
      }
      
      #length not specified and inputs are strings
    } else {
      for (idx in (1:length(inputsL))) 
      {
        outF=c(outF,inputsL[[idx]]);        
      }
    }
    
  } else {
    stop('A1D(): inputs must be all strings or all instances of the class numeric, NA, ts() or xts().');
  }
  
  
  
  return(outF);
}



# TSTRIM code ----------------------------------------

#TSTRIM trims trailing or leading selected values
TSTRIM <- function(x=NULL, VALUE=NA, TRAIL=TRUE, LEAD=TRUE, avoidCompliance=FALSE,...)
{
  if (is.null(x)) stop('TSTRIM(): input needs to be instance of c(), ts() or xts() class.');
  outF=x;
  
  if (length(outF)==0) return(outF)
  
  #get values
  inData=coredata(outF);
  
  #deal with all trimed series
  if (
    (is.na(VALUE)  && all(is.na(inData))) ||
    (!is.na(VALUE) && all(inData==VALUE))
    ) 
  { 
    cat(paste0('TSTRIM(): warning, all values have been trimmed out. Result is a NULL.\n'));
    return(NULL);
  }
  
  newStart=1;
  newEnd=length(inData);
  
  if (is.na(VALUE))
  {
    #get indexes to be kept
    tmpA=which(! is.na(inData));
    if (length(tmpA)>0)
    {
      #change bounds
      if (TRAIL) newStart=tmpA[1];
      if (LEAD) newEnd=tmpA[length(tmpA)];
    }
    
  } else {
    #get indexes to be kept
    tmpA=which((! (inData==VALUE)) | is.na(inData));
    if (length(tmpA)>0)
    {
      #change bounds
      if (TRAIL) newStart=tmpA[1];
      if (LEAD) newEnd=tmpA[length(tmpA)];
    }
  }
  
  if (is.ts(outF) )
  {
    if (! avoidCompliance ) 
    {
      tryCatch({.isCompliant(outF);},error=function(e){stop('TSTRIM():',e$message);});
    }
    
    return(TSERIES(outF[newStart:newEnd],
                   START=normalizeYP(
                            c(start(outF)[1],start(outF)[2]-1+newStart),
                            frequency(outF)),
                   FREQ=frequency(outF)))
    
  } else if (is.xts(outF))
  {
    return(fromTStoXTS(
                  TSTRIM(
                        fromXTStoTS(outF,avoidCompliance=avoidCompliance)
                        ,VALUE=VALUE
                        ,TRAIL=TRAIL
                        ,LEAD=LEAD
                        ,avoidCompliance=TRUE)
                  ,avoidCompliance=TRUE))
    
  } else
  {
    return(outF[newStart:newEnd])
  }
  
}

# TSEXTEND code ----------------------------------------

#TSEXTEND extends time series definition range by a specified criteria
TSEXTEND <- function(x=NULL,BACKTO=NULL,UPTO=NULL,EXTMODE='GROWTH',FACTOR=NA,avoidCompliance=FALSE,...)
{
	
	if (is.null(x)) stop('TSEXTEND(): input needs to be instance of ts() or xts() class.');
	outF=x;
	
	
	
	if (! avoidCompliance ) 
	{
		tryCatch({.isCompliant(x);},error=function(e){stop('TSEXTEND():',e$message);});
	}
	
	
	
	if (is.ts(x))
	{ 
	
		
		#BACKTO not null and before  start(x)			
		if (! is.null(BACKTO))
			if (.isMinorPeriodYP(BACKTO,start(x),frequency(x))) 
			{
				#default value to insert in extension
				value2ins=NA;
				value2ins2=NA;
				
				#TSEXTEND range
				tryCatch({outF=window(outF,start=BACKTO,extend=TRUE);},error=function(e){cat('TSEXTEND():',e$message);});				
				
				
				#select value to insert
				if (EXTMODE=='MISSING') 
				{
					value2ins=NA;
					outF[1:(NUMPERIOD(BACKTO,start(x),frequency(x))+.numberOfStartingMissing(x))]=value2ins;
				}
				
				if (EXTMODE=='ZERO') 
				{
					value2ins=0;
					outF[1:(NUMPERIOD(BACKTO,start(x),frequency(x))+.numberOfStartingMissing(x))]=value2ins;
				}
				
				if (EXTMODE=='CONSTANT') 
				{
					#select first non.missing value									
					i=1;
					while(i<=length(x) && is.na(value2ins))
					{ 
						value2ins=x[i];
						i=i+1;
					}
					
					outF[1:(NUMPERIOD(BACKTO,start(x),frequency(x))+.numberOfStartingMissing(x))]=value2ins;
				}
				
				
				if (EXTMODE=='MYCONST') 
				{
					#as.numeric warns if non numeric input
					suppressWarnings({
						tmpF=	as.numeric(FACTOR);
					});
					
					outF[1:(NUMPERIOD(BACKTO,start(x),frequency(x))+.numberOfStartingMissing(x))]=tmpF;
				}
				
				
				if (EXTMODE=='MEAN4') 
				{
				  value2ins=NA;    		  
				  
				  if (length(x)>3)
				  {
            #mean of first 4 consecutive non.missing value	()								
  					i=1;
  					
  					while(i<=(length(x)-3))
  					{ 
  						#if internal missing stop...
  						if (!(is.na(x[i])) && is.na(x[i+1])) break;
  						
  						if (!(is.na(x[i]) || is.na(x[i+1]) || is.na(x[i+2]) || is.na(x[i+3])))
  						{
  							value2ins=mean(c(x[i],x[i+1],x[i+2],x[i+3]));
  							break;
  						}
  						i=i+1;
  					}
				  }
          
					outF[1:(NUMPERIOD(BACKTO,start(x),frequency(x))+.numberOfStartingMissing(x))]=value2ins;
				}
				
				if (EXTMODE=='GROWTH4') 
				{
				  value2ins=NA;  			  
				  
				  if (length(x)>7)
				  {
            #look code 4 description :)								
  					i=1;
  					
  					while(i<=(length(x)-7))
  					{ 
  						
  						#if internal missing stop...
  						if (!(is.na(x[i])) && is.na(x[i+1])) break;
  						
  						if (!(is.na(x[i]) || is.na(x[i+1]) || is.na(x[i+2]) || is.na(x[i+3])
  									|| is.na(x[i+4])|| is.na(x[i+5])|| is.na(x[i+6])|| is.na(x[i+7])))
  						{
  							value2ins=((x[i]+x[i+1]+x[i+2]+x[i+3])/(x[i+4]+x[i+5]+x[i+6]+x[i+7]))^0.25;
  							break;
  						}
  						i=i+1;
  					}
				  }
					for (idx in ((NUMPERIOD(BACKTO,start(x),frequency(x))+.numberOfStartingMissing(x)):1))
						outF[idx]=value2ins*outF[idx+1];
					
				}
				
				if (EXTMODE=='GROWTH') 
				{
					#look code 4 description :)								
					i=1;
					
					while(i<=(length(x)-1))
					{ 
						#if internal missing stop...
						if (!(is.na(x[i])) && is.na(x[i+1])) break;
						
						if (!(is.na(x[i]) || is.na(x[i+1]) ))
						{
							value2ins=((x[i])/(x[i+1]));
							break;
						}
						i=i+1;
					}
					
					for (idx in ((NUMPERIOD(BACKTO,start(x),frequency(x))+.numberOfStartingMissing(x)):1))
						outF[idx]=value2ins*outF[idx+1];
					
				}
				
				if (EXTMODE=='MYRATE') 
				{
					suppressWarnings({
						tmpF=as.numeric(FACTOR);	
					});
					
					for (idx in ((NUMPERIOD(BACKTO,start(x),frequency(x))+.numberOfStartingMissing(x)):1))
						outF[idx]=tmpF*outF[idx+1];
					
				}
				
				if (EXTMODE=='LINEAR') 
				{
					
				  value2ins=NA;    		  
				  
				  if (length(x)>1)
				  {
            #look code 4 description :)								
  					i=1;
  					
  					while(i<=(length(x)-1))
  					{ 
  						#if internal missing stop...
  						if (!(is.na(x[i])) && is.na(x[i+1])) break;
  						
  						if (!(is.na(x[i]) || is.na(x[i+1]) ))
  						{
  							value2ins=((x[i])-(x[i+1]));
  							break;
  						}
  						i=i+1;
  					}
				  }
					for (idx in ((NUMPERIOD(BACKTO,start(x),frequency(x))+.numberOfStartingMissing(x)):1))
						outF[idx]=value2ins+outF[idx+1];
					
				}
				
				if (EXTMODE=='QUADRATIC') 
				{
				  
          value2ins2=NA;
				  value2ins=NA;
          
          if (length(x)>2)
          {         
          
            #look code 4 description :)								
  					i=1;
  					
  					#if internal missing stop...
  					if (!(is.na(x[i])) && is.na(x[i+1])) {} else
  					while(i<=(length(x)-2))
  					{ 
  						if (!(is.na(x[i]) || is.na(x[i+1]) || is.na(x[i+2]) ))
  						{
  							value2ins=((x[i]-2*x[i+1]+x[i+2])/2);
  							value2ins2=((3*x[i]-4*x[i+1]+x[i+2])/2);
  							break;
  						}
  						i=i+1;
  					}
          }
          
					tempIdx=(NUMPERIOD(BACKTO,start(x),frequency(x))+.numberOfStartingMissing(x))+1;	
					
					for (idx in ((tempIdx-1):1))
						outF[idx]=outF[tempIdx]+(value2ins2+value2ins*(tempIdx-idx))*(tempIdx-idx);
					
				}
				
			}
		
		#UPTO not null and after  end(x)			
		if (! is.null(UPTO))
			if (.isMinorPeriodYP(end(x), UPTO,frequency(x))) 
			{
				#default value to insert in extension
				value2ins=NA;
				value2ins2=NA;
				
				#TSEXTEND range
				tryCatch({outF=window(outF,end=UPTO,extend=TRUE);},error=function(e){cat('TSEXTEND():',e$message);});
				
				if (EXTMODE=='MISSING') 
				{
					value2ins=NA;
					outF[length(outF)-(0:(NUMPERIOD(end(x),UPTO,frequency(x))-1+.numberOfEndingMissing(x)))]=value2ins;
					
				}
				if (EXTMODE=='ZERO') 
				{
					value2ins=0;
					outF[length(outF)-(0:(NUMPERIOD(end(x),UPTO,frequency(x))-1+.numberOfEndingMissing(x)))]=value2ins;
					
				}
				
				if (EXTMODE=='CONSTANT') 
				{
					#select last non.missing value		
					i=length(x);
					value2ins=x[i];
					
					while(i>0 && is.na(value2ins))
					{ value2ins=x[i];
						i=i-1;
					}	
					
					outF[length(outF)-(0:(NUMPERIOD(end(x),UPTO,frequency(x))-1+.numberOfEndingMissing(x)))]=value2ins;
					
				}
				
				
				if (EXTMODE=='MYCONST') 
				{
					
					#as.numeric warns if non numeric input
					suppressWarnings({
						tmpF=as.numeric(FACTOR);
					});
					outF[length(outF)-(0:(NUMPERIOD(end(x),UPTO,frequency(x))-1+.numberOfEndingMissing(x)))]=tmpF;
				}
				
				
				if (EXTMODE=='MEAN4') 
				{
				  value2ins=NA;  			  
				  
				  if (length(x)>3)
				  {
            #mean of last 4 consecutive non.missing value	()								
  					i=1;
  					
  					#if internal missing stop...
  					if (!(is.na(x[length(x)+1-i])) && is.na(x[length(x)+1-(i+1)])) {} else
  					while(i<=(length(x)-3))
  					{ 
  						if (!(is.na(x[length(x)+1-i]) || is.na(x[length(x)+1-(i+1)]) || is.na(x[length(x)+1-(i+2)]) || is.na(x[length(x)+1-(i+3)])))
  						{
  							value2ins=mean(c(x[length(x)+1-i],x[length(x)+1-(i+1)],x[length(x)+1-(i+2)],x[length(x)+1-(i+3)]));
  							break;
  						}
  						i=i+1;
  					}
				  }
          
					outF[length(outF)-(0:(NUMPERIOD(end(x),UPTO,frequency(x))-1+.numberOfEndingMissing(x)))]=value2ins;
					
				}
				
				if (EXTMODE=='GROWTH4') 
				{
				  value2ins=NA;				  
				  
				  if (length(x)>7)
				  {
            #look code 4 description :)							
  					i=1;
  					
  					#if internal missing stop...
  					if (!(is.na(x[length(x)+1-i])) && is.na(x[length(x)+1-(i+1)])) {} else
  					while(i<=(length(x)-7))
  					{ 
  						if (!(is.na(x[length(x)+1-(i)]) || is.na(x[length(x)+1-(i+1)]) || is.na(x[length(x)+1-(i+2)])|| is.na(x[length(x)+1-(i+3)])
  									|| is.na(x[length(x)+1-(i+4)])|| is.na(x[length(x)+1-(i+5)])|| is.na(x[length(x)+1-(i+6)])|| is.na(x[length(x)+1-(i+7)])))
  						{
  							value2ins=((x[length(x)+1-(i)]+x[length(x)+1-(i+1)]+x[length(x)+1-(i+2)]+x[length(x)+1-(i+3)])/(x[length(x)+1-(i+4)]+x[length(x)+1-(i+5)]+x[length(x)+1-(i+6)]+x[length(x)+1-(i+7)]))^0.25;
  							break;
  						}
  						i=i+1;
  					}
				  }
          
					for (idx in (length(outF)-((NUMPERIOD(end(x),UPTO,frequency(x))-1+.numberOfEndingMissing(x)):0)))
						outF[idx]=value2ins*outF[idx-1];
					
					
					
				}
				
				if (EXTMODE=='GROWTH') 
				{
												
					i=1;
					
					#if internal missing stop...
					if (!(is.na(x[length(x)+1-i])) && is.na(x[length(x)+1-(i+1)])) {}
					while(i<=(length(x)-1))
					{ 
						if (!(is.na(x[length(x)+1-(i)]) || is.na(x[length(x)+1-(i+1)]) ))
						{
							value2ins=(x[length(x)+1-(i)]/(x[length(x)+1-(i+1)]));
							break;
						}
						i=i+1;
					}
					
					for (idx in (length(outF)-((NUMPERIOD(end(x),UPTO,frequency(x))-1+.numberOfEndingMissing(x)):0)))
						outF[idx]=value2ins*outF[idx-1];
					
					
				}
				
				if (EXTMODE=='MYRATE') 
				{
					suppressWarnings({
						tmpF=as.numeric(FACTOR);	
					});
					
					for (idx in (length(outF)-((NUMPERIOD(end(x),UPTO,frequency(x))-1+.numberOfEndingMissing(x)):0)))
						outF[idx]=tmpF*outF[idx-1];
					
				}
				
				if (EXTMODE=='LINEAR') 
				{
				  value2ins=NA;      	  
				  
				  if (length(x)>1)
				  {
            #look code 4 description :)							
  					i=1;
  					
  					#if internal missing stop...
  					if (!(is.na(x[length(x)+1-i])) && is.na(x[length(x)+1-(i+1)])) {}
  					while(i<=(length(x)-1))
  					{ 
  						if (!(is.na(x[length(x)+1-(i)]) || is.na(x[length(x)+1-(i+1)]) ))
  						{
  							value2ins=(x[length(x)+1-(i)]-(x[length(x)+1-(i+1)]));
  							break;
  						}
  						i=i+1;
  					}
				  }
					for (idx in (length(outF)-((NUMPERIOD(end(x),UPTO,frequency(x))-1+.numberOfEndingMissing(x)):0)))
						outF[idx]=value2ins+outF[idx-1];
					
					
				}
				
				if (EXTMODE=='QUADRATIC') 
				{
				  value2ins=NA;
          value2ins2=NA;
          
          if (length(x)>2)
          {
            i=1;
  					
  					#if internal missing stop...
  					if (!(is.na(x[length(x)+1-i])) && is.na(x[length(x)+1-(i+1)])) {}
  					while(i<=(length(x)-2))
  					{ 
  						if (!(is.na(x[length(x)+1-(i)]) || is.na(x[length(x)+1-(i+1)]) || is.na(x[length(x)+1-(i+2)]) ))
  						{
  							value2ins =((x[length(x)+1-(i)]-2*x[length(x)+1-(i+1)]+x[length(x)+1-(i+2)])/2);
  							value2ins2=((3*x[length(x)+1-(i)]-4*x[length(x)+1-(i+1)]+x[length(x)+1-(i+2)])/2);
  							break;
  						}
  						i=i+1;
  					}
          }
					tempIdx=length(outF)-(NUMPERIOD(end(x),UPTO,frequency(x))+.numberOfEndingMissing(x));
					
					for (idx in ((tempIdx+1):length(outF)))
						outF[idx]=outF[tempIdx]+(value2ins2+value2ins*(idx-tempIdx))*(idx-tempIdx);
					
				}
				
			}
		
		
		
		
	}#end is.ts()
	
	if (is.xts(x))
	{
		return(fromTStoXTS(TSEXTEND(fromXTStoTS(x,avoidCompliance=avoidCompliance),BACKTO=BACKTO,UPTO=UPTO,EXTMODE=EXTMODE,FACTOR=FACTOR,avoidCompliance=TRUE),avoidCompliance=TRUE));
	}
	
	return(outF);
}

#backward compatibility
EXTEND <- TSEXTEND;

# TSJOIN code ----------------------------------------

#TSJOIN joines two time series.
TSJOIN <- function(x=NULL, y=NULL, JPRD=NULL, ALLOWGAP=FALSE, WARN=FALSE, avoidCompliance=FALSE,...)
{
	
	if (is.null(x)) stop('TSJOIN(): input x needs to be instance of ts() or xts() class.');
	if (is.null(y)) stop('TSJOIN(): input y needs to be instance of ts() or xts() class.');
	
	outF=y;	
	
	if (! avoidCompliance ) 
	{
		tryCatch({.isCompliant(x);},error=function(e){stop('TSJOIN(): x - ',e$message);});
		tryCatch({.isCompliant(y);},error=function(e){stop('TSJOIN(): y - ',e$message);});
	}
	
	
	
	if (is.ts(x) && is.ts(y))
	{	
		if (!(frequency(x) == frequency(y))) stop('TSJOIN(): input time series have different frequencies.');
		
		if (!(is.null(JPRD)))
		{
			if (!(.isCompliantYP(JPRD,frequency(x)))) stop('TSJOIN(): uncompliant JPRD. Use JPRD=c(y,p).');
			if (.isMinorPeriodYP(JPRD,start(y),frequency(x)) || .isMinorPeriodYP(end(y),JPRD,frequency(x))) stop('TSJOIN(): joining period out of bounds.');
		} 
		else {
			JPRD=start(y);
		}
		
		if (NUMPERIOD(end(x),start(y),frequency(x))>1 )
		{
			if (!(ALLOWGAP==TRUE)) stop('TSJOIN(): There is a gap between the two input time series. Use ALLOWGAP=TRUE in order to fill it with missing value.');
			if (WARN) print('TSJOIN(): warning, there is a gap between the two input time series that has been filled with missing values.');
		}
		
		#select out ts start and end
		outFstart=NA;
		#if (.isMinorPeriodYP(start(x),start(y),frequency(x))) 
		if (.isMinorPeriodYP(start(x),JPRD,frequency(x)))
		{
			outFstart=start(x);
		}
		else {
			#outFstart=start(y);
			outFstart=JPRD;
		}
		
		
		outFend=end(y);
    
		#create out ts with missings
		outF=ts(start=outFstart,end=outFend,frequency=frequency(x));
		
		#project x on outF
		endTemp=NA;
		startTemp=NA;
		rangeTemp=NA;
		
		if (.isMinorPeriodYP(start(x),JPRD,frequency(x)))
		{
			if (.isMinorPeriodYP(end(x),JPRD,frequency(x)))
			{
				endTemp=length(x)-1;
			}
			else {
				endTemp=NUMPERIOD(start(x),JPRD,frequency(x))-1;
			}
			startTemp=(1+NUMPERIOD(start(outF),start(x),frequency(x)))
			rangeTemp=(startTemp:(startTemp+endTemp));
			
			outF[rangeTemp]=x[(1:(1+endTemp))];
		}
		else {
			if (WARN) print('TSJOIN(): warning, nothing got from the first time series. The result will be the second time series, in case plus padding.');
		}
		
		#project y on outF
		startTemp=NA;
		rangeTemp=NA;
		endTemp=NA;
		
		
		startTemp=1+NUMPERIOD(start(outF),JPRD,frequency(y));	
		endTemp=NUMPERIOD(JPRD,end(y),frequency(y));
		
		
		
		rangeTemp=(startTemp:(startTemp+endTemp));
		#print(rangeTemp);
		outF[rangeTemp]=y[((1+NUMPERIOD(start(y),JPRD,frequency(y))):length(y))];
		
		
	}#end is.ts()
	
	else if (is.xts(x) && is.xts(y))
	{
		return(fromTStoXTS(TSJOIN(fromXTStoTS(x,avoidCompliance=avoidCompliance),fromXTStoTS(y,avoidCompliance=avoidCompliance),WARN=WARN,JPRD=JPRD,ALLOWGAP=ALLOWGAP,avoidCompliance=TRUE),avoidCompliance=TRUE));
	}
	else
	{
		stop('TSJOIN(): input time series need to be instance of the same class ts() or xts().');
	}	
	return(outF);
}


# SEMIANNUAL code ----------------------------------------

#SEMIANNUAL creates a semiannual time series from an existing time series.
SEMIANNUAL <- function(x=NULL,fun=NULL,avoidCompliance=FALSE,...)
{
	
	if (is.null(x)) stop('SEMIANNUAL(): input time series needs to be instance of ts() or xts() class.');
	
	
	outF=x;	
	
	if (! avoidCompliance ) 
	{
		tryCatch({.isCompliant(x);},error=function(e){stop('SEMIANNUAL(): x - ',e$message);});
		
	}
	
	
	
	if (is.ts(x) )
	{			
		
		#input is semiannual
		if (frequency(x)==2) {outF=x;}
		
		#input is yearly
		else if (frequency(x)==1)
		{
		  if (length(x)<2) stop('SEMIANNUAL(): at least two observations required for interpolation.');
      
      #build out data
			inCD=coredata(x);
			#out data
			outCD=inCD[1];
			
			for (idx in (2:length(inCD)))
			{
				for (idx2 in (1:1))
				{
					outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/2);
				}
				
				outCD=c(outCD,inCD[idx]);
			}
			
			if (is.null(fun))
			{
				outF=ts(rep(coredata(x),each=2),start=start(x),frequency=2);								
			}
			else if (fun=='INTERP_END')		 	
			{			
				
				outF=ts(outCD,start=c(start(x)[1],2),frequency=2);
			}
			else if (fun=='INTERP_BEGIN')		 	
			{
				
				outF=ts(outCD,start=c(start(x)[1],1),frequency=2);				
				
			}
			else stop('SEMIANNUAL(): unknown agg/disagg function (fun) type.');
			
		}
		
		#input is quarterly
		else if (frequency(x)==4)
		{
			
			
			if (is.null(fun))
			{
				stop('SEMIANNUAL(): an option (fun=F) must be given when converting a quarterly time series.');			
			}
			else if (fun=='STOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				if (start(x)[2]<=4) outStartP=2;				
				if (start(x)[2]<=2) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even quarter add to out
					if ((idx + start(x)[2] -1) %% 2 == 0 ) outCD=c(outCD,x[idx]);
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 2 qtr. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else if (fun=='NSTOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				if (start(x)[2]<=4) outStartP=2;				
				if (start(x)[2]<=2) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even quarter add to out
					if ((idx + start(x)[2] -1) %% 2 == 0 ) 
					{
						idx_tmp=0;
						while (is.na(x[idx-idx_tmp]) && idx_tmp<1 && idx_tmp<idx-1) idx_tmp=idx_tmp+1;
						
						outCD=c(outCD,x[idx-idx_tmp]);
					}
					
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 6 months. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else if (fun=='SUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=3;
				if (start(x)[2]<=3) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even quarter and there is 1 qtr before add to out
					if (((idx + start(x)[2] -1) %% 2 == 0 ) && (idx>1)) outCD=c(outCD,sum(x[idx],x[(idx-1)]));
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 2 qtrs. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else if (fun=='NSUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=3;
				if (start(x)[2]<=3) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even quarter and there is 1 qtr before add to out
					if (((idx + start(x)[2] -1) %% 2 == 0 ) && (idx>1)) 
					{ 
						# nsum(NA,NA,NA,...)=NA 
						tempV=sum(x[idx],x[(idx-1)],na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)]) ) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 2 qtrs. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else if (fun=='AVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=3;
				if (start(x)[2]<=3) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even quarter and there is 1 qtr before add to out
					if (((idx + start(x)[2] -1) %% 2 == 0 ) && (idx>1)) outCD=c(outCD,mean(c(x[idx],x[(idx-1)])));
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 2 qtrs. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else if (fun=='NAVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=3;
				if (start(x)[2]<=3) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even quarter and there is 1 qtr before add to out
					if (((idx + start(x)[2] -1) %% 2 == 0 ) && (idx>1)) 
					{ 
						#nave(NA,NA,NA)=NA 
						tempV=mean(c(x[idx],x[(idx-1)]),na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)])) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 2 qtrs. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else stop('SEMIANNUAL(): unknown agg/disagg function (fun) type.');
			
		}
		
		#input is monthly
		else if (frequency(x)==12)
		{
			
			
			if (is.null(fun))
			{
				stop('SEMIANNUAL(): an option (fun=F) must be given when converting a monthly time series.');			
			}
			else if (fun=='STOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				if (start(x)[2]<=12) outStartP=2;				
				if (start(x)[2]<=6) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (jun  dec) add to out
					if ((idx + start(x)[2] -1) %% 6 == 0 ) outCD=c(outCD,x[idx]);
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 6 months. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else if (fun=='NSTOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				if (start(x)[2]<=12) outStartP=2;				
				if (start(x)[2]<=6) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (jun  dec) add to out
					if ((idx + start(x)[2] -1) %% 6 == 0 ) 
					{
						idx_tmp=0;
						while (is.na(x[idx-idx_tmp]) && idx_tmp<5 && idx_tmp<idx-1) idx_tmp=idx_tmp+1;
						
						outCD=c(outCD,x[idx-idx_tmp]);
					}
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 6 months. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else if (fun=='SUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=3;
				if (start(x)[2]<=7) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (jun  dec) and there are 6 month before add to out
					if (((idx + start(x)[2] -1) %% 6 == 0 ) && (idx>5)) outCD=c(outCD,sum(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)],x[(idx-4)],x[(idx-5)]));
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 6 months. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else if (fun=='NSUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=3;
				if (start(x)[2]<=7) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (jun dec) and there are 6 month before add to out
					if (((idx + start(x)[2] -1) %% 6 == 0 ) && (idx>5)) 
					{ 
						#nsum(NA,NA,NA,...)=NA 
						tempV=sum(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)],x[(idx-4)],x[(idx-5)],na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)]) && is.na(x[(idx-2)]) && is.na(x[(idx-3)]) && is.na(x[(idx-4)]) && is.na(x[(idx-5)])) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 6 months. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else if (fun=='AVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=3;
				if (start(x)[2]<=7) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (jun  dec) and there are 6 month before add to out
					if (((idx + start(x)[2] -1) %% 6 == 0 ) && (idx>5)) outCD=c(outCD,mean(c(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)],x[(idx-4)],x[(idx-5)])));
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 6 months. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else if (fun=='NAVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=3;
				if (start(x)[2]<=7) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (jun  dec) and there are 6 month before add to out
					if (((idx + start(x)[2] -1) %% 6 == 0 ) && (idx>5)) 
					{ 
						#nave(NA,NA,NA)=NA 
						tempV=mean(c(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)],x[(idx-4)],x[(idx-5)]),na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)]) && is.na(x[(idx-2)]) && is.na(x[(idx-3)]) && is.na(x[(idx-4)]) && is.na(x[(idx-5)])) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span 6 month. Nothing defined.');
				if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
				
			}
			else stop('SEMIANNUAL(): unknown agg/disagg function (fun) type.');
			
		}
		
		#input is daily
		else if (frequency(x)==366)
		{
		  
		  
		  if (is.null(fun))
		  {
		    stop('SEMIANNUAL(): an option (fun=F) must be given when converting a daily time series.');			
		  }
		  else if (fun=='STOCK')		 	
		  {	
		  	#start period of out ts
		  	outStartP=NA;
		  	#out data
		  	outCD=NULL;
		  	
		  	if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		  	{ #day is 366 and not bisextile
		  		outStartP=3;
		  	} else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
		  	{
		  		outStartP=1;
		  	} else {
		  		outStartP=2;
		  	}
		  	
		  	
		  	
		  	for (idx in (1:length(x)))
		  	{
		  		if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		  		{
		  			if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') outCD=c(outCD,x[idx]);
		  			if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') outCD=c(outCD,x[idx]);
		  		}
		  	}
		  	
		  	if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span a semester. Nothing defined.');
		  	if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
		  	
		  	outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
		  	
		  }
		  else if (fun=='NSTOCK')		 	
		  {	
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
        {
          outStartP=3;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
        {
		      outStartP=1;
        } else {
          outStartP=2;
        }
        
        
		    
		    for (idx in (1:length(x)))
		    {
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') 
		        {
		        	idx_tmp=0;
		        	while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE))>6) idx_tmp=idx_tmp+1
		        	outCD=c(outCD,x[idx-idx_tmp]);
		        }
		        
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') 
		        {
		        	idx_tmp=0;
		        	while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE))<7) idx_tmp=idx_tmp+1
		        	outCD=c(outCD,x[idx-idx_tmp]);
		        }
		        
		      }
		    }
		    
		    if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span a semester. Nothing defined.');
		    if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
		    
		  }
		  else if (fun=='SUM')		 	
		  {	
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first 6m if we dont have all days
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=3;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='01-01')
		    {
		      outStartP=1;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
		    {
		      outStartP=2;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='07-01')
		    {
		      outStartP=2;
		    } else {
		      outStartP=3;
		    }
		    
		    #tmp for sum
		    tmpOut=0;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='07-01'))
		        {
		          tmpOut=x[idx];
		          canInsert=TRUE;
		        } else {
		          tmpOut=x[idx]+tmpOut;
		        }
		        
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31'))
		        {
		          if (canInsert) outCD=c(outCD,tmpOut);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span a semester. Nothing defined.');
		    if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
		    
		  }
		  else if (fun=='NSUM')		 	
		  {  
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first 6m if we dont have all days
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=3;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='01-01')
		    {
		      outStartP=1;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
		    {
		      outStartP=2;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='07-01')
		    {
		      outStartP=2;
		    } else {
		      outStartP=3;
		    }
		    
		    #tmp for sum
		    tmpOut=NA;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx)))
		      {
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='07-01'))
		        {
		          tmpOut=NA;
		          if (! is.na(x[idx])) tmpOut=x[idx];
		          canInsert=TRUE;
		        } else {
		          
		          if ((! is.na(tmpOut)) && (! is.na(x[idx]))) tmpOut=x[idx]+tmpOut;
		          if (( is.na(tmpOut)) && (! is.na(x[idx]))) tmpOut=x[idx];
		        }
		        
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31'))
		        {
		          if (canInsert) outCD=c(outCD,tmpOut);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span a semester. Nothing defined.');
		    if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
		    
		  }
		  else if (fun=='AVE')		 	
		  {  
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first 6m if we dont have all days
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=3;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='01-01')
		    {
		      outStartP=1;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
		    {
		      outStartP=2;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='07-01')
		    {
		      outStartP=2;
		    } else {
		      outStartP=3;
		    }
		    
		    #tmp for ave
		    tmpOut=0;
		    tmpCnt=1;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='07-01'))
		        {
		          tmpOut=x[idx];
		          tmpCnt=1;
		          canInsert=TRUE;
		        } else {
		          tmpOut=x[idx]+tmpOut;
		          tmpCnt=1+tmpCnt;
		        }
		        
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31'))
		        {
		          if (canInsert) outCD=c(outCD,tmpOut/tmpCnt);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span a semester. Nothing defined.');
		    if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
		    
		  }
		  else if (fun=='NAVE')		 	
		  {  
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first 6m if we dont have all days
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=3;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='01-01')
		    {
		      outStartP=1;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
		    {
		      outStartP=2;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='07-01')
		    {
		      outStartP=2;
		    } else {
		      outStartP=3;
		    }
		    
		    #tmp for sum
		    tmpOut=NA;
		    tmpCnt=0;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx)))
		      {
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='07-01'))
		        {
		          tmpOut=NA;
		          if (! is.na(x[idx])) 
		          {
		            tmpOut=x[idx];
		            tmpCnt=1;
		          }
		          canInsert=TRUE;
		        } else {
		          
		          if ((! is.na(tmpOut)) && (! is.na(x[idx]))) 
		          {
		            tmpOut=x[idx]+tmpOut;
		            tmpCnt=1+tmpCnt;
		          }
		          if (( is.na(tmpOut)) && (! is.na(x[idx]))) 
		          {
		            tmpOut=x[idx];
		            tmpCnt=1;
		          }
		        }
		        
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31'))
		        {
		          if (canInsert) outCD=c(outCD,tmpOut/tmpCnt);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('SEMIANNUAL(): input time series does not span a semester. Nothing defined.');
		    if (length(outCD)<2) print('SEMIANNUAL(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=2);
		    
		  }
		  else stop('SEMIANNUAL(): unknown agg/disagg function (fun) type.');
		  
		}
    
    else {stop('SEMIANNUAL(): unsupported input frequency.');}
		
	}#end is.ts()
	
	if (is.xts(x) )
	{
		return(fromTStoXTS(SEMIANNUAL(fromXTStoTS(x,avoidCompliance=avoidCompliance),fun=fun,avoidCompliance=TRUE),avoidCompliance=TRUE));
	}
	
	return(outF);
}



# QUARTERLY code ----------------------------------------

#QUARTERLY creates a quarterly time series from an existing time series.
QUARTERLY <- function(x=NULL,fun=NULL,avoidCompliance=FALSE,...)
{
	
	if (is.null(x)) stop('QUARTERLY(): input time series needs to be instance of ts() or xts() class.');
	
	
	outF=x;	
	
	if (! avoidCompliance ) 
	{
		tryCatch({.isCompliant(x);},error=function(e){stop('QUARTERLY(): x - ',e$message);});		
	}
	
	
	
	if (is.ts(x) )
	{			
		
		#input is quarterly
		if (frequency(x)==4) {outF=x;}
		
		#input is yearly
		else if (frequency(x)==1)
		{
		  if (length(x)<2) stop('QUARTERLY(): at least two observations required for interpolation.');
      
      #build out data
			inCD=coredata(x);
			#out data
			outCD=inCD[1];
			
			for (idx in (2:length(inCD)))
			{
				for (idx2 in (1:3))
				{
					outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/4);
				}
				
				outCD=c(outCD,inCD[idx]);
			}
			
			if (is.null(fun))
			{
				outF=ts(rep(coredata(x),each=4),start=start(x),frequency=4);								
			}
			else if (fun=='INTERP_END')		 	
			{
				
				
				outF=ts(outCD,start=c(start(x)[1],4),frequency=4);
			}
			else if (fun=='INTERP_CENTER')		 	
			{
				
				outF=ts(outCD,start=c(start(x)[1],3),frequency=4);
				
			}
			else if (fun=='INTERP_BEGIN')		 	
			{
				
				outF=ts(outCD,start=c(start(x)[1],1),frequency=4);
				
				
			}
			else stop('QUARTERLY(): unknown agg/disagg function (fun) type.');
			
		}
		
		#input is semiannual
		else if (frequency(x)==2)
		{
		  if (length(x)<2) stop('QUARTERLY(): at least two observations required for interpolation.');
      
      #build out data
			inCD=coredata(x);
			#out data
			outCD=inCD[1];
			
			for (idx in (2:length(inCD)))
			{
				for (idx2 in (1:1))
				{
					outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/2)
				}
				
				outCD=c(outCD,inCD[idx]);
			}
			
			if (is.null(fun))
			{
				if (start(x)[2]==1) outF=ts(rep(coredata(x),each=2),start=c(start(x)[1],1),frequency=4);	
				if (start(x)[2]==2) outF=ts(rep(coredata(x),each=2),start=c(start(x)[1],3),frequency=4);
			}
			else if (fun=='INTERP_END')		 	
			{
				if (start(x)[2]==1) outF=ts(outCD,start=c(start(x)[1],2),frequency=4);
				if (start(x)[2]==2) outF=ts(outCD,start=c(start(x)[1],4),frequency=4);
			}
			else if (fun=='INTERP_CENTER')		 	
			{
				if (start(x)[2]==1) 	outF=ts(outCD,start=c(start(x)[1],2),frequency=4);
				if (start(x)[2]==2) 	outF=ts(outCD,start=c(start(x)[1],4),frequency=4);
			}
			else if (fun=='INTERP_BEGIN')		 	
			{	
				if (start(x)[2]==1) 	outF=ts(outCD,start=c(start(x)[1],1),frequency=4);
				if (start(x)[2]==2) 	outF=ts(outCD,start=c(start(x)[1],3),frequency=4);
			}
			else stop('QUARTERLY(): unknown agg/disagg function (fun) type.');
			
		}
		
		#input is monthly
		else if (frequency(x)==12)
		{
			
			
			if (is.null(fun))
			{
				stop('QUARTERLY(): an option (fun=F) must be given when converting a monthly time series.');			
			}
			else if (fun=='STOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				if (start(x)[2]<=12) outStartP=4;
				if (start(x)[2]<=9) outStartP=3;
				if (start(x)[2]<=6) outStartP=2;
				if (start(x)[2]<=3) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (mar jun sep dec) add to out
					if ((idx + start(x)[2] -1) %% 3 == 0 ) outCD=c(outCD,x[idx]);
				}
				
				if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
				if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
				
			}
			else if (fun=='NSTOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				if (start(x)[2]<=12) outStartP=4;
				if (start(x)[2]<=9) outStartP=3;
				if (start(x)[2]<=6) outStartP=2;
				if (start(x)[2]<=3) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (mar jun sep dec) add to out
					if ((idx + start(x)[2] -1) %% 3 == 0 ) 
					{
						idx_tmp=0;
						while (is.na(x[idx-idx_tmp]) && idx_tmp<2 && idx_tmp< idx-1) idx_tmp=idx_tmp+1;
						outCD=c(outCD,x[idx-idx_tmp]);
					}
					
				}
				
				if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
				if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
				
			}
			else if (fun=='SUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=5;
				if (start(x)[2]<=10) outStartP=4;
				if (start(x)[2]<=7) outStartP=3;
				if (start(x)[2]<=4) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (mar jun sep dec) and there are 2 month before add to out
					if (((idx + start(x)[2] -1) %% 3 == 0 ) && (idx>2)) outCD=c(outCD,sum(x[idx],x[(idx-1)],x[(idx-2)]));
				}
				
				if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
				if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
				
			}
			else if (fun=='NSUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=5;
				if (start(x)[2]<=10) outStartP=4;
				if (start(x)[2]<=7) outStartP=3;
				if (start(x)[2]<=4) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (mar jun sep dec) and there are 2 month before add to out
					if (((idx + start(x)[2] -1) %% 3 == 0 ) && (idx>2)) 
					{ 
						#nsum(NA,NA,NA)=NA 
						tempV=sum(x[idx],x[(idx-1)],x[(idx-2)],na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)]) && is.na(x[(idx-2)])) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
				if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
				
			}
			else if (fun=='AVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=5;
				if (start(x)[2]<=10) outStartP=4;
				if (start(x)[2]<=7) outStartP=3;
				if (start(x)[2]<=4) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (mar jun sep dec) and there are 2 month before add to out
					if (((idx + start(x)[2] -1) %% 3 == 0 ) && (idx>2)) outCD=c(outCD,mean(c(x[idx],x[(idx-1)],x[(idx-2)])));
				}
				
				if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
				if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
				
			}
			else if (fun=='NAVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=5;
				if (start(x)[2]<=10) outStartP=4;
				if (start(x)[2]<=7) outStartP=3;
				if (start(x)[2]<=4) outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (mar jun sep dec) and there are 2 month before add to out
					if (((idx + start(x)[2] -1) %% 3 == 0 ) && (idx>2)) 
					{ 
						#nave(NA,NA,NA)=NA 
						tempV=mean(c(x[idx],x[(idx-1)],x[(idx-2)]),na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)]) && is.na(x[(idx-2)])) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
				if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
				
			}
			else stop('QUARTERLY(): unknown agg/disagg function (fun) type.');
			
		}
    
		#input is daily
		else if (frequency(x)==366)
		{
		  
		  
		  if (is.null(fun))
		  {
		    stop('QUARTERLY(): an option (fun=F) must be given when converting a daily time series.');			
		  }
		  else if (fun=='STOCK')		 	
		  {	
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=5;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<4)
		    {
		      outStartP=1;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
		    {
		      outStartP=2;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<10)
		    {
		      outStartP=3;
		    } else {
		      outStartP=4;
		    }
		    
		    
		    
		    for (idx in (1:length(x)))
		    {
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') outCD=c(outCD,x[idx]);
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') outCD=c(outCD,x[idx]);
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='03-31') outCD=c(outCD,x[idx]);
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='09-30') outCD=c(outCD,x[idx]);
		      }
		    }
		    
		    if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
		    if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
		    
		  }
		  else if (fun=='NSTOCK')		 	
		  {	
		  	#start period of out ts
		  	outStartP=NA;
		  	#out data
		  	outCD=NULL;
		  	
		  	if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		  	{
		  		outStartP=5;
		  	} else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<4)
		  	{
		  		outStartP=1;
		  	} else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
		  	{
		  		outStartP=2;
		  	} else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<10)
		  	{
		  		outStartP=3;
		  	} else {
		  		outStartP=4;
		  	}
		  	
		  	
		  	
		  	for (idx in (1:length(x)))
		  	{
		  		if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		  		{
		  			if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE))>9) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp]);
		  				
		  			
		  			}
		  			
		  			if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE))>3) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp]);
		  				
		  			}
		  			
		  			if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='03-31') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE))<4) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp]);
		  				
		  			}
		  			 
		  			if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='09-30') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE))>6) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp]);
		  				
		  			}
		  			 
		  		}
		  	}
		  	
		  	if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
		  	if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
		  	
		  	outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
		  	
		  }
		  else if (fun=='SUM')		 	
		  {	
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first q if we dont have all days
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=5;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='01-01')
		    {
		      outStartP=1;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<4)
		    {
		      outStartP=2;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='04-01')
		    {
		      outStartP=2;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
		    {
		      outStartP=3;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='07-01')
		    {
		      outStartP=3;
		    } else if (as.numeric(GETDATE(x,1,format='%m,avoidCompliance=TRUE'))<10)
		    {
		      outStartP=4;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='10-01')
		    {
		      outStartP=4;
		    } else {
		      outStartP=5;
		    }
		    
		    #tmp for sum
		    tmpOut=0;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='07-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='04-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='10-01')  )
		        {
		          tmpOut=x[idx];
		          canInsert=TRUE;
		        } else {
		          tmpOut=x[idx]+tmpOut;
		        }
		        
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='03-31') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='09-30')  )
		        {
		          if (canInsert) outCD=c(outCD,tmpOut);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
		    if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
		    
		  }
		  else if (fun=='NSUM')		 	
		  {  
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first q if we dont have all days
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=5;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='01-01')
		    {
		      outStartP=1;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<4)
		    {
		      outStartP=2;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='04-01')
		    {
		      outStartP=2;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
		    {
		      outStartP=3;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='07-01')
		    {
		      outStartP=3;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<10)
		    {
		      outStartP=4;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='10-01')
		    {
		      outStartP=4;
		    } else {
		      outStartP=5;
		    }
		    
		    #tmp for sum
		    tmpOut=NA;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx)))
		      {
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='07-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='04-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='10-01')  )
		        {
		          tmpOut=NA;
		          if (! is.na(x[idx])) tmpOut=x[idx];
		          canInsert=TRUE;
		        } else {
		          
		          if ((! is.na(tmpOut)) && (! is.na(x[idx]))) tmpOut=x[idx]+tmpOut;
		          if (( is.na(tmpOut)) && (! is.na(x[idx]))) tmpOut=x[idx];
		        }
		        
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='03-31') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='09-30')  )
		        {
		          if (canInsert) outCD=c(outCD,tmpOut);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
		    if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
		    
		  }
		  else if (fun=='AVE')		 	
		  {  
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first q if we dont have all days
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=5;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='01-01')
		    {
		      outStartP=1;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<4)
		    {
		      outStartP=2;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='04-01')
		    {
		      outStartP=2;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
		    {
		      outStartP=3;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='07-01')
		    {
		      outStartP=3;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<10)
		    {
		      outStartP=4;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='10-01')
		    {
		      outStartP=4;
		    } else {
		      outStartP=5;
		    }
		    
		    #tmp for ave
		    tmpOut=0;
		    tmpCnt=1;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx)))
		      {
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='07-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='04-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='10-01')  )
		        {
		          tmpOut=x[idx];
		          tmpCnt=1;
		          canInsert=TRUE;
		        } else {
		          tmpOut=x[idx]+tmpOut;
		          tmpCnt=1+tmpCnt;
		        }
		        
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='03-31') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='09-30')  )
		        {
		          if (canInsert) outCD=c(outCD,tmpOut/tmpCnt);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
		    if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
		    
		  }
		  else if (fun=='NAVE')		 	
		  {  
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first q if we dont have all days
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=5;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='01-01')
		    {
		      outStartP=1;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<4)
		    {
		      outStartP=2;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='04-01')
		    {
		      outStartP=2;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<7)
		    {
		      outStartP=3;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='07-01')
		    {
		      outStartP=3;
		    } else if (as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))<10)
		    {
		      outStartP=4;
		    } else if (GETDATE(x,1,format='%m-%d',avoidCompliance=TRUE)=='10-01')
		    {
		      outStartP=4;
		    } else {
		      outStartP=5;
		    }
		    
		    #tmp for sum
		    tmpOut=NA;
		    tmpCnt=0;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx)))
		      {
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='07-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='04-01') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='10-01')  )
		        {
		          tmpOut=NA;
		          if (! is.na(x[idx])) 
		          {
		            tmpOut=x[idx];
		            tmpCnt=1;
		          }
		          canInsert=TRUE;
		        } else {
		          
		          if ((! is.na(tmpOut)) && (! is.na(x[idx]))) 
		          {
		            tmpOut=x[idx]+tmpOut;
		            tmpCnt=1+tmpCnt;
		          }
		          if (( is.na(tmpOut)) && (! is.na(x[idx]))) 
		          {
		            tmpOut=x[idx];
		            tmpCnt=1;
		          }
		        }
		        
		        if ((GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='03-31') || (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='09-30')  )
		        {
		          if (canInsert) outCD=c(outCD,tmpOut/tmpCnt);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('QUARTERLY(): input time series does not span a quarter. Nothing defined.');
		    if (length(outCD)<2) print('QUARTERLY(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=4);
		    
		  }
		  else stop('QUARTERLY(): unknown agg/disagg function (fun) type.');
		  
		}
		
		else {stop('QUARTERLY(): unsupported input frequency.');}
		
	}#end is.ts()
	
	if (is.xts(x) )
	{
		return(fromTStoXTS(QUARTERLY(fromXTStoTS(x,avoidCompliance=avoidCompliance),fun=fun,avoidCompliance=TRUE),avoidCompliance=TRUE));
	}
	
	return(outF);
}



# ANNUAL code ----------------------------------------

#ANNUAL creates a annual time series from an existing time series.
ANNUAL <- function(x=NULL,fun=NULL,avoidCompliance=FALSE,...)
{
	
	if (is.null(x)) stop('ANNUAL(): input time series needs to be instance of ts() or xts() class.');
	
	
	outF=x;	
	
	if (! avoidCompliance ) 
	{
		tryCatch({.isCompliant(x);},error=function(e){stop('ANNUAL(): x - ',e$message);});
		
	}
	
	
	
	if (is.ts(x) )
	{			
		
		#input is annual
		if (frequency(x)==1) {outF=x;}
		
		#input is semiannual
		else if (frequency(x)==2)
		{
			
			
			if (is.null(fun))
			{
				stop('ANNUAL(): an option (fun=F) must be given when converting a semiannual time series.');			
			}
			else if (fun=='STOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even semester add to out
					if ((idx + start(x)[2] -1) %% 2 == 0 ) outCD=c(outCD,x[idx]);
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='NSTOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even semester add to out
					if ((idx + start(x)[2] -1) %% 2 == 0 ) 
					{
						idx_tmp=0;
						while(is.na(x[idx-idx_tmp] && idx_tmp<1 && idx_tmp < idx-1 )) idx_tmp=idx_tmp+1;
						outCD=c(outCD,x[idx]);
					}
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='SUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even quarter and there is 1 qtr before add to out
					if (((idx + start(x)[2] -1) %% 2 == 0 ) && (idx>1)) outCD=c(outCD,sum(x[idx],x[(idx-1)]));
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='NSUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even quarter and there is 1 qtr before add to out
					if (((idx + start(x)[2] -1) %% 2 == 0 ) && (idx>1)) 
					{ 
						#nsum(NA,NA,NA,...)=NA 
						tempV=sum(x[idx],x[(idx-1)],na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)]) ) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='AVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even quarter and there is 1 qtr before add to out
					if (((idx + start(x)[2] -1) %% 2 == 0 ) && (idx>1)) outCD=c(outCD,mean(c(x[idx],x[(idx-1)])));
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='NAVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if even quarter and there is 1 qtr before add to out
					if (((idx + start(x)[2] -1) %% 2 == 0 ) && (idx>1)) 
					{ 
						#nave(NA,NA,NA)=NA 
						tempV=mean(c(x[idx],x[(idx-1)]),na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)])) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else stop('ANNUAL(): unknown agg/disagg function (fun) type.');
			
		}
		
		#input is QUARTERLY
		else if (frequency(x)==4)
		{
			
			
			if (is.null(fun))
			{
				stop('ANNUAL(): an option (fun=F) must be given when converting a quarterly time series.');			
			}
			else if (fun=='STOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if 4th quarter add to out
					if ((idx + start(x)[2] -1) %% 4 == 0 ) outCD=c(outCD,x[idx]);
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='NSTOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if 4th quarter add to out
					if ((idx + start(x)[2] -1) %% 4 == 0 ) 
					{
						idx_tmp=0;
						while(is.na(x[idx-idx_tmp]) && idx_tmp < 3 && idx_tmp < idx -1) idx_tmp=idx_tmp+1;
						outCD=c(outCD,x[idx-idx_tmp]);
					}
					
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='SUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if 4th quarter and there is 3 qtr before add to out
					if (((idx + start(x)[2] -1) %% 4 == 0 ) && (idx>3)) outCD=c(outCD,sum(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)]));
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='NSUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if 4th quarter and there is 3 qtr before add to out
					if (((idx + start(x)[2] -1) %% 4 == 0 ) && (idx>3)) 
					{ 
						#nsum(NA,NA,NA,...)=NA 
						tempV=sum(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)],na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)])  && is.na(x[(idx-2)]) && is.na(x[(idx-3)])) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='AVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if 4th quarter and there is 3 qtr before add to out
					if (((idx + start(x)[2] -1) %% 4 == 0 ) && (idx>3)) outCD=c(outCD,mean(c(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)])));
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='NAVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if 4th quarter and there is 3 qtr before add to out
					if (((idx + start(x)[2] -1) %% 4 == 0 ) && (idx>3)) 
					{ 
						#nave(NA,NA,NA)=NA 
						tempV=mean(c(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)]),na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)])  && is.na(x[(idx-2)]) && is.na(x[(idx-3)])) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else stop('ANNUAL(): unknown agg/disagg function (fun) type.');
			
		}
		
		#input is monthly
		else if (frequency(x)==12)
		{
			
			
			if (is.null(fun))
			{
				stop('ANNUAL(): an option (fun=F) must be given when converting a monthly time series.');			
			}
			else if (fun=='STOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (dec) add to out
					if ((idx + start(x)[2] -1) %% 12 == 0 ) outCD=c(outCD,x[idx]);
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='NSTOCK')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (dec) add to out
					if ((idx + start(x)[2] -1) %% 12 == 0 ) 
					{
						idx_tmp=0;
						while (is.na(x[idx-idx_tmp]) && idx_tmp < 11 && idx_tmp< idx-1) {idx_tmp=idx_tmp+1;}
						outCD=c(outCD,x[idx-idx_tmp]);
					}
					
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='SUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;				
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (dec) and there are 11 month before add to out
					if (((idx + start(x)[2] -1) %% 12 == 0 ) && (idx>11)) outCD=c(outCD,sum(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)],x[(idx-4)],x[(idx-5)],x[idx-6],x[(idx-7)],x[(idx-8)],x[(idx-9)],x[(idx-10)],x[(idx-11)]));
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='NSUM')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;				
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (dec) and there are 11 month before add to out
					if (((idx + start(x)[2] -1) %% 12 == 0 ) && (idx>11)) 
					{ 
						#nsum(NA,NA,NA,...)=NA 
						tempV=sum(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)],x[(idx-4)],x[(idx-5)],x[idx-6],x[(idx-7)],x[(idx-8)],x[(idx-9)],x[(idx-10)],x[(idx-11)],na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)]) && is.na(x[(idx-2)]) && is.na(x[(idx-3)]) && is.na(x[(idx-4)]) && is.na(x[(idx-5)])
								&& is.na(x[(idx-6)]) && is.na(x[(idx-7)]) && is.na(x[(idx-8)]) && is.na(x[(idx-9)]) && is.na(x[(idx-10)]) && is.na(x[(idx-11)])) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='AVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;				
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (dec) and there are 11 month before add to out
					if (((idx + start(x)[2] -1) %% 12 == 0 ) && (idx>11)) outCD=c(outCD,mean(c(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)],x[(idx-4)],x[(idx-5)],x[idx-6],x[(idx-7)],x[(idx-8)],x[(idx-9)],x[(idx-10)],x[(idx-11)])));
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else if (fun=='NAVE')		 	
			{	
				#start period of out ts
				outStartP=NA;
				#out data
				outCD=NULL;
				
				outStartP=2;				
				if (start(x)[2]<=1) outStartP=1;
				
				for (idx in (1:length(x)))
				{
					#if month == (dec) and there are 11 month before add to out
					if (((idx + start(x)[2] -1) %% 12 == 0 ) && (idx>11)) 
					{ 
						#nave(NA,NA,NA)=NA 
						tempV=mean(c(x[idx],x[(idx-1)],x[(idx-2)],x[(idx-3)],x[(idx-4)],x[(idx-5)],x[idx-6],x[(idx-7)],x[(idx-8)],x[(idx-9)],x[(idx-10)],x[(idx-11)]),na.rm=TRUE);
						if (is.na(x[(idx)]) && is.na(x[(idx-1)]) && is.na(x[(idx-2)]) && is.na(x[(idx-3)]) && is.na(x[(idx-4)]) && is.na(x[(idx-5)]) && is.na(x[(idx-6)]) && is.na(x[(idx-7)]) && is.na(x[(idx-8)]) && is.na(x[(idx-9)]) && is.na(x[(idx-10)]) && is.na(x[(idx-11)])) tempV=NA;
						outCD=c(outCD,tempV);
					}
				}
				
				if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
				if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
				
				outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
				
			}
			else stop('ANNUAL(): unknown agg/disagg function (fun) type.');
			
		}
		
		#input is daily
		else if (frequency(x)==366)
		{
		  
		  
		  if (is.null(fun))
		  {
		    stop('ANNUAL(): an option (fun=F) must be given when converting a daily time series.');			
		  }
		  else if (fun=='STOCK')		 	
		  {	
		    
		    #out data
		    outCD=NULL;
		    
		    outStartP=1;
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) outStartP=2;
        
		    for (idx in (1:length(x)))
		    {
          if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
          {
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') outCD=c(outCD,x[idx]);
          }
		    }
		    
		    if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
		    if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
		    
		  }
		  else if (fun=='NSTOCK')		 	
		  {	
		  	
		  	#out data
		  	outCD=NULL;
		  	
		  	outStartP=1;
		  	if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) outStartP=2;
		  	
		  	for (idx in (1:length(x)))
		  	{
		  		if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		  		{
		  			if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 &&  GETDATE(x,idx-idx_tmp,format='%y',avoidCompliance=TRUE)==GETDATE(x,idx,format='%y',avoidCompliance=TRUE)) idx_tmp=idx_tmp+1;
		  				outCD=c(outCD,x[idx-idx_tmp]);
		  			}
		  			
		  		}
		  	}
		  	
		  	if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
		  	if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
		  	
		  	outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
		  	
		  }
		  else if (fun=='SUM')		 	
		  {	
		   
		    #out data
		    outCD=NULL;
		    
        #skip first year if we dont have all days
		    outStartP=2;				
		    if (start(x)[2]<=1) outStartP=1;
		    
        #tmp for sum
		    tmpOut=0;
        
        #flag for skip incomplete years
		    canInsert=FALSE;
        
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
  		    {
            if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') 
            {
              tmpOut=x[idx];
              canInsert=TRUE;
  		      } else {
  		        tmpOut=x[idx]+tmpOut;
  		      }
            
  		      if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31')
  		      {
              if (canInsert) outCD=c(outCD,tmpOut);
  		      }
		      }
          
		    }
		    
		    if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
		    if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
		    
		  }
		  else if (fun=='NSUM')		 	
		  {  
		    
		    #out data
		    outCD=NULL;
		    
		    #skip first year if we dont have all days
		    outStartP=2;				
		    if (start(x)[2]<=1) outStartP=1;
		    
		    #tmp for sum
		    tmpOut=NA;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') 
		        {
		          tmpOut=NA;
              if (! is.na(x[idx])) tmpOut=x[idx];
		          canInsert=TRUE;
		        } else {
                
		          if ((! is.na(tmpOut)) && (! is.na(x[idx]))) tmpOut=x[idx]+tmpOut;
              if (( is.na(tmpOut)) && (! is.na(x[idx]))) tmpOut=x[idx];
		        }
		        
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31')
		        {
		          if (canInsert) outCD=c(outCD,tmpOut);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
		    if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
		    
		  }
		  else if (fun=='AVE')		 	
		  {  
		    
		    #out data
		    outCD=NULL;
		    
		    #skip first year if we dont have all days
		    outStartP=2;				
		    if (start(x)[2]<=1) outStartP=1;
		    
		    #tmp for ave
		    tmpOut=0;
        tmpCnt=1;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') 
		        {
		          tmpOut=x[idx];
		          tmpCnt=1;
		          canInsert=TRUE;
		        } else {
		          tmpOut=x[idx]+tmpOut;
		          tmpCnt=1+tmpCnt;
		        }
		        
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31')
		        {
		          if (canInsert) outCD=c(outCD,tmpOut/tmpCnt);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
		    if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
		    
		  }
		  else if (fun=='NAVE')		 	
		  {  
		    
		    #out data
		    outCD=NULL;
		    
		    #skip first year if we dont have all days
		    outStartP=2;				
		    if (start(x)[2]<=1) outStartP=1;
		    
		    #tmp for sum
		    tmpOut=NA;
		    tmpCnt=0;
        
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-01') 
		        {
		          tmpOut=NA;
		          if (! is.na(x[idx])) 
              {
                tmpOut=x[idx];
                tmpCnt=1;
		          }
		          canInsert=TRUE;
		        } else {
		          
		          if ((! is.na(tmpOut)) && (! is.na(x[idx]))) 
              {
                tmpOut=x[idx]+tmpOut;
                tmpCnt=1+tmpCnt;
		          }
		          if (( is.na(tmpOut)) && (! is.na(x[idx]))) 
              {
                tmpOut=x[idx];
                tmpCnt=1;
		          }
		        }
		        
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31')
		        {
		          if (canInsert) outCD=c(outCD,tmpOut/tmpCnt);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('ANNUAL(): input time series does not span a year. Nothing defined.');
		    if (length(outCD)<2) print('ANNUAL(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=1);
		    
		  }
		  else stop('ANNUAL(): unknown agg/disagg function (fun) type.');
		  
		}
    
		else {stop('ANNUAL(): unsupported input frequency.');}
		
	}#end is.ts()
	
	if (is.xts(x) )
	{
		return(fromTStoXTS(ANNUAL(fromXTStoTS(x,avoidCompliance=avoidCompliance),fun=fun,avoidCompliance=TRUE),avoidCompliance=TRUE));
	}
	
	return(outF);
}



# YEARLY code ----------------------------------------

YEARLY <- ANNUAL

# MONTHLY code ----------------------------------------

#MONTHLY creates a monthly time series from an existing time series.
MONTHLY <- function(x=NULL,fun=NULL,avoidCompliance=FALSE,...)
{
	
	if (is.null(x)) stop('MONTHLY(): input time series needs to be instance of ts() or xts() class.');
	
	
	outF=x;	
	
	if (! avoidCompliance ) 
	{
		tryCatch({.isCompliant(x);},error=function(e){stop('MONTHLY(): x - ',e$message);});
		
	}
	
	
	
	if (is.ts(x) )
	{			
		if (length(x)<2) stop('MONTHLY(): at least two observations required for interpolation.');
    
		#input is monthly
		if (frequency(x)==12) {outF=x;}
		
		#input is yearly
		else if (frequency(x)==1)
		{
			#build out data
			inCD=coredata(x);
			#out data
			outCD=inCD[1];
			
			for (idx in (2:length(inCD)))
			{
				for (idx2 in (1:11))
				{
					outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/12);
				}
				
				outCD=c(outCD,inCD[idx]);
			}
			
			if (is.null(fun))
			{
				outF=ts(rep(coredata(x),each=12),start=start(x),frequency=12);								
			}
			else if (fun=='INTERP_END')		 	
			{
				
				
				outF=ts(outCD,start=c(start(x)[1],12),frequency=12);
			}
			else if (fun=='INTERP_CENTER')		 	
			{
				
				outF=ts(outCD,start=c(start(x)[1],7),frequency=12);
				
			}
			else if (fun=='INTERP_BEGIN')		 	
			{
				
				outF=ts(outCD,start=c(start(x)[1],1),frequency=12);
				
				
			}
			else stop('MONTHLY(): unknown agg/disagg function (fun) type.');
			
		}
		
		#input is semiannual
		else if (frequency(x)==2)
		{
			#build out data
			inCD=coredata(x);
			#out data
			outCD=inCD[1];
			
			for (idx in (2:length(inCD)))
			{
				for (idx2 in (1:5))
				{
					outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/6)
				}
				
				outCD=c(outCD,inCD[idx]);
			}
			
			if (is.null(fun))
			{
				if (start(x)[2]==1) outF=ts(rep(coredata(x),each=6),start=c(start(x)[1],1),frequency=12);	
				if (start(x)[2]==2) outF=ts(rep(coredata(x),each=6),start=c(start(x)[1],7),frequency=12);
			}
			else if (fun=='INTERP_END')		 	
			{
				if (start(x)[2]==1) outF=ts(outCD,start=c(start(x)[1],6),frequency=12);
				if (start(x)[2]==2) outF=ts(outCD,start=c(start(x)[1],12),frequency=12);
			}
			else if (fun=='INTERP_CENTER')		 	
			{
				if (start(x)[2]==1) 	outF=ts(outCD,start=c(start(x)[1],4),frequency=12);
				if (start(x)[2]==2) 	outF=ts(outCD,start=c(start(x)[1],10),frequency=12);
			}
			else if (fun=='INTERP_BEGIN')		 	
			{	
				if (start(x)[2]==1) 	outF=ts(outCD,start=c(start(x)[1],1),frequency=12);
				if (start(x)[2]==2) 	outF=ts(outCD,start=c(start(x)[1],7),frequency=12);
			}
			else stop('MONTHLY(): unknown agg/disagg function (fun) type.');
			
		}
		
		#input is quarterly
		else if (frequency(x)==4)
		{
			#build out data
			inCD=coredata(x);
			#out data
			outCD=inCD[1];
			
			for (idx in (2:length(inCD)))
			{
				for (idx2 in (1:2))
				{
					outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/3)
				}
				
				outCD=c(outCD,inCD[idx]);
			}
			
			if (is.null(fun))
			{
				if (start(x)[2]==1) outF=ts(rep(coredata(x),each=3),start=c(start(x)[1],1),frequency=12);	
				if (start(x)[2]==2) outF=ts(rep(coredata(x),each=3),start=c(start(x)[1],4),frequency=12);
				if (start(x)[2]==3) outF=ts(rep(coredata(x),each=3),start=c(start(x)[1],7),frequency=12);
				if (start(x)[2]==4) outF=ts(rep(coredata(x),each=3),start=c(start(x)[1],10),frequency=12);
			}
			else if (fun=='INTERP_END')		 	
			{
				if (start(x)[2]==1) outF=ts(outCD,start=c(start(x)[1],3),frequency=12);
				if (start(x)[2]==2) outF=ts(outCD,start=c(start(x)[1],6),frequency=12);
				if (start(x)[2]==3) outF=ts(outCD,start=c(start(x)[1],9),frequency=12);
				if (start(x)[2]==4) outF=ts(outCD,start=c(start(x)[1],12),frequency=12);
			}
			else if (fun=='INTERP_CENTER')		 	
			{
				if (start(x)[2]==1) 	outF=ts(outCD,start=c(start(x)[1],2),frequency=12);
				if (start(x)[2]==2) 	outF=ts(outCD,start=c(start(x)[1],5),frequency=12);
				if (start(x)[2]==3) 	outF=ts(outCD,start=c(start(x)[1],8),frequency=12);
				if (start(x)[2]==4) 	outF=ts(outCD,start=c(start(x)[1],11),frequency=12);
			}
			else if (fun=='INTERP_BEGIN')		 	
			{	
				if (start(x)[2]==1) 	outF=ts(outCD,start=c(start(x)[1],1),frequency=12);
				if (start(x)[2]==2) 	outF=ts(outCD,start=c(start(x)[1],4),frequency=12);
				if (start(x)[2]==3) 	outF=ts(outCD,start=c(start(x)[1],7),frequency=12);
				if (start(x)[2]==4) 	outF=ts(outCD,start=c(start(x)[1],10),frequency=12);
			}
			else stop('MONTHLY(): unknown agg/disagg function (fun) type.');
			
		}
		
		#input is daily
		else if (frequency(x)==366)
		{
		  
		  
		  if (is.null(fun))
		  {
		    stop('MONTHLY(): an option (fun=F) must be given when converting a daily time series.');			
		  }
		  else if (fun=='STOCK')		 	
		  {	
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=13;
		    } else 
        {
		      outStartP=as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE));
		    } 
		    
		    
		    
		    for (idx in (1:length(x)))
		    {
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-31') outCD=c(outCD,x[idx])
		        
            #bisextile workaround... (if tomorrow is first march then today is last 28 or 29 feb)
		        else if (format(as.Date(as.Date(GETDATE(x,idx,avoidCompliance=TRUE))+1),format='%m-%d')=='03-01') outCD=c(outCD,x[idx])
		                    
            else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='03-31') outCD=c(outCD,x[idx])
		        else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='04-30') outCD=c(outCD,x[idx])
		        else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='05-31') outCD=c(outCD,x[idx])
		        else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') outCD=c(outCD,x[idx])
		        else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='07-31') outCD=c(outCD,x[idx])
		        else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='08-31') outCD=c(outCD,x[idx])
		        else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='09-30') outCD=c(outCD,x[idx])
		        else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='10-31') outCD=c(outCD,x[idx])
		        else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='11-30') outCD=c(outCD,x[idx])
		        else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') outCD=c(outCD,x[idx]);
		      }
		    }
		    
		    if (is.null(outCD)) stop('MONTHLY(): input time series does not span a month. Nothing defined.');
		    if (length(outCD)<2) print('MONTHLY(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=12);
		    
		  }
		  else if (fun=='NSTOCK')		 	
		  {	
		  	#start period of out ts
		  	outStartP=NA;
		  	#out data
		  	outCD=NULL;
		  	
		  	if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		  	{
		  		outStartP=13;
		  	} else 
		  	{
		  		outStartP=as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE));
		  	} 
		  	
		  	
		  	
		  	for (idx in (1:length(x)))
		  	{
		  		if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		  		{
		  			if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='01-31') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 
		  							 && as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==1) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  			
		  			#bisextile workaround... (if tomorrow is first march then today is last 28 or 29 feb)           
		  			else if (format(as.Date(as.Date(GETDATE(x,idx,avoidCompliance=TRUE))+1),format='%m-%d')=='03-01') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 
		  							 && as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==2) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  			
		  			else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='03-31') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && 
		  							 	as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==3) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  			else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='04-30') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && 
		  							 	as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==4) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  			else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='05-31') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && 
		  							 	as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==5) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  			else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='06-30') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && 
		  							 	as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==6) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  			else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='07-31') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && 
		  							 	as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==7) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  			else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='08-31') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && 
		  							 	as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==8) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  			else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='09-30') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && 
		  							 	as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==9) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  			else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='10-31') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && 
		  							 	as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==10) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  			else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='11-30') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && 
		  							 	as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==11) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  			else if (GETDATE(x,idx,format='%m-%d',avoidCompliance=TRUE)=='12-31') 
		  			{
		  				idx_tmp=0;
		  				while (is.na(x[idx-idx_tmp]) && idx_tmp<idx-1 && 
		  							 	as.numeric(GETDATE(x,idx-idx_tmp,format='%m',avoidCompliance=TRUE)) ==12) idx_tmp=idx_tmp+1
		  				outCD=c(outCD,x[idx-idx_tmp])
		  			}
		  		}
		  	}
		  	
		  	if (is.null(outCD)) stop('MONTHLY(): input time series does not span a month. Nothing defined.');
		  	if (length(outCD)<2) print('MONTHLY(): warning, the output time series has only one observation.')
		  	
		  	outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=12);
		  	
		  }
		  else if (fun=='SUM')		 	
		  {	
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first m if we dont have all days
		    if (is.na(GETDATE(x,1))) 
		    {
		      outStartP=13;
		    } else if (as.numeric(GETDATE(x,1,format='%d',avoidCompliance=TRUE))==1)
		    {
		      outStartP=as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE));
		    } else {
		      outStartP=as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))+1;
		    }
        
		    
		    #tmp for sum
		    tmpOut=0;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if (as.numeric(GETDATE(x,idx,format='%d',avoidCompliance=TRUE))==1)
		        {
		          tmpOut=x[idx];
		          canInsert=TRUE;
		        } else {
		          tmpOut=x[idx]+tmpOut;
		        }
		        
		        if (as.numeric(format(as.Date(as.Date(GETDATE(x,idx,avoidCompliance=TRUE))+1),format='%d'))==1)
  	        {
		          if (canInsert) outCD=c(outCD,tmpOut);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('MONTHLY(): input time series does not span a month. Nothing defined.');
		    if (length(outCD)<2) print('MONTHLY(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=12);
		    
		  }
		  else if (fun=='NSUM')		 	
		  {  
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first m if we dont have all days
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=13;
		    } else if (as.numeric(GETDATE(x,1,format='%d',avoidCompliance=TRUE))==1)
		    {
		      outStartP=as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE));
		    } else {
		      outStartP=as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))+1;
		    }
		    
		    #tmp for sum
		    tmpOut=NA;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if (as.numeric(GETDATE(x,idx,format='%d',avoidCompliance=TRUE))==1)
		        {
		          tmpOut=NA;
		          if (! is.na(x[idx])) tmpOut=x[idx];
		          canInsert=TRUE;
		        } else {
		          
		          if ((! is.na(tmpOut)) && (! is.na(x[idx]))) tmpOut=x[idx]+tmpOut;
		          if (( is.na(tmpOut)) && (! is.na(x[idx]))) tmpOut=x[idx];
		        }
		        
		        if (as.numeric(format(as.Date(as.Date(GETDATE(x,idx,avoidCompliance=TRUE))+1),format='%d'))==1)
		        {
		          if (canInsert) outCD=c(outCD,tmpOut);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('MONTHLY(): input time series does not span a month. Nothing defined.');
		    if (length(outCD)<2) print('MONTHLY(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=12);
		    
		  }
		  else if (fun=='AVE')		 	
		  {  
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first m if we dont have all days
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=13;
		    } else if (as.numeric(GETDATE(x,1,format='%d',avoidCompliance=TRUE))==1)
		    {
		      outStartP=as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE));
		    } else {
		      outStartP=as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))+1;
		    }
		    
		    #tmp for ave
		    tmpOut=0;
		    tmpCnt=1;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if (as.numeric(GETDATE(x,idx,format='%d',avoidCompliance=TRUE))==1)
		        {
		          tmpOut=x[idx];
		          tmpCnt=1;
		          canInsert=TRUE;
		        } else {
		          tmpOut=x[idx]+tmpOut;
		          tmpCnt=1+tmpCnt;
		        }
		        
		        if (as.numeric(format(as.Date(as.Date(GETDATE(x,idx,avoidCompliance=TRUE))+1),format='%d'))==1)
		        {
		          if (canInsert) outCD=c(outCD,tmpOut/tmpCnt);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('MONTHLY(): input time series does not span a month. Nothing defined.');
		    if (length(outCD)<2) print('MONTHLY(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=12);
		    
		  }
		  else if (fun=='NAVE')		 	
		  {  
		    #start period of out ts
		    outStartP=NA;
		    #out data
		    outCD=NULL;
		    
		    #skip first m if we dont have all days
		    if (is.na(GETDATE(x,1,avoidCompliance=TRUE))) 
		    {
		      outStartP=13;
		    } else if (as.numeric(GETDATE(x,1,format='%d',avoidCompliance=TRUE))==1)
		    {
		      outStartP=as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE));
		    } else {
		      outStartP=as.numeric(GETDATE(x,1,format='%m',avoidCompliance=TRUE))+1;
		    }
		    
		    #tmp for sum
		    tmpOut=NA;
		    tmpCnt=0;
		    
		    #flag for skip incomplete years
		    canInsert=FALSE;
		    
		    for (idx in (1:length(x)))
		    { 
		      
		      if (! is.na(GETDATE(x,idx,avoidCompliance=TRUE)))
		      {
		        if (as.numeric(GETDATE(x,idx,format='%d',avoidCompliance=TRUE))==1)
		        {
		          tmpOut=NA;
		          if (! is.na(x[idx])) 
		          {
		            tmpOut=x[idx];
		            tmpCnt=1;
		          }
		          canInsert=TRUE;
		        } else {
		          
		          if ((! is.na(tmpOut)) && (! is.na(x[idx]))) 
		          {
		            tmpOut=x[idx]+tmpOut;
		            tmpCnt=1+tmpCnt;
		          }
		          if (( is.na(tmpOut)) && (! is.na(x[idx]))) 
		          {
		            tmpOut=x[idx];
		            tmpCnt=1;
		          }
		        }
		        
            if (as.numeric(format(as.Date(as.Date(GETDATE(x,idx,avoidCompliance=TRUE))+1),format='%d'))==1)
		        {
		          if (canInsert) outCD=c(outCD,tmpOut/tmpCnt);
		        }
		      }
		      
		    }
		    
		    if (is.null(outCD)) stop('MONTHLY(): input time series does not span a month. Nothing defined.');
		    if (length(outCD)<2) print('MONTHLY(): warning, the output time series has only one observation.')
		    
		    outF=ts(outCD,start=c(start(x)[1],outStartP),frequency=12);
		    
		  }
		  else stop('MONTHLY(): unknown agg/disagg function (fun) type.');
		  
		}
		
		else {stop('MONTHLY(): unsupported input frequency.');}
    
	}#end is.ts()
	
	if (is.xts(x) )
	{
		return(fromTStoXTS(MONTHLY(fromXTStoTS(x,avoidCompliance=avoidCompliance),fun=fun,avoidCompliance=TRUE),avoidCompliance=TRUE));
	}
	
	return(outF);
}



# DAILY code ----------------------------------------

#DAILY creates a DAILY time series from an existing time series.
DAILY <- function(x=NULL,fun=NULL,avoidCompliance=FALSE,...)
{
  
  if (is.null(x)) stop('DAILY(): input time series needs to be instance of ts() or xts() class.');
  
  
  outF=x;	
  
  if (! avoidCompliance ) 
  {
    tryCatch({.isCompliant(x);},error=function(e){stop('DAILY(): x - ',e$message);});
    
  }
  
  
  if (is.ts(x) )
  {			
    if (length(x)<2) stop('DAILY(): at least two observations required for interpolation.');
    
    #input is daily
    if (frequency(x)==366) {outF=x;}
    
    #input is yearly
    else if (frequency(x)==1)
    {
      
      
      if (is.null(fun))
      {
        outF=ts(rep(coredata(x),each=366),start=start(x),frequency=366);								
      }
      else if (fun=='INTERP_END')		 	
      {
        
        
        #build out data
        inCD=coredata(x);
        #out data
        outCD=inCD[1];
        
        for (idx in (2:length(inCD)))
        {
          #print(.isBisextile(start(x)[1]-2+idx))
          if (.isBisextile(start(x)[1]-2+idx))
          {
            for (idx2 in (1:364))
            {
              outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/365);
            }           
            
          } else if (.isBisextile(start(x)[1]-1+idx))
          { 
            for (idx2 in (1:366))
            {
              outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/367);
            }            
          }
          else
          {
            for (idx2 in (1:365))
            {
              outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/366);
            }            
            
          }
          
          outCD=c(outCD,inCD[idx]);
        }
        
        if (.isBisextile(start(x)[1]))
        {
          outF=ts(outCD,start=c(start(x)[1],366),frequency=366);
        } else
        {
          outF=ts(outCD,start=c(start(x)[1],365),frequency=366);
        }
      }
      else if (fun=='INTERP_CENTER')		 	
      {
        #build out data
        inCD=coredata(x);
        #out data
        outCD=inCD[1];
        
        for (idx in (2:length(inCD)))
        {
          #print(.isBisextile(start(x)[1]-2+idx))
          if (.isBisextile(start(x)[1]-2+idx))
          {
            for (idx2 in (1:364))
            {
              outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/365);
            }           
                       
          } else if (.isBisextile(start(x)[1]-1+idx))
          { 
            for (idx2 in (1:366))
            {
              outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/367);
            }            
          }
          else
          {
            for (idx2 in (1:365))
            {
              outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/366);
            }            
            
          }
          
          outCD=c(outCD,inCD[idx]);
        }
        
        if (.isBisextile(start(x)[1]))
        {
          outF=ts(outCD,start=c(start(x)[1],183),frequency=366);
        } else
        {
          outF=ts(outCD,start=c(start(x)[1],182),frequency=366);
        }
        
      }
      else if (fun=='INTERP_BEGIN')		 	
      {
        #build out data
        inCD=coredata(x);
        #out data
        outCD=inCD[1];
        
        for (idx in (2:length(inCD)))
        {
          #print(start(x)[1]-2+idx)
          if (.isBisextile(start(x)[1]-2+idx))
          {
            for (idx2 in (1:365))
            {
              outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/366);
            }
            
          } else
          {
            for (idx2 in (1:364))
            {
              outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/365);
            }
            
            #repeat last obs
            outCD=c(outCD,inCD[(idx-1)]+idx2*(inCD[idx]-inCD[(idx-1)])/365);
          }
          
          outCD=c(outCD,inCD[idx]);
        }
        
        outF=ts(outCD,start=c(start(x)[1],1),frequency=366);
        
        
      }
      else stop('DAILY(): unknown agg/disagg function (fun) type.');
      
    }
    
    #input is semiannual
    else if (frequency(x)==2)
    {
      
      
      if (is.null(fun))
      {
        #out data
        outCD=c();        
        
          for (idx in (1:length(x)))
          {
            #first semester
            if ((idx +1-start(x)[2])%% 2 == 1) 
            {
              if (.isBisextile(start(x)[1]+trunc((1+(idx-1+start(x)[2]-2))/2)))
              {
                outCD=c(outCD,rep(x[idx],182));
              } else
              {
                outCD=c(outCD,rep(x[idx],181));
              }
            }else 
            {#second semester
              if (.isBisextile(start(x)[1]+trunc((1+(idx-1+start(x)[2]-2))/2)))
              {
                outCD=c(outCD,rep(x[idx],184));
              } else
              {
                outCD=c(outCD,rep(x[idx],185));
              }
            }
          }
          
          if (start(x)[2]==1)
          {
            outStart=1;
          } else
          {
            if (.isBisextile(start(x)[1]))
            {
              outStart=183;
            } else {
              outStart=182;
            }
          }
        
          outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
        
      }
      else if (fun=='INTERP_END')		 	
      {
        
        
        #out data
        outCD=c();        
        
        for (idx in (2:length(x)))
        {
          #first semester
          if ((idx -start(x)[2])%% 2 == 1) 
          {
            
            tmpBY=start(x)[1]+trunc((idx+start(x)[2]-2)/2);         
           
            outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:183)/184);
            
          } else 
          {#second semester
            tmpBY=start(x)[1]+trunc((idx+start(x)[2]-2)/2);
             if (.isBisextile(tmpBY) )
            {#bisextile
              outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:182)/183);
            } else if ( .isBisextile(tmpBY-1))
            {#bisextile bef
              outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:180)/181);
            }
            else
            {#no bisextile
              outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:181)/182);
            }
          }
        }
        
        outCD=c(outCD,x[idx]);
        
        if (start(x)[2]==1)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=182;
          } else {
            outStart=181;
          }
          
        } else
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=366;
          } else {
            outStart=365;
          }
        }
        
        outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
      }
      else if (fun=='INTERP_CENTER')		 	
      {
        #out data
        outCD=c();        
        
        for (idx in (2:length(x)))
        {
          #first semester
          if ((idx -start(x)[2])%% 2 == 1) 
          {
            
            outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:182)/183);
          
          } else 
          {#second semester
            tmpBY=start(x)[1]+trunc((idx+start(x)[2]-2)/2);
            
            if (.isBisextile(tmpBY) )
            {
             outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:183)/184);
            } else if ( .isBisextile(tmpBY-1))
            {
             outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:181)/182);
            }
            else
            {
             outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:182)/183);
            }
          }
        }
        
        outCD=c(outCD,x[idx]);
        
        if (start(x)[2]==1)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=92;
          } else {
            outStart=91;
          }
          
        } else
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=275;
          } else {
            outStart=274;
          }
        }
        
        outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
        
      }
      else if (fun=='INTERP_BEGIN')		 	
      {
        #out data
        outCD=c();        
        
        for (idx in (2:length(x)))
        {
          #first semester
          if ((idx -start(x)[2])%% 2 == 1) 
          {
            if (.isBisextile(start(x)[1]+trunc((1+(idx-2+start(x)[2]-2))/2)))
            {
              outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:181)/182);
            } else
            {
              outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:180)/181);
            }
          }else 
          {#second semester
            
            if (.isBisextile(start(x)[1]+trunc((1+(idx-2+start(x)[2]-2))/2)))
            {
              outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:183)/184);
            } else
            {
              outCD=c(outCD,x[idx-1]+(x[idx]-x[idx-1])*(0:184)/185);
            }
          }
        }
        
        outCD=c(outCD,x[idx]);
        
        if (start(x)[2]==1)
        {
          outStart=1;
        } else
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=183;
          } else {
            outStart=182;
          }
        }
        
        outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
        
        
      }
      else stop('DAILY(): unknown agg/disagg function (fun) type.');
      
    }
    
    #input is quarterly
    else if (frequency(x)==4)
    {
      
      
      if (is.null(fun))
      {
        #out data
        outCD=c();        
        
        for (idx in (1:length(x)))
        {
          #first quarter
          if ((idx -1+start(x)[2]) %% 4 == 1) 
          { 
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/4)))
            {
              outCD=c(outCD,rep(x[idx],91));
            } else
            {
              outCD=c(outCD,rep(x[idx],90));
            }
          } else if ((idx -1+start(x)[2]) %% 4 == 2) 
          {#second quarter
            
              outCD=c(outCD,rep(x[idx],91));
            
          } else if ((idx -1+start(x)[2]) %% 4 == 3) 
          {#3 quarter
            
              outCD=c(outCD,rep(x[idx],92));
            
          } else if ((idx -1+start(x)[2]) %% 4 == 0) 
          {#4 quarter
            #print('q4');
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/4)))
            {#print('b');
              outCD=c(outCD,rep(x[idx],92));
            } else
            {#print('nb');
              outCD=c(outCD,rep(x[idx],93));
            }
          }
        }
        
        if (start(x)[2]==1)
        {
          outStart=1;
        } else if (start(x)[2]==2)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=92;
          } else {
            outStart=91;
          }
        } else if (start(x)[2]==3)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=183;
          } else {
            outStart=182;
          }
        } else if (start(x)[2]==4)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=275;
          } else {
            outStart=274;
          }
        }
        
        outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
        
      }
      else if (fun=='INTERP_END')  	 	
      {
        #out data
        outCD=c();        
        
        for (idx in (1:(length(x)-1)))
        {
          #first quarter
          if ((idx -1+start(x)[2]) %% 4 == 1) 
          {# 
            
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:90)/91);
            
          } else if ((idx -1+start(x)[2]) %% 4 == 2) 
          {#second quarter
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:91)/92);
            
          } else if ((idx -1+start(x)[2]) %% 4 == 3) 
          {#3 quarter
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:91)/92);
            
          } else if ((idx -1+start(x)[2]) %% 4 == 0) 
          {#4 quarter
            
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/4)))
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:89)/90);
            } else  if (.isBisextile(1+start(x)[1]+trunc((idx-2+start(x)[2])/4)))
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:91)/92);
            } else {
              
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:90)/91);
            }
          }
        }
        
        outCD=c(outCD,x[idx+1]);
        
        if (start(x)[2]==1)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=91;
          } else {
            outStart=90;
          }
          
        } else if (start(x)[2]==2)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=182;
          } else {
            outStart=181;
          }
        } else if (start(x)[2]==3)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=274;
          } else {
            outStart=273;
          }
        } else if (start(x)[2]==4)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=366;
          } else {
            outStart=365;
          }
        }
        
        outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
        
      }
      else if (fun=='INTERP_CENTER')		 	
      {
        #out data
        outCD=c();        
        
        for (idx in (1:(length(x)-1)))
        {
          #first quarter
          if ((idx -1+start(x)[2]) %% 4 == 1) 
          {
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/4)))
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:89)/90);
            } else
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:88)/89);
            }
          } else if ((idx -1+start(x)[2]) %% 4 == 2) 
          {#second quarter
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:91)/92);
            
          } else if ((idx -1+start(x)[2]) %% 4 == 3) 
          {#3 quarter
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:91)/92);
            
          } else if ((idx -1+start(x)[2]) %% 4 == 0) 
          {#4 quarter
            
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/4)))
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:91)/92);
            } else
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:92)/93);
            }
          }
        }
        
        outCD=c(outCD,x[idx+1]);
        
        if (start(x)[2]==1)
        {
          outStart=46;
          
        } else if (start(x)[2]==2)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=136;
          } else {
            outStart=135;
          }
        } else if (start(x)[2]==3)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=197;
          } else {
            outStart=196;
          }
        } else if (start(x)[2]==4)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=320;
          } else {
            outStart=319;
          }
        }
        
        outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
        
      }
      else if (fun=='INTERP_BEGIN')		 	
      {
        #out data
        outCD=c();        
        
        for (idx in (1:(length(x)-1)))
        {
          #first quarter
          if ((idx -1+start(x)[2]) %% 4 == 1) 
          { 
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/4)))
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:90)/91);
            } else
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:89)/90);
            }
          } else if ((idx -1+start(x)[2]) %% 4 == 2) 
          {#second quarter
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:90)/91);
            
          } else if ((idx -1+start(x)[2]) %% 4 == 3) 
          {#3 quarter
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:91)/92);
            
          } else if ((idx -1+start(x)[2]) %% 4 == 0) 
          {#4 quarter
            
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/4)))
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:91)/92);
            } else
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:92)/93);
            }
          }
        }
        
        outCD=c(outCD,x[idx+1]);

        if (start(x)[2]==1)
        {
          outStart=1;
        } else if (start(x)[2]==2)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=92;
          } else {
            outStart=91;
          }
        } else if (start(x)[2]==3)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=183;
          } else {
            outStart=182;
          }
        } else if (start(x)[2]==4)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=275;
          } else {
            outStart=274;
          }
        }
        
        outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
        
      }
      else stop('DAILY(): unknown agg/disagg function (fun) type.');
      
    }
    
    #input is monthly
    else if (frequency(x)==12)
    {
      
      
      if (is.null(fun))
      {
        #out data
        outCD=c();        
        
        for (idx in (1:length(x)))
        {
          #month 1
          if ((idx -1+start(x)[2]) %% 12 == 1) 
          { 
            outCD=c(outCD,rep(x[idx],31));
            
          } else if ((idx -1+start(x)[2]) %% 12 == 2) 
          {#month 2
            
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/12)))
            {
              outCD=c(outCD,rep(x[idx],29));
            } else
            {
              outCD=c(outCD,rep(x[idx],28));
            }
            
          } else if ((idx -1+start(x)[2]) %% 12 == 3) 
          {#month 3
            
            outCD=c(outCD,rep(x[idx],31));
            
          } else if ((idx -1+start(x)[2]) %% 12 == 4) 
          {#month 4
            
            outCD=c(outCD,rep(x[idx],30));
            
          } else 
          if ((idx -1+start(x)[2]) %% 12 == 5) 
          {#month 5
            
            outCD=c(outCD,rep(x[idx],31));
            
          } else if ((idx -1+start(x)[2]) %% 12 == 6) 
          {#month 6            
            
              outCD=c(outCD,rep(x[idx],30));           
            
          } else if ((idx -1+start(x)[2]) %% 12 == 7) 
          {#month 7
            
            outCD=c(outCD,rep(x[idx],31));
            
          } else if ((idx -1+start(x)[2]) %% 12 == 8) 
          {#month 8
            
            outCD=c(outCD,rep(x[idx],31));  
            
          } else if ((idx -1+start(x)[2]) %% 12 == 9) 
          {#month 9
            
            outCD=c(outCD,rep(x[idx],30));
            
          } else if ((idx -1+start(x)[2]) %% 12 == 10) 
          {#month 10
            
              outCD=c(outCD,rep(x[idx],31));            
            
          } else if ((idx -1+start(x)[2]) %% 12 == 11) 
          {#month 11
            
            outCD=c(outCD,rep(x[idx],30));
            
          } else if ((idx -1+start(x)[2]) %% 12 == 0) 
          {#month 12
            
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/12)))
            {
              outCD=c(outCD,rep(x[idx],31));
            } else
            {
              outCD=c(outCD,rep(x[idx],32));
            }
            
            
          }
        }
        
        if (start(x)[2]==1)
        {
          outStart=1;
        } else if (start(x)[2]==2)
        {
          
          outStart=32;
          
        } else if (start(x)[2]==3)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=61;
          } else {
            outStart=60;
          }
        } else if (start(x)[2]==4)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=92;
          } else {
            outStart=91;
          }
        } else if (start(x)[2]==5)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=122;
          } else {
            outStart=121;
          }
          
        } else if (start(x)[2]==6)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=153;
          } else {
            outStart=152;
          }
        } else if (start(x)[2]==7)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=183;
          } else {
            outStart=182;
          }
        } else if (start(x)[2]==8)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=214;
          } else {
            outStart=213;
          }
        } else if (start(x)[2]==9)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=245;
          } else {
            outStart=244;
          }
        } else if (start(x)[2]==10)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=275;
          } else {
            outStart=274;
          }
        } else if (start(x)[2]==11)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=306;
          } else {
            outStart=305;
          }
        } else if (start(x)[2]==12)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=336;
          } else {
            outStart=335;
          }
        }
        
        outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
        
      }
      else if (fun=='INTERP_END')     	
      {
        #out data
        outCD=c();        
        
        for (idx in (1:(length(x)-1)))
        {
          #month 12
          if ((idx +start(x)[2]) %% 12 == 1) 
          { 
            
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/12)))
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            } else
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:31)/32);
            }
            
            
          } else if ((idx +start(x)[2]) %% 12 == 2) 
          {#month 2-1
            
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/12)))
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:28)/29);
            } else
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:27)/28);
            }
            
          } else if ((idx +start(x)[2]) %% 12 == 3) 
          {#month 3-1
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            
          } else if ((idx +start(x)[2]) %% 12 == 4) 
          {#month 4-1
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);
            
          } else if ((idx +start(x)[2]) %% 12 == 5) 
          {#month 5-1
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            
          } else if ((idx +start(x)[2]) %% 12 == 6) 
          {#month 6 -1           
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);           
            
          } else if ((idx +start(x)[2]) %% 12 == 7) 
          {#month 7-1
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            
          } else if ((idx +start(x)[2]) %% 12 == 8) 
          {#month 8-1
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);  
            
          } else if ((idx +start(x)[2]) %% 12 == 9) 
          {#month 9-1
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);
            
          } else if ((idx +start(x)[2]) %% 12 == 10) 
          {#month 10-1
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);            
            
          } else if ((idx +start(x)[2]) %% 12 == 11) 
          {#month 11-1
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);
            
          } else if ((idx +start(x)[2]) %% 12 == 0) 
          {#month 12-1
            
           
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            
            
            
          }
        }
        
        outCD=c(outCD,x[idx+1]);
        
        if (1+start(x)[2]==13)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=366;
          } else {
            outStart=365;
          }
          
        } else if (1+start(x)[2]==2)
        {
          
          outStart=32-1;
          
        } else if (1+start(x)[2]==3)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=61-1;
          } else {
            outStart=60-1;
          }
        } else if (1+start(x)[2]==4)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=92-1;
          } else {
            outStart=91-1;
          }
        } else if (1+start(x)[2]==5)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=122-1;
          } else {
            outStart=121-1;
          }
          
        } else if (1+start(x)[2]==6)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=153-1;
          } else {
            outStart=152-1;
          }
        } else if (1+start(x)[2]==7)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=183-1;
          } else {
            outStart=182-1;
          }
        } else if (1+start(x)[2]==8)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=214-1;
          } else {
            outStart=213-1;
          }
        } else if (1+start(x)[2]==9)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=245-1;
          } else {
            outStart=244-1;
          }
        } else if (1+start(x)[2]==10)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=275-1;
          } else {
            outStart=274-1;
          }
        } else if (1+start(x)[2]==11)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=306-1;
          } else {
            outStart=305-1;
          }
        } else if (1+start(x)[2]==12)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=336-1;
          } else {
            outStart=335-1;
          }
        }
        
        outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
        
      }
      else if (fun=='INTERP_CENTER')		 	
      {
        #out data
        outCD=c();        
        
        for (idx in (1:(length(x)-1)))
        {
          #month 1
          if ((idx -1+start(x)[2]) %% 12 == 1) 
          { 
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            
          } else if ((idx -1+start(x)[2]) %% 12 == 2) 
          {#month 2
            
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/12)))
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:28)/29);
            } else
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:27)/28);
            }
            
          } else if ((idx -1+start(x)[2]) %% 12 == 3) 
          {#month 3
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            
          } else if ((idx -1+start(x)[2]) %% 12 == 4) 
          {#month 4
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);
            
          } else if ((idx -1+start(x)[2]) %% 12 == 5) 
          {#month 5
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            
          } else if ((idx -1+start(x)[2]) %% 12 == 6) 
          {#month 6            
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);           
            
          } else if ((idx -1+start(x)[2]) %% 12 == 7) 
          {#month 7
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            
          } else if ((idx -1+start(x)[2]) %% 12 == 8) 
          {#month 8
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);  
            
          } else if ((idx -1+start(x)[2]) %% 12 == 9) 
          {#month 9
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);
            
          } else if ((idx -1+start(x)[2]) %% 12 == 10) 
          {#month 10
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);            
            
          } else if ((idx -1+start(x)[2]) %% 12 == 11) 
          {#month 11
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);
            
          } else if ((idx -1+start(x)[2]) %% 12 == 0) 
          {#month 12
            
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/12)))
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            } else
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:31)/32);
            }
            
            
          }
        }
        
        outCD=c(outCD,x[idx+1]);
        
        if (start(x)[2]==1)
        {
          outStart=1;
        } else if (start(x)[2]==2)
        {
          
          outStart=32;
          
        } else if (start(x)[2]==3)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=61;
          } else {
            outStart=60;
          }
        } else if (start(x)[2]==4)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=92;
          } else {
            outStart=91;
          }
        } else if (start(x)[2]==5)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=122;
          } else {
            outStart=121;
          }
          
        } else if (start(x)[2]==6)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=153;
          } else {
            outStart=152;
          }
        } else if (start(x)[2]==7)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=183;
          } else {
            outStart=182;
          }
        } else if (start(x)[2]==8)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=214;
          } else {
            outStart=213;
          }
        } else if (start(x)[2]==9)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=245;
          } else {
            outStart=244;
          }
        } else if (start(x)[2]==10)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=275;
          } else {
            outStart=274;
          }
        } else if (start(x)[2]==11)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=306;
          } else {
            outStart=305;
          }
        } else if (start(x)[2]==12)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=336;
          } else {
            outStart=335;
          }
        }
        
        outStart=outStart+14;
        
        outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
        
      }
      else if (fun=='INTERP_BEGIN')		 	
      {
        #out data
        outCD=c();        
        
        for (idx in (1:(length(x)-1)))
        {
          #month 1
          if ((idx -1+start(x)[2]) %% 12 == 1) 
          { 
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            
          } else if ((idx -1+start(x)[2]) %% 12 == 2) 
          {#month 2
            
            if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/12)))
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:28)/29);
            } else
            {
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:27)/28);
            }
            
          } else if ((idx -1+start(x)[2]) %% 12 == 3) 
          {#month 3
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
            
          } else if ((idx -1+start(x)[2]) %% 12 == 4) 
          {#month 4
            
            outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);
            
          } else if ((idx -1+start(x)[2]) %% 12 == 5) 
            {#month 5
              
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
              
            } else if ((idx -1+start(x)[2]) %% 12 == 6) 
            {#month 6            
              
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);           
              
            } else if ((idx -1+start(x)[2]) %% 12 == 7) 
            {#month 7
              
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
              
            } else if ((idx -1+start(x)[2]) %% 12 == 8) 
            {#month 8
              
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);  
              
            } else if ((idx -1+start(x)[2]) %% 12 == 9) 
            {#month 9
              
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);
              
            } else if ((idx -1+start(x)[2]) %% 12 == 10) 
            {#month 10
              
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);            
              
            } else if ((idx -1+start(x)[2]) %% 12 == 11) 
            {#month 11
              
              outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:29)/30);
              
            } else if ((idx -1+start(x)[2]) %% 12 == 0) 
            {#month 12
              
              if (.isBisextile(start(x)[1]+trunc((idx-2+start(x)[2])/12)))
              {
                outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:30)/31);
              } else
              {
                outCD=c(outCD,x[idx]+(x[idx+1]-x[idx])*(0:31)/32);
              }
              
              
            }
        }
        
        outCD=c(outCD,x[idx+1]);
        
        if (start(x)[2]==1)
        {
          outStart=1;
        } else if (start(x)[2]==2)
        {
          
            outStart=32;
         
        } else if (start(x)[2]==3)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=61;
          } else {
            outStart=60;
          }
        } else if (start(x)[2]==4)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=92;
          } else {
            outStart=91;
          }
        } else if (start(x)[2]==5)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=122;
          } else {
            outStart=121;
          }
          
        } else if (start(x)[2]==6)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=153;
          } else {
            outStart=152;
          }
        } else if (start(x)[2]==7)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=183;
          } else {
            outStart=182;
          }
        } else if (start(x)[2]==8)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=214;
          } else {
            outStart=213;
          }
        } else if (start(x)[2]==9)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=245;
          } else {
            outStart=244;
          }
        } else if (start(x)[2]==10)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=275;
          } else {
            outStart=274;
          }
        } else if (start(x)[2]==11)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=306;
          } else {
            outStart=305;
          }
        } else if (start(x)[2]==12)
        {
          if (.isBisextile(start(x)[1]))
          {
            outStart=336;
          } else {
            outStart=335;
          }
        }
        
        outF=ts(outCD,start=c(start(x)[1],outStart),frequency=366);
        
      }
      else stop('DAILY(): unknown agg/disagg function (fun) type.');
      
    }
    
    else {stop('DAILY(): unsupported input frequency.');}
    
  }#end is.ts()
  
  if (is.xts(x) )
  {
    return(fromTStoXTS(DAILY(fromXTStoTS(x,avoidCompliance=avoidCompliance),fun=fun,avoidCompliance=TRUE),avoidCompliance=TRUE));
  }
  
  return(outF);
}



# TSDELTA code ----------------------------------------

#TSDELTA computes differences from a time series.
TSDELTA <- function(x=NULL,L=1,O=1,avoidCompliance=FALSE,...)
{
	
	if (is.null(x)) stop('TSDELTA(): input time series needs to be instance of ts() or xts() class.');
	
	if (is.null(L)) stop('TSDELTA(): NULL lag L.');
	if (!(is.numeric(L) && L>0) ) stop('TSDELTA(): lag L must be a positive integer.');
	if (!(L%%1==0))  stop('TSDELTA(): non-integer lag L.');
	
	if (is.null(O)) stop('TSDELTA(): null order O.');
	if (!(is.numeric(O) && O>0) ) stop('TSDELTA(): order O must be a positive integer.');
	if (!(O%%1==0))  stop('TSDELTA(): non-integer oreder O.');
	
	outF=x;	
	
	if (! avoidCompliance ) 
	{
		tryCatch({.isCompliant(x);},error=function(e){stop('TSDELTA(): x - ',e$message);});
		
	}
	
	if (is.ts(x) )
	{			
		if (length(x)<=(L*O)) stop('TSDELTA(): Too much lags: attempting to define a NULL object.')
		
		outF=diff(x,lag=L,differences=O);		
	}
	
	if (is.xts(x) )
	{
		return(fromTStoXTS(TSDELTA(fromXTStoTS(x,avoidCompliance=avoidCompliance),L=L,O=O,avoidCompliance=TRUE),avoidCompliance=TRUE));
	}
	
	return(outF);
}

DELTA <- TSDELTA;


# TSLAG code ----------------------------------------

#TSLAG lags time series data by the specified number of time periods.
TSLAG <- function(x=NULL,L=1,avoidCompliance=FALSE,...)
{
	
	if (is.null(x)) stop('TSLAG(): input time series needs to be instance of ts() or xts() class.');
	
	if (is.null(L)) stop('TSLAG(): NULL lag L.');
	if (!(is.numeric(L) ) ) stop('TSLAG(): lag L must be an integer.');
	if (!(L%%1==0))  stop('TSLAG(): non-integer lag L.');
	
	
	outF=x;	
	
	if (! avoidCompliance ) 
	{
		tryCatch({.isCompliant(x);},error=function(e){stop('TSLAG(): x - ',e$message);});
		
	}
	
	if (is.ts(x) )
	{			
		if (length(x)<=abs(L)) cat('TSLAG(): warning, too much lags. Attempting to define a NULL object.\n')
		outF=stats::lag(x,k=-L);	#R direction inverse	
	}
	
	if (is.xts(x) )
	{
		return(fromTStoXTS(TSLAG(fromXTStoTS(x,avoidCompliance=avoidCompliance),L=L,avoidCompliance=TRUE),avoidCompliance=TRUE));
	}
	
	return(outF);
}

TSLEAD <- function(x=NULL,L=1,avoidCompliance=FALSE,...)
{
  
  if (is.null(x)) stop('TSLEAD(): input time series needs to be instance of ts() or xts() class.');
  
  if (is.null(L)) stop('TSLEAD(): NULL lag L.');
  if (!(is.numeric(L) ) ) stop('TSLEAD(): lag L must be an integer.');
  if (!(L%%1==0))  stop('TSLEAD(): non-integer lag L.');
  
  
  tryCatch({
   
    return(TSLAG(x=x,L=-L,avoidCompliance=avoidCompliance,...));
  
    },error=function(e){stop('TSLEAD(): ',e$message)});
}


# TSDELTALOG code ----------------------------------------

#TSDELTALOG computes differences of the log time series
TSDELTALOG <- function(x=NULL, L=1, avoidCompliance=FALSE,...)
{
  
  if (is.null(x)) stop('TSDELTALOG(): input time series needs to be instance of ts() or xts() class.');
  
  if (is.null(L)) stop('TSDELTALOG(): NULL lag L.');
  if (!(is.numeric(L) && L>0) ) stop('TSDELTALOG(): lag L must be a positive integer.');
  if (!(L%%1==0))  stop('TSDELTALOG(): non-integer lag L.');
  
  
  outF=x;	
  
  if (! avoidCompliance ) 
  {
    tryCatch({.isCompliant(x);},error=function(e){stop('TSDELTALOG(): x - ',e$message);});
    
  }
  
  
  
  if (is.ts(x) )
  {			
    if (length(x)<=(L)) stop('TSDELTALOG(): Too much lags: attempting to define a NULL object.')
    
    outF=TSDELTA(log(x),L=L,avoidCompliance=TRUE);
    
  }
  
  if (is.xts(x) )
  {
    return(fromTStoXTS(TSDELTALOG(fromXTStoTS(x,avoidCompliance=avoidCompliance),L=L, avoidCompliance=TRUE),avoidCompliance=TRUE));
  }
  
  return(outF);
}


# TSDELTAP code ----------------------------------------

#TSDELTAP computes the percent changes of a time series.
TSDELTAP <- function(x=NULL,L=1,ANNUALIZE=FALSE,avoidCompliance=FALSE,...)
{
	
	if (is.null(x)) stop('TSDELTAP(): input time series needs to be instance of ts() or xts() class.');
	
	if (is.null(L)) stop('TSDELTAP(): NULL lag L.');
	if (!(is.numeric(L) && L>0) ) stop('TSDELTAP(): lag L must be a positive integer.');
	if (!(L%%1==0))  stop('TSDELTAP(): non-integer lag L.');
	
	
	outF=x;	
	
	if (! avoidCompliance ) 
	{
		tryCatch({.isCompliant(x);},error=function(e){stop('TSDELTAP(): x - ',e$message);});
		
	}
	
	
	
	if (is.ts(x) )
	{			
		if (length(x)<=(L)) stop('TSDELTAP(): Too much lags: attempting to define a NULL object.')
		
		outF=100*TSDELTA(x,L=L,avoidCompliance=TRUE)/TSLAG(x,L,avoidCompliance=TRUE);
		
		if (ANNUALIZE==TRUE)
		{
			if (!(frequency(x) %% L == 0)) stop('TSDELTAP(): ANNUALIZE is incompatible with provided lag L.');
			
			#project TSDELTAP on annual
			tempTS=x;
			#apply the same delta% till end of year (i.e. f/L)
			tempTS2=TSLAG(x,avoidCompliance=TRUE)*(1+outF/100)^(frequency(x)/L);
			#evaluate new TSDELTAP
			tempTS2=100*(tempTS2/TSLAG(x,avoidCompliance=TRUE)-1);
			outF=tempTS2;
			
		}
		
	}
	
	if (is.xts(x) )
	{
		return(fromTStoXTS(TSDELTAP(fromXTStoTS(x,avoidCompliance=avoidCompliance),L=L,ANNUALIZE=ANNUALIZE,avoidCompliance=TRUE),avoidCompliance=TRUE));
	}
	
	return(outF);
}

DELTAP <- TSDELTAP;



# TSMERGE code ----------------------------------------

#TSMERGE merges two time series.
TSMERGE <- function(...,fun=NULL,MV=FALSE,avoidCompliance=FALSE)
{
	outF=NULL;
  
	#an input is null
	tryCatch({tsL=list(...);},error=function(e){stop('TSMERGE(): an input argument is NULL.')});
	
	
	if (length(tsL)==0) stop('TSMERGE(): usage: TSMERGE(ts1,ts2,..,tsN).');
	
	
	if (! avoidCompliance ) 
	{	
		tryCatch({lapply(tsL,.isCompliant);},error=function(e){stop('TSMERGE(): ',e$message);});		
	}
	
	#all inputs are ts()
	if (all(as.logical(lapply(tsL,is.ts))))
	{	
		if (length(unique(lapply(tsL,frequency)))>1)
		{
			stop('TSMERGE(): different frequencies on input time series.');
		}
		
		#get frequency
		fOutF=frequency(tsL[[1]]);
		
		#get first starting period
		startOutF=c(9999,1);
		
		for (idx in (1:length(tsL)))
		{
			if (.isMinorPeriodYP(start(tsL[[idx]]),startOutF,f=fOutF)) startOutF=start(tsL[[idx]]);
		}
		
		#get last ending period
		endOutF=c(0,1);
		
		for (idx in (1:length(tsL)))
		{
			if (.isMinorPeriodYP(endOutF,end(tsL[[idx]]),f=fOutF)) endOutF=end(tsL[[idx]]);
		}
		
		#create empty out time series
		outF=ts(start=startOutF,end=endOutF,frequency=fOutF);
		
		
		for (idx in (1:length(outF)))
		{
			#cat('idx:',idx,'\n');
			#...keep # of non missing columns (...used in SUM AVE etc)			
			activeColumns=0;
      
      #true if observation has been initializated 
      initializedValue=FALSE;
			
			for (idxL in (1:length(tsL)))
			{ 
				#cat('idL:',idxL,'\n');
				#periods betw start of out time series and start of input time series
				deltaStart=NUMPERIOD(start(outF),start(tsL[[idxL]]),fOutF);
				deltaEnd=NUMPERIOD(start(outF),end(tsL[[idxL]]),fOutF);
				
   
				#...can project values
				if ((idx-deltaStart>=1) && (1+deltaEnd-idx>=0))
				{ 
					#input column value is valid
					if (MV==TRUE || !(is.na(tsL[[idxL]][(idx-deltaStart)]))) 
					{
						activeColumns=activeColumns+1; 
						
						if (is.null(fun))
						{
							outF[idx]=tsL[[idxL]][(idx-deltaStart)];
              
              #we get first value then exit cycle
							break;
						}
						
						else if (fun=='AVE')
						{	
							#set temp sum to 0 if first is na
							if (initializedValue==FALSE  ) {tmpS=0;}
							else {tmpS=outF[idx];}
							
							#moving ave
							outF[idx]=((activeColumns-1)*(tmpS)/activeColumns)+tsL[[idxL]][(idx-deltaStart)]/activeColumns;
							initializedValue=TRUE;
						}
						
						else if (fun=='SUM')
						{	
							#set temp sum to 0 if first is na
							if (initializedValue==FALSE  ) {tmpS=0;}
							else {tmpS=outF[idx];}
							
							#moving sum
							outF[idx]=tmpS+tsL[[idxL]][(idx-deltaStart)];
							initializedValue=TRUE;
						}
						
						else if (fun=='MAX')
						{	
							#set temp sum to 0 if first is na
							if (initializedValue==FALSE  ) 			  			
							{
								outF[idx]=tsL[[idxL]][(idx-deltaStart)];
								initializedValue=TRUE;
							}
							else 
							{
								outF[idx]=max(outF[idx],tsL[[idxL]][(idx-deltaStart)]);
							}			  			
							
						}
						
						else if (fun=='MIN')
						{	
							#set temp sum to 0 if first is na
							if (initializedValue==FALSE  ) 			  			
							{
								outF[idx]=tsL[[idxL]][(idx-deltaStart)];
								initializedValue=TRUE;
							}
							else 
							{
								outF[idx]=min(outF[idx],tsL[[idxL]][(idx-deltaStart)]);
							}
							
							
						}
						
						else stop('TSMERGE(): unknown merge function (fun) type');
						
					}
					#input column value is na
					else
					{
						if (MV==TRUE)
						{
						  outF[idx]=NA;
						}
					}
				}
				#missing wont be ignored
				else if (MV==TRUE)
				{
					outF[idx]=NA;
					break;
				}
				
			}	
			#cat('outF[idx]',outF[idx],'\n');
			
			
		}
		
		
		
		
	}#end is.ts()
	
	#all inputs are xts()
	else if (all(as.logical(lapply(tsL,is.xts))))
	{
		return(fromTStoXTS(do.call(TSMERGE,as.list(c(lapply(tsL,fromXTStoTS,avoidCompliance=avoidCompliance),fun=fun,MV=MV,avoidCompliance=TRUE))),avoidCompliance=TRUE));
	}
	else
	{
		stop('TSMERGE(): input time series need to be instance of the same class ts() or xts().');
	}	
	
	return(outF);
}





# TSERIES code ----------------------------------------


#TSERIES defines a time series ....
TSERIES <- function(..., START = c(2000,1), FREQ = 1, SOURCE=NULL, TITLE=NULL, UNITS=NULL, SCALEFAC=0, class=NULL, avoidCompliance=FALSE)
{
  
  if (is.null(FREQ)) stop('TIMESERIES(): FREQ must be one of the following values: 1, 2, 3, 4, 12, 24, 36, 366, A, Y, S, Q, M or D.');
  if (FREQ=='A' || FREQ=='Y') FREQ=1;
  if (FREQ=='S') FREQ=2;
  if (FREQ=='Q') FREQ=4;
  if (FREQ=='M') FREQ=12;
  if (FREQ=='W') FREQ=53;
  if (FREQ=='D') FREQ=366;
  
  #check FREQ
  tryCatch({.isCompliantF(FREQ);},error=function(e){stop('TIMESERIES(): FREQ must be one of the following values: 1, 2, 3, 4, 12, 24, 36, 53, 366, A, Y, S, Q, M, W or D.')});  
  if (! FREQ %in% c(1,2,3,4,12,24,36,53,366)) stop('TIMESERIES(): FREQ must be 1, 2, 3, 4, 12, 24, 36, 53 or 366.');  
  
  tryCatch({
    if (class(START)=='Date') START=date2yp(START,FREQ);
    if (class(START)=='yearmon' && FREQ==12) START=ym2yp(START);
    
    if (class(START)=='yearqtr' && FREQ==4) START=yq2yp(START);
    .isCompliantYP(START,FREQ);},error=function(e){stop('TIMESERIES(): uncompliant start date.')});  
  
 
  outF=NULL;
  
  #an input is null
  tryCatch({inputsL=list(...);},error=function(e){stop('TIMESERIES(): an input argument is null.')});  
  
  if (! all(as.logical(lapply(inputsL,.A1DCompliantInput)))) stop('TIMESERIES(): all input data must be numeric.')  
  
  #no args
  if (is.null(START) ) stop('TIMESERIES(): start date is required. Please use START=c(y,p).');
  
  #no args
  if (!(is.null(inputsL[['end']]))) stop('TIMESERIES(): end date is not allowed in TIMESERIES().');
  
  #print(typeof(attributes(inputsL))); 
  
  
  #no data provided
  if (length(inputsL)==0)
  {
    #crete ts
    tryCatch({outF=ts(A1D(NA,avoidCompliance=avoidCompliance),start=START,frequency=FREQ)},error=function(e){stop('TIMESERIES(): ',e$message);});
    
  } else
  {
    #crete ts
    tryCatch({outF=ts(A1D(...,avoidCompliance=avoidCompliance),start=START,frequency=FREQ)},error=function(e){stop('TIMESERIES(): ',e$message);});
    
  }
  
  startTS=start(outF);
  endTS=end(outF);
  
  #check if date in bimets range
  if (startTS[1]<bimets::bimets_static_startYear___ || startTS[1]>(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1) ) stop(paste0('TIMESERIES(): dates must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
  if (endTS[1]<bimets::bimets_static_startYear___ || endTS[1]>(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1) ) stop(paste0('TIMESERIES(): dates must be in range ',bimets::bimets_static_startYear___,'-',(bimets::bimets_static_startYear___ + bimets::bimets_static_totalLength___ -1),'.'));
  
  
  
  #add attributes
  if (!(is.null(SOURCE))) attr(outF,'Source')=SOURCE;
  if (!(is.null(TITLE))) attr(outF,'Title')=TITLE;
  if (!(is.null(UNITS))) attr(outF,'Units')=UNITS;
  
  if ((!(is.null(SCALEFAC))) && (SCALEFAC!=0))
  {
    if (!(is.numeric(SCALEFAC))) stop('TIMESERIES(): SCALEFAC must be numeric.');
    if ((is.na(SCALEFAC))) stop('TIMESERIES(): SCALEFAC must be numeric.');
    if (!(SCALEFAC >=0) ) stop('TIMESERIES(): SCALEFAC must be a positive integer or zero.');
    if (!(SCALEFAC%%1==0))  stop('TIMESERIES(): SCALEFAC must be a positive integer or zero.');
    attr(outF,'ScaleFac')=SCALEFAC;
  }
  
  
  
  if (! is.null(class))
  {
    if (toupper(.TRIM(class))=='XTS')
    {
      tryCatch({
        outF=fromTStoXTS(outF,avoidCompliance=TRUE)
      },error=function(e){stop('TIMESERIES(): ',e$message);});
      
      return(outF);
    } 
  } else
  { 
    globalCCT=getBIMETSconf(opt='BIMETS_CONF_CCT');
    
    if (! is.null(globalCCT) && (globalCCT=='XTS') )
    {
      tryCatch({
        outF=fromTStoXTS(outF,avoidCompliance=TRUE)
      },error=function(e){stop('TIMESERIES(): ',e$message);});
      
      return(outF);
    } 
  }   
  
  return(outF);
}

TIMESERIES <- TSERIES;


# MOVAVG code ----------------------------------------

#MOVAVG moving average
MOVAVG <- function(x=NULL, L = NULL, DIRECTION=NULL, avoidCompliance=FALSE,...)
{
  
  if (is.null(x)) stop('MOVAVG(): input needs to be instance of ts() or xts() class or a numeric array c().');
  
  if (is.null(L)) stop('MOVAVG(): NULL lag L.');
  if (!(is.numeric(L) ) ) stop('MOVAVG(): lag L must be a positive integer.');
  if (!(L%%1==0))  stop('MOVAVG(): non-integer lag L.');
  if (!(L>0))  stop('MOVAVG(): lag L must be a positive integer.');
  
  outF=x;
  
  #is xts
  if (is.xts(x)) {
    
    return(fromTStoXTS(MOVAVG(fromXTStoTS(x,avoidCompliance=avoidCompliance),L=L,DIRECTION=DIRECTION,avoidCompliance=TRUE),avoidCompliance=TRUE));
    
  } else  if (is.ts(x) ) #is ts
  {
    
    if (! avoidCompliance ) tryCatch({.isCompliant(x);},error=function(e){cat('MOVAVG(): x - ',e$message);});
    
    #get frequency
    locF=frequency(x);
    
    #get data
    locData=coredata(x);
    
    #getStartDate
    locStartDateY=start(x)[1];
    locStartDateP=start(x)[2];
    
    #gen out
    if ((! is.null(DIRECTION)) && (DIRECTION=='AHEAD') )
    {
      outF=ts(MOVAVG(locData,L,avoidCompliance=avoidCompliance),start=c(locStartDateY,locStartDateP),frequency=locF);
    } else if ((! is.null(DIRECTION)) && (DIRECTION=='CENTER') )
    {
      outF=ts(MOVAVG(locData,L,avoidCompliance=avoidCompliance),start=c(locStartDateY,locStartDateP+as.integer(trunc(L/2))),frequency=locF);
    } else 
    {
      outF=ts(MOVAVG(locData,L,avoidCompliance=avoidCompliance),start=c(locStartDateY,locStartDateP+L-1),frequency=locF);
    }
    
    
  } else {#is array
    
    
    if (!(is.numeric(x) || is.na(x))) stop('MOVAVG(): input needs to be instance of ts() or xts() class or a numeric array c().');
    
    #not enough observation
    if (length(x)<L) stop('MOVAVG(): input has not enough observations as required by lag parameter L.');
    
    if (1==L) return(outF);
    
    outF=c();
    
    #do math
    for (idx in (1:(1+length(x)-L))) 
    { 
      tempSum=x[idx];
      
      for (idx2 in (1:(L-1))) tempSum=tempSum+x[idx+idx2];
      outF=c(outF,tempSum/L);
    }
    
  }
  
  
  return(outF);
}

MAVE <- MOVAVG;


# MOVTOT code ----------------------------------------

MOVTOT <- function(x=NULL, L = NULL, DIRECTION=NULL, avoidCompliance=FALSE,...)
{
  if (is.null(x)) stop('MOVTOT(): input needs to be instance of ts() or xts() class or a numeric array c().');
  
  if (is.null(L)) stop('MOVTOT(): NULL lag L.');
  if (!(is.numeric(L) ) ) stop('MOVTOT(): lag L must be a positive integer.');
  if (!(L%%1==0))  stop('MOVTOT(): non-integer lag L.');
  if (!(L>0))  stop('MOVTOT(): lag L must be a positive integer.');
  
  outF=NULL;
  tryCatch(
    {
      outF=MOVAVG(x=x, L=L, DIRECTION=DIRECTION, avoidCompliance=avoidCompliance,...)*L;
    },error=function(err){
      cat('MOVTOT(): x - ',err$message);
    });
  
  return(outF);
  
}

MTOT <- MOVTOT;
MSUM <- MTOT;
MOVSUM <- MOVTOT;

# GETDATE code ----------------------------------------

#GETDATE get dates of selected observations.
GETDATE <- function(x=NULL, index=NULL, format='%Y-%m-%d', avoidCompliance=FALSE,...)
{
  
  if (is.null(x)) stop('GETDATE(): input time series needs to be instance of ts() or xts() class.');
 
  if (!(is.null(index)))
  {
    if (!(is.numeric(index) && index>0) ) stop('GETDATE(): index must be a positive integer.');
    if (!(index%%1==0))  stop('GETDATE(): non-integer index');
    
  }  
  
  if (is.null(format)) stop('GETDATE(): format needs to be a valid string.');
  
  outF=NULL;
  
  #is xts
  if (is.xts(x)) {
    
  	return(GETDATE(fromXTStoTS(x,avoidCompliance=avoidCompliance),index=index,format=format,avoidCompliance=TRUE));
      
  } else  if (is.ts(x) ) #is ts
  {
    
    if (! avoidCompliance ) tryCatch({.isCompliant(x);},error=function(e){stop('GETDATE(): x - ',e$message);});
    
    #get frequency
    locF=frequency(x);
    
    outF=c();
    
    #if null index get all dates
    if (is.null(index))
    {       
      tryCatch({
        outF=.getStaticDates(start=start(x),end=end(x),freq=locF);
      },error=function(e){stop('GETDATE(): ',e$message);});
      
      
    } else {
      
      startY=start(x)[1];
      startP=start(x)[2];  
      
      #normalized year-period
      normYP=normalizeYP(c(startY,startP+index-1),locF);      
      
      normY=normYP[1];
      normP=normYP[2];
      
      if (index>length(x)) stop('GETDATE(): index is greater than time series length.'); 
      
      tryCatch({
        outF=.getStaticDates(start=normYP,freq=locF);
      },error=function(e){stop('GETDATE(): ',e$message);});
    }
     
  
  } else stop('GETDATE(): input needs to be instance of class ts() or xts().');
  
  #asked for quarter?
  if (length(grep('%q', format ))>0)
  {
  	
  	tryCatch({outF=format(as.yearqtr(as.Date(outF)),format);},error=function(e){'GETDATE(): format needs to be a valid string.'});
  	
  } else
  { 
    
    
  	tryCatch({outF=format(as.Date(outF),format);},error=function(e){'GETDATE(): format needs to be a valid string.'});
  	
  }
  
  return(outF);
}


#NOELS code ----------------------------------------

#NOELS support function
.NOELSCompliantInput <- function(x=NULL){
  
  if (is.null(x)) return(FALSE);
  if (is.numeric(x)) return(TRUE);
  if (is.xts(x)) return(TRUE);
  if (is.ts(x)) return(TRUE);
  if (all(is.na(x))) return(TRUE);
  return(FALSE);
}

#ELIMELS remove selcted elements from array/ts.
ELIMELS <- function(x=NULL,idx=NULL,avoidCompliance=FALSE,...)
{
  if (is.null(x)) stop('ELIMELS(): NULL input.'); 
  if (is.null(idx)) stop('ELIMELS(): NULL index.'); 
  if (! is.numeric(idx)) stop('ELIMELS(): index must be a numeric array.');
  
  if (!(.NOELSCompliantInput(x))) stop('ELIMELS(): inputs must be instances of class array, numeric, ts() or xts().')
  
  if (is.ts(x))
  {
    
    if (! avoidCompliance ) tryCatch({.isCompliant(x);},error=function(e){cat('ELIMELS(): x - ',e$message);});
    
    tryCatch({
    #get ts data
    tmpData=coredata(x);
    
    #r has different time array
    if (frequency(x)==1) 
      {
        tmpTsTime=index(x);
      } else 
      {
        tmpTsTime=index(x)+1/frequency(x);
      }
        
    #get indexes of requested obs
    idx=which(tmpTsTime %in% idx);
    
    if (length(idx)==0) return(x);
    
    if (any(idx < 1)) stop('ELIMELS(): index out of bounds.');
    if (any(idx > length(x))) stop('ELIMELS(): index out of bounds.');
    
    idx=unique(-idx);
    
    outF=x[idx];
    
    return(outF);
    
    },error=function(e){cat('ELIMELS(): ',e$message);});
    
    
  } else if (is.xts(x))
  {
    if (! avoidCompliance ) tryCatch({.isCompliant(x);},error=function(e){cat('ELIMELS(): x - ',e$message);});
    
    return(ELIMELS(fromXTStoTS(x,avoidCompliance=TRUE),idx=idx));
    
  } else
  { 
    #all indexes must be postive integers
    if (any(idx %% 1 != 0)) stop('ELIMELS(): index must be a positive integer.');
    if (any(idx < 1)) stop('ELIMELS(): index out of bounds.');
    if (any(idx > length(x))) stop('ELIMELS(): index out of bounds.');
    
    idx=unique(-idx);
    
    outF=x[idx];
    
    return(outF);
    
  }
  
}


#NOELS returns the number of elements of input arguments
NOELS <- function(...)
{
  
  outF=c();
  
  #an input is null
  tryCatch({inputsL=list(...);},error=function(e){stop('NOELS(): an input argument is NULL.')});  
  
  #no args
  if (length(inputsL)==0) stop('NOELS(): at least one parameter is required.');
  
  #inputs are ts xts or numeric or na
  if (all(as.logical(lapply(inputsL,.NOELSCompliantInput))))
  {
    
    #combine array/ts length
    for (idx in (1:length(inputsL))) 
    {
      outF=c(outF,length(inputsL[[idx]]));
    }
    
    
    
    
  } else if (all(as.logical(lapply(inputsL,is.character)))) 
  {
 
    
    #outF=0;
    
    for (idx in (1:length(inputsL))) 
     {
      #print(inputsL[[idx]])
      #outF=outF+length(inputsL[[idx]]);
      outF=c(outF,nchar(inputsL[[idx]]));
     }
    
  } else {
    stop('NOELS(): inputs must be either strings or instances of class numeric, NA, ts(), or xts().');
  }
  
  
  
  return(outF);
}

# GETYEARPERIOD code ----------------------------------------

#GETYEARPERIOD get the year and period arrays of a time series.
GETYEARPERIOD <- function(x=NULL,YEARS='YEAR', PERIODS='PRD', JOIN=FALSE, avoidCompliance=FALSE,...)
{
  
if (is.null(x)) stop('GETYEARPERIOD(): input time series needs to be instance of ts() or xts() class.');
  
  if (is.null(YEARS)) stop('GETYEARPERIOD(): NULL YEARS.');
  if (is.null(PERIODS)) stop('GETYEARPERIOD(): NULL PERIODS.');
  
  if (!(is.character(YEARS))) stop('GETYEARPERIOD(): YEARS must be a string.');
  if (!(is.character(PERIODS))) stop('GETYEARPERIOD(): PERIODS must be a string.');
  
  if (nchar(YEARS)==0) stop('GETYEARPERIOD(): YEARS must be a string.');
  if (nchar(PERIODS)==0) stop('GETYEARPERIOD(): PERIODS must be a string.');  
  
  if (! is.logical(JOIN)) stop('GETYEARPERIOD(): JOIN must be TRUE or FALSE.');  
  
  	
  
  if (! avoidCompliance ) 
  {
    tryCatch({.isCompliant(x);},error=function(e){cat('GETYEARPERIOD(): x - ',e$message);});
    
  }
  
  freq=frequency(x)
  
  if (is.ts(x) )
  {			
  
      
    localStart=start(x)
    
    outY=vector(length = length(x),mode = 'numeric')
    outP=vector(length = length(x),mode = 'numeric')
    
    for (idx in 1:length(x))
    {
      tempYP=normalizeYP(c(localStart[1],localStart[2]-1+idx),f=freq)
      outY[idx]=tempYP[1]
      outP[idx]=tempYP[2]
    }
    
    if (!JOIN)
    {
      outF=list();
        
      #assign years
      #outF[[YEARS]]=trunc(coredata(time(x)));
      outF[[YEARS]]=outY
      
      #assign prds
      #outF[[PERIODS]]=coredata(cycle(x));
      outF[[PERIODS]]=outP
      
    } else
    {
      #outF=cbind(trunc(coredata(time(x))),coredata(cycle(x)));
      outF=cbind(outY,outP)
    }
    
  }
  
  if (is.xts(x) )
  {
    return(TSDATES(fromXTStoTS(x,avoidCompliance=avoidCompliance),YEARS=YEARS, PERIODS=PERIODS,avoidCompliance=TRUE));
  }
  
  return(outF);
}

#legacy compatibility
TSDATES <- GETYEARPERIOD;

# TSPROJECT code ----------------------------------------

#TSPROJECT projects time series into a time interval.
TSPROJECT <- function(x=NULL, TSRANGE=NULL, ARRAY=FALSE, EXTEND=FALSE, avoidCompliance=FALSE,...)
{
  
  if (is.null(x)) stop('TSPROJECT(): input time series needs to be instance of ts() or xts() class.');
  if (is.null(ARRAY)) stop('TSPROJECT(): ARRAY must be boolean.');
  
  if (is.null(TSRANGE)) stop('TSPROJECT(): NULL TSRANGE. Usage: TSPROJECT(ts, TSRANGE=c(START_Y, START_P, END_Y, END_P))');
  if (length(TSRANGE)==0 && is.na(TSRANGE)) stop('TSPROJECT(): NULL TSRANGE. Usage: TSPROJECT(ts, TSRANGE=c(START_Y, START_P, END_Y, END_P))');
  
  if (!(is.numeric(TSRANGE))) stop('TSPROJECT(): NULL TSRANGE. Usage: TSPROJECT(ts, TSRANGE=c(START_Y, START_P, END_Y, END_P))');
  if (!(length(TSRANGE)==4)) stop('TSPROJECT(): NULL TSRANGE. Usage: TSPROJECT(ts, TSRANGE=c(START_Y, START_P, END_Y, END_P))');
  
  outF=x;	
  
  if (! avoidCompliance ) 
  {
    tryCatch({.isCompliant(x);},error=function(e){stop('TSPROJECT(): x - ',e$message);});
    
  }
  
  
  if (is.ts(x) )
  {			
    #get frequency
    localF=frequency(x);
    
    #check TSRANGE
    tryCatch({
      normalStart=normalizeYP(c(TSRANGE[1],TSRANGE[2]),localF);
      normalEnd=normalizeYP(c(TSRANGE[3],TSRANGE[4]),localF);
      
    },error=function(e){stop('TSPROJECT(): TSRANGE misformed. Usage: TSPROJECT(ts, TSRANGE=c(START_Y, START_P, END_Y, END_P))');});
    
    
    suppressWarnings({
      tryCatch({
        
      if (ARRAY)
      {
        return(coredata(window(x,start=normalStart,end=normalEnd,extend=EXTEND)));
      } else
      {
        outF=window(x,start=normalStart,end=normalEnd,extend=EXTEND);
      }
      
      },error=function(e){stop('TSPROJECT(): TSRANGE and input time series do not overlap.');});
      
    });
    
    return(outF);
    
  }
  
  else if (is.xts(x) )
  {
    if (ARRAY)
    {
      return(TSPROJECT(fromXTStoTS(x,avoidCompliance=avoidCompliance), TSRANGE=TSRANGE, ARRAY=ARRAY, EXTEND=EXTEND, avoidCompliance=TRUE));
    } else
    {
      return(fromTStoXTS(TSPROJECT(fromXTStoTS(x,avoidCompliance=avoidCompliance), TSRANGE=TSRANGE, ARRAY=ARRAY, EXTEND=EXTEND, avoidCompliance=TRUE), avoidCompliance=TRUE));
    }
  }
  
  else stop('TSPROJECT(): input time series needs to be instance of ts() or xts() class.');
  
  return(outF);
}



# LOCS code ----------------------------------------


#LOCS find array indexes by condition...
LOCS <- function(x=NULL, options='ALL',...)
{
  
  if (is.null(x)) stop('LOCS(): NULL input.'); 
  
  if (!(is.logical(x))) 
  {
    stop('LOCS(): input must be logical.');    
  }
  
  outF=c();
  
  if (is.null(options)) stop('LOCS(): NULL options.'); 
  if (!(is.character(options))) stop('LOCS(): allowed options are: ALL, UNIQUE, FIRST, LAST.'); 
  
  #univariate check
  if (  (!(is.null(dim(x)))) && (length(dim(x))>1) ) stop('LOCS(): input must be univariate.');
  
  tryCatch({
    outF=which(x);
  },error=function(s){stop('LOCS(): misformed input.')});
  
  if (options=='ALL')
  {
     
    if (length(outF)==0) outF=c(0);
    return(outF);
    
  } else if (options=='UNIQUE')
  {
    if (length(outF)==0) stop('LOCS(): input has no true elements.');
    if (length(outF)!=1) stop('LOCS(): input has more than one true element.');
    return(outF[1]);
    
  } else if (options=='FIRST')
  {
     
    if (length(outF)==0) stop('LOCS(): input has no true elements.');
    return(outF[1]);
    
  } else if (options=='LAST')
  {
     
    if (length(outF)==0) stop('LOCS(): input has no true elements.');
    return(outF[length(outF)]);
  } else
  {
    stop('LOCS(): allowed options are: ALL, UNIQUE, FIRST, LAST.'); 
  }
  
  
  
  return(outF);
}

# NAMELIST code ----------------------------------------

#NAMELIST defines a list of names...
NAMELIST <- function(...)
{
  
  
  outF=c();
  
  #an input is null
  tryCatch({inputsL=list(...);},error=function(e){stop('NAMELIST(): an input argument is NULL')});  
  
  #no args
  if (is.null(inputsL) ) stop('NAMELIST(): at least one parameter is required.');
  if (length(inputsL)==0) stop('NAMELIST(): at least one parameter is required.');
  
  
  
  
  #inputs must be chars
  if (all(as.logical(lapply(inputsL,is.character)))) {
    
    
    
    for (idx in (1:(length(inputsL)))) 
    { 
      #verify that string is a variable name i.e. '9', '', 'special chars', etc...
      #remove dots
      tmpName=.MAKENAMES(inputsL[[idx]],callerName = 'NAMELIST(): ');
      #if (tmpName!=inputsL[[idx]]) warning('NAMELIST(): "',inputsL[[idx]],'" will be converted into "',tmpName,'"');
      
      #make unique
      #if (idx>1 && (tmpName %in% outF[1:(idx-1)]) ) tmpName=paste(tmpName,as.character(idx),sep='');
     
      outF=c(outF,tmpName);
    }
    
    
    #outF=inputsL;
    
    return(outF);
    
    
  } else if (all(as.logical(lapply(inputsL,is.ts))) || all(as.logical(lapply(inputsL,is.xts)))) {
      
    outF=list();
    
    tmpNames=.MAKENAMES(.getFunArgsNames(...),callerName='NAMELIST(): ');
    
      for (idx in (1:(length(inputsL)))) 
      { 
        outF[[tmpNames[idx]]]=inputsL[[idx]];
      }
    
    } else {
    stop('NAMELIST(): all inputs must be either string or instances of class ts() or xts().');
  }
  
  
  
  return(outF);
}


# INTS code ----------------------------------------

#INTS defines an array of integers.
INTS <- function(FROM=NULL,TO=NULL,BY=NULL,...)
{
  if (is.null(FROM)) stop('INTS(): at least one input is required.');
  
  if (!(is.numeric(FROM) ) ) stop('INTS(): inputs must be integers.');
  if (!(FROM%%1==0))  stop('INTS(): inputs must be integers.');
  
  if (!(is.null(TO))) {
  if (!(is.numeric(TO) ) ) stop('INTS(): inputs must be integers.');
  if (!(TO%%1==0))  stop('INTS(): inputs must be integers.');
  }
  
  if (!(is.null(BY))) {
  if (!(is.numeric(BY) ) ) stop('INTS(): inputs must be integers.');
  if (!(BY%%1==0))  stop('INTS(): inputs must be integers.');
  
  }
  
  if (is.null(TO) && is.null(BY)) 
  {
    if (FROM==0) stop('INTS(): magnitude must be greater than 1');
    if (FROM>0) return(seq(1,FROM));
    if (FROM<0) return(seq(-1,FROM));
  }
  
  if (is.null(BY)) return(seq(FROM,TO));
  
  return(seq(FROM,TO,BY));    
    
}
  
# TSINFO code ----------------------------------------

#TSINFO retrieve info on time series.
TSINFO <- function(..., MODE = NULL, avoidCompliance=FALSE)
{
  
  if (is.null(MODE)) stop('TSINFO(): NULL MODE');    
  
  outF=c();
  
  #an input is null
  tryCatch({inputsL=list(...);},error=function(e){stop('TSINFO(): an input argument is NULL.')});  
  
  
  #no ts
  if (length(inputsL)==0)  stop('TSINFO(): an input argument is NULL.');
  
  #must return a nx2 array
  if (MODE=='START2')
  {
    outF=cbind(TSINFO(...,MODE='STARTY',avoidCompliance=avoidCompliance),TSINFO(...,MODE='STARTP',avoidCompliance=TRUE));
    return(outF);
    
  } else if (MODE=='END2')
  {
    outF=cbind(TSINFO(...,MODE='ENDY',avoidCompliance=avoidCompliance),TSINFO(...,MODE='ENDP',avoidCompliance=TRUE));
    return(outF);
  }
  
  #loop list
  for (idx in (1:length(inputsL))) 
  {
    
    #if inputs is ts/xts check if required
    if (!(avoidCompliance) )
    {
      tryCatch({.isCompliant(inputsL[[idx]]);},error=function(e){stop('TSINFO(): ',e$message);});
    }      
    
    if (MODE=='STARTY')
    {
      outF=c(outF,start(fromXTStoTS(inputsL[[idx]],avoidCompliance=TRUE))[1]);
      
    } else if (MODE=='ENDY')
    {
      outF=c(outF,end(fromXTStoTS(inputsL[[idx]]),avoidCompliance=TRUE)[1]);
      
    } else if (MODE=='STARTP')
    {
      outF=c(outF,start(fromXTStoTS(inputsL[[idx]],avoidCompliance=TRUE))[2]);
      
    } else if (MODE=='ENDP')
    {
      outF=c(outF,end(fromXTStoTS(inputsL[[idx]]),avoidCompliance=TRUE)[2]);
      
    } else if (MODE=='START')
    { 
      tmpTS=fromXTStoTS(inputsL[[idx]],avoidCompliance=TRUE);
      tmpSY=as.numeric(start(tmpTS)[1]);
      tmpSP=as.numeric(start(tmpTS)[2]);
      tmpF=as.numeric(frequency(tmpTS));
      outF=c(outF,tmpSY+tmpSP/tmpF);
      
    } else if (MODE=='END')
    {
      tmpTS=fromXTStoTS(inputsL[[idx]],avoidCompliance=TRUE);
      tmpEY=as.numeric(end(tmpTS)[1]);
      tmpEP=as.numeric(end(tmpTS)[2]);
      tmpF=as.numeric(frequency(tmpTS));
      outF=c(outF,tmpEY+tmpEP/tmpF);
      
    } else if (MODE=='FREQ')
    { 
      if (is.ts(inputsL[[idx]]))  outF=c(outF,frequency(inputsL[[idx]]));
      if (is.xts(inputsL[[idx]])) outF=c(outF,frequency(inputsL[[idx]]));        
      
    } else if (MODE=='FACTOR')
    {
      if (is.null(attr(inputsL[[idx]],'ScaleFac'))) 
      {
        outF=c(outF,0);
      }
      else
      {
        outF=c(outF,attr(inputsL[[idx]],'ScaleFac'));    
      }
    } else if (MODE=='UNITS')
    {
      if (is.null(attr(inputsL[[idx]],'Units'))) 
      {
        outF=c(outF,'');
      }
      else
      {
        outF=c(outF,attr(inputsL[[idx]],'Units'));    
      }
    } else if (MODE=='TITLE')
    {
      if (is.null(attr(inputsL[[idx]],'Title'))) 
      {
        outF=c(outF,'');
      }
      else
      {
        outF=c(outF,attr(inputsL[[idx]],'Title'));    
      }
    } else if (MODE=='SOURCE')
    {
      if (is.null(attr(inputsL[[idx]],'Source'))) 
      {
        outF=c(outF,'');
      }
      else
      {
        outF=c(outF,attr(inputsL[[idx]],'Source'));    
      }
    } else 
    {
      stop('TSINFO(): unknown MODE.');
    }
  }
  
  
  return(outF);
}

#TSLOOK retrieve info on time series
TSLOOK <- function(x=NULL, avoidCompliance=FALSE,...)
{
  if (is.null(x)) stop('TSLOOK(): input time series needs to be instance of ts() or xts() class.');
  
  outF=list();  
  
  if (! avoidCompliance ) 
  {
    tryCatch({.isCompliant(x);},error=function(e){stop('TSLOOK(): x - ',e$message);});    
  }
  
  tryCatch({outF[['START']]=TSINFO(x,MODE='STARTY',avoidCompliance=TRUE)},error=function(e){stop('TSLOOK(): cannot retrieve info on time series. - ',e$message);});
  outF[['STARTY']]=outF[['START']];
  tryCatch({outF[['PRD']]=TSINFO(x,MODE='STARTP',avoidCompliance=TRUE)},error=function(e){stop('TSLOOK(): cannot retrieve info on time series. - ',e$message);});
  outF[['STARTP']]=outF[['PRD']];
  tryCatch({outF[['ENDY']]=TSINFO(x,MODE='ENDY',avoidCompliance=TRUE)},error=function(e){stop('TSLOOK(): cannot retrieve info on time series. - ',e$message);});
  tryCatch({outF[['ENDP']]=TSINFO(x,MODE='ENDP',avoidCompliance=TRUE)},error=function(e){stop('TSLOOK(): cannot retrieve info on time series. - ',e$message);});
  tryCatch({outF[['FREQ']]=TSINFO(x,MODE='FREQ',avoidCompliance=TRUE)},error=function(e){stop('TSLOOK(): cannot retrieve info on time series. - ',e$message);});
  
  return(outF);
  
}

# CUMPROD code ----------------------------------------

#CUMPROD defines the cumulative product of the elements of an input object.
CUMPROD <- function(x=NULL, TSRANGE=NULL, avoidCompliance=FALSE,...)
{
  
  if (is.null(x)) stop('CUMPROD(): input needs to be instance of ts() or xts() class or a numeric array c().');
  
  if (!(is.null(TSRANGE)))
  {
    if (length(TSRANGE)==0 && is.na(TSRANGE)) stop('CUMPROD(): null TSRANGE. Usage: CUMPROD(ts, TSRANGE=c(START_Y, START_P, END_Y, END_P))');
    if (!(is.numeric(TSRANGE))) stop('CUMPROD(): null TSRANGE. Usage: CUMPROD(ts, TSRANGE=c(START_Y, START_P, END_Y, END_P))');
    if (!(length(TSRANGE)==4)) stop('CUMPROD(): null TSRANGE. Usage: CUMPROD(ts, TSRANGE=c(START_Y, START_P, END_Y, END_P))');
  }
  
  
  
  outF=x;
  
  #is xts
  if (is.xts(x)) {
    
    return(fromTStoXTS(CUMPROD(fromXTStoTS(x,avoidCompliance=avoidCompliance),TSRANGE=TSRANGE,avoidCompliance=TRUE),avoidCompliance=TRUE));
    
  } else  if (is.ts(x) ) #is ts
  {
    
    if (! avoidCompliance ) tryCatch({.isCompliant(x);},error=function(e){cat('CUMPROD(): x - ',e$message);});
    
    if (!(is.null(TSRANGE)))
    {
      tryCatch({
        outF=TSPROJECT(outF,TSRANGE=TSRANGE,avoidCompliance=TRUE)
      },error=function(e){stop('CUMPROD(): cannot project time series: ',e$message)});
      
    }
    
    
  } else {#is array    
    
    if (!(is.numeric(x))) stop('CUMPROD(): input needs to be either an instance of ts() or xts() class, or a numeric array c().');
    
  }
  
  #trivial  
  if (length(outF)==1) return(outF);    
  
  for (idxTmp in 2: length(outF)) outF[idxTmp]=outF[idxTmp]*outF[idxTmp-1];
  
  return(outF);
}

# CUMSUM code ----------------------------------------

#CUMSUM defines the cumulative sum of the elements of an input object.
CUMSUM <- function(x=NULL, TSRANGE=NULL, MODE=NULL, avoidCompliance=FALSE,...)
{

  if (is.null(x)) stop('CUMSUM(): input needs to be either an instance of ts() or xts() class, or a numeric array c().');
  
  if (!(is.null(TSRANGE)))
  {
    if (length(TSRANGE)==0 && is.na(TSRANGE)) stop('CUMSUM(): error in TSRANGE. Usage: CUMSUM(ts, TSRANGE=c(START_Y, START_P, END_Y, END_P))');
    if (!(is.numeric(TSRANGE))) stop('CUMSUM(): error in TSRANGE. Usage: CUMSUM(ts, TSRANGE=c(START_Y, START_P, END_Y, END_P))');
    if (!(length(TSRANGE)==4)) stop('CUMSUM(): error in TSRANGE. Usage: CUMSUM(ts, TSRANGE=c(START_Y, START_P, END_Y, END_P))');
  }
  
  if (!(is.null(MODE)))
  {
  	if (length(MODE)==0 && is.na(MODE)) stop('CUMSUM(): MODE can be YEAR, MONTH or NULL.');
  
  }
  
  outF=x;
  
  #is xts
  if (is.xts(x)) {
    
    return(fromTStoXTS(CUMSUM(fromXTStoTS(x,avoidCompliance=avoidCompliance),TSRANGE=TSRANGE,MODE=MODE,avoidCompliance=TRUE),avoidCompliance=TRUE));
    
  } else  if (is.ts(x) ) #is ts
  {
    
    if (! avoidCompliance ) tryCatch({.isCompliant(x);},error=function(e){cat('CUMSUM(): x - ',e$message);});
    
    if (!(is.null(TSRANGE)))
    {
      tryCatch({
        outF=TSPROJECT(outF,TSRANGE=TSRANGE,avoidCompliance=TRUE)
      },error=function(e){stop('CUMSUM(): cannot project time series: ',e$message)});
      
    }
    
        
  } else {#is array    
    
    if (!(is.numeric(x))) stop('CUMSUM(): input needs to be either an instance of ts() or xts() class, or a numeric array.');
            
  }
  
  #trivial  
  if (length(outF)==1) return(outF);    
 
  #array or ts with no mode
	if ( ! is.ts(x) || (is.ts(x) && is.null(MODE) ))
	{
		for (idxTmp in 2: length(outF)) outF[idxTmp]=outF[idxTmp]+outF[idxTmp-1];
	} 
  else { #ts with mode
		if (MODE=='YEARLY')
		{
			outFdates=as.integer(GETDATE(outF,NULL,'%Y',avoidCompliance=TRUE));			
			
			if (length(outFdates) != length(outF)) stop('CUMSUM(): unknown error.');
			
			for (idxTmp in 2: length(outF)) if (outFdates[idxTmp] == outFdates[idxTmp-1]) outF[idxTmp]=outF[idxTmp]+outF[idxTmp-1];
			
		} else	if (MODE=='MONTHLY')
		{
			outFdates=as.integer(GETDATE(outF,NULL,'%m',avoidCompliance=TRUE));			
			
			if (length(outFdates) != length(outF)) stop('CUMSUM(): unknown error.');
			
			for (idxTmp in 2: length(outF)) if (outFdates[idxTmp] == outFdates[idxTmp-1]) outF[idxTmp]=outF[idxTmp]+outF[idxTmp-1];
			
		}
		else
		{
			for (idxTmp in 2: length(outF)) outF[idxTmp]=outF[idxTmp]+outF[idxTmp-1];
		}
	}
  
  return(outF);
}

#CUMULO defines the cumulative sum of the elements of an ts in the year
CUMULO <- function(x=NULL, TSRANGE=NULL,avoidCompliance=FALSE,...)
{
	tryCatch({ return(CUMSUM(x,TSRANGE=TSRANGE,MODE='YEARLY',avoidCompliance=avoidCompliance));},error=function(e){stop('CUMULO(): x - ',e$message);});
	
}

# TABIT code ----------------------------------------

#TABIT print ts in human readable form
TABIT <- function(..., TSRANGE=NULL, digits=getOption('digits'),avoidCompliance=FALSE)
{
  
  outF=c();
  
  if ((! is.finite(digits) )|| (digits %% 1 !=0) || digits<=0 || digits > 22) 
    stop('TABIT(): digits must be an integer between 1 and 22.');
  
  
  #an input is null
  tryCatch({inputsL=list(...);},error=function(e){stop('TABIT(): an input argument is NULL.')});  
  
  #no args
  if (length(inputsL)==0) stop('TABIT(): at least one parameter is required.');
  
 
  if (! is.null(TSRANGE)  && (! is.numeric(TSRANGE) || length(TSRANGE) != 4)) {    
    stop("TABIT(): TSRANGE must be a 4-elements numerical vector.");    
  } 
  
  
  #check compliance
  if (! avoidCompliance ) 
  {	
    tryCatch({lapply(inputsL,.isCompliant);},error=function(e){stop('TABIT(): ',e$message);});		
  }
  
  outFreq=NULL;
  
  #project if TSRANGE specified
  for (idx in (1:length(inputsL)))
  {
    #convert to ts if xts  
    if (is.xts(inputsL[[idx]])) inputsL[[idx]]=fromXTStoTS(inputsL[[idx]],avoidCompliance=TRUE);
    
    if (idx==1) outFreq=frequency(inputsL[[idx]]);
    
    if (frequency(inputsL[[idx]])!=outFreq) stop('TABIT(): all time series must have the same frequency.');
    
    if (!(is.null(TSRANGE)))
    {
      tryCatch({
        inputsL[[idx]]=TSPROJECT(inputsL[[idx]],TSRANGE=TSRANGE,avoidCompliance=TRUE)
      },error=function(e){stop('TABIT(): cannot project time series: ',e$message)});
      
    }
  }
  
  
  
  stdFormat=paste0("% -",digits+8,".",digits,'g');
  stdFormatS=paste0("% -",digits+8,'s');
  
  #backup list
  seriesListADSL=inputsL
  
  #contains dates of observations
  arrayOfDates=c();
  
  #array of names max 10 chars
  seriesListADSLstr=sprintf(stdFormatS,.getFunArgsNames(...));
   
  for (idx in (1:length(seriesListADSL))){
    
    localStart=start(seriesListADSL[[idx]])
    for (idx2 in (1:length(seriesListADSL[[idx]]))){
      
      localYP=normalizeYP(c(localStart[1],localStart[2]+idx2-1),f = outFreq)
                          
      arrayOfDates=c(arrayOfDates,
                     1000*localYP[1]+localYP[2]);
      
    }
  }
  
  if (is.null(arrayOfDates))  {stop('TABIT(): empty arrayOfDates.');}
  
  #remove duplicates and sort
  arrayOfDates=sort(unique(arrayOfDates));
  
  #add missings days
  arrayOfDatesMin=min(arrayOfDates);
  arrayOfDatesMax=max(arrayOfDates);
  
  #print(arrayOfDatesMin);
  #print(arrayOfDatesMax);
  
  tmpIdx=arrayOfDatesMin;
  arrayOfDates2=c(tmpIdx);
  
  while (tmpIdx<arrayOfDatesMax)
  {
    if (((tmpIdx %% 1000) %% outFreq) == 0 )
    {
      tmpIdx=(tmpIdx+1000)-(tmpIdx %% 1000)+1;
    } else
    {
      tmpIdx=tmpIdx+1;
    }
    
    arrayOfDates2=c(arrayOfDates2,tmpIdx);
  }
  
  arrayOfDates=arrayOfDates2;
  
  outF=arrayOfDates;
  #print(outF);
  
  #create string of names
  seriesListADSLstr=paste(seriesListADSLstr,collapse=', ');
  
  #init some arrays
  startYint=c();
  startPint=c();
  endYint=c();
  endPint=c();
  freqint=c();
  
  #get start, end, freq 
  for (idx in (1:length(seriesListADSL))) {
    
    startYint[idx]=as.integer(start(seriesListADSL[[idx]])[1]);
    startPint[idx]=as.integer(start(seriesListADSL[[idx]])[2]);
    
    endYint[idx]=as.integer(end(seriesListADSL[[idx]])[1]);
    endPint[idx]=as.integer(end(seriesListADSL[[idx]])[2]);
    
    freqint[idx]=as.integer(frequency(seriesListADSL[[1]])); 
    
    #cat(startYint[idx],startPint[idx],endYint[idx],endPint[idx],freqint[idx],'\n');
  }
  
  #print header
  cat('\n      Date, Prd., ',seriesListADSLstr,'\n\n',sep='');
  
  
  #main cycle
  for (idx in (1:length(arrayOfDates))){
    
    
    localYint=trunc(arrayOfDates[idx]/1000);
    localPint=arrayOfDates[idx] %% 1000;
    
    #cat(localYint,' - ',localPint);
   
    #init row str
    tmpOutStr='';
    
    #init array of out values
    value=c();
    
    #cycle in time series
    for (idx2 in (1:length(seriesListADSL))) 
    {
      
          
      #a time series can have null value in selected date
      value[idx2]= sprintf(stdFormatS,'');          
      
      
      #check if real values exist on DB 'cause R set NA on extra observation
      if ((localYint>endYint[idx2])  || ( (localYint==endYint[idx2])  && (localPint>endPint[idx2])   ) ) 
      {
        value[idx2]=  sprintf(stdFormatS,'');
      } else if ((localYint<startYint[idx2])  || ( (localYint==startYint[idx2])  && (localPint<startPint[idx2])   ) ) 
      {
        value[idx2]=  sprintf(stdFormatS,'');
        
      } else 
      {
        value[idx2]= try(
          #substring(
          sprintf(stdFormat,as.numeric(seriesListADSL[[idx2]][(localYint-startYint[idx2])* freqint[idx2]+ (1+localPint-startPint[idx2])]))
                    #,1,10)
                    );                
      }
      
      
      #compose row str      
      tmpOutStr=paste(tmpOutStr,', ',value[idx2],sep='');
      
      
    }  
    
    #print out row
    if (outFreq==1) {cat(paste(sprintf("%10s",GETDATE(ts(1,start=c(localYint,localPint),frequency=outFreq),1,format='%Y',avoidCompliance=TRUE)),', ', sprintf("%-4d",localPint), tmpOutStr, '\n',sep=''),sep='');   }
    else if (outFreq==2) {cat(paste(sprintf("%10s",GETDATE(ts(1,start=c(localYint,localPint),frequency=outFreq),1,format='%Y',avoidCompliance=TRUE)),', ', sprintf("%-4d",localPint), tmpOutStr, '\n',sep=''),sep='');}
    else if (outFreq==4) {cat(paste(sprintf("%10s",GETDATE(ts(1,start=c(localYint,localPint),frequency=outFreq),1,format='%Y Q%q',avoidCompliance=TRUE)),', ', sprintf("%-4d",localPint), tmpOutStr, '\n',sep=''),sep='');}   
    else if (outFreq==12) {cat(paste(sprintf("%10s",GETDATE(ts(1,start=c(localYint,localPint),frequency=outFreq),1,format='%b %Y',avoidCompliance=TRUE)),', ', sprintf("%-4d",localPint), tmpOutStr, '\n',sep=''),sep=''); }  
    else {cat(paste(sprintf("%10s",GETDATE(ts(1,start=c(localYint,localPint),frequency=outFreq),1,avoidCompliance=TRUE)),', ', sprintf("%-4d",localPint), tmpOutStr, '\n',sep=''),sep='');   }
    
    
    
  }
  cat('\n');
  
  #return(TRUE);
}



# trim --------------------------------------------

#.TRIM delete trailing and leading spaces on a string
.TRIM <- function(s=NULL)
{
 if (is.null(s)) return(NULL);
 if (! is.character(s)) return(s);
 
 outF=c();
 
 for(idx in 1:length(s)) {
   
   outF=c(outF,gsub("^\\s+|\\s+$", "", s[idx]));
 }
 
 return(outF);
 
}

# make.names  --------------------------------------------

#make.names convert string in string that can be var names
.MAKENAMES <- function(s=NULL,giveWarning=TRUE,callerName=NULL)
{
  if (is.null(s)) stop('.MAKENAMES(): input must be a either string or an array of strings.');
  if (! is.character(s)) stop('.MAKENAMES(): input must be either a string or an array of strings.');
  
  outF=c();
  
  outF=make.names(s,unique=FALSE); 
  
  #this must be after 'cause make.names adds dots
  outF=gsub('\\.','',outF);
  
     
    suffixVar=0;
    for (idx in 1: length(s)) 
    { 
      #wont allow empty string
      if (outF[idx] == '') 
      { 
        suffixVar=suffixVar+1;
        outF[idx]=paste('var',suffixVar,sep='');  
      }
      
      if (giveWarning==TRUE) 
      { 
      if (s[idx]!=outF[idx]) cat(ifelse(is.null(callerName),'.MAKENAMES(): warning, ',callerName),s[idx],' name will be converted in ',outF[idx],'\n',sep='');
      }
    
    }
  
  return(outF);
  
}


# INDEXNUM code -------------------------------------------------------------------------

INDEXNUM <- function(x=NULL, BASEYEAR=NULL, avoidCompliance=FALSE,...)
{
  
  if (is.null(x)) stop('INDEXNUM(): input needs to be an instance of BIMETS class.');
  
  if (is.null(BASEYEAR)) stop('INDEXNUM(): NULL BASEYEAR.');
  if (! is.numeric(BASEYEAR) || BASEYEAR<0 || (BASEYEAR %% 1 !=0)) stop('INDEXNUM(): BASEYEAR must be a positive integer.');
  
  outF=x;  
  
  if (! avoidCompliance ) 
  {
    tryCatch({.isCFCompliant(x);},error=function(e){stop('INDEXNUM(): x - ',e$message);});
    
  }
  
  #create annual time series
  annualTmp=ANNUAL(x,fun='AVE',avoidCompliance=TRUE);
  
  urtslookTmp=TSLOOK(annualTmp,avoidCompliance=TRUE)
  
  #check if base year is in ts range
  if (BASEYEAR<urtslookTmp$START || BASEYEAR>urtslookTmp$ENDY) stop('INDEXNUM(): the time series is not defined in the base year.')
  
  #check if factor is finite  
  if (is.na(annualTmp[[BASEYEAR,1]])) stop('INDEXNUM(): the base year contains some missing value.')  
  if (annualTmp[[BASEYEAR,1]]==0) stop('INDEXNUM(): the base year average is zero.')
  
  #calc factor
  factor=100/annualTmp[[BASEYEAR,1]]
  #print(factor)
  
  #normalize input
  outF=x*factor;
  
  return(outF);
  
}





