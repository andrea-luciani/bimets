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
# @description: BIMETS - Modeling FUNs 		    
#
# @authors: ANDREA LUCIANI, ROBERTO STOK 
#
# @copyright: Bank of Italy
#
# @license: GPL-3 - GNU GENERAL PUBLIC LICENSE - Version 3
#
#############################################################################


# support FUNs -------------------------------------------

#combine list by names
.combineList = function(list1=NULL,list2=NULL)
{
  
  if (is.null(list1)) return(list2);
  
  if ((!is.list(list1)) || (!is.list(list2))) stop('.combineList(): inputs must be of class list().'); 
  
  outList=list1;
  
  matchedNames=match(names(list2), names(list1))
  commonIndexes=which(!is.na(matchedNames))
  
  if (length(commonIndexes) > 0)
  {
    # take values from list2 in matching dimension names
    matchedNames = matchedNames[!is.na(matchedNames)]
    outList[matchedNames] = list2[commonIndexes]
    
    # append elements of 'list2' with unmatched names
    outList = c(outList, list2[-commonIndexes])
  }
  else
  {
    outList = c(outList, list2)
  }
  
  return(outList)
}

#rename list items
.renameListItems = function(inputList=NULL,namesInput=NULL,namesOutput=NULL)
{
  #check inputs
  if (is.null(inputList) || !(inherits(inputList,'list'))) stop(paste0('.renameListItems(): please provide an input list'));
  if (length(namesInput) != length(namesOutput)) stop(paste0('.renameListItems(): input and output names array must have the same length.'));
  
  #prepare output
  outL=inputList;
  
  #find indexes to be modified
  idxs=which(names(outL) %in% namesInput);
  names(outL)[idxs]=namesOutput[idxs];
  
  return(outL);
}

.extractFunsNames = function(inputS=NULL)
{
  if (is.null(inputS)) return(character(0))
  
  out=regmatches(inputS,gregexpr('(\\w+)\\(',inputS))
  out=gsub('\\(','',out[[1]])
  return(out)
}

#return list of unkonw fun names case ignored
.unknownFunsNames = function(inputS=NULL,reservedKeyw)
{
 
  allFunNames=.extractFunsNames(inputS);
  
  return( allFunNames[! (toupper(allFunNames) %in% reservedKeyw)])
  
}

.RegExGlobalDefinition = function()
{
  #symbols/funcs allowed on EQ
  reservedKeyw=c('LOG','TSLAG','LAG','MOVAVG','MAVE','EXP','TSDELTA','DEL','ABS','MOVSUM','MTOT','TSDELTAP','TSDELTALOG') 

  #list of availables lhs funs
  allowedLhsEQfuns=c('LOG','EXP','TSDELTA','DEL','TSDELTAP','TSDELTALOG');
  allowedLhsEQfunsPub=c('LOG','EXP','TSDELTA','TSDELTAP','TSDELTALOG');
  
  #what to remove from equations in order to obtains symbol names
  #symOnEqCleaner='(\\+|\\-|\\(|\\)|\\*|\\/|\\,|\\=|LOG\\(|LAG\\(|MAVE\\(|EXP\\(|DEL\\(|ABS\\()';
  symOnEqCleaner=paste0('(\\+|\\-|\\(|\\)|\\*|\\/|\\,|\\^|\\=|<|>|\\.GE\\.|\\.LE\\.|\\.GT\\.|\\.LT\\.|\\.EQ\\.',paste(paste0('|',reservedKeyw,'\\('),collapse=''),')');
  symOnIfCleaner=paste0('(\\&|\\||\\+|\\-|\\(|\\)|\\*|\\/|\\,|\\^|\\=|<|>|\\.GE\\.|\\.LE\\.|\\.GT\\.|\\.LT\\.|\\.EQ\\.',paste(paste0('|',reservedKeyw,'\\('),collapse=''),')');
  
  
  #regex allowed on symbol names
  allowedCharOnName='^[a-zA-Z_]+[a-zA-Z0-9_]*$';
  
  #regex allowed on numbers (we use only 'strongCharsOnNumbs')
  charsOnNumbs='([0-9]+[\\.]?|[\\.]?[0-9]+|[0-9]+\\.[0-9]+|pi)';
  charsOnNumWithSign=paste0('(\\+|-)?',charsOnNumbs);
  strongCharOnNumbs=paste0('^',charsOnNumbs,'$');
  strongCharOnNumbsWithSign=paste0('^',charsOnNumWithSign,'$');
  
  return(list(reservedKeyw=reservedKeyw,
              symOnEqCleaner=symOnEqCleaner,
              symOnIfCleaner=symOnIfCleaner,
              allowedCharOnName=allowedCharOnName,
              charsOnNumbs=charsOnNumbs,
              charsOnNumWithSign=charsOnNumWithSign,
              strongCharOnNumbs=strongCharOnNumbs,
              strongCharOnNumbsWithSign=strongCharOnNumbsWithSign,
              allowedLhsEQfuns=allowedLhsEQfuns,
              allowedLhsEQfunsPub=allowedLhsEQfunsPub))
}

#move vars from local env localE into global, usefull in debugging
.copyLocalEnvInToGlobal = function(sourceE=NULL,destE=.GlobalEnv,...)
{
  if (is.null(sourceE)) 
  {
    cat('Please provide a valid environment.\n');
  } else {
    lapply(names(sourceE),function(n){assign(n,get(n,envir = sourceE),envir = destE);});
  }
}

.parseLhsEQ = function (inputS='', allowedLhsEQfuns=NULL, ...)
{
   
  outL=list();
  
  #cycle in available lhs funs
  
  for (funName in allowedLhsEQfuns)
  {
     
    #search current fun name in input expression
    tmpList=.explodeFunctionCall(inputS,funName)
    
    #we are looking for first expression
    if (length(tmpList)>0 && tmpList$firstLoc==1)
    {
      outL=tmpList;
      outL$funName=funName;
      outL$raw=inputS;
      
      return(outL);
    }
    
  }
  
  #no lhs fun found so return identity
  outL$funName='I';
  outL$args=inputS;
  outL$raw=inputS;
  
  return(outL);
  
}

#get text components of a function call, e.g. arguments and stuff
.explodeFunctionCall = function (inputS='',fName='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeFunctionCall(): NULL input string'));
  if (is.null(fName)) stop(paste0('.explodeFunctionCall(): NULL function name'));
  if ((! is.character(inputS)) || (! is.character(fName)))
    stop(paste0('.explodeFunctionCall(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeFunctionCall(): empty input string'));
  if (''==(fName)) stop(paste0('.explodeFunctionCall(): empty function name'));
  
  outL=list();
  
  #check fun name exists in string
  if (grepl("\\.",fName))
  {
    fNameEx=paste0('\\.\\b',gsub("\\.","",fName),'\\b\\(');
  } else {
    #must move dot out of \\b
    fNameEx=paste0('\\b',fName,'\\b\\(');
  }
  
  #check function call exists in string
  firstLoc=regexpr(fNameEx,inputS);
  
  #if none then exit
  if (firstLoc==-1) return(outL);
  
  #remove attributes from regex results
  attributes(firstLoc)=NULL;
  
  #init tmp vars... consider we start whit one open (
  openP=1;
  openSP=0;
  
  #outputs
  commaPos=c();
  args=c();
  argsStart=firstLoc+nchar(fName)+1
  argsEnd=c()
  
  #cycle in chars
  for (idxS in (argsStart):nchar(inputS))
  {  
    #get char
    tmpChar=substr(inputS,idxS,idxS);
    
    if (tmpChar=='(')
    {
      openP=openP+1;
    } else if (tmpChar==')')
    {
      openP=openP-1;
      if (openP==0 && openSP==0) 
      {
        #we are exiting function call
        argsEnd=idxS-1;
        break;
      }
    } else if (tmpChar==',' && openP==1 && openSP==0)
    {
      #we are in next argument
      commaPos=c(commaPos,idxS);
      
    } else if (tmpChar=='[')
    {
      #we need to account also squared []
      openSP=openSP+1;
      
    } else if (tmpChar==']')
    {
      openSP=openSP-1
    }
    
  }
  
  #checks
  if (openP!=0) 
    stop(paste0('.explodeFunctionCall(): syntax error, wrong parentheses matching.'))
  
  #NULL argument
  if (length(commaPos)>1 && min(diff(commaPos))==1)
    stop(paste0('.explodeFunctionCall(): syntax error, empty arguments.'))
  
  #NULL fun body
  if (argsEnd-argsStart<0)
    stop(paste0('.explodeFunctionCall(): syntax error, empty arguments.'))
  
  #get arguments
  argsCount=length(commaPos)+1
  
  if (argsCount==1)
  {
    args=c(args,substr(inputS,argsStart,argsEnd))
    
  } else
    for (idxA in 1:argsCount)
    {
      #first argumnet
      if (idxA==1)
      {
        args=c(args,substr(inputS,argsStart,commaPos[idxA]-1))
        
      } else if (argsCount==idxA)
      {
        #last argument
        args=c(args,substr(inputS,commaPos[idxA-1]+1,argsEnd))
      } else
      {
        args=c(args,substr(inputS,commaPos[idxA-1]+1,commaPos[idxA]-1))
      }
    }
  
  #trim spaces
  args=gsub("\\s*", "", args);
  
  if (any(args=='')) stop(('.explodeFunctionCall(): syntax error, empty arguments.'))
    
  #results
  outL$firstLoc=firstLoc
  outL$commaPos=commaPos
  outL$argsStart=argsStart
  outL$argsEnd=argsEnd
  outL$argsCount=argsCount
  outL$args=args
  
  return(outL)
  
}

#add indexes to a list of var names, e.g. myVAR -> myVAR[indexN,]
.appendIndexToVars = function (inputS='', vName=c(), indexN='', ...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.appendIndexToVars(): NULL input string'));
  if (is.null(vName)) stop(paste0('.appendIndexToVars(): NULL variables name'));
  if (is.null(indexN)) stop(paste0('.appendIndexToVars(): NULL index name'));
  
  if ((! is.character(inputS)) 
      || (! is.character(vName))
      || (! is.character(indexN))
  )
    stop(paste0('.appendIndexToVars(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.appendIndexToVars(): empty input string'));
  if (0==any(nchar(vName))) stop(paste0('.appendIndexToVars(): there is an empty variable name'));
  if (''==(indexN)) stop(paste0('.appendIndexToVars(): empty index name'));
  
  outS=inputS;
  
  #replace varName with varName[indexN,], if varName is not a fun call
  for (idxV in 1:length(vName))
    outS=gsub(paste0('\\b(',
                     vName[idxV],
                     ')\\b($|[^\\(])'),
              paste0('\\1\\[',indexN,',\\]\\2'),
              outS)
  
  return(outS);
  
}

#add _ADDFACTOR to a list of var names, e.g. myVAR -> myVAR_ADDFACTOR
.appendAFtoEndVars = function (inputS='', vName=c(), AF='__ADDFACTOR', ...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.appendAFtoEndVars(): NULL input string'));
  if (is.null(vName)) stop(paste0('.appendAFtoEndVars(): NULL variables name'));
  if (is.null(AF)) stop(paste0('.appendAFtoEndVars(): NULL AF name'));
  
  if ((! is.character(inputS)) 
      || (! is.character(vName))
      || (! is.character(AF))
  )
    stop(paste0('.appendAFtoEndVars(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.appendAFtoEndVars(): empty input string'));
  if (0==any(nchar(vName))) stop(paste0('.appendAFtoEndVars(): there is an empty variable name'));
  if (''==(AF)) stop(paste0('.appendAFtoEndVars(): empty AF name'));
  
  outS=inputS;
  
  #replace varName with varName_ADDFACTOR, if varName is not a fun call
  for (idxV in 1:length(vName))
    outS=gsub(paste0('\\b(',
                     vName[idxV],
                     ')\\b($|[^\\(])'),
              paste0('\\1\\',AF,'\\2'),
              outS)
  
  return(outS);
  
}

#add an integer to var index varName[a,] -> varName[a+b,]
.addToIndexInString = function (inputS='', value=0, ...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.addToIndexInString(): NULL input string'));
  if (is.null(value)) stop(paste0('.addToIndexInString(): NULL value'));
  
  if ((! is.character(inputS)) )
    stop(paste0('.addToIndexInString(): inputs need to be strings'));
  
  if ((! is.numeric(value)) 
      || (! value %% 1 == 0)
  )
    stop(paste0('.addToIndexInString(): value needs to be an integer'));
  
  if (''==(inputS)) stop(paste0('.addToIndexInString(): empty input string'));
  
  #init tmp vars
  openIdxs=c()
  closeIdxs=c();
  
  #cycle in chars and get positions of squared
  for (idxS in 1:nchar(inputS))
  {
    #get current char
    tmpChar=substr(inputS,idxS,idxS);
    
    if (tmpChar=='[')
    {
      openIdxs=c(openIdxs,idxS);
    }
    
    if (tmpChar==']')
    {
      closeIdxs=c(closeIdxs,idxS);
    }
    
  }
  
  #same counts check
  if (length(openIdxs) != length(closeIdxs))
    stop(paste0('.addToIndexInString(): syntax error on indices'));
  
  #nothing to do
  if (length(openIdxs) ==0) return(inputS);
  
  #empty indexes
  if (min(closeIdxs-openIdxs)<2)
    stop(paste0('.addToIndexInString(): syntax error on indices'));
  
  #no interescting [ [ ] .. [ ] ]
  if (max(+c(-Inf,closeIdxs)-c(openIdxs,Inf))>=-1)
    stop(paste0('.addToIndexInString(): syntax error on indices'));
  
  tmpP=1;
  outS='';
  
  tryCatch(
    {
      
      #cycle every index []
      for (idxI in 1:length(openIdxs))
      { 
        #get index str
        tmpS=substr(inputS,openIdxs[idxI]+1,closeIdxs[idxI]-1);
        tmpS=strsplit(tmpS,',')[[1]][1]
        
        
        #get index as integer
        currentI=eval(parse(text=tmpS));
        
        if (! is.finite(currentI)) stop();
        
        
        
        #add value to current index
        currentI=currentI+value;
        
        #replace index in string
        outS=paste0(outS
                    ,substr(inputS,tmpP,openIdxs[idxI])
                    ,currentI,',');
        
        tmpP=closeIdxs[idxI];
      }
    },
    error=function(e)
    {stop(paste0('.addToIndexInString(): cannot evaluate index "',tmpS,'" ',e$message))}
  ) 
  
  #...create last output
  outS=paste0(outS
              ,substr(inputS,tmpP,nchar(inputS)))
  
  return(outS);
} 

#transform first TSLAG in inputS into exploded expression with related lags
.explodeSingleTSLAG = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeSingleTSLAG(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeSingleTSLAG(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeFunctionCall(): empty input string'));
  
  #init tmp vars
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  outL$tsLag=0;
  
  tryCatch({
    outSE=.explodeFunctionCall(inputS,'.MODEL_TSLAG');
  },error=function(e){stop(paste0('.explodeSingleTSLAG(): ',e$message))}
  )
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0;
  
  #no lag provided so is 1
  if (outSE$argsCount==1) 
  {
    tsLag=1;
  } else
  { #otherwise eval
    tryCatch({
      tsLag=eval(parse(text=outSE$args[2]));
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>=0)) stop()
    },error=function(e){stop(paste0('.explodeSingleTSLAG(): lag value must be a non-negative integer. ',e$message))}
    )
  }
  
  #replace varName[a,] -> varName[a-lag,]
  outL$output=paste0(ifelse(outSE$firstLoc>1,substr(inputS,1,outSE$firstLoc-1),'')
                     ,'(',
                     .addToIndexInString(outSE$args[1],-tsLag),
                     ')',
                     ifelse(outSE$argsEnd<nchar(inputS)-1,substr(inputS,outSE$argsEnd+2,nchar(inputS)),'')
  );
  
  #store flag if mod has been made
  outL$flag=TRUE;
  outL$tsLag=tsLag;
  
  return(outL)
}

#explode all TSLAG()
.explodeTSLAG = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeTSLAG(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeTSLAG(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeTSLAG(): empty input string'));
  
  #init tmp var
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  
  tryCatch({
    
    #explode first TSLAG
    tmpL=.explodeSingleTSLAG(inputS);
    
    #while found a TSLAG()...
    while(tmpL$flag)
    {
      outL$output=tmpL$output;
      outL$flag=TRUE;
      tmpL=.explodeSingleTSLAG(tmpL$output);
    }
  },error=function(e){stop(paste0('.explodeTSLAG(): ',e$message))})
  return(outL);
  
}

#transform first DELTA in inputS into exploded expression with related lags
.explodeSingleTSDELTA = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeSingleTSDELTA(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeSingleTSDELTA(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeSingleTSDELTA(): empty input string'));
  
  #init tmp var
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  outL$tsLag=0;
  
  tryCatch({
    outSE=.explodeFunctionCall(inputS,'.MODEL_TSDELTA');
  },error=function(e){stop(paste0('.explodeSingleTSDELTA(): ',e$message))}
  )
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0;
  
  #if no specified lag=1
  if (outSE$argsCount==1) 
  {
    tsLag=1;
  } else
  {
    tryCatch({
      tsLag=eval(parse(text=outSE$args[2]));
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>0)) stop()
    },error=function(e){stop(paste0('.explodeSingleTSDELTA(): DELTA value must be a positive integer. ',e$message))}
    )
  }
  
  #DELTA(varName[a,],b)=((varName[a,])-(varName[a-b,]))
  outL$output=paste0(ifelse(outSE$firstLoc>1,substr(inputS,1,outSE$firstLoc-1),'')
                     ,'((',
                     outSE$args[1],
                     ')-(',
                     .addToIndexInString(outSE$args[1],-tsLag),
                     '))',
                     ifelse(outSE$argsEnd<nchar(inputS)-1,substr(inputS,outSE$argsEnd+2,nchar(inputS)),'')
  );
  
  #store flag if mod has been made
  outL$flag=TRUE;
  outL$tsLag=tsLag;
  
  return(outL)
}

#explode all DELTA()
.explodeTSDELTA = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeTSDELTA(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeTSDELTA(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeTSDELTA(): empty input string'));
  
  #init tmp var
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  
  tryCatch({
    
    tmpL=.explodeSingleTSDELTA(inputS);
    
    #while we have more DELTA()
    while(tmpL$flag)
    {
      outL$output=tmpL$output;
      outL$flag=TRUE;
      tmpL=.explodeSingleTSDELTA(tmpL$output);
    }
  },error=function(e){stop(paste0('.explodeTSDELTA(): ',e$message))})
  
  return(outL);
  
}

#transform first DELTAP in inputS into exploded expression with related lags
.explodeSingleTSDELTAP = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeSingleTSDELTAP(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeSingleTSDELTAP(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeSingleTSDELTAP(): empty input string'));
  
  #init tmp var
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  outL$tsLag=0;
  
  tryCatch({
    outSE=.explodeFunctionCall(inputS,'.MODEL_TSDELTAP');
  },error=function(e){stop(paste0('.explodeSingleTSDELTAP(): ',e$message))}
  )
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0;
  
  #if no specified lag=1
  if (outSE$argsCount==1) 
  {
    tsLag=1;
  } else
  {
    tryCatch({
      tsLag=eval(parse(text=outSE$args[2]));
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>0)) stop()
    },error=function(e){stop(paste0('.explodeSingleTSDELTAP(): DELTAP value must be a positive integer. ',e$message))}
    )
  }
  
  
  
  #DELTAP(varName[a,],b)=(100*((varName[a,])-(varName[a-b,]))/(varName[a-b,]))
  outL$output=paste0(ifelse(outSE$firstLoc>1,substr(inputS,1,outSE$firstLoc-1),'')
                     ,'(100*((',
                     outSE$args[1],
                     ')-(',
                     .addToIndexInString(outSE$args[1],-tsLag),
                     '))/(',.addToIndexInString(outSE$args[1],-tsLag),'))',
                     ifelse(outSE$argsEnd<nchar(inputS)-1,substr(inputS,outSE$argsEnd+2,nchar(inputS)),'')
                    );
  
  #store flag if mod has been made
  outL$flag=TRUE;
  outL$tsLag=tsLag;
  
  return(outL)
}

#explode all DELTAP()
.explodeTSDELTAP = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeTSDELTAP(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeTSDELTAP(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeTSDELTAP(): empty input string'));
  
  #init tmp var
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  
  tryCatch({
    
    tmpL=.explodeSingleTSDELTAP(inputS);
    
    #while we have more DELTAP()
    while(tmpL$flag)
    {
      outL$output=tmpL$output;
      outL$flag=TRUE;
      tmpL=.explodeSingleTSDELTAP(tmpL$output);
    }
  },error=function(e){stop(paste0('.explodeTSDELTAP(): ',e$message))})
  
  return(outL);
  
}

#transform first DELTALOG in inputS into exploded expression with related lags
.explodeSingleTSDELTALOG = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeSingleTSDELTALOG(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeSingleTSDELTALOG(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeSingleTSDELTALOG(): empty input string'));
  
  #init tmp var
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  outL$tsLag=0;
  
  tryCatch({
    outSE=.explodeFunctionCall(inputS,'.MODEL_TSDELTALOG');
  },error=function(e){stop(paste0('.explodeSingleTSDELTALOG(): ',e$message))}
  )
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0;
  
  #if no specified lag=1
  if (outSE$argsCount==1) 
  {
    tsLag=1;
  } else
  {
    tryCatch({
      tsLag=eval(parse(text=outSE$args[2]));
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>0)) stop()
    },error=function(e){stop(paste0('.explodeSingleTSDELTALOG(): DELTALOG value must be a positive integer. ',e$message))}
    )
  }
  
  
  
  #DELTALOG(varName[a,],b)=(log((varName[a,])/(varName[a-b,])))
  outL$output=paste0(ifelse(outSE$firstLoc>1,substr(inputS,1,outSE$firstLoc-1),'')
                     ,'(log((',
                     outSE$args[1],
                     ')/(',
                     .addToIndexInString(outSE$args[1],-tsLag),
                     ')))',
                     ifelse(outSE$argsEnd<nchar(inputS)-1,substr(inputS,outSE$argsEnd+2,nchar(inputS)),'')
  );
  
  #store flag if mod has been made
  outL$flag=TRUE;
  outL$tsLag=tsLag;
  
  return(outL)
}

#explode all DELTALOG()
.explodeTSDELTALOG = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeTSDELTALOG(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeTSDELTALOG(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeTSDELTALOG(): empty input string'));
  
  #init tmp var
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  
  tryCatch({
    
    tmpL=.explodeSingleTSDELTALOG(inputS);
    
    #while we have more DELTALOG()
    while(tmpL$flag)
    {
      outL$output=tmpL$output;
      outL$flag=TRUE;
      tmpL=.explodeSingleTSDELTALOG(tmpL$output);
    }
  },error=function(e){stop(paste0('.explodeTSDELTALOG(): ',e$message))})
  
  return(outL);
  
}


#transform first MAVE in inputS into exploded expression with related lags
.explodeSingleMAVE = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeSingleMAVE(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeSingleMAVE(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeSingleMAVE(): empty input string'));
  
  #init tmp var
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  outL$tsLag=0;
  
  
  tryCatch({
    outSE=.explodeFunctionCall(inputS,'.MODEL_MAVE');
  },error=function(e){stop(paste0('.explodeSingleMAVE(): ',e$message))}
  )
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0;
  
  #if not specified lag=1
  if (outSE$argsCount==1) 
  {
    tsLag=1;
    
  } else
  {
    tryCatch({
      tsLag=eval(parse(text=outSE$args[2]));
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>0)) stop()
    },error=function(e){stop(paste0('.explodeSingleMAVE(): window size must be a positive integer. ',e$message))}
    )
  }
  
  outTS='0';
  
  #MAVE(varName[a,],b) -> (0+varName[a,]+varName[a-1,]+...+varName[a-b,])/b
  for (idxMA in 1:tsLag)
    outTS=paste0(outTS,'+',.addToIndexInString(outSE$args[1],1-idxMA));
  
  
  outL$output=paste0(ifelse(outSE$firstLoc>1,substr(inputS,1,outSE$firstLoc-1),'')
                     ,'((',
                     outTS,
                     ')/',tsLag,')',
                     ifelse(outSE$argsEnd<nchar(inputS)-1,substr(inputS,outSE$argsEnd+2,nchar(inputS)),'')
  );
  
  #store flag if mod has been made
  outL$flag=TRUE;
  outL$tsLag=tsLag;
  
  return(outL)
}

#explode all MAVE()
.explodeMAVE  = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeMAVE(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeMAVE(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeMAVE(): empty input string'));
  
  #init tmp var
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  
  tryCatch({
    
    tmpL=.explodeSingleMAVE(inputS);
    
    #while we have more MAVE()
    while(tmpL$flag)
    {
      outL$output=tmpL$output;
      outL$flag=TRUE;
      tmpL=.explodeSingleMAVE(tmpL$output);
    }
  },error=function(e){stop(paste0('.explodeMAVE(): ',e$message))})
  return(outL);
  
}

#transform first MTOT in inputS into exploded expression with related lags
.explodeSingleMTOT = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeSingleMTOT(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeSingleMTOT(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeSingleMTOT(): empty input string'));
  
  #init tmp var
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  outL$tsLag=0;
  
  tryCatch({
    outSE=.explodeFunctionCall(inputS,'.MODEL_MTOT');
  },error=function(e){stop(paste0('.explodeSingleMTOT(): ',e$message))}
  )
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0;
  
  #if not specified tslag=1
  if (outSE$argsCount==1) 
  {
    tsLag=1;
    
  } else
  {
    tryCatch({
      tsLag=eval(parse(text=outSE$args[2]));
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>0)) stop()
    },error=function(e){stop(paste0('.explodeSingleMTOT(): window size must be a positive integer. ',e$message))}
    )
  }
  
  outTS='0';
  
  #MTOT(varName[a,],b) -> 0+varName[a,]+varName[a-1,]+...+varName[a-b,]
  for (idxMA in 1:tsLag)
    outTS=paste0(outTS,'+',.addToIndexInString(outSE$args[1],1-idxMA));
  
  
  outL$output=paste0(ifelse(outSE$firstLoc>1,substr(inputS,1,outSE$firstLoc-1),'')
                     ,'((',
                     outTS,
                     '))',
                     ifelse(outSE$argsEnd<nchar(inputS)-1,substr(inputS,outSE$argsEnd+2,nchar(inputS)),'')
  );
  
  #store flag if mod has been made
  outL$flag=TRUE;
  outL$tsLag=tsLag;
  
  return(outL)
}

#explode all MTOT()
.explodeMTOT  = function (inputS='',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.explodeMTOT(): NULL input string'));
  if (! is.character(inputS))
    stop(paste0('.explodeMTOT(): inputs need to be strings'));
  
  if (''==(inputS)) stop(paste0('.explodeMTOT(): empty input string'));
  
  #init tmp var
  outL=list()
  outL$output=inputS;
  outL$flag=FALSE;
  
  tryCatch({
    
    tmpL=.explodeSingleMTOT(inputS);
    
    #while we have more MTOT()
    while(tmpL$flag)
    {
      outL$output=tmpL$output;
      outL$flag=TRUE;
      tmpL=.explodeSingleMTOT(tmpL$output);
    }
  },error=function(e){stop(paste0('.explodeMTOT(): ',e$message))})
  return(outL);
  
}

#retrieve minimum for all lagged vars
.getLowerLag = function (inputS='',...)
{
   
  
  #check args
  if (is.null(inputS)) stop(paste0('.getLowerLag(): NULL input.'));
  if (! is.character(inputS))
    stop(paste0('.getLowerLag(): input need to be character.'));
  
  if (''==(inputS)) stop(paste0('.getLowerLag(): empty input string.'));
  
  #init tmp var
  outL=+Inf;
  openIdxs=c()
  closeIdxs=c();
  
  #cycle in chars and get sbracket positions
  for (idxS in 1:nchar(inputS))
  {
    #get current char
    tmpChar=substr(inputS,idxS,idxS);
    
    if (tmpChar=='[')
    {
      openIdxs=c(openIdxs,idxS);
    }
    
    if (tmpChar==']')
    {
      closeIdxs=c(closeIdxs,idxS);
    }
    
  }
  
  #counts check
  if (length(openIdxs) != length(closeIdxs))
    stop(paste0('.getLowerLag(): syntax error on indices'));
  
  #no indexes so exit
  if (length(openIdxs) ==0) return(0);
  
  #empty argument
  if (min(closeIdxs-openIdxs)<2)
    stop(paste0('.getLowerLag(): syntax error on indices'));
  
  #no interescting [ [ ] .. [ ] ]
  if (max(+c(-Inf,closeIdxs)-c(openIdxs,Inf))>=-1)
    stop(paste0('.getLowerLag(): syntax error on indices'));
  
  tryCatch(
    {
      #clicle in indexes
      for (idxI in 1:length(openIdxs))
      {
        #get index as string
        tmpS=substr(inputS,openIdxs[idxI]+1,closeIdxs[idxI]-1);
        tmpS=strsplit(tmpS,',')[[1]][1]
        
        
        #get index as numric
        currentI=eval(parse(text=tmpS));
        
        if (! is.finite(currentI)) stop();
        
        #get minimum
        outL=min(outL,currentI);
        
      }},
    error=function(e)
    {stop(paste0('.getLowerLag(): cannot evaluate index "',tmpS,'"'))}
    
  )
  
  return(outL)
  
}

#return vName[a,] in inputS that has a==indexN
.getIncidenceVendogs = function (inputS='',vName=c(),indexN='0',...)
{
  #check args
  if (is.null(inputS)) stop(paste0('.getIncidenceVendogs(): NULL input string'));
  if (is.null(vName)) stop(paste0('.getIncidenceVendogs(): NULL variables name'));
  if (is.null(indexN)) stop(paste0('.getIncidenceVendogs(): NULL index name'));
  
  if ((! is.character(inputS)) 
      || (! is.character(vName))
      || (! is.character(indexN))
  )
    stop(paste0('.getIncidenceVendogs(): inputs need to be strings'));
  
  #init tmp var
  outC=c();
  
  if (length(vName)==0) return(outC);
  
  if (''==(inputS)) stop(paste0('.getIncidenceVendogs(): empty input string'));
  if (0==any(nchar(vName))) stop(paste0('.getIncidenceVendogs(): there is an empty variable name'));
  if (''==(indexN)) stop(paste0('.getIncidenceVendogs(): empty index name'));
  
  
  
  
  
  #inputS=gsub(paste0('.MODEL_VIF\\(\\s*',vName[idxV],'\\[\\s*',indexN,'\\s*,\\s*\\]\\s*\\,'),'\\(',inputS);
  
  #cycle in vNames
  if (length(vName)>0) for (idxV in 1:length(vName))
  {
    
    #find "vName[idxV][indexN,]" NO MORE excluding LHS and .MODEL_VIF(vName)[indexN,],
    #if (grepl(paste0('\\b',vName[idxV],'\\[\\s*',indexN,'\\s*,\\s*\\]\\s*[^\\=]'),inputS))
    if (grepl(paste0('\\b',vName[idxV],'\\[\\s*',indexN,'\\s*,\\s*\\]'),inputS))
    {
      outC=c(outC,vName[idxV])
    }
  }
  
  return(outC)
  
}

#evaluate IF> condition in simulation of identities
.MODEL_VIF <- function(vendog, index_which=T, values)
{
  
  if (! all(is.finite(index_which))) stop('Uncomputable IF condition.')
  
  #get vendog of identity
  tmpV=vendog;
  
  #condition can be boolean or array
  if (length(index_which)>1)
  {
    tmpI=which(index_which);
  } else {
    tmpI=index_which;
  }
  
  #RHS can be scalar or array
  if (length(values)>1)
  {
    tmpV[tmpI]=values[tmpI];
  } else {
    tmpV[tmpI]=values;
  }
  
  
  return(tmpV)
}

#limit TSLAG to positive lags
#used in Estimate
.MODEL_TSLAG <- function(ts,lag=1)
{
  
  #check lag arg
  if (
    is.null(lag) ||
    ! is.finite(lag) ||
    lag<0
  )
  {
    stop(".MODEL_TSLAG(): please provide a positive lag.");
  } 
  
  return(TSLAG(ts, lag, avoidCompliance=TRUE));
  
}

#used in Estimate
.MODEL_TSDELTA <- function(x=NULL,L=1)
{
  return(TSDELTA(x=x, L=L, avoidCompliance=TRUE));
}

#used in Estimate
.MODEL_TSDELTALOG <- function(x=NULL,L=1)
{
  return(TSDELTALOG(x=x, L=L, avoidCompliance=TRUE));
}

#used in Estimate
.MODEL_TSDELTAP <- function(x=NULL,L=1)
{
  return(TSDELTAP(x=x, L=L, avoidCompliance=TRUE));
}

#used in Estimate
.MODEL_MAVE <- function(x=NULL, L = 1)
{
  return(MAVE(x=x, L = L, avoidCompliance=TRUE)); 
}

#used in Estimate
.MODEL_MTOT <- function(x=NULL, L = 1)
{
  return(MTOT(x=x, L = L, avoidCompliance=TRUE)); 
}

#convert funs names in model
.MODEL_MOD_FUNC_NAMES <- function(inputS=NULL)
{
  #reservedKeyw=c('LOG','LAG','MAVE','EXP','DEL','ABS','MTOT');
  
  outF=inputS;
  
  if (is.character(inputS))
  {
    outF=gsub('TSLAG\\(','LAG(',outF,ignore.case=TRUE);
    outF=gsub('LAG\\(','.MODEL_TSLAG(',outF,ignore.case=TRUE);
    outF=gsub('TSDELTA\\(','.MODEL_TSDELTA(',outF,ignore.case=TRUE);
    outF=gsub('DEL\\(','.MODEL_TSDELTA(',outF,ignore.case=TRUE);
    outF=gsub('LOG\\(','log(',outF,ignore.case=TRUE);
    outF=gsub('EXP\\(','exp(',outF,ignore.case=TRUE);
    outF=gsub('ABS\\(','abs(',outF,ignore.case=TRUE);
    outF=gsub('MOVAVG\\(','MAVE(',outF,ignore.case=TRUE);
    outF=gsub('MAVE\\(','.MODEL_MAVE(',outF,ignore.case=TRUE);
    outF=gsub('MOVSUM\\(','MTOT(',outF,ignore.case=TRUE);
    outF=gsub('MTOT\\(','.MODEL_MTOT(',outF,ignore.case=TRUE);
    outF=gsub('TSDELTAP\\(','.MODEL_TSDELTAP(',outF,ignore.case=TRUE);
    outF=gsub('TSDELTALOG\\(','.MODEL_TSDELTALOG(',outF,ignore.case=TRUE);
    
  }
  
  return(outF);
}

#used in PDL
.bm_tartaglia_pdl <- function(degree=1)
{
  if (is.null(degree) 
      || (!is.numeric(degree))
      || (! degree >-1)
      || (! ((degree %% 1) ==0))      
  ) stop('.bm_tartaglia_pdl(): degree must be a positive integer.');
  
  if (degree==0) 
  {
    return(c(1,-1))
  } else 
  {
    levelUpResult=.bm_tartaglia_pdl(degree-1);    
    outF=levelUpResult[1];
    
    for (idx in 1:(length(levelUpResult)-1))
    {
      outF=c(outF,abs(levelUpResult[idx])+abs(levelUpResult[idx+1]))
    }
    
    outF=c(outF,1);
    
    for (idx in 1:length(outF))
    {
      if (idx %% 2 ==0) outF[idx]=-outF[idx];
    } 
    
    return(outF);
  }
}


.MODEL_outputText <- function(...,sep=' ', outputText=TRUE)
{
  if (outputText) cat(...,sep=sep);
  return(NULL);
}


.checkExpression <- function(inputS)
{
  if (is.null(inputS)) return(FALSE)
  if (! is.character(inputS)) return(FALSE)
  if (nchar(inputS)==0) return(FALSE)
  
  flagX=TRUE;
  tryCatch({parse(text = inputS);flagX=FALSE;},error=function(e){})
  if (flagX) return(FALSE);
  
  return(TRUE)
}

.checkExpressionIsAtomic <- function(inputS=NULL,...)
{
  
  if (is.null(inputS)) return(FALSE)
  if (! is.character(inputS)) return(FALSE)
  if (nchar(inputS)==0) return(FALSE)
  
  #operators to be checked for atomicity
  dualOperators=c('\\+','\\-','<','>','<=','>=','!=','==','&','&&','\\|','\\|\\|');
  DOcollapse=paste0('(',paste(dualOperators,collapse = '|'),')');
  
  #check start
  if (grepl(paste0('^',DOcollapse),inputS)) return(FALSE)
  
  #check end
  if (grepl(paste0(DOcollapse,'$'),inputS)) return(FALSE)
  
  #check parentheses
  inputSnc=nchar(inputS)
  parCountArray=rep(0,inputSnc)
  
  #each "(" is +1 each ")" is -1
  tmpCounter=0;
  for (idx in 1:inputSnc)
  {
    if (substring(inputS,idx,idx)=="(") tmpCounter=tmpCounter+1
    if (substring(inputS,idx,idx)==")") tmpCounter=tmpCounter-1
    
    #too much )
    if (tmpCounter<0) return(FALSE)
    
    parCountArray[idx]=tmpCounter;
  }
  
  #too much (
  if (tmpCounter>0) return(FALSE)
  
  #get position of operators
  DOpos=as.numeric(gregexpr(DOcollapse,inputS)[[1]])
  
  #operators cannot be out of parentheses
  if (any(DOpos >0) && any(parCountArray[DOpos]==0)) return(FALSE)
  
  #check expression 
  
  if (! .checkExpression(inputS)) return(FALSE)
  
  return(TRUE)
}




# LOAD MODEL code ----------------------------------------

LOAD_MODEL <- function(modelFile=NULL,
                       modelText=NULL,
                       quietly=FALSE,
                       oldStyleModel=FALSE,
                       ...)
{
  
  
  #rawData:       original text file
  #cleanModel:    rawData without MODEL, END, comments and trim spaces
  #totNumEqs:     equations count
  #totNumIds:     identities count (same id multiple if counts 1)
  #totNumCoeff:   coefficients count
  #behaviorals:   contains data of behavioral definition
  #identities:    contains data of identity definition
  #max_lag:       contains max lag required in all eqs
  #oldStyleModel  removes starting dollar signs if there 
  #               is another dollar sign at the end of the line
  
  
  
  
  
  #main output
  model=list();
  class( model )='BIMETS_MODEL';
  
  
  #get regex definitions
  regExDefs=.RegExGlobalDefinition();
  reservedKeyw=regExDefs$reservedKeyw;
  symOnEqCleaner=regExDefs$symOnEqCleaner;
  symOnIfCleaner=regExDefs$symOnIfCleaner;
  allowedCharOnName=regExDefs$allowedCharOnName;
  charsOnNumbs=regExDefs$charsOnNumbs;
  charsOnNumWithSign=regExDefs$charsOnNumWithSign;
  strongCharOnNumbs=regExDefs$strongCharOnNumbs;
  strongCharOnNumbsWithSign=regExDefs$strongCharOnNumbsWithSign;
  allowedLhsEQfuns=regExDefs$allowedLhsEQfuns;
  allowedLhsEQfunsPub=regExDefs$allowedLhsEQfunsPub;
    
 
  
  #check arg
  if (!(is.logical(quietly))) stop('LOAD_MODEL(): "quietly" must be TRUE or FALSE.')
  
  
  #input is a file
  if (is.null(modelText))
  {
    #file exists?
    if (is.null(modelFile) || !is.character(modelFile) || !file.exists(modelFile)) stop('LOAD_MODEL(): model file "',as.character(modelFile),'" not found.');
    
    
    modelName=modelFile;
    
    #read file
    tryCatch({
      rawData=readLines(modelFile,warn = FALSE);
    },error=function(e){stop('LOAD_MODEL(): cannot read the model file. ',e$message);});
    
  } else 
  {
    #input is text
    modelName=substitute(modelText);
    
    #deal with win carriage
    modelText=gsub('\r\n','\n',modelText);
    
    #get lines
    rawData=strsplit(modelText,'\n')[[1]];
    
  }
  
  #.MODEL_outputText(outputText=!quietly,paste0('Loading model: "',modelName,'"...\n'));
  
  
  
  #check consistence
  if (length(rawData)==0) stop('LOAD_MODEL(): empty model file.')
  model$rawData=rawData;
  
  #trim leading/trailing spaces
  cleanModel=gsub("^\\s+|\\s+$", "", rawData)
  
  #no code in file
  if (length(cleanModel)==0) stop('LOAD_MODEL(): empty model file.')
  
  #remove extra $ if required
  if (oldStyleModel==TRUE)
  {
    for (idxR in 1:length(cleanModel))
    {
      
      
      #if double dollar sign then it is a comment
      if (grepl('^\\s*\\$.*\\$\\s*$',cleanModel[idxR]))
        next;
      
      #...else remove first $
      cleanModel[idxR]=gsub("^\\s*\\$", "", cleanModel[idxR]);
      
    }
  }
  
  #remove empty lines
  emptyLinesIdx=which(cleanModel=='');
  if (length(emptyLinesIdx)>0) cleanModel=cleanModel[-emptyLinesIdx];
  
  if (cleanModel[1]!='MODEL' || cleanModel[length(cleanModel)]!='END') 
    stop('LOAD_MODEL(): model file must begin with MODEL and must end with END keywords.')
  
  #again trim leading/trailing spaces
  cleanModel=gsub("^\\s+|\\s+$", "", cleanModel)
  
  #remove comments (line starting with $ or COMMENT) and first/last line (MODEL|END)
  commentsIndexes=grep("^(\\$|COMMENT\\s*>)",cleanModel,ignore.case=TRUE);
  if (length(commentsIndexes)>0) cleanModel=cleanModel[-commentsIndexes];
  cleanModel=cleanModel[-1];cleanModel=cleanModel[-length(cleanModel)];
  
  if (length(grep("\\$",cleanModel))>0) 
    stop('LOAD_MODEL(): dollar symbol (i.e. a comment) allowed only at the beginning of a line.\nCheck line "',cleanModel[grep("\\$",cleanModel)[1]],'"');
  
  #eqs and ids num
  totNumEqs=0;
  totNumIds=0;
  
  #collect keyword indexes
  behavioralIndexes=grep("^(EQUATION|BEHAVIORAL)\\s*>",cleanModel, ignore.case = TRUE); 
  identityIndexes=grep("^IDENTITY\\s*>",cleanModel, ignore.case = TRUE);
  eqIndexes=grep("^EQ\\s*>",cleanModel, ignore.case = TRUE); 
  coeffIndexes=grep("^COEFF\\s*>",cleanModel, ignore.case = TRUE); 
  storeIndexes=grep("^STORE\\s*>",cleanModel, ignore.case = TRUE); 
  pdlIndexes=grep("^PDL\\s*>",cleanModel, ignore.case = TRUE); 
  restrictIndexes=grep("^RESTRICT\\s*>",cleanModel, ignore.case = TRUE);
  errorIndexes=grep("^ERROR\\s*>",cleanModel, ignore.case = TRUE);
  ifIndexes=grep("^IF\\s*>",cleanModel, ignore.case = TRUE);
  ivIndexes=grep("^IV\\s*>",cleanModel, ignore.case = TRUE);
  
  
  #sorted indexes of all keywords
  kwIdx=sort(c(length(cleanModel)+1,
               behavioralIndexes,
               identityIndexes,
               eqIndexes,
               coeffIndexes,
               storeIndexes,
               pdlIndexes,
               restrictIndexes,
               errorIndexes,
               ifIndexes,
               ivIndexes));
  
  #sorted indexes of eq/ident keywords
  eqIdenDefIdx=sort(c(length(cleanModel)+1,
                      behavioralIndexes,
                      identityIndexes));
  
  #coeff num
  totNumCoeff=0;
 
  
  # analyze code behaviorals -----------------------------------------------------------------------------
  
  model$behaviorals=list();
  
  #cycle and extract all behaviorals
  if (length(behavioralIndexes)>0) for(idxBI in 1:length(behavioralIndexes))
  {
    
    
    #empty tmp behavioral to be added to model list
    behavioralTmp=list();    
    
    #..we extract all code between an eq/id definition and the next one
    #get next keyword definition index
    nextKwIdx=min(kwIdx[which(kwIdx>behavioralIndexes[idxBI])]);    
    
    #get next eq/ident definition index, needed to check COEFF exists
    nextDefIdx=min(eqIdenDefIdx[which(eqIdenDefIdx>behavioralIndexes[idxBI])]);    
    
    #read all lines of behavior definition
    behavioralRaw=paste(cleanModel[behavioralIndexes[idxBI]:(nextKwIdx-1)],collapse=' ')
    behavioralRaw=gsub("^(EQUATION|BEHAVIORAL)\\s*>", "", behavioralRaw, ignore.case = TRUE);
    behavioralRaw=gsub("^\\s+|\\s+$", "", behavioralRaw);
    
    #get behavioral name (split if TSRANGE defined)
    behavioralName=gsub("^\\s+|\\s+$", "",strsplit(behavioralRaw,'TSRANGE')[[1]][1]);    
    if (is.na(behavioralName) || nchar(behavioralName)==0) 
      stop('LOAD_MODEL(): unknown behavioral name in line: "',cleanModel[behavioralIndexes[idxBI]],'".');
    
    #check name is compliant with regex
    if (length(grep(allowedCharOnName,behavioralName))==0) 
      #stop('LOAD_MODEL(): invalid behavioral name in line: "',cleanModel[behavioralIndexes[idxBI]],'".');
      stop('LOAD_MODEL(): invalid behavioral name: "',behavioralName,'".');
    
    #check TSRANGE inline definition
    if (length(grep('TSRANGE',behavioralRaw))>0)
    { 
      tryCatch({
        #get text after TSRANGE kw and then split with spaces
        
        behavioralTsrange=suppressWarnings(as.numeric(strsplit(gsub("^\\s+|\\s+$", "",strsplit(behavioralRaw,'TSRANGE')[[1]][2]),'\\s+')[[1]]));
        if (any(! is.finite(behavioralTsrange))) stop();
        if (any(behavioralTsrange<=0)) stop();
        if (any(behavioralTsrange %% 1 != 0)) stop();
        if (length(behavioralTsrange)!=4) stop();
      },error=function(e){
        #stop('LOAD_MODEL(): invalid TSRANGE in line: "',cleanModel[behavioralIndexes[idxBI]],'".')
        stop('LOAD_MODEL(): invalid TSRANGE in line: "',behavioralRaw,'".')});
      
    } else behavioralTsrange=NA;    
    
    #check behavioral has the COEFF definition
    coeffLocalIdx=which(coeffIndexes %in% behavioralIndexes[idxBI]:nextDefIdx);
    if (length(coeffLocalIdx)==0) stop('LOAD_MODEL(): no COEFF definition in behavioral "',behavioralName,'".');
    if (length(coeffLocalIdx)>1) stop('LOAD_MODEL(): multiple COEFF definitions in behavioral "',behavioralName,'".');
    
    #read all lines of coeff definition    
    nextKwIdx=min(kwIdx[which(kwIdx>coeffIndexes[coeffLocalIdx])]);
    coeffRaw=paste(cleanModel[coeffIndexes[coeffLocalIdx]:(nextKwIdx-1)],collapse=' ');
    coeffRaw=gsub("^COEFF\\s*>", "", coeffRaw, ignore.case = TRUE);
    coeffRaw=gsub("^\\s+|\\s+$", "", coeffRaw);
    
    #split and remove duplicates
    coeff=unique(strsplit(coeffRaw,'\\s+')[[1]]);
    
    #check coeef have allowed names
    if (any(! grepl(allowedCharOnName,coeff))) stop('LOAD_MODEL(): invalid name in COEFF definition "',coeffRaw,'" in behavioral "',behavioralName,'".');
    
    coeffNum=length(coeff);
    totNumCoeff=totNumCoeff+coeffNum;
    if (coeffNum==0) stop('LOAD_MODEL(): no COEFF definition in behavioral "',behavioralName,'".');
    
    #check behavioral has EQ definition
    eqLocalIdx=which(eqIndexes %in% behavioralIndexes[idxBI]:nextDefIdx);
    if (length(eqLocalIdx)==0) stop('LOAD_MODEL(): no EQ definition in behavioral "',behavioralName,'".');
    if (length(eqLocalIdx)>1) stop('LOAD_MODEL(): multiple EQ definitions in behavioral "',behavioralName,'".');
    
    
    #read all lines of EQ definition
    nextKwIdx=min(kwIdx[which(kwIdx>eqIndexes[eqLocalIdx])]);
    eqRaw=paste(cleanModel[eqIndexes[eqLocalIdx]:(nextKwIdx-1)],collapse=' ');
    eqRaw=gsub("^EQ\\s*>", "", eqRaw, ignore.case = TRUE);
    eqRaw=gsub("\\s*", "", eqRaw); 
    
    
    #check unknown funs names
    unknownFuns=.unknownFunsNames(eqRaw,reservedKeyw)
    if (length(unknownFuns)>0) stop('LOAD_MODEL(): unsupported functions in EQ definition: "',eqRaw,'" in behavioral "',behavioralName,'": ',
                                    paste0(paste0(unknownFuns,'()'),collapse=', '));
      
    
    #trim and extract symbol names from EQ
    namesOnEq=strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnEqCleaner,' ',eqRaw,ignore.case=TRUE)),'\\s+')[[1]];
    
    #check EQ is non trivial
    if (nchar(eqRaw)==0 || length(namesOnEq)==0 || !grepl('=',eqRaw))
      stop('LOAD_MODEL(): syntax error in EQ definition: "',eqRaw,'" in behavioral "',behavioralName,'".');
    
    
    
    
    #remove numbers from names
    rmvNumOnEqIdx=c();
    for (idxNamesOnEq in 1:length(namesOnEq)) {
      if (length(grep(strongCharOnNumbs,namesOnEq[idxNamesOnEq]))>0) 
      {
        rmvNumOnEqIdx=c(rmvNumOnEqIdx,idxNamesOnEq);
        
      }
    }
    if (length(rmvNumOnEqIdx)>0) namesOnEq=namesOnEq[-rmvNumOnEqIdx];
    
    
    
    eqRawSplitted=strsplit(eqRaw,'\\=')[[1]];
    if (length(eqRawSplitted)!=2) stop('LOAD_MODEL(): syntax error in EQ definition: "',eqRaw,'".')
    
    #convert RHS text to expression
    #rhsExp=parse(text=.MODEL_MOD_FUNC_NAMES(eqRawSplitted[2]));
    rhsExp=eqRawSplitted[2];
    lhsExp=eqRawSplitted[1];
    
    if (lhsExp=='') stop('LOAD_MODEL(): syntax error in EQ definition: "',eqRaw,'" in behavioral "',behavioralName,'".','".')
    
    lhsFun=NULL;
    
    #find lhs func
    tryCatch({
      lhsFun=.parseLhsEQ(lhsExp,allowedLhsEQfuns);
    },error=function(err){
      stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" in behavioral "',behavioralName,'".')
    }
    )
    
    # lhs behavioral ------------------------------------------------
    
    #check lhs fun syntax
    if (lhsFun$funName=='I')
    {
      #check EQ contains behavioral name
      #if (length(grep(paste0("^",behavioralName,"="),eqRaw))==0) 
      if (lhsFun$args!=behavioralName)
        stop('LOAD_MODEL(): LHS of EQ definition: "',eqRaw,'" must contain only the behavioral name "',behavioralName,'" or the allowed LHS functions: ',paste0(paste0(allowedLhsEQfunsPub,'()'),collapse=', '));
      
    } else if (lhsFun$funName=='LOG')
    {
      if (lhsFun$argsCount!=1 || lhsFun$args!=behavioralName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the behavioral "',behavioralName,'". If the LOG of the endogenous variable is requested please try the following: "LOG(',behavioralName,')"');
      
    } else if (lhsFun$funName=='EXP')
    {
      if (lhsFun$argsCount!=1 || lhsFun$args!=behavioralName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the behavioral "',behavioralName,'". If the EXP of the endogenous variable is requested please try the following: "EXP(',behavioralName,')"');
      
    } else if (lhsFun$funName=='TSDELTA' || lhsFun$funName=='DEL')
    {
      if (lhsFun$argsCount>2 || lhsFun$args[1]!=behavioralName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the behavioral "',behavioralName,'". If the TSDELTA of the endogenous variable is requested please try the following: "TSDELTA(',behavioralName,')" or "TSDELTA(',behavioralName,',n)"');
      
    } else if (lhsFun$funName=='TSDELTAP' )
    {
      if (lhsFun$argsCount>2 || lhsFun$args[1]!=behavioralName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the behavioral "',behavioralName,'". If the TSDELTAP of the endogenous variable is requested please try the following: "TSDELTAP(',behavioralName,')" or "TSDELTAP(',behavioralName,',n)"');
      
    } else if (lhsFun$funName=='TSDELTALOG' )
    {
      if (lhsFun$argsCount>2 || lhsFun$args[1]!=behavioralName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the behavioral "',behavioralName,'". If the TSDELTALOG of the endogenous variable is requested please try the following: "TSDELTALOG(',behavioralName,')" or "TSDELTALOG(',behavioralName,',n)"');
      
    }
    
    
    
    #check EQ contains COEFFs
    for(coeffTmp in coeff) if (length(grep(allowedCharOnName,coeffTmp))==0) 
      stop('LOAD_MODEL(): invalid coefficient name "',coeffTmp,'" in COEFF definition of behavioral: "',behavioralName,'".');    
    for(coeffTmp in coeff) if (! (coeffTmp %in% namesOnEq) ) 
      stop('LOAD_MODEL(): coefficient "',coeffTmp,'" not found in EQ definition: "',eqRaw,'".');    
    
    #remove coeff from EQ components names
    rmvCoeffOnEqIdx=c();
    for (idxNamesOnEq in 1:length(namesOnEq)) {
      if (namesOnEq[idxNamesOnEq] %in% coeff) 
      {
        rmvCoeffOnEqIdx=c(rmvCoeffOnEqIdx,idxNamesOnEq);            
      }
    }
    
    
    if (!(all(namesOnEq[rmvCoeffOnEqIdx]==coeff))) 
      stop('LOAD_MODEL(): coefficients in EQ definition: "',eqRaw,'" must appear in the same order as in the COEFF definition: "',paste(coeff,collapse=', '),'".');
    
    if (length(rmvCoeffOnEqIdx)>0) namesOnEq=namesOnEq[-rmvCoeffOnEqIdx];
    
    #check components name
    for (idxNOEQ in 1:length(namesOnEq)) {
      if (length(grep(allowedCharOnName,namesOnEq[idxNOEQ]))==0) 
      {
        stop('LOAD_MODEL(): syntax error in EQ definition: "',eqRaw,'". The following symbol cannot be a variable name: "',namesOnEq[idxNOEQ],'".');
        
      }
    }
    
    
    #normalize regressors
    for(coeffTmp in coeff)
    {
      #isolated coeff
      if (!grepl(paste0(coeffTmp,'\\*'),rhsExp))
      {
        #coeff+ -> coeff*1+
        rhsExp=gsub(paste0(coeffTmp,'\\+'),paste0(coeffTmp,'*1+'),rhsExp);
        #+coeff at the end -> +coeff*1
        rhsExp=gsub(paste0(coeffTmp,'$'),paste0(coeffTmp,'*1'),rhsExp);
        #coeff at the start -> +coeff
        rhsExp=gsub(paste0('^',coeffTmp),paste0('+',coeffTmp),rhsExp);
      }
      
      #rhs start with coeef without sign
      rhsExp=gsub(paste0('^',coeffTmp),paste0('+',coeffTmp),rhsExp);
      
    }
    
    #for(coeffTmp in coeff) if (length(grep(paste0('(\\+|\\=)',coeffTmp,'[\\*]?'),eqRaw))==0 )
    for(coeffTmp in coeff) if (length(grep(paste0('(\\+)',coeffTmp,'\\*'),rhsExp))==0 ) 
      stop('LOAD_MODEL(): please provide EQ "',eqRaw,'" in form of a linear combination of coefficients: fix coeff. "',coeffTmp,'".');    
    
    
    
    
    #split using coeffs and get EQ regressors
    #eqRegressorsNames=strsplit(eqRaw,paste0('(\\+|\\=)(',paste(coeff,collapse='|'),')[\\*]?'))[[1]];
    eqRegressorsNames=strsplit(rhsExp,paste0('(\\+)(',paste(coeff,collapse='|'),')\\*'))[[1]];
    if (length(eqRegressorsNames)<2) stop('LOAD_MODEL(): regressors count error in EQ definition: "',eqRaw,'"');
    
    if (eqRegressorsNames[1]!='')
      stop('LOAD_MODEL(): please provide EQ "',eqRaw,'" in form of a linear combination of coefficients.');
    
    eqRegressorsNames=eqRegressorsNames[-1]; #remove first void
    
    
    for (idxR in 1:length(eqRegressorsNames))
      if (! .checkExpressionIsAtomic(eqRegressorsNames[idxR])) 
        stop('LOAD_MODEL(): please provide EQ "',eqRaw,'" in form of a linear combination of coefficients: fix regressor "',eqRegressorsNames[idxR],'".');
    
    
    #if (length(eqRegressorsNames)!=length(coeff)+1)
    if (length(eqRegressorsNames)!=length(coeff)) 
      stop('LOAD_MODEL(): regressors count in EQ definition: "',eqRaw,'" is different from coefficients count in COEFF definition: "',paste(coeff,collapse=', '),'".');
    
    #check if duplicated regressor
    if (any(duplicated(eqRegressorsNames))) 
      stop('LOAD_MODEL(): duplicated regressor "',paste(eqRegressorsNames[which(duplicated(eqRegressorsNames))],collapse=' '),'" in EQ definition: "',eqRaw,'".');
    
    #check unique intercept 
    flagSC=FALSE;
    for(regrTmp in eqRegressorsNames)
    {
      if (grepl(strongCharOnNumbs,regrTmp))
      {
        if (flagSC) stop('LOAD_MODEL(): multiple intercepts defined in EQ: "',eqRaw,'"' );
        flagSC=TRUE;
      }
    }
    
   
    #check behavioral has STORE definition
    storeLocalIdx=which(storeIndexes %in% behavioralIndexes[idxBI]:nextDefIdx);
    storeVarName=NULL;
    storePosition=NULL;
    if (length(storeLocalIdx)>0)
    { 
      if (length(storeLocalIdx)>1) stop('LOAD_MODEL(): multiple STORE definitions in behavioral "',behavioralName,'".');
      
      #read all lines of store definition
      nextKwIdx=min(kwIdx[which(kwIdx>storeIndexes[storeLocalIdx])]);
      storeRaw=paste(cleanModel[storeIndexes[storeLocalIdx]:(nextKwIdx-1)],collapse=' ');
      storeRaw=gsub("^STORE\\s*>", "", storeRaw, ignore.case = TRUE);
      storeRaw=gsub("\\s*", "", storeRaw);
      
      
      #get store var name and position
      storeSplit=strsplit(storeRaw,'\\(')[[1]];
      if (length(storeSplit)!=2) 
        stop('LOAD_MODEL(): syntax error in STORE definition: "',storeRaw,'" in behavioral "',behavioralName,'".')
      
      storeVarName=gsub("\\s*", "", storeSplit[1]);
      if (length(grep(allowedCharOnName,storeVarName))==0) 
        stop('LOAD_MODEL(): invalid var name on STORE definition: "',storeRaw,'" in behavioral "',behavioralName,'".');
      
      
      #STORE location must be positive integer
      if (length(grep("^\\s*[0]*[1-9]+[0-9]*\\s*\\)$",storeSplit[2]))==0) 
        stop('LOAD_MODEL(): syntax error in STORE definition: "',storeRaw,'" in behavioral "',behavioralName,'".');
      storePosition=as.numeric(gsub('\\)','',storeSplit[2]));        
      if (storePosition<1) 
        stop('LOAD_MODEL(): unknown error on retrieving STORE position in "',storeRaw,'" in behavioral "',behavioralName,'".');
      
      
    }
    
    #check behavioral has PDL definition
    #pdlLocalIdx contains lines indexes of pdl in behavioral
    pdlLocalIdx=which(pdlIndexes %in% behavioralIndexes[idxBI]:nextDefIdx);
    pdlRaw=NULL;
    bm_pdlMatrix=NULL;      
    
    bm_pdlRestrictionMatrix=c();
    bm_pdlRestrictionVector=c();
    
    #backUp old coeff and eqRegressorsNames          
    coeffOriginal=coeff;
    eqRegressorsNamesOriginal=eqRegressorsNames;
    
    #...there are PDL definitions
    if (length(pdlLocalIdx)>0) 
    {
      
      #analize all pdl definitions
      pdlRaw='';
      for (pdlLLidx in 1:length(pdlLocalIdx))
      {
        #read all lines of pdl definition
        nextKwIdx=min(kwIdx[which(kwIdx>pdlIndexes[pdlLocalIdx[pdlLLidx]])]);
        
        #pdlRawSingle="PDL> C06 2 14 F"
        #clean...
        pdlRawSingle=paste(cleanModel[pdlIndexes[pdlLocalIdx[pdlLLidx]]:(nextKwIdx-1)],collapse=';');
        pdlRawSingle=gsub("^PDL\\s*>", "", pdlRawSingle, ignore.case = TRUE);
        pdlRawSingle=gsub("^\\s*|\\s*$", "", pdlRawSingle);            
        
        
        #pdlRaw="C07 2 14 F;C06 2 14 F;"
        pdlRaw=paste0(pdlRawSingle,';',pdlRaw);
      }
      
      
      #analize pdlRaw
      pdlComponents=strsplit(pdlRaw,';')[[1]];
      
      if (length(pdlComponents)<1) 
        stop('LOAD_MODEL(): NULL length on PDL definition "',pdlRaw,'" in behavioral "',behavioralName,'".')
      
      #create empty list
      bm_pdl=list();
      
      #following 'bm_pdlMatrix' matrix will have:
      #in first row 1 if restricion applies to i-th coefficient
      #in second and third row degree and lag values
      #in 4 1 if N 
      #in 5 1 if F 
      
      bm_pdlMatrix=matrix(rep(0,5*length(coeff)),nrow=5,ncol=length(coeff));
      
      for (idxPC in 1:length(pdlComponents ))
      {
        #split with spaces " C06 2 14 F" -> c('C06',2,14,'F')
        pdlSingleElement=strsplit(pdlComponents[idxPC],'\\s+')[[1]];
        
        
        if ((length(pdlSingleElement)<3) || (length(pdlSingleElement)>5)) 
          stop('LOAD_MODEL(): syntax error in PDL definition "',pdlComponents[idxPC],'" in behavioral "',behavioralName,'". Usage: PDL> coeff degree laglength [N] [F].')
        
        #check coefficient exists
        if ( ! (pdlSingleElement[1] %in% coeff)) 
          stop('LOAD_MODEL(): unknown coefficient "',pdlSingleElement[1],'" in PDL definition in behavioral "',behavioralName,'".');
        
        coeffPos=which(coeff==pdlSingleElement[1]);            
        
        
        bm_pdlMatrix[1,coeffPos]=1;
        
        #pdl on constant term throws an error
        if (grepl(strongCharOnNumbs,eqRegressorsNames[coeffPos]))
        {
          stop('LOAD_MODEL(): cannot expand a PDL on a constant term: coeff. "',pdlSingleElement[1],'" on behavioral "',behavioralName,'".');
        }
        
        #deal with degree
        tmpV=as.numeric(pdlSingleElement[2])
        if ( 
          ( ! is.finite(tmpV))    ||
          (length(tmpV)==0) ||
          (!((tmpV %% 1) == 0) ) ||
          (!(tmpV >-1))
        ) stop('LOAD_MODEL(): degree of the polynomial must be a non-negative integer in PDL definition of coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".');
        
        bm_pdlMatrix[2,coeffPos]=tmpV;
        
        
        #deal with laglength
        tmpV=as.numeric(pdlSingleElement[3])
        if ( 
          ( ! is.finite(tmpV))    ||
          (length(tmpV)==0) ||
          (!((tmpV %% 1) == 0) ) ||
          (!(tmpV >0))
        ) stop('LOAD_MODEL(): laglength must be a positive integer in PDL definition of coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".');
        
        bm_pdlMatrix[3,coeffPos]=tmpV;
        
        if (bm_pdlMatrix[3,coeffPos]<=bm_pdlMatrix[2,coeffPos]) 
          stop('LOAD_MODEL(): laglength must be greater than polynomial degree in PDL definition of coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".')
        
        #deal with N/F				
        if (length(pdlSingleElement)>3)
        {              
          tmpV=pdlSingleElement[4]
          if ( 
            (tmpV != 'N')  &&
            (tmpV != 'F') 
          ) stop('LOAD_MODEL(): options must be "N" and/or "F" in PDL definition of coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".');
          
          if (tmpV == 'N') bm_pdlMatrix[4,coeffPos]=1;
          if (tmpV == 'F') bm_pdlMatrix[5,coeffPos]=1;
        }
        
        if (length(pdlSingleElement)>4)
        {              
          tmpV=pdlSingleElement[5]
          if ( 
            (tmpV != 'N')  &&
            (tmpV != 'F') 
          ) stop('LOAD_MODEL(): options must be "N" and/or "F" in PDL definition of coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".');
          
          if (tmpV == 'N') bm_pdlMatrix[4,coeffPos]=1;
          if (tmpV == 'F') bm_pdlMatrix[5,coeffPos]=1;
          
        }
        
        #check if pdl is 0 1 then error due to redundance
        if (bm_pdlMatrix[2,coeffPos]==0 && bm_pdlMatrix[3,coeffPos]==1) 
          stop('LOAD_MODEL(): PDL with degree 0 and length 1 can be removed: coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".');
        
      }		
      
      
      #add lagged coefficient to coefficient list 
      for (pdlMatrixIdx in 1:length(coeffOriginal)) 
      {				
        #coefficient has pdl
        if (bm_pdlMatrix[1,pdlMatrixIdx]==1)
        {
          
          #insert required coefficient in list of coefficients
          coeffToBeInserted=c();
          eqCompToBeInserted=c();
          
          #pdl length > 1 then insert
          if (bm_pdlMatrix[3,pdlMatrixIdx]>1)
          {
            for (idxCoeffToBeIns in 1:(bm_pdlMatrix[3,pdlMatrixIdx]-1))
            {
              #coeffToBeInserted=C06_PDL_6
              coeffToBeInserted=c(coeffToBeInserted,paste0(coeffOriginal[pdlMatrixIdx],'__PDL__',idxCoeffToBeIns));
              #eqCompToBeInserted=LAG(  (LAG(LOG(KSTAR))-LAG(LOG(KSTAR),2))  ,9)
              eqCompToBeInserted=c(eqCompToBeInserted,paste0('TSLAG(',eqRegressorsNamesOriginal[pdlMatrixIdx],',',idxCoeffToBeIns,')'))
            }					
            
          }
          
          #new coefficients and new regressors will be inserted after original ones...e.g. 
          
          idxIntoInsert=which(coeff==coeffOriginal[pdlMatrixIdx])
          
          if (idxIntoInsert<length(coeff)) coeff=c(coeff[1:idxIntoInsert],coeffToBeInserted,coeff[(idxIntoInsert+1):length(coeff)])
          else coeff=c(coeff[1:idxIntoInsert],coeffToBeInserted);
          
          idxIntoInsert=which(eqRegressorsNames==eqRegressorsNamesOriginal[pdlMatrixIdx])
          #adding LAGs can replicate regressors e.g. p and LAG(p,1) already on model so take last one 
          #(regressor must be different in model definition so last one is ok)
          idxIntoInsert=idxIntoInsert[length(idxIntoInsert)]
          
          if (idxIntoInsert<length(eqRegressorsNames)) eqRegressorsNames=c(eqRegressorsNames[1:idxIntoInsert],eqCompToBeInserted,eqRegressorsNames[(idxIntoInsert+1):length(eqRegressorsNames)])
          else eqRegressorsNames=c(eqRegressorsNames[1:idxIntoInsert],eqCompToBeInserted);
          
          
          
        }           
        
      }		
      
      
      #create pdl restriction matrix and vector (using tartaglia)
      for (pdlMatrixIdx in 1:length(coeffOriginal)) 
      {
        #coefficient has pdl
        if (bm_pdlMatrix[1,pdlMatrixIdx]==1)
        {
          #pdllength > pdldegree+1 then insert row
          if (bm_pdlMatrix[3,pdlMatrixIdx]>bm_pdlMatrix[2,pdlMatrixIdx]+1)
          {
            #get tartaglia coefficients
            tmpTartagliaRow=.bm_tartaglia_pdl(bm_pdlMatrix[2,pdlMatrixIdx]);
            for(tartagliaIdx in 1:(bm_pdlMatrix[3,pdlMatrixIdx]-(bm_pdlMatrix[2,pdlMatrixIdx]+1)))
            {
              #create R matrix row
              temp_row=rep(0,length(coeff));
              idxCoeff=which(coeff==coeffOriginal[pdlMatrixIdx])
              #insert tartaglia coeff starting from correct index
              temp_row[(idxCoeff-1+tartagliaIdx):(idxCoeff+length(tmpTartagliaRow)-2+tartagliaIdx)]=tmpTartagliaRow
              
              
              #bm_pdlRestrictionMatrix=(0 0 1 -2 1 0 0; 0 0 0 1 -2 1 0 )
              bm_pdlRestrictionMatrix=rbind(bm_pdlRestrictionMatrix,temp_row);
              bm_pdlRestrictionVector=c(bm_pdlRestrictionVector,0);
              
            }					
            
          }
          
          #check N
          #put one row in matrix with 1 on original coeff (no LAGged) e.g. c02
          if (bm_pdlMatrix[4,pdlMatrixIdx]==1 && bm_pdlMatrix[3,pdlMatrixIdx]>1)
          {			
            
            temp_row=rep(0,length(coeff));
            idxCoeff=which(coeff==coeffOriginal[pdlMatrixIdx])
            temp_row[idxCoeff]=1;
            
            bm_pdlRestrictionMatrix=rbind(bm_pdlRestrictionMatrix,temp_row);
            bm_pdlRestrictionVector=c(bm_pdlRestrictionVector,0);				
            
          }
          
          #check F
          #put one row in matrix with 1 on last LAGged coeff e.g. LAG(C02,4)
          if (bm_pdlMatrix[5,pdlMatrixIdx]==1 && bm_pdlMatrix[3,pdlMatrixIdx]>1)
          {
            temp_row=rep(0,length(coeff));
            idxCoeff=which(coeff==coeffOriginal[pdlMatrixIdx])
            temp_row[idxCoeff+bm_pdlMatrix[3,pdlMatrixIdx]-1]=1;
            
            bm_pdlRestrictionMatrix=rbind(bm_pdlRestrictionMatrix,temp_row);
            bm_pdlRestrictionVector=c(bm_pdlRestrictionVector,0);
            
          }
          
          #check N and F and length=2 -> error
          if (bm_pdlMatrix[4,pdlMatrixIdx]==1 && bm_pdlMatrix[5,pdlMatrixIdx]==1 && bm_pdlMatrix[3,pdlMatrixIdx]==2)
          {
            stop('LOAD_MODEL(): redundant options "N" and "F" set in coefficient "',coeffOriginal[pdlMatrixIdx],'" with PDL length 2 in behavioral "',behavioralName,'".')
          }
          
        }
        
        
      }    
      
      
    }#endif pdl
    
    if (! is.null(bm_pdlRestrictionMatrix)) 
    {
      rownames(bm_pdlRestrictionMatrix)=c();
      colnames(bm_pdlRestrictionMatrix)=c();
    }
    
    if (! is.null(bm_pdlRestrictionVector)) 
    {
      rownames(bm_pdlRestrictionVector)=c();
      colnames(bm_pdlRestrictionVector)=c();
    }
    
    #check behavioral has RESTRIC def
    #... WARNING! RESTRICT CAN SPAN ON SEVERAL LINES WITH A SINGLE TAG RESTRICT>
    restrictLocalIdx=which(restrictIndexes %in% behavioralIndexes[idxBI]:nextDefIdx);
    restrictRaw=NULL;
    if (length(restrictLocalIdx)>0) 
    {
      
      #analize all restrict definitions
      
      restrictRaw='';
      for (restrictLLidx in 1:length(restrictLocalIdx))
      {
        #read all lines of restrict definition
        nextKwIdx=min(kwIdx[which(kwIdx>restrictIndexes[restrictLocalIdx[restrictLLidx]])]);
        restrictRawSingle=paste(cleanModel[restrictIndexes[restrictLocalIdx[restrictLLidx]]:(nextKwIdx-1)],collapse=';');
        restrictRawSingle=gsub("^RESTRICT\\s*>", "", restrictRawSingle, ignore.case = TRUE);
        restrictRawSingle=gsub("\\s*", "", restrictRawSingle);            
        
        #restrictRaw="C01+C02=0;C01+C03=0"
        restrictRaw=paste0(restrictRawSingle,';',restrictRaw);
      }
      
      
      #analize restrictRaw
      restrictComponents=strsplit(restrictRaw,';')[[1]];
      if (length(restrictComponents)<1) stop('LOAD_MODEL(): NULL length on restriction definition "',restrictRaw,' in behavioral "',behavioralName,'".')
      
      #matrix R and vector r of restriction
      bm_matrixR=c();
      bm_vectorR=c();
      
      for (idxRestricComp in 1:length(restrictComponents))
      {
        #split equal
        restrComponentSplit=strsplit(restrictComponents[[idxRestricComp]],'\\=')[[1]]
        
        #check equal signs count
        if (length(restrComponentSplit)!=2) 
          stop('LOAD_MODEL(): syntax error on restriction "',restrictComponents[[idxRestricComp]],'" in behavioral "',behavioralName,'".\nUsage: "RESTRICT> alpha0 * coeff0 + ... + alphaN * coeffN = const".')
        
        restrictionRHS=restrComponentSplit[2];
        restrictionLHS=restrComponentSplit[1];
        
        #here we replace LAG(COEFF,X) with COEFF_PDL_X in RESTRICT
        if (! is.null(bm_pdlMatrix))
        {
          
          #find coeff with pdl
          idxCoeffInPdlMatrix=which(bm_pdlMatrix[1,]==1)
          
          
          #cycle coeff in pdl
          for (idxLoopInPdlMatrix in idxCoeffInPdlMatrix)
            for (idxReplaceLagInRestriction in 1:(bm_pdlMatrix[3,idxLoopInPdlMatrix]-1))
            {
              restrictionLHS=gsub(paste0('LAG\\(',coeffOriginal[idxLoopInPdlMatrix],',',idxReplaceLagInRestriction,'\\)'),
                                  paste0(coeffOriginal[idxLoopInPdlMatrix],'__PDL__',idxReplaceLagInRestriction),
                                  restrictionLHS)
            }
          
        }
        
        #check RHS constant is a number
        if (length(grep(strongCharOnNumbsWithSign,restrictionRHS))==0) 
          stop('LOAD_MODEL(): syntax error on restriction "',restrictComponents[[idxRestricComp]],'" in behavioral "',behavioralName,'".\nUsage: "RESTRICT> alpha0 * coeff0 + ... + alphaN * coeffN = const".')
        
        
        #check LHS is linear comb of coeff
        regExprRes=paste0('^((',
                          charsOnNumWithSign,paste0('\\*|\\+|-)?(',
                                                    paste0(coeff,collapse='|')  ,')'),')+$')
        
        
        if (length(grep(regExprRes,restrictionLHS))==0) 
          stop('LOAD_MODEL(): syntax error on restriction "',restrictComponents[[idxRestricComp]],'" in behavioral "',behavioralName,'".\nUsage: "RESTRICT> alpha0 * coeff0 + ... + alphaN * coeffN = const".')
        
        
        bm_tmpRrow=c();
        
        #exctract coeff multiplier in restriction
        for (idxCoeff in 1:length(coeff))
        {  
          
          tmpRegExp=regexpr(paste0('(',charsOnNumWithSign,'\\*|\\+|-)?(',
                                   coeff[idxCoeff],')(\\+|-|$)'),restrictionLHS);
          
          
          #localMultiplier="+C02"
          localMultiplier=regmatches(restrictionLHS,tmpRegExp)
          if (length(localMultiplier)>0)
          {
            #coeff exists in restriction
            #localMultiplier="+"
            localMultiplier=gsub(paste0('(\\*|',coeff[idxCoeff],'(\\+|-)?)'),'',localMultiplier)
            
            #exctract value
            if (localMultiplier=='+' || localMultiplier=='' ) {
              localMultiplier=1;
            } else if (localMultiplier=='-')
            {
              localMultiplier=-1;
            } else {
              
              localMultiplier=as.numeric(localMultiplier);
            }
            
          } else 
          {
            localMultiplier=0;
          }
          
          bm_tmpRrow=c(bm_tmpRrow,localMultiplier)
          
        }
        
        #C01+C02=0;C01+C03=0
        #bm_vectorR=(0;0)
        bm_vectorR=c(bm_vectorR,as.numeric(restrictionRHS))
        #bm_matrixR=(0 1 1 0 ...; 0 1 0 1 0 0 ...)
        bm_matrixR=rbind(bm_matrixR,bm_tmpRrow)
        
        
        
        
      }#end cycle component of restriction
      
      if (! is.null(bm_matrixR))
      {
        rownames(bm_matrixR)=c();
        colnames(bm_matrixR)=c();
      }
      
      if (! is.null(bm_vectorR))
      {
        rownames(bm_vectorR)=c();
        colnames(bm_vectorR)=c();        
      }      
      
      behavioralTmp$matrixR=bm_matrixR;
      behavioralTmp$vectorR=bm_vectorR;
      
    }#endif restrict
    
    #get error
    #check behavioral has error def
    errorLocalIdx=which(errorIndexes %in% behavioralIndexes[idxBI]:nextDefIdx);
    errorRaw=NULL;
    errorType=NULL;
    errorDim=NULL;
    
    if (length(errorLocalIdx)>0) 
    {
      if (length(errorLocalIdx)>1) stop('LOAD_MODEL(): multiple ERROR definitions in behavioral "',behavioralName,'".');
      
      #analize all restrict definitions
      errorRaw='';         
      #read all lines of pdl definition
      nextKwIdx=min(kwIdx[which(kwIdx>errorIndexes[errorLocalIdx])]);
      errorRaw=paste(cleanModel[errorIndexes[errorLocalIdx]:(nextKwIdx-1)],collapse=' ');
      errorRaw=gsub("^ERROR\\s*>", "", errorRaw, ignore.case = TRUE);
      errorRaw=gsub("^\\s*|\\s*$", "", errorRaw);           
      
      
      #catch auto(n) directive with n=1..9
      if (length(grep('^AUTO\\([1-9]\\)',errorRaw))>0){
        errorType='AUTO';
        errorDim=gsub("AUTO\\(", "", errorRaw);
        errorDim=as.numeric(gsub("\\)", "", errorDim));
        
        if ((length(errorDim)==0) || ! is.finite(errorDim) || (errorDim<1 )  || (errorDim>9 )) 
          stop('LOAD_MODEL(): syntax error in ERROR definition "',errorRaw,'" in behavioral "',behavioralName,'". Usage: ERROR> AUTO(n)');
        
      } else {
        stop('LOAD_MODEL(): syntax error in ERROR definition "',errorRaw,'" in behavioral "',behavioralName,'". Usage: ERROR> AUTO(n)')
      }
      
    }
    
   
    
    #check behavioral has IV def
    #... WARNING! IV CAN SPAN ON SEVERAL LINES WITH A SINGLE TAG IV>
    ivLocalIdx=which(ivIndexes %in% behavioralIndexes[idxBI]:nextDefIdx);
    ivRaw=NULL;
    namesOnIV=c();
    if (length(ivLocalIdx)>0) 
    {
      #analize all iv definitions
      
      ivRaw='';
      for (ivLLidx in 1:length(ivLocalIdx))
      {
        #read all lines of restrict definition
        nextKwIdx=min(kwIdx[which(kwIdx>ivIndexes[ivLocalIdx[ivLLidx]])]);
        ivRawSingle=paste(cleanModel[ivIndexes[ivLocalIdx[ivLLidx]]:(nextKwIdx-1)],collapse='');
        ivRawSingle=gsub("^IV\\s*>", "", ivRawSingle, ignore.case = TRUE);
        ivRawSingle=gsub("\\s*", "", ivRawSingle);            
        
        #restrictRaw="C01+C02=0;C01+C03=0"
        ivRaw=paste0(ivRawSingle,';',ivRaw);
      }
    
    
    #analize ivRaw
    ivComponents=strsplit(ivRaw,';')[[1]];
    if (length(ivComponents)<1) stop('LOAD_MODEL(): NULL length on IV definition "',ivRaw,' in behavioral "',behavioralName,'".')
    
    
    for (idxIVComp in 1:length(ivComponents))
    {
      #check unknown funs names
      unknownFuns=.unknownFunsNames(idxIVComp,reservedKeyw);
      if (length(unknownFuns)>0) stop('LOAD_MODEL(): unsupported functions in IV definition: "',ivComponents[idxIVComp],'" in behavioral "',behavioralName,'": ',
                                      paste0(paste0(unknownFuns,'()'),collapse=', '));
      
    
      #trim and extract symbol names from IV
      namesOnIV=c(namesOnIV,strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnEqCleaner,' ',ivComponents[idxIVComp],ignore.case=TRUE)),'\\s+')[[1]]);
      
      #check IV is non trivial
      if (nchar(ivComponents[idxIVComp])==0 || length(ivComponents[idxIVComp])==0 )
        stop('LOAD_MODEL(): syntax error in IV definition: "',ivComponents[idxIVComp],'" in behavioral "',behavioralName,'".');
      
    }
    
    
    #remove numbers from names
    rmvNumOnIVIdx=c();
    for (idxNamesOnIV in 1:length(namesOnIV)) {
      if (length(grep(strongCharOnNumbs,namesOnIV[idxNamesOnIV]))>0) 
      {
        rmvNumOnIVIdx=c(rmvNumOnIVIdx,idxNamesOnIV);
        
      }
    }
    if (length(rmvNumOnIVIdx)>0) namesOnIV=namesOnIV[-rmvNumOnIVIdx];
    
    
    #check components name
    for (idxNOEQ in 1:length(namesOnIV)) {
      if (length(grep(allowedCharOnName,namesOnIV[idxNOEQ]))==0) 
      {
        stop('LOAD_MODEL(): syntax error in IV definition: "',ivRaw,'". The following symbol cannot be a variable name: "',namesOnIV[idxNOEQ],'".');
        
      }
    }
    
    }
    
    behavioralTmp$eq=eqRaw;
    behavioralTmp$iv=ivRaw;
    behavioralTmp$eqCoefficientsNames=coeff;
    behavioralTmp$eqCoefficientsCount=length(coeff);
    behavioralTmp$eqCoefficientsNamesOriginal=coeffOriginal;
    behavioralTmp$eqComponentsNames=sort(unique(namesOnEq));
    behavioralTmp$IVComponentsNames=sort(unique(namesOnIV));
    behavioralTmp$tsrange=behavioralTsrange;
    behavioralTmp$storeVarName=storeVarName;
    behavioralTmp$storePosition=storePosition;
    behavioralTmp$eqRegressorsNames=eqRegressorsNames;
    behavioralTmp$eqRegressorsNamesOriginal=eqRegressorsNamesOriginal;
    
    behavioralTmp$pdlRaw=pdlRaw;
    behavioralTmp$pdlMatrix=bm_pdlMatrix;
    behavioralTmp$pdlRestrictionMatrix=bm_pdlRestrictionMatrix;
    behavioralTmp$pdlRestrictionVector=bm_pdlRestrictionVector;
    behavioralTmp$restrictRaw=restrictRaw;
    behavioralTmp$errorRaw=errorRaw;
    behavioralTmp$errorType=errorType;
    behavioralTmp$errorDim=errorDim;
    
    behavioralTmp$lhsFun=lhsFun;
    
    
    #append to model list of behavioral
    if (! is.null(model$behaviorals[[behavioralName]])) 
      stop('LOAD_MODEL(): duplicated behavioral name "',behavioralName,'".');
    model$behaviorals[[behavioralName]]=behavioralTmp;
    
    #add name to vendog in related position (required for ordering algo)
    model$vendog[behavioralIndexes[idxBI]]=behavioralName;
    
    
  }# end behavioral loop
  
  
  #analize code identities ----------------------------------------------------------------------
  model$identities=list();
  if (length(identityIndexes)>0) for(idxII in 1:length(identityIndexes))
  {
    
    identityTmp=list();    
    
    #get next keyword definition index
    nextKwIdx=min(kwIdx[which(kwIdx>identityIndexes[idxII])]);    
    
    
    #read all lines of behavior definition
    identitylRaw=paste(cleanModel[identityIndexes[idxII]:(nextKwIdx-1)],collapse=' ')
    identitylRaw=gsub("^IDENTITY\\s*>", "", identitylRaw, ignore.case = TRUE);
    identitylRaw=gsub("^\\s+|\\s+$", "", identitylRaw);
    
    #get identity name
    identityName=gsub("^\\s+|\\s+$", "",identitylRaw);    
    if (nchar(identityName)==0) stop('LOAD_MODEL(): unknown identity name in line: "',cleanModel[identityIndexes[idxII]],'".');
    
    #if (length(grep(allowedCharOnName,identityName))==0) stop('LOAD_MODEL(): invalid identity name in line: "',cleanModel[identityIndexes[idxII]],'"');
    if (length(grep(allowedCharOnName,identityName))==0) stop('LOAD_MODEL(): invalid identity name: "',identityName,'"');
    
    #get next eq/ident definition index
    nextDefIdx=min(eqIdenDefIdx[which(eqIdenDefIdx>identityIndexes[idxII])]);    
    
    #check identity has eq def
    eqLocalIdx=which(eqIndexes %in% identityIndexes[idxII]:nextDefIdx);
    if (length(eqLocalIdx)==0) stop('LOAD_MODEL(): no EQ definition in identity "',identityName,'".');
    if (length(eqLocalIdx)>1) stop('LOAD_MODEL(): multiple EQ definitions in identity "',identityName,'".');
    
    #get eq
    #read all lines of EQ definition
    nextKwIdx=min(kwIdx[which(kwIdx>eqIndexes[eqLocalIdx])]);
    eqRaw=paste(cleanModel[eqIndexes[eqLocalIdx]:(nextKwIdx-1)],collapse=' ');
    eqRaw=gsub("^EQ\\s*>", "", eqRaw, ignore.case = TRUE);
    eqRaw=gsub("\\s*", "", eqRaw); 
    
    #check unknown funs names
    unknownFuns=.unknownFunsNames(eqRaw,reservedKeyw)
    if (length(unknownFuns)>0) stop('LOAD_MODEL(): unsupported functions in EQ definition: "',eqRaw,'" in identity "',identityName,'": ',
                                    paste0(paste0(unknownFuns,'()'),collapse=', '));
    
    
    #extract components names from EQ
    namesOnEq=strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnEqCleaner,' ',eqRaw,ignore.case=TRUE)),'\\s+')[[1]];
    
    eqRawSplitted=strsplit(eqRaw,'\\=')[[1]];
    if (length(eqRawSplitted)!=2) stop('LOAD_MODEL(): syntax error in EQ definition: "',eqRaw,'".')
    
    #convert RHS text to expression
    #rhsExp=parse(text=.MODEL_MOD_FUNC_NAMES(eqRawSplitted[2]));
    rhsExp=eqRawSplitted[2];
    lhsExp=eqRawSplitted[1];
    
   
    
    if (lhsExp=='') stop('LOAD_MODEL(): syntax error in EQ definition: "',eqRaw,'" in identity "',identityName,'".')
    
    #find lhs func
    tryCatch({
      lhsFun=.parseLhsEQ(lhsExp,allowedLhsEQfuns);
    },error=function(err){
      stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" in identity "',identityName,'".')
    }
    )
    
    # lhs identity -------------------------------------------------------------------
    
    #check lhs fun syntax
    if (lhsFun$funName=='I')
    {
      #check EQ contains behavioral name
      #if (length(grep(paste0("^",behavioralName,"="),eqRaw))==0) 
      if (lhsFun$args!=identityName)
        stop('LOAD_MODEL(): LHS of EQ definition: "',eqRaw,'" must contain only the idenity name "',identityName,'" or the allowed LHS functions: ',paste0(paste0(allowedLhsEQfunsPub,'()'),collapse=', '));
      
    } else if (lhsFun$funName=='LOG')
    {
      if (lhsFun$argsCount!=1 || lhsFun$args!=identityName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the identity "',identityName,'". If the LOG of the endogenous variable is requested please try the following: "LOG(',identityName,')"');
      
    } else if (lhsFun$funName=='EXP')
    {
      if (lhsFun$argsCount!=1 || lhsFun$args!=identityName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the identity "',identityName,'". If the EXP of the endogenous variable is requested please try the following: "EXP(',identityName,')"');
      
    } else if (lhsFun$funName=='TSDELTA' || lhsFun$funName=='DEL')
    {
      if (  lhsFun$argsCount>2 || lhsFun$args[1]!=identityName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the identity "',identityName,'". If the TSDELTA of the endogenous variable is requested please try the following: "TSDELTA(',identityName,')" or "TSDELTA(',behavioralName,',n)"');
      
    } else if (lhsFun$funName=='TSDELTAP' )
    {
      if (  lhsFun$argsCount>2 || lhsFun$args[1]!=identityName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the identity "',identityName,'". If the TSDELTAP of the endogenous variable is requested please try the following: "TSDELTAP(',identityName,')" or "TSDELTAP(',behavioralName,',n)"');
      
    } else if (lhsFun$funName=='TSDELTALOG' )
    {
      if (  lhsFun$argsCount>2 || lhsFun$args[1]!=identityName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the identity "',identityName,'". If the TSDELTALOG of the endogenous variable is requested please try the following: "TSDELTALOG(',identityName,')" or "TSDELTALOG(',behavioralName,',n)"');
      
    }
    
    
    #check EQ contains identity name
    #if (length(grep(paste0("^",identityName,"="),eqRaw))==0) 
    #  stop('LOAD_MODEL(): LHS of EQ definition: "',eqRaw,'" must contain only the identity name "',identityName,'".');
    
    
    #check EQ contains other names
    if (length(namesOnEq[-which(namesOnEq==identityName)])==0) 
      stop('LOAD_MODEL(): singular equation in EQ "',eqRaw,'" in identity "',identityName,'"');
    
    
    if (! .checkExpression(rhsExp)) stop('LOAD_MODEL(): syntax error in RHS of EQ definition: "',eqRaw,'".')
    
    
    #remove numbers
    rmvNumOnEqIdx=c();
    for (idxNamesOnEq in 1:length(namesOnEq)) {
      if (length(grep(strongCharOnNumbs,namesOnEq[idxNamesOnEq]))>0) 
      {
        rmvNumOnEqIdx=c(rmvNumOnEqIdx,idxNamesOnEq);
        
      }
    }
    if (length(rmvNumOnEqIdx)>0) namesOnEq=namesOnEq[-rmvNumOnEqIdx];
    
    #check components name
    for (idxNOEQ in 1:length(namesOnEq)) {
      if (length(grep(allowedCharOnName,namesOnEq[idxNOEQ]))==0) 
      {
        stop('LOAD_MODEL(): syntax error in EQ definition: "',eqRaw,'". The following symbol cannot be a variable name: "',namesOnEq[idxNOEQ],'".');
        
      }
    }
    
    #check identity has IF def
    ifLocalIdx=which(ifIndexes %in% identityIndexes[idxII]:nextDefIdx);
    ifRaw=NULL;
    identityTmp$hasIF=FALSE;
    if (length(ifLocalIdx)>0) 
    {
      if (length(ifLocalIdx)>1) stop('LOAD_MODEL(): multiple IF definitions in identity "',identityName,'".');
      
      #analize all restrict definitions
      ifRaw='';         
      #read all lines of pdl definition
      nextKwIdx=min(kwIdx[which(kwIdx>ifIndexes[ifLocalIdx])]);
      ifRaw=paste(cleanModel[ifIndexes[ifLocalIdx]:(nextKwIdx-1)],collapse=' ');
      ifRaw=gsub("^IF\\s*>", "", ifRaw, ignore.case = TRUE);
      ifRaw=gsub("^\\s*|\\s*$", "", ifRaw);           
      
      #check for unknown funs
      unknownFuns=.unknownFunsNames(ifRaw,reservedKeyw)
      if (length(unknownFuns)>0) stop('LOAD_MODEL(): unsupported functions in IF definition: "',ifRaw,'" in identity "',identityName,'": ',
                                      paste0(paste0(unknownFuns,'()'),collapse=', '));
      
      #check double logical operator
      if (grepl('&&',ifRaw)) stop('LOAD_MODEL(): IF definition: "',ifRaw,'" in identity "',identityName,'" cannot contain "&&". Please consider using the single operator "&"');
      if (grepl('\\|\\|',ifRaw)) stop('LOAD_MODEL(): IF definition: "',ifRaw,'" in identity "',identityName,'" cannot contain "||". Please consider using the single operator "|"');
      
      #extract components names from if
      namesOnIf=strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnIfCleaner,' ',ifRaw,ignore.case=TRUE)),'\\s+')[[1]];
      
      #namesOnIf=gsub("^\\s+|\\s+$", "", namesOnIf);  
      #remove numbers
      rmvNumOnIfIdx=c();
      for (idxNamesOnIf in 1:length(namesOnIf)) {
        if (length(grep(strongCharOnNumbs,namesOnIf[idxNamesOnIf]))>0) 
        {
          rmvNumOnIfIdx=c(rmvNumOnIfIdx,idxNamesOnIf);
          
        }
      }
      if (length(rmvNumOnIfIdx)>0) namesOnIf=namesOnIf[-rmvNumOnIfIdx];
      
      
      #check IF condition is time series
      if (length(namesOnIf)==0) stop(paste0('LOAD_MODEL(): syntax error in IF condition "',ifRaw,'" in identity "',identityName,'". Logical condition must contain time series.'));
      
      #check components name
      for (idxNOEQ in 1:length(namesOnIf)) {
        if (length(grep(allowedCharOnName,namesOnIf[idxNOEQ]))==0) 
        {
          stop(paste0('LOAD_MODEL(): syntax error in IF definition: "',ifRaw,'" on identity "',identityName,'". The following symbol cannot be a variable name: "',namesOnIf[idxNOEQ],'".'));
          
        }
      }
      
      namesOnEq=c(namesOnEq,namesOnIf);
    }	
    
    #store stuff
    identityTmp$eqRaw=paste0(eqRaw,';');
    #identityTmp$lhsFun=lhsFun; 
    
    #we must deal with multiple lhsFun due to IF conditions
    identityTmp$multipleLhsFun=list(lhsFun);
    
    #build identity eq as IF (condition) THEN eq
    if (! is.null(ifRaw)) 
    {
      
      
      tmpIfRaw=ifRaw;
      tmpIfRaw=gsub('\\.EQ\\.',' == ',tmpIfRaw);
      tmpIfRaw=gsub('\\.NE\\.',' != ',tmpIfRaw);
      tmpIfRaw=gsub('\\.GE\\.',' >= ',tmpIfRaw);
      tmpIfRaw=gsub('\\.LE\\.',' <= ',tmpIfRaw);
      tmpIfRaw=gsub('\\.GT\\.',' > ',tmpIfRaw);
      tmpIfRaw=gsub('\\.LT\\.',' < ',tmpIfRaw);
      
	  tmpIfRaw=gsub('<-','< -',tmpIfRaw);											
      if (! grepl('(==|<|>|>=|<=|!=|&|\\|)',tmpIfRaw))
        stop(paste0('LOAD_MODEL(): syntax error in IF condition "',ifRaw,'" in identity "',identityName,'". No logical operators found.'))
      
      
      if (! .checkExpression(tmpIfRaw)) stop('LOAD_MODEL(): syntax error in IF definition: "',tmpIfRaw,'" in identity "',identityName,'".')
      
      identityTmp$eqFull=paste0('__IF__ (',ifRaw,') __THEN__ ',identityTmp$eqRaw);
      
      identityTmp$ifCondition=paste0(tmpIfRaw,';');
      identityTmp$hasIF=TRUE;
    } 
    else
    {
      identityTmp$ifCondition=paste0('__TRUE__;');
      identityTmp$eqFull=identityTmp$eqRaw;
    }
    
    identityTmp$eqComponentsNames=sort(unique(namesOnEq));
    
    #RAW must be refined
    if (! is.null(ifRaw)) identityTmp$ifRaw=paste0(ifRaw,';');
    
    #append to model list of identities. if double exclusive IF only the first one will be stored in identity
    if (! is.null(model$identities[[identityName]])) 
    { 
      
      model$identities[[identityName]]$eqRaw=paste0(model$identities[[identityName]]$eqRaw,identityTmp$eqRaw);
      model$identities[[identityName]]$ifCondition=paste0(model$identities[[identityName]]$ifCondition,identityTmp$ifCondition);
      model$identities[[identityName]]$multipleLhsFun[length(model$identities[[identityName]]$multipleLhsFun)+1]=
        list(lhsFun);
      
      if (! is.null(ifRaw)) 
      {
        model$identities[[identityName]]$ifRaw=paste0(model$identities[[identityName]]$ifRaw,identityTmp$ifRaw);
        
        #identity$hasIf == TRUE if any eq has IF condition
        model$identities[[identityName]]$hasIF=model$identities[[identityName]]$hasIF || identityTmp$hasIF;
      }
      model$identities[[identityName]]$eqFull=paste0(model$identities[[identityName]]$eqFull,identityTmp$eqFull);
      
      model$identities[[identityName]]$eqComponentsNames=sort(unique(c(model$identities[[identityName]]$eqComponentsNames,identityTmp$eqComponentsNames)));           
    } else
    {
      model$identities[[identityName]]=identityTmp;
      
      #add name to vendog in related position (required for ordering algo)
      model$vendog[identityIndexes[idxII]]=identityName;
      
    }
    
  }# end identities loop
  
  model$cleanModel=cleanModel;
  model$rawData=rawData;
  
  totNumEqs=length(behavioralIndexes);
  totNumIds=length(model$identities);#can have more indexes than identities due to IF
  
  model$totNumEqs=totNumEqs;
  model$totNumIds=totNumIds;
  model$eqCoeffNum=totNumCoeff;
  
  #set max lag to default
  model$max_lag=0;
  
  if (totNumEqs+totNumIds==0) stop('LOAD_MODEL(): empty model.');
  
  
  #create field for endogenous variables. do not sort 'cause ordering algo
  
  #remove NAs from vendog
  model$vendog=model$vendog[! is.na(model$vendog)];
  #model$vendog=sort(model$vendog);
  
  #create sublists for vendog behaviorals and vendog identities
  vendogBehaviorals=base::intersect(model$vendog,names(model$behaviorals));
  if (length(vendogBehaviorals)>0)
    if (all(nchar(vendogBehaviorals)>0))
      model$vendogBehaviorals=vendogBehaviorals;
  
  vendogIdentities=base::intersect(model$vendog,names(model$identities));
  if (length(vendogIdentities)>0)
    if (all(nchar(vendogIdentities)>0))
      model$vendogIdentities=vendogIdentities;
  
  #create field for exogenous variables
  vexog=c();
  #IV_fullComponentList=c();
  
  if (length(model$behaviorals)>0) for (i in 1:length(model$behaviorals))
  {
    vexog=c(vexog,model$behaviorals[[i]]$eqComponentsNames)
    #IV_fullComponentList=c(IV_fullComponentList,model$behaviorals[[i]]$IVComponentsNames)
  }
  
  if (length(model$identities)>0) for (i in 1:length(model$identities))
  {
    vexog=c(vexog,model$identities[[i]]$eqComponentsNames)
  }
  vexog=unique(vexog);
  
  #remove enogenous
  rmvIdx=which(vexog %in% model$vendog)
  if (length(rmvIdx)>0) vexog=vexog[-rmvIdx];
  
  model$vexog=vexog;
  
  
  #build incidence matrix
  incidence_matrix=matrix(0,nrow = length(model$vendog), ncol = length(model$vendog));
  colnames(incidence_matrix)=model$vendog;
  rownames(incidence_matrix)=model$vendog;
  
  #not used
  #model_fullComponentList=c(model$vendog,model$vexog);
  #model_IV_fullComponentList=unique(IV_fullComponentList);
  
  # behavioral expr and max_lag  ----------------------------------
  
  .MODEL_outputText(outputText=!quietly,'Analyzing behaviorals...\n');
  
  #build RHS expressions of behaviorals and identities (we need to speedup simulation...)
  #cycle in behaviorals
  
  
  if (length(model$behaviorals)>0) for(idxB in 1:length(model$behaviorals))
  {#cycle in components
    outRHS='';
    
    currentVendog=names(model$behaviorals)[idxB];
    currentBehavioral=model$behaviorals[[idxB]];
    
    for (idxC in 1:length(currentBehavioral$eqCoefficientsNames))
    {
      
 
      #build "+COEF*REGR"
      outRHS=paste0(outRHS, '+' ,
                    paste0(currentVendog,'__',currentBehavioral$eqCoefficientsNames[idxC]),
                    '*',
                    (currentBehavioral$eqRegressorsNames[idxC]));
      
      
    }
    
    
    #replace fun names
    outRHS=.MODEL_MOD_FUNC_NAMES(outRHS);
    
    #add constant adjustment (outRHS already starts with +)
    outRHS=paste0(currentVendog,'__ADDFACTOR',outRHS);
    
    #adapt eqRHS to autocorrelation if required
    if (! is.null(currentBehavioral$errorType) && currentBehavioral$errorType=='AUTO')
    {
      
      #add lagged errors to eqRHS
            
      #errorEqRaw=paste0(currentVendog,'-(',outRHS,')');
      errorEqRaw=paste0(.MODEL_MOD_FUNC_NAMES(currentBehavioral$lhsFun$raw),'-(',outRHS,')');
      
      for (idxED in 1:currentBehavioral$errorDim)
      {
        #final RHS eq is orginal plus lagged errors, i.e. historical endog less fitted
        outRHS=paste0(outRHS,'+',currentVendog,'__RHO__',idxED,'*.MODEL_TSLAG(',errorEqRaw,',',idxED,')');
      }
      
    }
    
    
    localComponentsNames=currentBehavioral$eqComponentsNames;
    
    #create local sublist for behaviorals and identities components
    localComponentsNamesBehaviorals=base::intersect(model$vendogBehaviorals,localComponentsNames);
    
    if (length(localComponentsNamesBehaviorals)>0)
      if (all(nchar(localComponentsNamesBehaviorals)>0))
        currentBehavioral$eqComponentsNamesBehaviorals=localComponentsNamesBehaviorals;
    
    localComponentsNamesIdentities=base::intersect(model$vendogIdentities,localComponentsNames);
    if (length(localComponentsNamesIdentities)>0)
      if (all(nchar(localComponentsNamesIdentities)>0))
        currentBehavioral$eqComponentsNamesIdentities=localComponentsNamesIdentities;
    
    localComponentsNamesExogenous=base::intersect(model$vexog,localComponentsNames);
    if (length(localComponentsNamesExogenous)>0)
      if (all(nchar(localComponentsNamesExogenous)>0))
        currentBehavioral$eqComponentsNamesExogenous=localComponentsNamesExogenous;
    
    tryCatch({
      
     
      outSim='';
      if(currentBehavioral$lhsFun$funName=='I')
      {
        #build outSim
        outSim=paste0(currentVendog,'=',outRHS)
        
      } else if (currentBehavioral$lhsFun$funName=='LOG')
      {
        outSim=paste0(currentVendog,'=exp(',outRHS,')')
        
      } else if (currentBehavioral$lhsFun$funName=='EXP')
      {
        outSim=paste0(currentVendog,'=log(',outRHS,')')
        
      } else if (currentBehavioral$lhsFun$funName=='TSDELTA' || currentBehavioral$lhsFun$funName=='DEL')
      { 
        if (currentBehavioral$lhsFun$argsCount>1 && ! is.finite(as.numeric(currentBehavioral$lhsFun$args[2])))
          stop('Non-numeric lag in LHS function: TSDELTA()');
        
        outSim=paste0(currentVendog,'=',outRHS,'+.MODEL_TSLAG(',currentVendog,',',ifelse(currentBehavioral$lhsFun$argsCount>1,currentBehavioral$lhsFun$args[2],'1'),')')
        
      } else if (currentBehavioral$lhsFun$funName=='TSDELTAP' )
      { 
        if (currentBehavioral$lhsFun$argsCount>1 && ! is.finite(as.numeric(currentBehavioral$lhsFun$args[2])))
          stop('Non-numeric lag in LHS function: TSDELTAP()');
        
        outSim=paste0(currentVendog,'=(',outRHS,
                      ')*.MODEL_TSLAG(',currentVendog,',',ifelse(currentBehavioral$lhsFun$argsCount>1,currentBehavioral$lhsFun$args[2],'1'),')/100',
                      '+.MODEL_TSLAG(',currentVendog,',',ifelse(currentBehavioral$lhsFun$argsCount>1,currentBehavioral$lhsFun$args[2],'1'),')')
        
      } else if (currentBehavioral$lhsFun$funName=='TSDELTALOG' )
      { 
        if (currentBehavioral$lhsFun$argsCount>1 && ! is.finite(as.numeric(currentBehavioral$lhsFun$args[2])))
          stop('Non-numeric lag in LHS function: TSDELTALOG()');
        
        outSim=paste0(currentVendog,'=exp(',outRHS,
                      ')*.MODEL_TSLAG(',currentVendog,',',ifelse(currentBehavioral$lhsFun$argsCount>1,currentBehavioral$lhsFun$args[2],'1'),')')
        
      }
      
      
      
      #add indexes to vars
      outSim=.appendIndexToVars(outSim,c(localComponentsNames,
                                         paste0(currentVendog,'__ADDFACTOR'))
                                ,'0');
      
      #explode model funs
      s=.explodeTSLAG(outSim)
      s=.explodeTSDELTA(s$output)
      s=.explodeTSDELTAP(s$output)
      s=.explodeTSDELTALOG(s$output)
      s=.explodeMTOT(s$output)
      s=.explodeMAVE(s$output)
      
      outSim=s$output
      
      #get max lag
      model$max_lag=max(model$max_lag,-.getLowerLag(outSim));
      
      #save sim expression
      currentBehavioral$eqSimExp=outSim;
      
      
      
    },error=function(e){stop(paste0('LOAD_MODEL(): error while parsing EQ expression "',currentBehavioral$eq,'" of behavioral "',
                                    currentVendog,'". ',e$message))});
    
    
    
    tryCatch({
      
            
      #get incidence matrix row
      vendogComponents=base::intersect(localComponentsNames,
                                       model$vendog)
      
      
      #remove "currentVendog[indexN,]" and ".MODEL_VIF(currentVendog[indexN,]"
      indexN='0';
      inputS=gsub(paste0('\\b',currentVendog,'\\[\\s*',indexN,'\\s*,\\s*\\]\\s*\\='),'',outSim);
      
      incidenceVendogs=.getIncidenceVendogs(inputS,
                                            vendogComponents,indexN);
      
      #save stuff
      if (length(incidenceVendogs)>0)
        for (idxIV in 1:length(incidenceVendogs))
          incidence_matrix[[currentVendog,
                            incidenceVendogs[idxIV]]]=1
      
    },error=function(e){stop(paste0('LOAD_MODEL(): error while building incidence vector for behavioral ',
                                    currentVendog,'. ',e$message))});
    
    #store stuff
    model$behaviorals[[idxB]]=currentBehavioral;
    
  }
  
  
  # identity expr and max lag  ----------------------------------
  
  .MODEL_outputText(outputText=!quietly,'Analyzing identities...\n');
  
  #cycle in identities
  length_model_identities=length(model$identities);
  names_model_identities=names(model$identities);
  
  if (length_model_identities>0) for(idxI in 1:length_model_identities)
  {
    
    
    currentIdentity=model$identities[[idxI]];
    
    currentVendog=names_model_identities[idxI];
    localComponentsNames=currentIdentity$eqComponentsNames;
    
    #create local sublist for behaviorals and identities components
    localComponentsNamesBehaviorals=base::intersect(model$vendogBehaviorals,localComponentsNames);
    if (length(localComponentsNamesBehaviorals)>0)
      if (all(nchar(localComponentsNamesBehaviorals)>0))
        currentIdentity$eqComponentsNamesBehaviorals=localComponentsNamesBehaviorals;
      
      localComponentsNamesIdentities=base::intersect(model$vendogIdentities,localComponentsNames);
      if (length(localComponentsNamesIdentities)>0)
        if (all(nchar(localComponentsNamesIdentities)>0))
          currentIdentity$eqComponentsNamesIdentities=localComponentsNamesIdentities;
        
        localComponentsNamesExogenous=base::intersect(model$vexog,localComponentsNames);
        if (length(localComponentsNamesExogenous)>0)
          if (all(nchar(localComponentsNamesExogenous)>0))
            currentIdentity$eqComponentsNamesExogenous=localComponentsNamesExogenous;
          
          #get identities multiple eqs and conditions
          eqRawSplitted=strsplit(currentIdentity$eqRaw,';')[[1]];
          ifConditionSplitted=strsplit(currentIdentity$ifCondition,';')[[1]];
          
          if (length(eqRawSplitted)!=length(ifConditionSplitted))
            stop(paste0('LOAD_MODEL(): equations EQ count differs from logical IF condition count in identity "',currentVendog,'".'));
          
          if (length(eqRawSplitted)!=length(currentIdentity$multipleLhsFun))
            stop(paste0('LOAD_MODEL(): equations EQ count differs from LHS functions count in identity "',currentVendog,'".'));
          
          
          outSim='';
          
          
          
          #cycle in eq
          for (eqIdx in 1:length(eqRawSplitted))
          {
            tryCatch({ 
              
              #get single eq and if condition in identity
              eqRaw=eqRawSplitted[eqIdx];
              ifConditionRaw=ifConditionSplitted[eqIdx];
              
              RHSRawComponent=strsplit(eqRaw,'=')[[1]][2];
              
              
              
              if (ifConditionRaw=='__TRUE__') 
              {
                ifRawComponent='TRUE';
                
              } else 
              {
                #ifRawComponent=paste0('which(',ifConditionRaw,')');
                ifRawComponent=ifConditionRaw;
              
              }
              
              #parse expression
              ifModText=.MODEL_MOD_FUNC_NAMES(ifRawComponent);
              RHSmodeText=.MODEL_MOD_FUNC_NAMES(RHSRawComponent);
              RHSmodeText=paste0(currentVendog,'__ADDFACTOR+',RHSmodeText);
              
              if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'I')
              {
                
              } else if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'LOG')
              {
                RHSmodeText=paste0('exp(',RHSmodeText,')');
                
              } else if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'EXP')
              {
                RHSmodeText=paste0('log(',RHSmodeText,')');
                
              } else if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'TSDELTA' || currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'DEL')
              {
                 
                if (currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1 && ! is.finite(as.numeric(currentIdentity$multipleLhsFun[[eqIdx]]$args[2])))
                  stop('Non-numeric lag in LHS function: TSDELTA()');
                
                RHSmodeText=paste0(RHSmodeText,'+.MODEL_TSLAG(',currentVendog,',',ifelse(currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1,currentIdentity$multipleLhsFun[[eqIdx]]$args[2],'1'),')')
                
              } else if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'TSDELTAP' )
              {
                
                if (currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1 && ! is.finite(as.numeric(currentIdentity$multipleLhsFun[[eqIdx]]$args[2])))
                  stop('Non-numeric lag in LHS function: TSDELTAP()');
                
                
                RHSmodeText=paste0('(',RHSmodeText,
                                   ')*.MODEL_TSLAG(',currentVendog,',',ifelse(currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1,currentIdentity$multipleLhsFun[[eqIdx]]$args[2],'1'),')/100',
                                   '+.MODEL_TSLAG(',currentVendog,',',ifelse(currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1,currentIdentity$multipleLhsFun[[eqIdx]]$args[2],'1'),')')
                
                
              } else if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'TSDELTALOG' )
              {
                
                if (currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1 && ! is.finite(as.numeric(currentIdentity$multipleLhsFun[[eqIdx]]$args[2])))
                  stop('Non-numeric lag in LHS function: TSDELTALOG()');
                
                
                RHSmodeText=paste0('exp(',RHSmodeText,
                                   ')*.MODEL_TSLAG(',currentVendog,',',ifelse(currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1,currentIdentity$multipleLhsFun[[eqIdx]]$args[2],'1'),')')
                
                
              }
              
              
                
              #build sim expresion w. constant adjustmnet
              outSim=paste0(outSim,
                            ifelse(ifRawComponent=='TRUE',
                                   paste0(currentVendog,'=',RHSmodeText,';'),
                                   paste0(currentVendog,'=.MODEL_VIF(',currentVendog,
                                          ',',
                                          ifModText,',',RHSmodeText,');'))
              )
              
            },error=function(e){
              stop(paste0('LOAD_MODEL(): error parsing EQ expression "',eqRaw,'" in identity "',currentVendog, '" - ', e$message));
            });
            
            
          }
          
          
          tryCatch({
            
             
            #add indexes to vars
            outSim=.appendIndexToVars(outSim,c(localComponentsNames,
                                               paste0(currentVendog,'__ADDFACTOR'))
                                      ,'0');
            
            #explode model funs
            s=.explodeTSLAG(outSim)
            s=.explodeTSDELTA(s$output)
            s=.explodeTSDELTAP(s$output)
            s=.explodeTSDELTALOG(s$output)
            s=.explodeMTOT(s$output)
            s=.explodeMAVE(s$output)
            outSim=s$output
            
            currentIdentity$eqSimExp=outSim
            
            #get max lag
            model$max_lag=max(model$max_lag,-.getLowerLag(outSim));
            
            
          },error=function(e){stop(paste0('LOAD_MODEL(): error while building simulation expression of identity ',
                                          currentVendog,'. ',e$message))});
          
          
          tryCatch({
            
            #get incidence matrix
            vendogComponents=base::intersect(localComponentsNames,
                                             model$vendog);
            
            
            
            #remove "currentVendog=[indexN,]" and ".MODEL_VIF(currentVendog[indexN,]"
            indexN='0';
            inputS=gsub(paste0('\\b',currentVendog,'\\[\\s*',indexN,'\\s*,\\s*\\]\\s*\\='),'',outSim);
            if (currentIdentity$hasIF) inputS=gsub(paste0('.MODEL_VIF\\(\\s*',currentVendog,'\\[\\s*',indexN,'\\s*,\\s*\\]\\s*\\,'),'\\(',inputS);
            
            incidenceVendogs=.getIncidenceVendogs(inputS,
                                                  vendogComponents,indexN);
            
            
            #save stuff
            if (length(incidenceVendogs)>0)
              for (idxIV in 1:length(incidenceVendogs))
                incidence_matrix[[currentVendog,
                                  incidenceVendogs[idxIV]]]=1
              
          },error=function(e){stop(paste0('LOAD_MODEL(): error while building incidence vector of identity ',
                                          currentVendog,'. ',e$message))});
          
          
          #update identity
          model$identities[[idxI]]=currentIdentity;
          
          
          
  }#end loop identities
  
 
  # reordering incidence ------------------------
  
  .MODEL_outputText(outputText=!quietly,'Optimizing...\n');
  
  #analize incident matrix
  temp_incidence_matrix=incidence_matrix;
  
  #speed-up variables
  total_im_rows=dim(incidence_matrix)[1]
  full_im_rows=1:total_im_rows;
  
  #list of dynamic removed vendog
  inactive_vendog_index=vector('integer',length=total_im_rows);
  
  
  #get VPRE endogenous
  VPRE=c();
  #we need to iterate till no changes to incidence_matrix
  #if flag > #vendog exit loop 'cause no further variable can be removed
  flag=0;
  #index of looping into vendog set
  localIdx=0;
  while (flag<=total_im_rows)
  {
    flag=flag+1;
    localIdx=localIdx+1;
    
    #reset vendog index if required...
    if (localIdx>total_im_rows) localIdx=1;
    
    #skip if current vendog has already been removed
    if (inactive_vendog_index[localIdx]==1) next;
    
    #if all 0 this vendog moves to VPRE
    if (all(temp_incidence_matrix[localIdx,which(inactive_vendog_index==0)]==0))
    { 
      #add vendog to VPRE
      VPRE=c(VPRE,row.names(temp_incidence_matrix)[localIdx]);
      
      #add vendog to inactive set
      inactive_vendog_index[localIdx]=1;
      
      #reset flag
      flag=0;
    }
    
  }
  
  
  #get VPOST endogenous
  VPOST=c();
  #we need to iterate till no changes to incidence_matrix
  #if flag > #vendog exit loop 'cause no further variable can be removed
  flag=0;
  #index of looping into vendog set
  localIdx=0;
  while (flag<=total_im_rows)
  {
    flag=flag+1;
    localIdx=localIdx+1;
    
    #reset vendog index if required...
    if (localIdx>total_im_rows) localIdx=1;
    
    #skip if current vendog has already been removed
    if (inactive_vendog_index[localIdx]==1) next;
    
    #if all 0 this vendog moves to VPOST
    if (all(temp_incidence_matrix[which(inactive_vendog_index==0),localIdx]==0))
    { 
      #add vendog to VPOST
      VPOST=c(row.names(temp_incidence_matrix)[localIdx],VPOST);
      
      #add vendog to inactive set
      inactive_vendog_index[localIdx]=1;
      
      #reset flag
      flag=0;
    }
    
  }
  
  
  #get VFEED endogenous
  VFEED=c();
  #we need to iterate till no changes to incidence_matrix
  #if flag > #vendog exit loop 'cause no further variable can be removed
  flag=0;
  #index of looping into vendog set
  localIdx=0;
  #flag_4 is set if step4 remove some vendog
  flag4=0;
  
  while(flag4==0)
  {
    flag4=1;
    
    while (flag<=total_im_rows )
    {
      flag=flag+1;
      localIdx=localIdx+1;
      
      #speedup
      which_inactive_vendog_index=which(inactive_vendog_index==0);
      
      #reset vendog index if required...
      if (localIdx>total_im_rows) localIdx=1;
      
      #skip if current vendog has already been removed
      if (inactive_vendog_index[localIdx]==1) next;
      
      #RULE #1
      #if self-feedback i.e. im[i,i]==1 add to VFEED
      if (temp_incidence_matrix[localIdx,localIdx]==1)
      {
        #add vendog to VFEED
        VFEED=c(VFEED,row.names(temp_incidence_matrix)[localIdx]);
        
        #add vendog to inactive set
        inactive_vendog_index[localIdx]=1;
        
        #check for all removed
        if (all(inactive_vendog_index==1)) break;
        
        #reset flag
        flag=0;
        next;
      }
      
      #RULE #2 
      #if column or row is 0 remove it
      if ((all(temp_incidence_matrix[which_inactive_vendog_index,localIdx]==0) || 
           all(temp_incidence_matrix[localIdx,which_inactive_vendog_index]==0)) )
      {
        #add vendog to inactive set
        inactive_vendog_index[localIdx]=1;
        
        #check for all removed
        if (all(inactive_vendog_index==1)) break;
        
        
        #reset flag
        flag=0;
        next;
      }
      
      #RULE #3 
      #if sum column is 1 merge and remove it
      if (sum(temp_incidence_matrix[which_inactive_vendog_index,localIdx])==1)  
      {
        
        #get vendog to be merged
        destinationVendogIndex=which(temp_incidence_matrix[which_inactive_vendog_index,localIdx]==1);
        #get rows
        sourceRow=temp_incidence_matrix[localIdx,which_inactive_vendog_index];
        destinationRow=temp_incidence_matrix[which_inactive_vendog_index[destinationVendogIndex],which_inactive_vendog_index];
        #build merge
        destinationRow[which(destinationRow | sourceRow)]=1;
        #save merge on destination row
        temp_incidence_matrix[which_inactive_vendog_index[destinationVendogIndex],which_inactive_vendog_index]=destinationRow;
        
        #add vendog to inactive set
        inactive_vendog_index[localIdx]=1;
        
        #check for all removed
        if (all(inactive_vendog_index==1)) break;
        
        #reset flag
        flag=0;
        next;
      }
      
      #if sum row is 1 merge and remove it
      if (sum(temp_incidence_matrix[localIdx,which_inactive_vendog_index])==1)  
      {
        
        #get vendog to be merged
        destinationVendogIndex=which(temp_incidence_matrix[localIdx,which_inactive_vendog_index]==1);
        #get columns
        sourceColumn=temp_incidence_matrix[which_inactive_vendog_index,localIdx];
        destinationColumn=temp_incidence_matrix[which_inactive_vendog_index,which_inactive_vendog_index[destinationVendogIndex]];
        #build merge
        destinationColumn[which(destinationColumn | sourceColumn)]=1;
        #save merge on destination row
        temp_incidence_matrix[which_inactive_vendog_index,which_inactive_vendog_index[destinationVendogIndex]]=destinationColumn;
        
        #add vendog to inactive set
        inactive_vendog_index[localIdx]=1;
        
        #check for all removed
        if (all(inactive_vendog_index==1)) break;
        
        #reset flag
        flag=0;
        next;
      }
      
      
    }
    
    #RULE4
    #remove most populated (row+col)
    targetIdxIM=0;
    tempSum=0;
    
    #check for all removed
    if (all(inactive_vendog_index==1)) break;
    
    #speedup
    which_inactive_vendog_index=which(inactive_vendog_index==0);
    
    for (idxIM in which_inactive_vendog_index)
    {
      if (sum(temp_incidence_matrix[idxIM,which_inactive_vendog_index])*
          sum(temp_incidence_matrix[which_inactive_vendog_index,idxIM])>tempSum) 
      {
        targetIdxIM=idxIM;
        tempSum=sum(temp_incidence_matrix[idxIM,which_inactive_vendog_index])*
          sum(temp_incidence_matrix[which_inactive_vendog_index,idxIM]);
      }
    }
    
    #add to VFEED selected vendog
    VFEED=c(VFEED,row.names(temp_incidence_matrix)[targetIdxIM]);
    
    #add vendog to inactive set
    inactive_vendog_index[targetIdxIM]=1;
    
    #check for all removed
    if (all(inactive_vendog_index==1)) break;
    
    #reset flags
    flag4=0;
    flag=0;
  }
  
  #reset incative vendogs
  inactive_vendog_index[T]=0;
  
  #get VSIM 
  VSIM=c();
  
  #remove VPRE
  if (length(VPRE)>0)
  {   
    #remove VPRE vendog from incidence matrix
    for (idxVF in 1:length(VPRE))
    {
      #index to be removed
      inactive_vendog_index[which(rownames(temp_incidence_matrix)==VPRE[idxVF])]=1;
      
    }
    
  }
  
  #remove VPOST
  if (length(VPOST)>0)
  {   
    #remove VPOST vendog from incidence matrix
    for (idxVF in 1:length(VPOST))
    {
      #index to be removed
      inactive_vendog_index[which(rownames(temp_incidence_matrix)==VPOST[idxVF])]=1;
      
    }
    
  }
  
  #remove VFEED
  if (length(VFEED)>0)
  {   
    #remove VFEED vendog from incidence matrix
    for (idxVF in 1:length(VFEED))
    {
      #index to be removed
      inactive_vendog_index[which(rownames(temp_incidence_matrix)==VFEED[idxVF])]=1;
    }
    
  }
  
  
  #we need to iterate till no empty or changes to incidence_matrix
  flag=0; 
  
  while (flag==0 ) 
  {
    flag=1;
    which_inactive_vendog_index=which(inactive_vendog_index==0);
    
    #if row is 0 save vendog and remove it
    for (idxIM in which_inactive_vendog_index)
      if (all(temp_incidence_matrix[idxIM,which_inactive_vendog_index]==0)) 
      {      
        
        #add vendog to VSIM
        VSIM=c(VSIM,row.names(temp_incidence_matrix)[idxIM]);
        
        #remove vendog from analysis
        inactive_vendog_index[idxIM]=1;
        
        #check for all removed
        if (all(inactive_vendog_index==1)) 
        {flag=1;
        break;}
        
        flag=0;
      }
    
    
  }
  
  #...hope never happen
  if (any(inactive_vendog_index==0)) stop(paste0('LOAD_MODEL(): cannot reduce incidence matrix.'))
  
  
  
  #reorder as model definition
  if (length(VFEED)>0) VFEED=model$vendog[which(model$vendog %in% VFEED)]
  
  #append VFEED to VSIM
  VSIM=c(VSIM,VFEED);
  
  #assign to model
  model$vpre=VPRE;
  model$vpost=VPOST;
  model$vfeed=VFEED;
  model$vsim=VSIM;
  
  
  
  if (length(c(model$vpre,model$vsim,model$vpost)) != length(model$vendog)) 
    stop('LOAD_MODEL(): unknown error while optimizing model. Countings do not match.');
  
  # build sim expressions ----------------------------------------------
  
  #we need at least 2 rows in sim matrices due to forecast
  max_lag_sim=max(model$max_lag,1)  													  
  tryCatch({
    if (length(model$behaviorals)>0) for(idxB in 1:length(model$behaviorals))
    {
      
      model$behaviorals[[idxB]]$eqSimExp=parse(text=.addToIndexInString(model$behaviorals[[idxB]]$eqSimExp,
                                                                        max_lag_sim+1
      ))
    }
  },error=function(err){stop(paste0('LOAD_MODEL(): error while building sim expression on behavioral "',names(model$behaviorals)[idxB],'". ',err$message))});
  
  
  tryCatch({
    if (length(model$identities)>0) for(idxI in 1:length(model$identities))
    {
      #if (names(model$identities)[idxI]=='CRIMPN')  
      
      model$identities[[idxI]]$eqSimExp=parse(text=.addToIndexInString(model$identities[[idxI]]$eqSimExp,
                                                                       max_lag_sim+1
      ))
    }
  },error=function(err){stop(paste0('LOAD_MODEL(): error while building sim expression on identity "',names(model$identities)[idxI],'". ',err$message))});
  
  #deal with reserved words
  allNames=c(model$vendog,model$vexog);
  if (any(grepl('__',allNames)))
    stop(paste0('LOAD_MODEL(): variable names cannot contain double underscore "__". Please change the following variable names: ',paste0(allNames[which(grepl('__',allNames))],collapse=', ')))
  
  
  
  .MODEL_outputText(outputText=!quietly,'Loaded model "',modelName,'":\n',
                    sprintf("%5i",model$totNumEqs),' behaviorals\n',
                    sprintf("%5i",model$totNumIds),' identities\n',
                    sprintf("%5i",model$eqCoeffNum),' coefficients\n',sep='');
  
  if (length(model$vexog)==0 && (! quietly) ) .MODEL_outputText(outputText=!quietly,'\nLOAD_MODEL(): warning, model has no exogenous variables.\n')
  if (length(model$vendog)==0 && (! quietly) ) .MODEL_outputText(outputText=!quietly,'\nLOAD_MODEL(): warning, model has no endogenous variables.\n')
  
  
  model$modelName=modelName;
  model$incidence_matrix=incidence_matrix;
  
  #if (cleanWorkspaceOnExit) CLEAN_WORKSPACE(model);
  
  .MODEL_outputText(outputText=!quietly,'...LOAD MODEL OK\n');
  
  return(model);
  
}


# LOAD MODEL DATA code ----------------------------------------

LOAD_MODEL_DATA <- function(model=NULL, 
                            modelData=NULL, 
                            quietly=FALSE, 
                            ...)
{
  #check args
  if (is.null(model)) stop('LOAD_MODEL_DATA(): NULL model argument.');  
  if (!(inherits( model ,'BIMETS_MODEL')) ) stop('LOAD_MODEL_DATA(): model must be instance of BIMETS_MODEL class.');
  
  #check arg
  if (!(is.logical(quietly))) stop('LOAD_MODEL_DATA(): "quietly" must be TRUE or FALSE.')
  
  
  .MODEL_outputText(outputText=!quietly,paste0('Load model data "',substitute(modelData),'" into model "', model$modelName,'"...\n'));
  
  #copy list to model
  model$modelData=modelData;
  
  tryCatch({
    .CHECK_MODEL_DATA(model,showWarnings=(! quietly),'LOAD_MODEL_DATA(): ');
  },error=function(e){stop('LOAD_MODEL_DATA(): ',e$message)});
  
  .MODEL_outputText(outputText=!quietly,'...LOAD MODEL DATA OK\n');
  
  #add frequency info
  model$frequency=frequency(model$modelData[[1]]);
  
  return(model);
}

# .CHECK MODEL_DATA code ----------------------------------------

.CHECK_MODEL_DATA <- function(model=NULL,showWarnings=TRUE, caller='.CHECK_MODEL_DATA(): ',...)
{
  #check args
  
  if (is.null(model)) stop(caller,'NULL model argument.');  
  if (!(inherits( model ,'BIMETS_MODEL'))) stop(caller,'model must be instance of BIMETS_MODEL class.');
  
  if (is.null(model$vendog))  stop(caller,'list of endogenous variables not found.');
  if (is.null(model$vexog))  stop(caller,'list of exogenous variables not found.');
  
  
  if (is.null(model$modelData)) stop(caller,'model has no data. Please use LOAD_MODEL_DATA().');
  modelData=model$modelData;
  
  if (! is.list(modelData)) stop(caller,'modelData must be a list of BIMETS time series.');
  if (length(modelData)==0) stop(caller,'empty modelData list.');
  
  if (is.null(names(modelData))) stop(caller,'names of modelData list are NULL.');
  if (any(names(modelData)=='')) stop(caller,'there are NULL names in modelData list.');
  
  
  #check time series
  for (idx in 1:length(model$modelData))
  {
    if (! is.bimets(model$modelData[[idx]])) 
      stop(caller,'modelData must be a list of BIMETS time series: "',names(model$modelData)[idx],'" time series is not compliant.');
    if (frequency(model$modelData[[idx]])!=frequency(model$modelData[[1]])) 
      stop(caller,'time series must have the same frequency. Check time series "',names(model$modelData)[idx],'"');
    if (showWarnings && any(! is.finite(coredata(model$modelData[[idx]])))) 
     .MODEL_outputText(outputText=showWarnings,paste0(caller,'warning, there are undefined values in time series "',names(model$modelData)[idx],'".\n',sep=''))
    
  }
  
  
  #if (length(base::setdiff(model$vendog,names(model$modelData)))>0)
    #stop('.CHECK_MODEL_DATA(): modelData must contain time series "',paste0(base::setdiff(model$vendog,names(model$modelData)),collapse=', '),'"');
  
  #if (length(base::setdiff(model$vexog,names(model$modelData)))>0)
    #stop('.CHECK_MODEL_DATA(): modelData must contain time series "',paste0(base::setdiff(model$vexog,names(model$modelData)),collapse=', '),'"');
  
  
  return(TRUE);
  
}

# ESTIMATE code ----------------------------------------

ESTIMATE <- function(model=NULL,
                     eqList=NULL,
                     TSRANGE=NULL,
                     forceTSRANGE=FALSE,
                     estTech='OLS',
                     IV=NULL,
                     forceIV=FALSE,
                     quietly=FALSE,
                     tol=1e-28,
                     digits=getOption('digits'),
                     centerCOV=TRUE,
                     CHOWTEST=FALSE,
                     CHOWPAR=NULL,
                     avoidCompliance=FALSE,
                     ...)
{	
  
  #check args
  if (is.null(estTech) || ((estTech!='OLS') && (estTech!='IV'))) stop('ESTIMATE(): estimation technique not supported.');
  if (is.null(model) ) stop('ESTIMATE(): NULL model.');
  if (!(inherits( model ,'BIMETS_MODEL'))) stop('ESTIMATE(): model must be instance of BIMETS_MODEL class.');
  
    if (! is.finite(tol) || tol<=0) stop('ESTIMATE(): please provide a valid tolerance value.')
  
  if ((! is.finite(digits) )|| (digits %% 1 !=0) || digits<=0 || digits > 22) 
    stop('ESTIMATE(): digits must be an integer between 1 and 16.');
  if (!(is.logical(centerCOV)) || is.na(centerCOV)) stop('ESTIMATE(): "centerCOV" must be TRUE or FALSE.');
  if (!(is.logical(quietly)) || is.na(quietly)) stop('ESTIMATE(): "quietly" must be TRUE or FALSE.')
  if (!(is.logical(forceTSRANGE)) || is.na(forceTSRANGE)) stop('ESTIMATE(): "forceTSRANGE" must be TRUE or FALSE.')
  if (!(is.logical(forceIV)) || is.na(forceTSRANGE)) stop('ESTIMATE(): "forceIV" must be TRUE or FALSE.')
  
  if ((! is.logical(CHOWTEST))  || is.na(CHOWTEST)) stop('ESTIMATE(): "CHOWTEST" must be TRUE or FALSE.');
  
  if (is.null(model$modelData) ) stop('ESTIMATE(): model has no data. Please reload model data.');
  if (is.null(model$frequency) ) stop('ESTIMATE(): model has no frequency. Please reload model data.');
  
  if (CHOWTEST==TRUE)
  {
    #if (is.null(CHOWPAR) ) stop('ESTIMATE(): please provide a "CHOWPAR" parameter.');
    
    if (! is.null(CHOWPAR) )
    tryCatch({
      
      CHOWPAR=normalizeYP(CHOWPAR,f=model$frequency);
      
    },error=function(e){
      stop('ESTIMATE(): please provide "CHOWPAR" as a c(year,period) array.');
    })
  }
  
  #flag for messages vertbosity
  outputText=!quietly;
  
  #get regex definitions
  regExDefs=.RegExGlobalDefinition();
  reservedKeyw=regExDefs$reservedKeyw;
  symOnEqCleaner=regExDefs$symOnEqCleaner;
  allowedCharOnName=regExDefs$allowedCharOnName;
  charsOnNumbs=regExDefs$charsOnNumbs;
  charsOnNumWithSign=regExDefs$charsOnNumWithSign;
  strongCharOnNumbs=regExDefs$strongCharOnNumbs;
  strongCharOnNumbsWithSign=regExDefs$strongCharOnNumbsWithSign;
  
  #a local env will contains model time series in assign(), get() and eval()
  localE = new.env();
  
  
  if (! avoidCompliance)
  {
    tryCatch({
    
      .CHECK_MODEL_DATA(model,showWarnings=(! quietly),'ESTIMATE(): ');
    
    },error=function(e){stop('ESTIMATE(): ',e$message)});
  }
  
  #avoid compliance on ts
  #tempNOC=getBIMETSconf('BIMETS_CONF_NOC');
  #setBIMETSconf('BIMETS_CONF_NOC',TRUE,suppressOutput=TRUE);
  
  
  #check eq names requested are in model behavioral names
  if (! is.null(eqList)) 
  {
    for ( eqReqName in eqList) 
    {
      if (! (eqReqName %in% names(model$behaviorals))) 
        stop('ESTIMATE(): requested equation "',eqReqName,'" is not in model behaviorals list.')
    }
  }	else 
  {
    eqList=names(model$behaviorals);
  }
  
  if (length(eqList)==0) 
  {
    .MODEL_outputText(outputText=!quietly,('ESTIMATE(): no behavioral equations to be estimated. Estimation will exit.\n'));
    return(model);
  }
  
  .MODEL_outputText(outputText=!quietly,'\nEstimate the Model ',model$modelName,':\n',sep='');
  .MODEL_outputText(outputText=!quietly,'the number of behavioral equations to be estimated is ',length(eqList),'.\n',sep='');
  
  # num of total coeffs
  totCoeff=0;
  
  for (eqIdx in 1:length(eqList))
  {
    
    totCoeff=totCoeff+length(model$behaviorals[[eqList[eqIdx]]]$eqCoefficientsNames);
  }
  
  .MODEL_outputText(outputText=!quietly,'The total number of coefficients is ',totCoeff,'.\n',sep='');
  
  #model data frequecy
  frequency=model$frequency;
  
  # main loop -----------------------------------------------
  
  for (eqIdx in 1:length(eqList))
  { 
    #save current behavioral
    currentBehavioral=model$behaviorals[[eqList[eqIdx]]];
    
    #deal with IV
    IVlist=list();
    
    #check args
    if (estTech=='IV' && is.null(IV) && is.null(currentBehavioral$iv)) stop('ESTIMATE(): please provide instrumental variables IV.')
    if (estTech=='IV' && is.null(IV) && forceIV==TRUE) stop('ESTIMATE(): please provide instrumental variables IV. "forceIV" is TRUE.')
    #if (estTech=='IV' && is.null(currentBehavioral$iv) && forceIV==FALSE) stop('ESTIMATE(): please provide instrumental variables IV. "forceIV" is FALSE.')
    
    if (!forceIV && !is.null(currentBehavioral$iv))
    {
      #split iv from mdl
      IV=strsplit(currentBehavioral$iv,';')[[1]];
    }
    
    if (!(is.null(IV)) && (length(IV)==0 || !(is.character(IV)) )) stop('ESTIMATE(): misspecified IV.')
    
    if (! is.null(IV)) 
    {
      namesOnIV=c()
      
      #trim and check size
      for (idxIV in 1:length(IV))
      {
        #trim leading/trailing spaces
        IV[idxIV]=gsub("\\s*", "", IV[idxIV])
        if (min(nchar(IV[idxIV]))==0) stop(paste0('ESTIMATE(): misspecified IV element #'),idxIV);
        
        namesOnIV=c(namesOnIV,strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnEqCleaner,' ',IV[idxIV],ignore.case=TRUE)),'\\s+')[[1]]);
        
      }
      
      #remove numbers from names
      rmvNumOnIVIdx=c();
      for (idxNamesOnEq in 1:length(namesOnIV)) {
        if (length(grep(strongCharOnNumbs,namesOnIV[idxNamesOnEq]))>0) 
        {
          rmvNumOnIVIdx=c(rmvNumOnIVIdx,idxNamesOnEq);
          
        }
      }
      if (length(rmvNumOnIVIdx)>0) namesOnIV=namesOnIV[-rmvNumOnIVIdx];
      
      namesOnIV=sort(unique(namesOnIV));
      
      
      #create local proxies for all references (components names used in eq) in IV
      for (idxName in 1:length(namesOnIV))
      {
        
        
        tryCatch(
          {
            tempName=namesOnIV[idxName];
            
            if (is.null(model$modelData[[tempName]])) stop();
            
            assign(tempName,model$modelData[[tempName]]
                   ,envir=localE
                   ,inherits = FALSE
            );
            
            
            
          },error=function(e){
            stop('ESTIMATE(): modelData must contain instrumental variable time series "',tempName,'".');
          })
        
      }
      
      #eval expression IV[idx]
      for (idxIV in 1:length(IV))
      {
        
        tryCatch(
          {
            #fix funs names
            tempName=.MODEL_MOD_FUNC_NAMES(IV[idxIV]);
            
            #eval IV expr
            tmpIV=eval(parse(text=tempName)
                       ,envir=localE
            );
            
            #assign to output list
            IVlist[[idxIV]]=tmpIV;
            
            
          },error=function(e){
            stop(paste0('ESTIMATE(): cannot evaluate instrumental variable expression "',IV[idxIV],'": ',e$message));
          })
      }
      
      #check compliance expression IV[idx]
      for (idxIV in 1:length(IV))
      {
        
        if ( (! is.bimets(IVlist[[idxIV]])) && (! is.finite(IVlist[[idxIV]])) )
        {
          
          stop(paste0('ESTIMATE(): instrumental variable "',IV[[idxIV]],'" is not a compliant time series.'));
          
        }
        
      }
      
    }# IV exists
    
    statsOut=NULL;
    
    
    #P matrix and options in Cochrane Orcutt (must be defined even if no Cochrane nedeed)
    bm_resP=NULL;
    bm_COmaxIters=1;#CO max iterations
    bm_COconvergence=0.005;#CO convergence 
    bm_COcurrentIter=0;#CO current iteration
    bm_COprevRho=NULL;#CO previous rho values
    
    .MODEL_outputText(outputText=outputText,'\n_________________________________________\n',sep='');
    .MODEL_outputText(outputText=outputText,'\nBEHAVIORAL EQUATION: ',eqList[eqIdx],'\n',sep='');
    .MODEL_outputText(outputText=outputText,'Estimation Technique: ',estTech,'\n',sep='');
    
    
    #number of current behavioral coefficients
    bm_coeffNum=length(currentBehavioral$eqCoefficientsNames);
    
    #deal with PDL
    thereArePDL=FALSE;
    bm_pdlMatrix=NULL;
    
    if (!(is.null(currentBehavioral$pdlMatrix) || 
          is.null(currentBehavioral$pdlRaw) ))
    {
      thereArePDL=TRUE;
      
      bm_pdlMatrix=currentBehavioral$pdlMatrix;
      bm_pdlRestrictionMatrix=currentBehavioral$pdlRestrictionMatrix;
      bm_pdlRestrictionVector=currentBehavioral$pdlRestrictionVector;
      
      
      
    }
    
    
    #deal with error autocorrelation
    
    thereAreErrorCorr=FALSE;    
    bm_errorType=NULL;
    bm_errorDim=0;    
    
    if (!(is.null(currentBehavioral$errorRaw) || 
          is.null(currentBehavioral$errorType) ||
          is.null(currentBehavioral$errorDim) ))
    {
      thereAreErrorCorr=TRUE;
      bm_errorType=currentBehavioral$errorType;
      bm_errorDim=currentBehavioral$errorDim;
      bm_errorRaw=currentBehavioral$errorRaw;
      bm_COmaxIters=20;
    }
    
    if (thereAreErrorCorr==TRUE)
    {
      .MODEL_outputText(outputText=outputText,'Autoregression of Order ',bm_errorDim,' (Cochrane-Orcutt procedure)\n');
    }
    
    #current behavioral TSRANGE
    localTSRANGE=NULL;
    localTSRANGE_bk=NULL; #this is needed if error autocorrelation is rewquired
    
    bm_vectorY_bk = NULL;
    bm_matrixX_bk = NULL;
    
    #behavioral has TSRANGE
    if (!forceTSRANGE && all(is.finite(currentBehavioral$tsrange)))
    {
      localTSRANGE=currentBehavioral$tsrange;
    } else 
    {
      
      #check if TSRANGE function argument is compliant
      if (! is.null(TSRANGE)) {
        tryCatch({
          if (! ( is.numeric(TSRANGE) && length(TSRANGE)==4 && 
                  .isCompliantYP(c(TSRANGE[1],TSRANGE[2]),frequency) && 
                  .isCompliantYP(c(TSRANGE[3],TSRANGE[4]),frequency) &&
                  NUMPERIOD(c(TSRANGE[1],TSRANGE[2]),c(TSRANGE[3],TSRANGE[4]),frequency)>=0
          )
          ) stop();  
        },error=function(e) {stop('ESTIMATE(): syntax error in TSRANGE: ',e$message)});
        
        localTSRANGE=TSRANGE;
        
      } else
      { 
        stop('ESTIMATE(): neither a local TSRANGE in the behavioral "',eqList[eqIdx],'" MDL nor a global one defined.');  
      }
    }		
    
    
    
    #build main X matrix: y = X * b + u
    bm_matrixX=c();
    
    localTSRANGE_bk=localTSRANGE;
    
    if (thereAreErrorCorr==TRUE)
    {
      #extend tsrange errorDim obs in the past. Cochrane-Orcutt procedure iteration 1
      
      tmpTSR=normalizeYP(c(localTSRANGE[1],localTSRANGE[2]-bm_errorDim),frequency);
      localTSRANGE[1]=tmpTSR[1];
      localTSRANGE[2]=tmpTSR[2];      
      
    }
    
    
    
    
    #check tsrange consistence
    tryCatch({
      if (NUMPERIOD(c(localTSRANGE[1],localTSRANGE[2]),c(localTSRANGE[3],localTSRANGE[4]),frequency)<0) stop() 
    },error=function(e){stop('ESTIMATE(): uncompliant TSRANGE in behavioral "',eqList[eqIdx],'"')});
    
    
    #create local proxies for all references (components names used in eq) in current behavior
    for (idxName in 1:length(currentBehavioral$eqComponentsNames))
    {
      
      tryCatch(
        {
          tempName=currentBehavioral$eqComponentsNames[idxName]
          
          if (is.null(model$modelData[[tempName]])) stop();
          
          assign(tempName,model$modelData[[tempName]]
                 ,envir=localE
                 ,inherits = FALSE
          );
          
          
          
        },error=function(e){
          stop('ESTIMATE(): modelData must contain time series "',currentBehavioral$eqComponentsNames[idxName],'"');
        })  
      
    }
    
    #do we have a constant term in regression?
    bm_ConstTermExist=FALSE;
    
    #used to check regressors have the same length
    regressorLengthTest=NULL
    
    #evaluate regressors: min 2
    if (length(currentBehavioral$eqRegressorsNames)<1) 
      stop('ESTIMATE(): behavioral "',eqList[eqIdx],'" has no regressors.')
    
    # regressor eval -----------------------------
    
    #cycle in regressors (#1 is eq name)
    regressorLengthTest=1+NUMPERIOD(c(localTSRANGE[1],localTSRANGE[2]),c(localTSRANGE[3],localTSRANGE[4]),frequency);
    
    #build matrix Z if IV
    bm_matrixZ=matrix(ncol=length(IVlist),nrow=regressorLengthTest);
    
    if (! is.null(IV))
    {
      
      #check IV count is non singular
      if (length(IVlist) < length(currentBehavioral$eqCoefficientsNames))
        .MODEL_outputText(outputText=!quietly,paste0('\nESTIMATE(): warning, instrumental variables count is less than total regressors count. System may become singular.',
                  ifelse(thereArePDL,' Please consider also PDL expansion.',''),'\n'))
      
      #project IV into TSRANGE
      tryCatch({
        
        for (idxIV in 1:length(IVlist))
        {
          
          if ((! is.bimets(IVlist[[idxIV]])) && is.finite(IVlist[[idxIV]]))
          {
            tempRegressor=rep(IVlist[[idxIV]],regressorLengthTest);
          } else  tempRegressor=(TSPROJECT(IVlist[[idxIV]],ARRAY=TRUE,TSRANGE=localTSRANGE,avoidCompliance=TRUE)); 
          
          
          #check regressor size 
          if (length(tempRegressor)!=regressorLengthTest) stop();
          if (any(! is.finite(tempRegressor))) stop();
          
          #assign to matrix
          bm_matrixZ[,idxIV]=tempRegressor;
        }
        
      },error=function(e)
      {
        stop('ESTIMATE(): instrumental variable expression "',IV[idxIV],
             '" in behavioral "',
             eqList[eqIdx],'"\n is not defined over the entire TSRANGE ',paste0(localTSRANGE,collapse=','),': ',e$message);        
      }
      );
    }  #now we have Z matrix
    
    
    
    for (idxRegrs in 1:length(currentBehavioral$eqRegressorsNames))
    {
      
      
      #replace numerical eq component with columns of numbers (constant term)
      #if (currentBehavioral$eqRegressorsNames[idxRegrs]=='') 
      if(grepl(strongCharOnNumbs,currentBehavioral$eqRegressorsNames[idxRegrs]))
      {
        tempRegressorExpr=currentBehavioral$eqRegressorsNames[idxRegrs];
        #evaluate expression (regressors definition e.g. 1)
        tryCatch({
          
            tempRegressor=eval(parse(text=tempRegressorExpr)
                             ,envir=localE
          );       
        },error=function(e)
        {
          stop('ESTIMATE(): cannot evaluate expression "',currentBehavioral$eqRegressorsNames[idxRegrs],
               '"\n(regressor #',idxRegrs,ifelse(thereArePDL,' with PDL expansion',''),') in behavioral "',
               eqList[eqIdx],'": ',e$message);        
        }
        );
        tempRegressor=rep(tempRegressor,regressorLengthTest)
        bm_ConstTermExist=TRUE;
        
        #if (is.null(regressorLengthTest)) regressorLengthTest=length(tempRegressor)
        
      } else {
        
        
        
        #fix functions names (e.g. LAG -> TSLAG)
        tempRegressorExpr=.MODEL_MOD_FUNC_NAMES(currentBehavioral$eqRegressorsNames[idxRegrs]);
        
        
        #evaluate expression (regressors definition e.g. LAG(GDP,3))
        tryCatch({
          
           tempRegressor=eval(parse(text=tempRegressorExpr)
                             ,envir=localE
          );       
        },error=function(e)
        {
          stop('ESTIMATE(): cannot evaluate expression "',currentBehavioral$eqRegressorsNames[idxRegrs],
               '"\n(regressor #',idxRegrs,ifelse(thereArePDL,' with PDL expansion',''),') in behavioral "',
               eqList[eqIdx],'": ',e$message);        
        }
        );
        
        #project regressor on tsrange
        tryCatch({
          
          tempRegressor=(TSPROJECT(tempRegressor,TSRANGE=localTSRANGE,ARRAY=TRUE,avoidCompliance=TRUE)); 
          
          
          #check regressor size 
          if (length(tempRegressor)!=regressorLengthTest) stop();
          
        },error=function(e)
        {
          stop('ESTIMATE(): expression "',currentBehavioral$eqRegressorsNames[idxRegrs],
               '"\n(regressor #',idxRegrs,
               ifelse(thereArePDL,' with PDL expansion',''),
               ifelse(thereAreErrorCorr,' with ERROR autocorrelation TSRANGE extension',''),
               ') in behavioral "',
               eqList[eqIdx],'"\ndoes not overlap with TSRANGE ',paste0(localTSRANGE,collapse=','),': ',e$message);        
        }
        );
        
      }	
      
      #check regressor has NA
      if (any(! is.finite(tempRegressor))) 
        stop('\n ESTIMATE(): there are undefined values in expression "',
             currentBehavioral$eqRegressorsNames[idxRegrs],
             '" (regressor #',idxRegrs,ifelse(thereArePDL,' with PDL expansion',''),
             ')\n inside the TSRANGE of behavioral "',eqList[eqIdx],'"\n',sep='')
      
      
      #adde evaluated regressor to X matrix
      bm_matrixX=cbind(bm_matrixX,tempRegressor);    
      
    }#end regressor generating cycle
    #now we have X matrix
    
    
    #project y
    bm_vectorY=get(eqList[eqIdx]
                   ,envir=localE,
                   inherits = F);
    
    #deal with LHS funs
    if (currentBehavioral$lhsFun$funName!='I')
    {
      tryCatch({
        if (currentBehavioral$lhsFun$funName=='LOG')
        {
          bm_vectorY=log(bm_vectorY);
          
        } else if (currentBehavioral$lhsFun$funName=='EXP')
        {
          bm_vectorY=exp(bm_vectorY);
          
        } else if (currentBehavioral$lhsFun$funName=='TSDELTA' || currentBehavioral$lhsFun$funName=='DEL')
        {
         
          if (currentBehavioral$lhsFun$argsCount>1 && ! is.finite(as.numeric(currentBehavioral$lhsFun$args[2])))
            stop('Non-numeric lag in TSDELTA()');
          
          bm_vectorY=TSDELTA(bm_vectorY,ifelse(currentBehavioral$lhsFun$argsCount>1,as.numeric(currentBehavioral$lhsFun$args[2]),1));
          
        } else if (currentBehavioral$lhsFun$funName=='TSDELTAP' )
        {
          
          if (currentBehavioral$lhsFun$argsCount>1 && ! is.finite(as.numeric(currentBehavioral$lhsFun$args[2])))
            stop('Non-numeric lag in TSDELTAP()');
          
          bm_vectorY=TSDELTAP(bm_vectorY,ifelse(currentBehavioral$lhsFun$argsCount>1,as.numeric(currentBehavioral$lhsFun$args[2]),1));
          
        } else if (currentBehavioral$lhsFun$funName=='TSDELTALOG' )
        {
          
          if (currentBehavioral$lhsFun$argsCount>1 && ! is.finite(as.numeric(currentBehavioral$lhsFun$args[2])))
            stop('Non-numeric lag in TSDELTALOG()');
          
          bm_vectorY=TSDELTALOG(bm_vectorY,ifelse(currentBehavioral$lhsFun$argsCount>1,as.numeric(currentBehavioral$lhsFun$args[2]),1));
          
        }
        
      },error=function(e)
      {
        stop('ESTIMATE(): error in LHS caclulation of "',currentBehavioral$lhsFun$raw,'" in behavioral "',eqList[eqIdx],'"',': ',e$message);        
      });
      
    }
    
    tryCatch({
      
      bm_vectorY=(TSPROJECT(bm_vectorY,TSRANGE=localTSRANGE,ARRAY=TRUE,avoidCompliance=TRUE)); 
      
      #check vector Y size
      if (length(bm_vectorY)!=regressorLengthTest) stop()
      
    },error=function(e)
    {
      stop('ESTIMATE(): LHS expression "',
           currentBehavioral$lhsFun$raw, '" in behavioral "',eqList[eqIdx],'" does not overlap with TSRANGE ',
           paste0(localTSRANGE,collapse=','),': ',e$message);        
    }
    );
    
    #check Y has NA
    if (any(! is.finite(bm_vectorY))) 
      stop('\n ESTIMATE(): there are undefined values in LHS expression "',
           currentBehavioral$lhsFun$raw, '" inside the TSRANGE of behavioral "',eqList[eqIdx],'"\n',sep='')
    
    
    #now we have vector Y
    #here we have X and Y
    
    
    thereAreRestrictions=FALSE;
    bm_restrictionsNum=0;
    
    #deal with restrictions
    if (!(is.null(currentBehavioral$restrictRaw) || 
          is.null(currentBehavioral$vectorR) ||
          is.null(currentBehavioral$matrixR) ))
    {
      
      
      thereAreRestrictions=TRUE;          
      
    }  
    
    if (thereAreRestrictions == TRUE || 
        (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
    {
      bm_matrixR=c();
      bm_vectorR=c();
      
      if (thereAreRestrictions == TRUE && thereArePDL == TRUE)
      {
        bm_matrixR=rbind(currentBehavioral$matrixR,currentBehavioral$pdlRestrictionMatrix);
        bm_vectorR=c(currentBehavioral$vectorR,currentBehavioral$pdlRestrictionVector);
      }
      else if (thereArePDL == TRUE )  {
        
        bm_matrixR=currentBehavioral$pdlRestrictionMatrix;
        bm_vectorR=currentBehavioral$pdlRestrictionVector;              
        thereAreRestrictions = TRUE;
      } else {
        #extract restriction from model definition
        bm_matrixR=currentBehavioral$matrixR;
        bm_vectorR=currentBehavioral$vectorR; 
      }
      
      bm_restrictionsNum=length(bm_vectorR)
      
    }
    
    #obs count
    bm_numObs=1+NUMPERIOD(c(localTSRANGE_bk[1],localTSRANGE_bk[2]),c(localTSRANGE_bk[3],localTSRANGE_bk[4]),frequency)
    
    
    #degree of freedom
    bm_DoF=bm_numObs-bm_coeffNum+bm_restrictionsNum-bm_errorDim;
    statsOut$DegreesOfFreedom=bm_DoF;
    
    
    #check consistence of regression
    if (bm_DoF<0) stop('ESTIMATE(): negative count of degrees of freedom in behavioral "',eqList[eqIdx],'".');
    if (bm_DoF==0) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, zero degrees of freedom in behavioral "',eqList[eqIdx],'". Other warnings and errors may arise.\n'));
    
    # CO start --------------------------------------------------------
    
    #used in CO error autocorrelation
    bm_COlastIter=FALSE;
    
    #cycle multiple time if error correlation required, else just 1 time
    for (bm_COcurrentIter in 1:bm_COmaxIters)
    {     
      
      
      
      #this is skipped anyway on firts iteration
      if ((thereAreErrorCorr==TRUE) && (bm_COcurrentIter>1))
      {
        #CO transformation
        
        
        
        #backup original vector and matrix
        bm_vectorY_bk = bm_vectorY;
        bm_matrixX_bk = bm_matrixX;
        if (estTech=='IV') bm_matrixZ_bk = bm_matrixZ;
        #CO transform
        bm_vectorY = bm_resP %*% bm_vectorY;
        bm_matrixX = bm_resP %*% bm_matrixX;  
        
        
        #in CO betaHat is calculated on original tsrange
        bm_vectorY = bm_vectorY[(1+bm_errorDim):length(bm_vectorY)];
        bm_matrixX = bm_matrixX[(1+bm_errorDim):nrow(bm_matrixX),];  
        if (estTech=='IV') bm_matrixZ = bm_matrixZ[(1+bm_errorDim):nrow(bm_matrixZ),];  
        
        
      }
      
      
      bm_betaHat=c()        
      
      
      #if restriction or pdl with length>degree+1 else OLS
      if (thereAreRestrictions == TRUE || 
          (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
      {
        
        
        if (estTech=='OLS') 
        {
          bm_WpX=t(bm_matrixX) %*% bm_matrixX;
        }
        
        if (estTech=='IV') 
        {
          tryCatch({
            
            
            bm_matrixXZ=bm_matrixZ %*% solve( crossprod( bm_matrixZ ),
                                              crossprod( bm_matrixZ, bm_matrixX ),
                                              tol=tol)
            
          },error=function(e)
          {
            stop('ESTIMATE(): given Z = the matrix of the instrumental variables as columns, Z\'*Z is not invertible. Behavioral: "',eqList[eqIdx],'". Check regressors and "IV" or try to reduce tolerance "tol" value. ',e$message);        
          }
          );
          
          bm_WpX=t(bm_matrixXZ) %*% bm_matrixXZ;
        }
        
        
        bm_ave_WpX=base::mean(bm_WpX);
        
        
        bm_Rscale=c();
        
        for (idxRscale in 1:dim(bm_matrixR)[1])
        {
          bm_Rscale=c(bm_Rscale,bm_ave_WpX/max(abs(bm_matrixR[idxRscale,])));        
        }
        
        
        
        bm_matrixR_tx=bm_matrixR*bm_Rscale;
        
        
        
        if (dim(bm_WpX)[1] != dim(bm_matrixR_tx)[2]) stop('ESTIMATE(): elements size mismatch in restriction augmented-cross product calculation. Behavioral: "',eqList[eqIdx],'".')
        
        bm_AWpX=rbind(bm_WpX,bm_matrixR_tx)
        
        
        
        bm_AmatrixR=cbind(bm_matrixR_tx,matrix(rep(0,dim(bm_matrixR_tx)[1]^2),nrow=dim(bm_matrixR_tx)[1]))
        
        
        bm_AAWpX=cbind(bm_AWpX,t(bm_AmatrixR))
        
        
        
        if (estTech=='OLS') bm_AvectorY=c(t(bm_matrixX) %*% bm_vectorY,bm_vectorR * bm_Rscale)
        if (estTech=='IV') bm_AvectorY=c(t(bm_matrixXZ) %*% bm_vectorY,bm_vectorR * bm_Rscale)
        
        
        
        
        tryCatch({
          
          bm_AAWpXi =solve(bm_AAWpX,
                           tol=tol);
          bm_betaHat=bm_AAWpXi %*% bm_AvectorY;
          bm_betaHat=bm_betaHat[1:dim(bm_matrixX)[2],,drop=FALSE];
        },error=function(e)
        {
          stop('ESTIMATE(): in restriction ',currentBehavioral$restrictRaw,
               ' given betaHat = (W\'X)^(-1) * (W\'Y), W\'X is not invertible.  Behavioral: "',eqList[eqIdx],'". Check regressors or try to reduce tolerance "tol" value. ',e$message);         
        }
        );
        
        #get f-statistics for restrictions
        #unrestricted beta OLS
        if (estTech=='OLS') {
          #check inverse exist
          bm_matrixXpXinv=NULL;
          
          tryCatch({
           
		   bm_matrixXpXinv=solve(t(bm_matrixX) %*% bm_matrixX,
                                  tol=tol);       
          },error=function(e)
          {
            stop('ESTIMATE(): given y = X * b + u, X\'*X is not invertible (un-restricted case in F-test). Behavioral: "',eqList[eqIdx],'". Check regressors or try to reduce tolerance "tol" value. ',e$message);        
          }
          );
          
          #OLS unrestricted :)
          bm_betaHat_unrestricted=bm_matrixXpXinv %*% t(bm_matrixX) %*% bm_vectorY;
          
          
        }
        
        #get f-statistics for restrictions
        #unrestricted beta IV
        if (estTech=='IV') {
          
          #check inverse exist
          bm_matrixXpXinv=NULL;
          
          
          
          tryCatch({
            bm_matrixXpXinv=solve( crossprod( bm_matrixXZ ),
                                   tol=tol);
            bm_betaHat_unrestricted=bm_matrixXpXinv %*% crossprod( bm_matrixXZ, bm_vectorY ) ;
            
          },error=function(e)
          {
            stop('ESTIMATE(): given y = X * b + u and given Z = the matrix of the instrumental variables as columns, and X_hat = Z * ( Z\' * Z )^(-1) * Z\' * X, X_hat\'*X_hat is not invertible (un-restricted case in F-test). Behavioral: "',eqList[eqIdx],'". Check regressors or try to reduce tolerance "tol" value. ',e$message);        
          }
          );
          
          
        }
        
        
        
      } else 
      {  
        
        #no restriction           
        #default OLS
        
        if (estTech=='OLS') {
          #check inverse exist
          bm_matrixXpXinv=NULL;
          
          tryCatch({
            bm_matrixXpXinv=solve(t(bm_matrixX) %*% bm_matrixX,
                                  tol=tol);       
          },error=function(e)
          {
            stop('ESTIMATE(): given y = X * b + u, X\'*X is not invertible. Behavioral: "',eqList[eqIdx],'". Check regressors or try to reduce tolerance "tol" value. ',e$message);        
          }
          );
          
          
          
          #OLS :)
          bm_betaHat=bm_matrixXpXinv %*% t(bm_matrixX) %*% bm_vectorY;
          
          
        }
        
        
        
        if (estTech=='IV') {
          
          #check inverse exist
          bm_matrixXpXinv=NULL;
          
          tryCatch({
            
            
            bm_matrixXZ=bm_matrixZ %*% solve( crossprod( bm_matrixZ ),
                                              crossprod( bm_matrixZ, bm_matrixX ),
                                              tol=tol )
            
          },error=function(e)
          {
            stop('ESTIMATE(): given Z = the matrix of the instrumental variables as columns, Z\'*Z is not invertible. Behavioral: "',eqList[eqIdx],'". Check regressors and "IV" or try to reduce tolerance "tol" value. ',e$message);        
          }
          );
          
          
          tryCatch({
            bm_matrixXpXinv=solve( crossprod( bm_matrixXZ ),
                                   tol=tol);
            bm_betaHat=bm_matrixXpXinv %*% crossprod( bm_matrixXZ, bm_vectorY ) ;
            
          },error=function(e)
          {
            stop('ESTIMATE(): given y = X * b + u and given Z = the matrix of the instrumental variables as columns, and X_hat = Z * ( Z\' * Z )^(-1) * Z\' * X, X_hat\'*X_hat is not invertible. Behavioral: "',eqList[eqIdx],'". Check regressors and "IV" or try to reduce tolerance "tol" value. ',e$message);        
          }
          );
          
          
        }
        
        
        
      } # end if no restriction
      
      if (thereAreRestrictions == TRUE || 
           (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
      {
        statsOut$matrixXaug=bm_AAWpX;
        statsOut$vectorYaug=bm_AvectorY;
      }
      #deal with error correlations
      if ((thereAreErrorCorr==TRUE) && (bm_COcurrentIter>1))
      {         
        
        #restore original Y and X (the long ones...): we need extended regressor in order to have coherent errors lags         
        bm_vectorY = bm_vectorY_bk;
        bm_matrixX = bm_matrixX_bk;
        if (estTech=='IV') bm_matrixZ = bm_matrixZ_bk;
        
        
        
      }       
      
      #exit from loop if convergence reached (variable set later...)
      if (bm_COlastIter==TRUE) break;
      
      #these are residuals
      bm_unSquare=bm_vectorY - bm_matrixX %*% bm_betaHat;
      
      #f-statistics for restrictions
      if (thereAreRestrictions == TRUE || 
          (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
        bm_unSquare_unrestricted=bm_vectorY - bm_matrixX %*% bm_betaHat_unrestricted;
      
      #OLS on errors: u=LAG(u)*rho + z
      if (thereAreErrorCorr==TRUE)
      {
        #cochrane orcutt evaluation
        
        #OLS on AR(bm_errorDim) on residuals
        bm_resX=NULL
        bm_resY=NULL
        
        #project residuals on original tsrange
        if (length(bm_unSquare)<(bm_errorDim+1)) 
          stop('ESTIMATE(): order of error autocorrelation greater than either available observations or TSRANGE specifications (Cochrane-Orcutt). Behavioral: "',eqList[eqIdx],'".');
        bm_resY=bm_unSquare[(1+bm_errorDim):length(bm_unSquare)];
        
        
        #create lagged residuals
        for(idxRS in 1:bm_errorDim)
        {
          
          bm_resX=cbind(bm_resX,bm_unSquare[(1+bm_errorDim-idxRS):(length(bm_unSquare)-idxRS)]);
        }
        
        #check inverse exist
        bm_matrixresXpresXinv=NULL;
        
        tryCatch({
          bm_matrixresXpresXinv=solve(t(bm_resX) %*% bm_resX,
                                      tol=tol);       
        },error=function(e)
        {
          stop('ESTIMATE(): given u = LAG(u) * rho + z, LAG(u)\'*LAG(u) is not invertible (Cochrane-Orcutt). Behavioral: "',
               eqList[eqIdx],'". Check residuals or try to reduce tolerance "tol" value. ',e$message);        
        }
        );
        
        
        
        #there are error autocorrelation coefficients
        bm_rhoHat=bm_matrixresXpresXinv %*% t(bm_resX) %*% bm_resY;  
        
        #deal with regression standard errors...
        bm_COunSquare=bm_resY-bm_resX %*% bm_rhoHat;
        bm_COuSquare=bm_COunSquare * bm_COunSquare;
        bm_COssr=sum(bm_COuSquare);
        bm_COcoeffNum=bm_errorDim;
        bm_COnumObs=1+NUMPERIOD(c(localTSRANGE_bk[1],localTSRANGE_bk[2]),c(localTSRANGE_bk[3],localTSRANGE_bk[4]),frequency);
        
        #bm_COser=sqrt(bm_COssr/(bm_COnumObs-bm_COcoeffNum))
        bm_COser=ifelse(bm_COnumObs - bm_COcoeffNum - bm_coeffNum +bm_restrictionsNum==0,0,sqrt((bm_COssr-(sum(bm_COunSquare)^2)/bm_COnumObs)/
                                                                                                  (bm_COnumObs - bm_COcoeffNum - bm_coeffNum +bm_restrictionsNum)) #...dont know why bm_COcoeffNum and bm_restrictionsNum
        );
        
        bm_COserAdj=bm_COser;
        if (!centerCOV) bm_COserAdj=ifelse(bm_COnumObs - bm_COcoeffNum - bm_coeffNum +bm_restrictionsNum==0,0,sqrt((bm_COssr)/
                                                                                                                     (bm_COnumObs - bm_COcoeffNum - bm_coeffNum +bm_restrictionsNum) #...dont know why bm_COcoeffNum and bm_restrictionsNum
        ));
        
        
        
        
        bm_COvcov=(bm_COserAdj*bm_COserAdj) * bm_matrixresXpresXinv;
        bm_COstderr=sqrt(diag(bm_COvcov));            
        
        
        statsOut$RhosCovariance=bm_COvcov;
        rownames(statsOut$RhosCovariance)=paste0("RHO_",1:(bm_COcoeffNum));
        colnames(statsOut$RhosCovariance)=paste0("RHO_",1:(bm_COcoeffNum));
        
        
        #create CO transf matrix (P matrix in user guide)
        bm_realTSRANGEobs=1+NUMPERIOD(c(localTSRANGE[1],localTSRANGE[2]),c(localTSRANGE[3],localTSRANGE[4]),frequency);
        bm_resP=matrix(rep(0,bm_realTSRANGEobs^2),nrow=bm_realTSRANGEobs);
        
        
        #bm_repP is modifier for original Y and X
        for (idxP in 1:bm_realTSRANGEobs)
        {
          bm_resP[idxP,idxP]=1;
          for (idxRS in 1:bm_errorDim)
          {
            if (idxP>idxRS) bm_resP[idxP,idxP-idxRS]=-bm_rhoHat[idxRS];
          }
        }
        
        
      }
      
      #set flag if convergence (we need another cycle in order to get betahat with that rho)
      if (thereAreErrorCorr==TRUE)
      {
        if (bm_COcurrentIter>1)
        {
          
          
          #if (any(100*abs((bm_rhoHat/bm_COprevRho)-1)<bm_COconvergence)) break;
          
          #convergence check
          if (all(abs(bm_rhoHat-bm_COprevRho)<bm_COconvergence)) bm_COlastIter=TRUE;
          #if (sqrt(sum((bm_rhoHat-bm_COprevRho)^2))/sqrt(sum(bm_COprevRho^2)) < bm_COconvergence) bm_COlastIter=TRUE;
          
        }
        
        #save results for next iteration
        bm_COprevRho=bm_rhoHat;
        
      }
      
      #take stop between iterations
      #readline('k0>');
      
    }#end CO iteration
    
    # CO end --------------------------------------------------
    
    
    if (thereAreErrorCorr==TRUE)
    { 
      #restore localRANGE
      localTSRANGE=localTSRANGE_bk;
      
      .MODEL_outputText(outputText=outputText,'\nConvergence was reached in ',bm_COcurrentIter,' / ',bm_COmaxIters,' iterations.\n\n')
    }
    
    
    bm_unSquare_no_error_correction=NULL;
    
    #if error correlation u must be calculated with tranformed y and x
    if ((thereAreErrorCorr==TRUE) )
    {
      #transorm with matrix P
      bm_vectorY = bm_resP %*% bm_vectorY;
      bm_matrixX = bm_resP %*% bm_matrixX;  
      
      #project on localTSRANGE
      bm_vectorY = bm_vectorY[(1+bm_errorDim):length(bm_vectorY)];
      bm_matrixX = bm_matrixX[(1+bm_errorDim):nrow(bm_matrixX),];  
      
      #save regressors and y (with ERROR> we save the modified ones)
      
      statsOut$matrixX_error_corrected=as.matrix(bm_matrixX);
      statsOut$vectorY_error_corrected=as.matrix(bm_vectorY);
      colnames(statsOut$matrixX_error_corrected)=currentBehavioral$eqRegressorsNames;
      
      #colnames(statsOut$vectorY_error_corrected)=eqList[eqIdx];
      colnames(statsOut$vectorY_error_corrected)=currentBehavioral$lhsFun$raw;
       
      #calc error
      bm_unSquare= bm_vectorY - bm_matrixX %*% bm_betaHat;   
      
      #f-statistics for restrictions
      if (thereAreRestrictions == TRUE || 
          (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
        bm_unSquare_unrestricted=bm_vectorY - bm_matrixX %*% bm_betaHat_unrestricted;
      
      #restore vectors
      bm_vectorY = bm_vectorY_bk;
      bm_matrixX = bm_matrixX_bk;
      
      bm_vectorY = bm_vectorY[(1+bm_errorDim):length(bm_vectorY)]
      bm_matrixX = bm_matrixX[(1+bm_errorDim):nrow(bm_matrixX),]   
      
      bm_unSquare_no_error_correction=bm_vectorY - bm_matrixX %*% bm_betaHat;   
       
    }
    
    bm_uSquare=bm_unSquare * bm_unSquare;
    
    #save regressors and y
    statsOut$matrixX=as.matrix(bm_matrixX);
    statsOut$vectorY=as.matrix(bm_vectorY);
    
    
    colnames(statsOut$matrixX)=currentBehavioral$eqRegressorsNames;
    #colnames(statsOut$vectorY)=eqList[eqIdx];
    colnames(statsOut$vectorY)=currentBehavioral$lhsFun$raw;
    
    #sum squared residuals
    bm_ssr=sum(bm_uSquare)
    
    
    
    #check residuals length
    if (NUMPERIOD(localTSRANGE[1:2],localTSRANGE[3:4],frequency)  +1 != length(bm_unSquare))
    {
      stop(paste0('ESTIMATE(): unknown error on residuals length in behavioral ',eqList[eqIdx]));
    }
    
    #time series for residuals
    bm_unSquareTS=TSERIES(bm_unSquare,START=localTSRANGE[1:2],FREQ=frequency,avoidCompliance = TRUE);
    if ((thereAreErrorCorr==TRUE) ) bm_unSquare_no_error_correctionTS=TSERIES(bm_unSquare_no_error_correction,START=localTSRANGE[1:2],FREQ=frequency,avoidCompliance = TRUE);
    
    
    statsOut$estimationTechnique=estTech;
    statsOut$TSRANGE=localTSRANGE;
    statsOut$InstrumentalVariablesRaw=IV;
    statsOut$tol=tol;
    statsOut$digits=digits;
    statsOut$centerCOV=centerCOV;
    
    
    #standard error regression    
    #bm_ser=sqrt(bm_ssr/(bm_numObs-bm_coeffNum+bm_restrictionsNum));
    #here below is centered
    bm_ser=ifelse(bm_DoF==0,0,sqrt((bm_ssr-(sum(bm_unSquare)^2)/bm_numObs)/(bm_DoF)));
    bm_serAdj=bm_ser;
    
    if (!centerCOV) bm_serAdj=ifelse(bm_DoF==0,0,sqrt((bm_ssr)/(bm_DoF)));
    
    
    
    statsOut$StandardErrorRegression=bm_ser;
    statsOut$StandardErrorRegressionNotCentered=ifelse(bm_DoF==0,0,sqrt((bm_ssr)/(bm_DoF)));
    
    #var-covar coeff
    if ( ! thereAreRestrictions)
    {
      bm_vcov=(bm_serAdj*bm_serAdj) * bm_matrixXpXinv;
      #bm_vcov=vcov(linear_model)
    } else {
      bm_vcov=(bm_serAdj*bm_serAdj) * bm_AAWpXi;      
    }
    
    
    
    #if restrictions diag cov elements can be very small but negatives... so abs(diag()) here below
    #coeff of variation betaHat
    #if restrictions size vcov != size betaHat
    bm_vcov=bm_vcov[1:length(bm_betaHat),1:length(bm_betaHat),drop=FALSE];
    suppressWarnings({bm_betaHatStdErr=sqrt(abs(diag(bm_vcov)))}); #warnings if restrictions due to NAs
    #bm_betaHatStdErr=bm_betaHatStdErr[1:length(bm_betaHat)]; 
    #remove 0/0 cases
    bm_betaHatCoV=ifelse(bm_betaHatStdErr==0,Inf,bm_betaHat/bm_betaHatStdErr);
    names(bm_betaHatCoV)=currentBehavioral$eqCoefficientsNames;
    
    
    
    
    #bm_vcovOut=bm_vcov[1:length(bm_betaHat),1:length(bm_betaHat),drop=FALSE];
    rownames(bm_vcov)=currentBehavioral$eqCoefficientsNames;
    colnames(bm_vcov)=currentBehavioral$eqCoefficientsNames;
    
    statsOut$CoeffCovariance=bm_vcov;
    statsOut$CoeffTstatistic=bm_betaHatCoV;
    
    
    
    # R & SPK differs in Rsquared! we choosed R 
    # https://stats.stackexchange.com/questions/26176/removal-of-statistically-significant-intercept-term-increases-r2-in-linear-mo
    
    bm_rSquared=1-(bm_ssr)/(sum((bm_vectorY-ifelse(bm_ConstTermExist,ave(bm_vectorY),0))^2))
    
    # #r-squared
    # if (thereAreErrorCorr==TRUE  )
    # {
    #   #this is ok even without error correlation - DEFAULT R lm formula
    #   bm_rSquared=1-(bm_ssr)/(sum((bm_vectorY - ave(bm_vectorY)  )^2)); #eq by wiki works fine with error autocorrelation
    #   
    # }   
    # 
    # if ((thereAreRestrictions == TRUE || 
    #      (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix)))))
    # { 
    #     # DEFAULT SPK 
    # 
    #     #eq by user guide
    #     bm_rSquared_tempY=bm_matrixX %*% bm_betaHat;
    #    
    #     bm_rSquared=(sum((bm_vectorY-base::mean(bm_vectorY))*(((bm_rSquared_tempY)-base::mean(bm_rSquared_tempY))))^2)/
    #          ((sum((((bm_rSquared_tempY)-base::mean(bm_rSquared_tempY))^2)))*(sum((bm_vectorY-base::mean(bm_vectorY))^2)));
    #   
    
    
    
    # }
    
    
    
    
    statsOut$RSquared=bm_rSquared;
    
    #adj r-squared
    #R lm default formula
    bm_adjrSquared=ifelse(bm_DoF==0,1,1-(1-bm_rSquared)*(bm_numObs-ifelse(bm_ConstTermExist,1,0))/(bm_DoF)); #+1-1
    
    if ((thereAreErrorCorr==TRUE  ) || ((thereAreRestrictions == TRUE || 
                                         (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))))
      bm_adjrSquared=ifelse(bm_DoF==0,1,1-(1-bm_rSquared)*(bm_numObs- 1)/(bm_DoF)); #+1-1
    
    
    statsOut$AdjustedRSquared=bm_adjrSquared;
    
    #assign beta hat to coefficients
    currentBehavioral$coefficients=bm_betaHat;
    
    if (thereAreErrorCorr==TRUE) rownames(bm_rhoHat)=paste0("RHO_",1:(bm_COcoeffNum));
    if (thereAreErrorCorr==TRUE) currentBehavioral$errorCoefficients=bm_rhoHat;
    
    
    if (length(currentBehavioral$coefficients) !=
        length(currentBehavioral$eqCoefficientsNames))
    {
      stop('ESTIMATE(): coefficients count differs from the length of the coefficients names.');
    }
    
    rownames(currentBehavioral$coefficients)=currentBehavioral$eqCoefficientsNames;
    
    #save residuals
    currentBehavioral$residuals=bm_unSquareTS;
    if (thereAreErrorCorr==TRUE) currentBehavioral$residuals_no_error_correction=bm_unSquare_no_error_correctionTS;
    
    
    #f-statistics for restrictions
    if (thereAreRestrictions == TRUE || 
        (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
    { 
      bm_uSquare_unrestricted=bm_unSquare_unrestricted * bm_unSquare_unrestricted;
      
      #sum squared residuals not restricted
      bm_ssr_unrestricted=sum(bm_uSquare_unrestricted);
      
      #f-test for restrictions
      if( thereAreRestrictions==TRUE ) 
      {
        
        bm_Ftest_restriction=ifelse(bm_numObs-bm_coeffNum-bm_errorDim<=0,NA,((bm_ssr-bm_ssr_unrestricted )/bm_restrictionsNum)/((bm_ssr_unrestricted)/(bm_numObs-bm_coeffNum-bm_errorDim)));
        bm_Ftest_probability=ifelse(bm_numObs-bm_coeffNum-bm_errorDim<=0,NA,((1-pf(bm_Ftest_restriction,bm_restrictionsNum,bm_numObs-bm_coeffNum))));
      }
      
    }
    
    
    #calc p-values
    pValues=bm_betaHat*0;
    if (bm_DoF>0) pValues=2*pt(-abs(bm_betaHatCoV),df=bm_DoF);
    
    
    statsOut$CoeffPvalues=pValues;
    
    #print stuff in screen
    #print estimated eq data
    
    stdFormat=paste0("%-",digits+5,".",digits,'g');
    stdFormatS=paste0("%-",digits+5,'s');
    
    
    for (idxCoeff in 1:length(bm_betaHat))
    {
      tmpSign='+';
      if (bm_betaHat[idxCoeff]<0) tmpSign='-';
      
      tmpPrefix=paste0(sprintf("%-20s",''),tmpSign,'   ');
      if (idxCoeff==1) {
        
        if (bm_betaHat[idxCoeff]<0) {
          tmpPrefix=paste0(sprintf("%-18s",currentBehavioral$lhsFun$raw),'= -   ');
        } else {
          tmpPrefix=paste0(sprintf("%-20s",currentBehavioral$lhsFun$raw),'=   '); 
        }
        
      }      
      
      suffix=''
      if (grepl(strongCharOnNumbs,currentBehavioral$eqRegressorsNames[idxCoeff])) 
      {
        #suffix=paste0('\U00B7 ',currentBehavioral$eqRegressorsNames[idxCoeff+1]);
        if(currentBehavioral$eqRegressorsNames[idxCoeff]!='1')
          suffix=paste0('CONST*',currentBehavioral$eqRegressorsNames[idxCoeff]);
      } else {
        suffix=paste0(currentBehavioral$eqRegressorsNames[idxCoeff]);
      }
      
      
      if (thereArePDL==FALSE)
      { 
        #if modify these lines also modify below
        #print tmpPrefix (+ or -) then coeff value then regressor eq
        .MODEL_outputText(outputText=outputText,'\n',tmpPrefix,sprintf(stdFormat,abs(bm_betaHat[idxCoeff])),suffix,'\n',sep='');
        
        #if restricted print string else CoV
        tmpCoV=bm_betaHatCoV[idxCoeff];  
        
        
        if (! is.finite(tmpCoV) || abs(tmpCoV)>1e9) 
        {
          tmpCoV='RESTRICT';
        } else
        {
          localPvalueS='';
          #if (pValues[idxCoeff]<0.1) localPvalueS='.';
          if (pValues[idxCoeff]<0.05) localPvalueS='*';
          if (pValues[idxCoeff]<0.01) localPvalueS='**';
          if (pValues[idxCoeff]<0.001) localPvalueS='***';
          
          tmpCoV=sprintf(stdFormat,tmpCoV);
          tmpCoV=paste0('T-stat. ',tmpCoV,localPvalueS);
        }
        #.MODEL_outputText(outputText=outputText,sprintf("%-22s",''),'( ',tmpCoV,' )\n',sep='');   
        .MODEL_outputText(outputText=outputText,sprintf("%-24s",''),tmpCoV,'\n',sep='');   
        
      } else {                
        
        if (! currentBehavioral$eqCoefficientsNames[idxCoeff] %in% currentBehavioral$eqCoefficientsNamesOriginal ) next;
        
        tmpMatrix=currentBehavioral$pdlMatrix
        tmpCoeff=currentBehavioral$eqCoefficientsNames[idxCoeff]
        tmpOrigCoeff=currentBehavioral$eqCoefficientsNamesOriginal
        tmpIdxinOriginal=which(tmpOrigCoeff==tmpCoeff)
        
        
        #if coefficient has pdl
        if (tmpMatrix[1,tmpIdxinOriginal]==1)
        {
          #print coeff name if pdl instead of value
          .MODEL_outputText(outputText=outputText,'\n',gsub('-','+',tmpPrefix),sprintf(stdFormatS,currentBehavioral$eqCoefficientsNames[idxCoeff]),suffix,'\n',sep='');
          #.MODEL_outputText(outputText=outputText,sprintf("%-22s",''),'( PDL )\n',sep=''); 
          .MODEL_outputText(outputText=outputText,sprintf("%-24s",''),'PDL\n',sep=''); 
          
        } else {
          #print tmpPrefix (+ or -) then coeff value then regressor eq
          .MODEL_outputText(outputText=outputText,'\n',tmpPrefix,sprintf(stdFormat,abs(bm_betaHat[idxCoeff])),suffix,'\n',sep='');
          
          #if restricted print string else CoV
          tmpCoV=bm_betaHatCoV[idxCoeff];          
          
          
          if (! is.finite(tmpCoV) || abs(tmpCoV)>1e9) 
          {
            tmpCoV='RESTRICT'          
          } else
          {
            localPvalueS='';
            #if (pValues[idxCoeff]<0.1) localPvalueS='.';
            if (pValues[idxCoeff]<0.05) localPvalueS='*';
            if (pValues[idxCoeff]<0.01) localPvalueS='**';
            if (pValues[idxCoeff]<0.001) localPvalueS='***';
            
            tmpCoV=sprintf(stdFormat,tmpCoV);
            tmpCoV=paste0('T-stat. ',tmpCoV,localPvalueS);
          }
          
          #.MODEL_outputText(outputText=outputText,sprintf("%-22s",''),'( ',tmpCoV,' )\n',sep='');
          .MODEL_outputText(outputText=outputText,sprintf("%-24s",''),tmpCoV,'\n',sep='');
        }
      }
    }
    
    
    
    #durbin watson
    bm_dw=NA;
    if (bm_numObs>1) {
      
      if (thereAreErrorCorr==TRUE)
      {
        bm_vectorY = bm_vectorY_bk
        bm_matrixX = bm_matrixX_bk
        
        bm_vectorY = bm_resP %*% bm_vectorY;
        bm_matrixX = bm_resP %*% bm_matrixX;  
        
        bm_vectorY = bm_vectorY[(1+bm_errorDim):length(bm_vectorY)];
        bm_matrixX = bm_matrixX[(1+bm_errorDim):nrow(bm_matrixX),]; 
        
      }      
      
      bm_tmp_dw=bm_vectorY - bm_matrixX %*% bm_betaHat;
      for (idxdw in 2:length(bm_tmp_dw)) bm_dw[idxdw-1]=bm_tmp_dw[idxdw]-bm_tmp_dw[idxdw-1];
      bm_dw=bm_dw*bm_dw;
      bm_dw=sum(bm_dw)
      bm_dw=bm_dw/bm_ssr
      
      #restore originals
      if (thereAreErrorCorr==TRUE)
      {
        bm_vectorY = bm_vectorY_bk
        bm_matrixX = bm_matrixX_bk
        
        bm_vectorY = bm_vectorY[(1+bm_errorDim):length(bm_vectorY)]
        bm_matrixX = bm_matrixX[(1+bm_errorDim):nrow(bm_matrixX),]  
      }
      
    }
    
    
    
    #log likelihood
    bm_loglike=-(bm_numObs/2)*(1+log(2*pi)+log(bm_ssr/bm_numObs))
    
    #Akaike
    bm_AIC=2*(1+bm_coeffNum-bm_restrictionsNum+bm_errorDim)-2*bm_loglike;
    
    #Schwarz
    bm_BIC=log(bm_numObs)*(1+bm_coeffNum-bm_restrictionsNum+bm_errorDim)-2*bm_loglike;
    
    
    if (thereAreErrorCorr==TRUE)
    {
      .MODEL_outputText(outputText=outputText,'\nERROR STRUCTURE: ',bm_errorRaw,'\n');
      
    }
    
    
    bm_fStat=ifelse(bm_DoF==0,+Inf,(bm_rSquared/(bm_coeffNum-ifelse(bm_ConstTermExist,1,0)+bm_errorDim-bm_restrictionsNum))*
                      ((bm_DoF)/(1-bm_rSquared)));
    
    bm_fProb=ifelse(is.finite(bm_fStat) && bm_DoF>0,1-pf(bm_fStat,bm_coeffNum-ifelse(bm_ConstTermExist,1,0)+bm_errorDim-bm_restrictionsNum,bm_DoF),1)
    
    
    if (thereAreRestrictions && (! is.null(currentBehavioral$restrictRaw)))
    {
      .MODEL_outputText(outputText=outputText,'\nRESTRICTIONS:');      
      .MODEL_outputText(outputText=outputText,'\n')
      .MODEL_outputText(outputText=outputText,paste(strsplit(currentBehavioral$restrictRaw,';')[[1]],collapse='\n'))
      .MODEL_outputText(outputText=outputText,'\n')
    }
    
    if (thereArePDL == TRUE)
    {
      .MODEL_outputText(outputText=outputText,'\nPDL:\n');
      
      
      .MODEL_outputText(outputText=outputText,paste(strsplit(currentBehavioral$pdlRaw,';')[[1]],collapse='\n'));
      tmpCoeff=currentBehavioral$eqCoefficientsNames
      .MODEL_outputText(outputText=outputText,'\n');
      
      for(idxPrintPDL in (which(bm_pdlMatrix[1,]==1)))
      {
        .MODEL_outputText(outputText=outputText,'\n');
        tmpLaggedCoeff=currentBehavioral$eqCoefficientsNamesOriginal[idxPrintPDL]
        .MODEL_outputText(outputText=outputText,'Distributed Lag Coefficient:',tmpLaggedCoeff);
        .MODEL_outputText(outputText=outputText,'\n');
        .MODEL_outputText(outputText=outputText,
                          sprintf(paste0("%-",8,"s"),'Lag'),
                          sprintf(paste0("%-",digits+8,"s"),'Coeff.'),
                          sprintf(paste0("%-",digits+8,"s"),'Std. Error'),
                          sprintf(paste0("%-",digits+8,"s"),'T-stat.'),
                          '\n',sep='');
        
        posCoeffInCoeffArray=which(tmpCoeff==tmpLaggedCoeff);
        
        tmpSum=0;
        for(idxPrintSinglePDL in 0:(bm_pdlMatrix[3,idxPrintPDL]-1))
        { 
          tmpValue=bm_betaHat[posCoeffInCoeffArray+idxPrintSinglePDL];
          tmpSum=tmpSum+tmpValue;
          
          if(idxPrintSinglePDL==0 && bm_pdlMatrix[4,idxPrintPDL]==1)
          {#PDL N so this is 0
            .MODEL_outputText(outputText=outputText,
                              sprintf(paste0("%-",8,"d"),idxPrintSinglePDL),' ',
                              sprintf(paste0("%-",digits+8,"f"),0),
                              sprintf(paste0("%-",digits+8,"s"),'RESTRICT'),
                              sprintf(paste0("%-",digits+8,"s"),'RESTRICT'),'\n',sep='');  
          } else if(idxPrintSinglePDL==(bm_pdlMatrix[3,idxPrintPDL]-1) && bm_pdlMatrix[5,idxPrintPDL]==1)
          {#PDL F so this is 0
            .MODEL_outputText(outputText=outputText,
                              sprintf(paste0("%-",8,"d"),idxPrintSinglePDL),' ',
                              sprintf(paste0("%-",digits+8,"f"),0),
                              sprintf(paste0("%-",digits+8,"s"),'RESTRICT'),
                              sprintf(paste0("%-",digits+8,"s"),'RESTRICT'),'\n',sep=''); 
          } else {
            
            
            localErr=bm_betaHatStdErr[posCoeffInCoeffArray+idxPrintSinglePDL];
            tStat=ifelse(localErr==0,Inf,(tmpValue)/localErr);
            pValue=ifelse(bm_DoF==0,0,2*pt(-abs(tStat),bm_DoF));
            
            localPvalueS='';
            if (pValue<0.05) localPvalueS='*';
            if (pValue<0.01) localPvalueS='**';
            if (pValue<0.001) localPvalueS='***';
            
            tmpSD=sprintf(paste0("% -",digits+8,".",digits,"g"),bm_betaHatStdErr[posCoeffInCoeffArray+idxPrintSinglePDL])
            tmpTstat=sprintf(paste0("% -",digits+8,".",digits,"g"),(tStat))
            
            if (! is.finite(tStat) || abs(tStat)>1e9)
            {
              tmpSD=sprintf(paste0("%-",digits+8,"s"),' RESTRICT');
              tmpTstat=sprintf(paste0("%-",digits+8,"s"),' RESTRICT');
            }
            
            
            .MODEL_outputText(outputText=outputText,
                              sprintf(paste0("%-",8,"d"),idxPrintSinglePDL),
                              sprintf(paste0("% -",digits+8,".",digits,"g"),(tmpValue)),
                              tmpSD,
                              tmpTstat,
                              localPvalueS,
                              '\n',sep='');
          }
        }
        
        #get pdl covariance matrix (subset of whole cov matrix)
        
        tmpCovPdlMatrix=bm_vcov[posCoeffInCoeffArray:(posCoeffInCoeffArray+bm_pdlMatrix[3,idxPrintPDL]-1),
                                posCoeffInCoeffArray:(posCoeffInCoeffArray+bm_pdlMatrix[3,idxPrintPDL]-1)];
        
        
        .MODEL_outputText(outputText=outputText,
                          sprintf(paste0("%-",8,"s"),'SUM'),
                          sprintf(paste0("% -",digits+8,".",digits,"g"),(tmpSum)),
                          sprintf(paste0("% -",digits+8,".",digits,"g"),sqrt(sum(tmpCovPdlMatrix))),'\n',sep='');
      }
    }
    
    if (thereAreRestrictions == TRUE || 
        (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
    {
      .MODEL_outputText(outputText=outputText,'\nRESTRICTIONS F-TEST:\n');
      .MODEL_outputText(outputText=outputText,
                        sprintf(paste0("%-",16+2,"s"),'F-value'),
                        sprintf(paste0(":% -",digits+8,".",digits,"g"),(bm_Ftest_restriction)),'\n');
      .MODEL_outputText(outputText=outputText,
                        sprintf(paste0("%-",16+2,"s"),paste0('F-prob(',bm_restrictionsNum,',',bm_numObs-bm_coeffNum,')')),
                        sprintf(paste0(":% -",digits+8,".",digits,"g"),(bm_Ftest_probability)),'\n');
      
      statsOut$FtestRestrValue=bm_Ftest_restriction;
      statsOut$FtestRestrProbability=bm_Ftest_probability;
      statsOut$FtestRestrDoFs=c(bm_restrictionsNum,bm_numObs-bm_coeffNum);
      
    }
    
    if (thereAreErrorCorr==TRUE)
    {
      .MODEL_outputText(outputText=outputText,'\nAUTOREGRESSIVE PARAMETERS:\n');
      .MODEL_outputText(outputText=outputText,
                        sprintf(paste0("%-",digits+8,"s"),'Rho'),
                        sprintf(paste0("%-",digits+8,"s"),'Std. Error'),
                        sprintf(paste0("%-",digits+8,"s"),'T-stat.'),'\n');
      
      
      tStat=(bm_rhoHat)/bm_COstderr;
      pValue=2*pt(-abs(tStat),bm_DoF);
      
      statsOut$RhosTstatistics=tStat;
      statsOut$RhosPvalues=pValue;
      
      rownames(statsOut$RhosTstatistics)=paste0("RHO_",1:bm_COcoeffNum);
      rownames(statsOut$RhosPvalues)=paste0("RHO_",1:bm_COcoeffNum);
      
      
      for (idxRho in 1:length(bm_rhoHat))
      {
        
        localPvalueS='';
        if (pValue[idxRho]<0.05) localPvalueS='*';
        if (pValue[idxRho]<0.01) localPvalueS='**';
        if (pValue[idxRho]<0.001) localPvalueS='***';
        
        .MODEL_outputText(outputText=outputText,
                          sprintf(paste0("% -",digits+8,".",digits,"g"),(bm_rhoHat[idxRho])),
                          sprintf(paste0("% -",digits+8,".",digits,"g"),bm_COstderr[idxRho]),
                          sprintf(paste0("% -",digits+8,".",digits,"g"),(tStat[idxRho])),
                          localPvalueS,'\n');
      }
    }
    
    
    .MODEL_outputText(outputText=outputText,'\n\nSTATs:');
    .MODEL_outputText(outputText=outputText,'\nR-Squared                      : ',sprintf(stdFormat,bm_rSquared),sep='')
    .MODEL_outputText(outputText=outputText,'\nAdjusted R-Squared             : ',sprintf(stdFormat,bm_adjrSquared),sep='')
    .MODEL_outputText(outputText=outputText,'\nDurbin-Watson Statistic        : ',sprintf(stdFormat,bm_dw),sep='')
    .MODEL_outputText(outputText=outputText,'\nSum of squares of residuals    : ',sprintf(stdFormat,bm_ssr),sep='')
    .MODEL_outputText(outputText=outputText,'\nStandard Error of Regression   : ',sprintf(stdFormat,bm_ser),sep='')
    .MODEL_outputText(outputText=outputText,'\nLog of the Likelihood Function : ',sprintf(stdFormat,bm_loglike),sep='')
    .MODEL_outputText(outputText=outputText,'\nF-statistic                    : ',sprintf(stdFormat,bm_fStat),sep='')
    .MODEL_outputText(outputText=outputText,'\nF-probability                  : ',sprintf(stdFormat,bm_fProb),sep='')
    .MODEL_outputText(outputText=outputText,'\nAkaike\'s IC                    : ',sprintf(stdFormat,bm_AIC),sep='')
    .MODEL_outputText(outputText=outputText,'\nSchwarz\'s IC                   : ',sprintf(stdFormat,bm_BIC),sep='')
    .MODEL_outputText(outputText=outputText,'\nMean of Dependent Variable     : ',sprintf(stdFormat,base::mean(bm_vectorY)),sep='');
    .MODEL_outputText(outputText=outputText,'\nNumber of Observations         : ',bm_numObs,sep='');
    .MODEL_outputText(outputText=outputText,'\nNumber of Degrees of Freedom   : ',bm_DoF,sep='');
    .MODEL_outputText(outputText=outputText,'\nCurrent Sample (year-period)   : ',paste0(localTSRANGE[1],'-',localTSRANGE[2],' / ',localTSRANGE[3],'-',localTSRANGE[4]),sep='');
    
    .MODEL_outputText(outputText=outputText,'\n\n\n');
    .MODEL_outputText(outputText=outputText,'Signif. codes:   *** 0.001  ** 0.01  * 0.05  \n');
    
    .MODEL_outputText(outputText=outputText,'\n');
    .MODEL_outputText(outputText=outputText,'\n');
    
    
    statsOut$DurbinWatson=bm_dw;
    statsOut$SumSquaresResiduals=bm_ssr;
    statsOut$LogLikelihood=bm_loglike;
    statsOut$AIC=bm_AIC;
    statsOut$BIC=bm_BIC;
    statsOut$Fstatistics=bm_fStat;
    statsOut$Fprobability=bm_fProb;
    statsOut$MeanDependentVariable=base::mean(bm_vectorY);
    statsOut$ObservationsCount=bm_numObs;
    
    #export stats
    currentBehavioral$statistics=statsOut;
    
    #save stsuff
    model$behaviorals[[eqList[eqIdx]]]=currentBehavioral;
    
    CP_provided=FALSE;
    
    # CHOW test code ---------------------------------------
    if (CHOWTEST==TRUE)
    {
      
      #get estimation tsrange
      first_estimation_tsrange=currentBehavioral$statistics$TSRANGE
      if (is.null(first_estimation_tsrange)) stop('ESTIMATE(): cannot get base estimation TSRANGE of "',eqList[eqIdx],'" during the CHOW test.');
      
      #check if we have a CHOWPAR
      if (! is.null(CHOWPAR))
      {
        CP_provided=TRUE;
        
        #check chowpar is beyond end of estimation range
        if (NUMPERIOD(c(first_estimation_tsrange[3],first_estimation_tsrange[4]),CHOWPAR,model$frequency)<1) 
          stop('ESTIMATE(): CHOWPAR ',paste(CHOWPAR,collapse='-'),
               ' must be beyond the estimation TSRANGE of "',eqList[eqIdx],'" ',
               paste(first_estimation_tsrange[1:2],collapse='-'),
               ' / ',paste(first_estimation_tsrange[3:4],collapse='-'),'.');
        
        # perform second estimation on extended tsrange
        tryCatch({
           
          second_model=ESTIMATE(model=model
                                ,TSRANGE = c(first_estimation_tsrange[1],first_estimation_tsrange[2],CHOWPAR[1],CHOWPAR[2])
                                ,forceTSRANGE = TRUE
                                ,eqList=eqList[eqIdx]
                                ,quietly=TRUE
                                ,digits=digits
                                ,CHOWTEST=FALSE #this is mandatory
                                ,tol=tol
                                ,IV=IV
                                ,forceIV=forceIV
                                ,centerCOV = centerCOV
                                ,avoidCompliance = TRUE
                                ,...);
          
        },error=function(e){
          stop('ESTIMATE(): error in extended estimation of "',eqList[eqIdx],'", during the CHOW test: ',e$message);
        })
      } else
      {
        #CHOWPAR must be auto calculated
        
        #set CHOWPAR to first year-period outsite TSRANGE
        CHOWPAR=normalizeYP(c(first_estimation_tsrange[3],first_estimation_tsrange[4]+1),f=frequency);
        
        #perform second estimation on extended tsrange
        tryCatch({
          
          second_model=ESTIMATE(model=model
                                ,TSRANGE = c(first_estimation_tsrange[1],first_estimation_tsrange[2],CHOWPAR[1],CHOWPAR[2])
                                ,forceTSRANGE = TRUE
                                ,eqList=eqList[eqIdx]
                                ,quietly=TRUE
                                ,digits=digits
                                ,CHOWTEST=FALSE #this is mandatory
                                ,tol=tol
                                ,IV=IV
                                ,forceIV=forceIV
                                ,centerCOV = centerCOV
                                ,avoidCompliance = TRUE
                                ,...);
          
        },error=function(e){
          stop('ESTIMATE(): cannot extend TSRANGE during CHOW test in estimation of "',eqList[eqIdx],'": ',e$message);
        })
        
        tryCatch({
          
          #cycle until error in extended range
          while(TRUE)
          {
            
            #try incremental chow pars.
            CHOWPAR_tmp=normalizeYP(c(CHOWPAR[1],CHOWPAR[2]+1),f=frequency);
            
            second_model=ESTIMATE(model=model
                                  ,TSRANGE = c(first_estimation_tsrange[1],first_estimation_tsrange[2],CHOWPAR_tmp[1],CHOWPAR_tmp[2])
                                  ,forceTSRANGE = TRUE
                                  ,eqList=eqList[eqIdx]
                                  ,quietly=TRUE
                                  ,digits=digits
                                  ,CHOWTEST=FALSE #this is mandatory
                                  ,avoidCompliance=TRUE
                                  ,tol=tol
                                  ,IV=IV
                                  ,forceIV=forceIV
                                  ,centerCOV = centerCOV
                                  ,...);
            
            #if estimation completes, update the CHOWPAR
            CHOWPAR=CHOWPAR_tmp;
          
          }
        },error=function(e){});
        
      }
      
      baseBehavioral=currentBehavioral;
      extendedBehavioral=second_model$behaviorals[[eqList[eqIdx]]];
      
      #get first SSR and df
      SSR = baseBehavioral$statistics$SumSquaresResiduals;
      df  = baseBehavioral$statistics$DegreesOfFreedom;
      if (!is.finite(SSR)) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, SSR in first estimation of ',eqList[eqIdx],' is not finite.\n'));
      if (!quietly && !is.finite(df)) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, degrees of freedom in first estimation of ',eqList[eqIdx],' are not finite.\n'));
      
      
      #get first SSR and df
      chow_SSR = extendedBehavioral$statistics$SumSquaresResiduals;
      chow_df  = extendedBehavioral$statistics$DegreesOfFreedom;
      if ( !is.finite(SSR)) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, SSR in second estimation of ',eqList[eqIdx],' is not finite.\n'));
      if ( !is.finite(df)) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, degrees of freedom in second estimation of ',eqList[eqIdx],' are not finite.\n'));
      
      if (chow_df==df) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, base and extended estimations of ',eqList[eqIdx],' have the same degrees of freedom\n'));                                     
      
      #out stats
      chowOut=list();
      
      #calc stats.
      Fchow = (chow_SSR/SSR-1)*chow_df/(chow_df-df);
      chowOut$Fvalue=Fchow
      
      DoFchow=c();
      DoFchow[1]=chow_df-df;
      DoFchow[2]=chow_df;
      chowOut$FtestDegreesOfFreedom=DoFchow;
      
      ProbFchow=pf(Fchow,DoFchow[1],DoFchow[2],lower.tail=FALSE);
      chowOut$Fprob=ProbFchow;
      
      #out of sample simulation RESCHECK
      outOfSampleTSRANGE=c(normalizeYP(c(first_estimation_tsrange[3],first_estimation_tsrange[4]+1),f = model$frequency),CHOWPAR);
      predOutSample=NULL;
      tryCatch({
        predOutSample <- SIMULATE(model,
                                  TSRANGE = outOfSampleTSRANGE,
                                  simType = 'RESCHECK',
                                  simIterLimit = 100,
                                  quietly=TRUE,
                                  RESCHECKeqList=eqList[eqIdx],
                                  avoidCompliance=TRUE);
      },error=function(e){
        stop('ESTIMATE(): error in out-of-sample prediction (extended RESCHECK simulation) of "',eqList[eqIdx],'" during CHOW test: ',e$message);
      });
      
      PREDICT =predOutSample$simulation[[eqList[eqIdx]]];
      ACTUAL  =predOutSample$modelData[[eqList[eqIdx]]];
      ERROR   =ACTUAL-PREDICT;
      
      
      #fitted standard error calc
      fit_se = vector(mode="numeric", length=length(PREDICT))
      
      #get matrices and sers
      base_ser       =baseBehavioral$statistics$StandardErrorRegression;
      ext_ser        =extendedBehavioral$statistics$StandardErrorRegression;
      base_matrixX   =baseBehavioral$statistics$matrixX;
      ext_matrixX    =extendedBehavioral$statistics$matrixX;
      base_nobs      =baseBehavioral$statistics$ObservationsCount
      base_matrixXaug=baseBehavioral$statistics$matrixXaug;
      ext_matrixXaug =extendedBehavioral$statistics$matrixXaug;
      
      
      tryCatch({
        #no restrictions on model
        if (is.null(currentBehavioral$matrixR) && is.null(currentBehavioral$pdlRestrictionMatrix))
        {
          #THIS IS CODE DERIVED FROM FORMULA
          for (k in 1:(length(PREDICT))) 
           fit_se[k] = base_ser*sqrt(1+ext_matrixX[base_nobs+k,]%*%solve(t(base_matrixX)%*%base_matrixX)%*%ext_matrixX[base_nobs+k,])
          
          #THIS IS CODE DERIVED FROM SPK RESULTS
          #for (k in 1:(length(PREDICT))) 
          #  fit_se[k] = ext_ser*sqrt(1+ext_matrixX[base_nobs+k,]%*%solve(t(ext_matrixX)%*%ext_matrixX)%*%ext_matrixX[base_nobs+k,])
          
        } else
        {
          coeffCount=baseBehavioral$eqCoefficientsCount;
          
          #THIS IS CODE DERIVED FROM FORMULA
          for (k in 1:(length(PREDICT))) 
             fit_se[k] = base_ser*sqrt(1+ext_matrixX[base_nobs+k,]%*%(solve(base_matrixXaug)[1:coeffCount,1:coeffCount])%*%ext_matrixX[base_nobs+k,])
          
          #THIS IS CODE DERIVED FROM SPK RESULTS
          #for (k in 1:(length(PREDICT))) 
          #  fit_se[k] = ext_ser*sqrt(1+ext_matrixX[base_nobs+k,]%*%(solve(ext_matrixXaug)[1:coeffCount,1:coeffCount])%*%ext_matrixX[base_nobs+k,])
          
        }  
      },error=function(e){
        stop('ESTIMATE(): cannot calculate the standard error of "',eqList[eqIdx],'" in year-period ',
             paste(normalizeYP(c(outOfSampleTSRANGE[1],outOfSampleTSRANGE[2]+k-1),f = model$frequency),collapse = '-'),' during CHOW test: ',e$message);
      });
      
      STDERR=TSERIES(fit_se,START=outOfSampleTSRANGE[1:2],FREQ=model$frequency,avoidCompliance = TRUE);
      TSTAT=ERROR/STDERR;
      
      #project on tsrange
      ACTUAL=TSPROJECT(ACTUAL,TSRANGE=outOfSampleTSRANGE,avoidCompliance = TRUE)
      PREDICT=TSPROJECT(PREDICT,TSRANGE=outOfSampleTSRANGE,avoidCompliance = TRUE)
      ERROR=TSPROJECT(ERROR,TSRANGE=outOfSampleTSRANGE,avoidCompliance = TRUE)
      STDERR=TSPROJECT(STDERR,TSRANGE=outOfSampleTSRANGE,avoidCompliance = TRUE)
      TSTAT=TSPROJECT(TSTAT,TSRANGE=outOfSampleTSRANGE,avoidCompliance = TRUE)
      
      chowOut$PredictivePower=list()
      chowOut$PredictivePower$ACTUAL=ACTUAL;
      chowOut$PredictivePower$PREDICT=PREDICT;
      chowOut$PredictivePower$ERROR=ERROR;
      chowOut$PredictivePower$STDERR=STDERR;
      chowOut$PredictivePower$TSTAT=TSTAT;
      chowOut$TSRANGE=first_estimation_tsrange;
      chowOut$CHOWPAR=CHOWPAR;
      
      #print output
      
      .MODEL_outputText(outputText = !quietly,"\nSTABILITY ANALYSIS:\nBehavioral equation:",eqList[eqIdx],"\n\n");
      .MODEL_outputText(outputText = !quietly,"Chow test:\n");
      .MODEL_outputText(outputText = !quietly,sprintf(paste0("%-",16+2,"s"),ifelse(!CP_provided,'Sample (auto)','Sample')),
                        ":",paste0(outOfSampleTSRANGE[1:2],collapse='-'),'/',paste0(outOfSampleTSRANGE[3:4],collapse='-'),'\n');
      .MODEL_outputText(outputText = !quietly,sprintf(paste0("%-",16+2,"s"),'F-value'),
                        sprintf(paste0(":% -",digits+8,".",digits,"g"),(Fchow)),'\n');
      .MODEL_outputText(outputText = !quietly,
                        sprintf(paste0("%-",16+2,"s"),paste0('F-prob(',DoFchow[1],',',DoFchow[2],')')),
                        sprintf(paste0(":% -",digits+8,".",digits,"g"),(ProbFchow)),'\n');
      #print(Fchow)
      #print(DoFchow)
      #print(ProbFchow);
      #print(outOfSampleTSRANGE);
      
      .MODEL_outputText(outputText = !quietly,"\nPredictive Power:\n");
      
      if (!quietly) 
      {
        #change TABIT column names
        Actual=ACTUAL;
        Predict=PREDICT;
        Error=ERROR;
        `Std. Error`=STDERR;
        `T-stat`=TSTAT;
        #TABIT(ACTUAL, PREDICT, ERROR, STDERR, TSTAT, digits = digits)
        TABIT(Actual, Predict, Error, `Std. Error`, `T-stat`, digits = digits)
      }
      
      #store results
      model$behaviorals[[eqList[eqIdx]]]$ChowTest=chowOut;
      
    }#end CHOW test
    
  }#end requested eqs cycle
  
  
  .MODEL_outputText(outputText=!quietly,'...ESTIMATE OK\n');
  
  return(model);
}


# RENORM code ---------------------------------------

RENORM <- function(model=NULL,
                   TSRANGE=NULL,
                   simType='DYNAMIC',
                   simConvergence=0.01,
                   simIterLimit=100,
                   ZeroErrorAC=FALSE,
                   Exogenize=NULL,
                   ConstantAdjustment=NULL,
                   verbose=FALSE,
                   verboseSincePeriod=0,
                   verboseVars=NULL,
                   renormIterLimit=10,
                   renormConvergence=1e-4,
                   TARGET=NULL,
                   INSTRUMENT=NULL,
                   MM_SHOCK=0.00001,
                   quietly=FALSE,
                   tol=1e-28,
                   avoidCompliance=FALSE,
                   ...)
{
  #reset status
  model$renorm=NULL;
  model$simulation=NULL;
  
  #checks...
  if (is.null(model) ) stop('RENORM(): NULL model.');
  if (is.null(class( model )) || !(inherits( model ,'BIMETS_MODEL')) )
    stop('RENORM(): "model" must be instance of BIMETS_MODEL class.');
  if (! (
    is.numeric(renormIterLimit) &&  (renormIterLimit > 0) 
  ) ) stop('RENORM(): "renormIterLimit" must be a positive number.');
  
  if (is.null(TSRANGE) ) stop('RENORM(): "TSRANGE" must be defined.');
  
  if (! is.finite(tol) || tol<=0) stop('RENORM(): please provide a valid tolerance value.')
  
  if (!(is.logical(quietly)) || is.na(quietly)) stop('RENORM(): "quietly" must be TRUE or FALSE.')
  if (!(is.logical(verbose))) stop('RENORM(): "verbose" must be TRUE or FALSE.')
  
  if (! avoidCompliance)
  {
    #check model data
    tryCatch({
      
      .CHECK_MODEL_DATA(model,showWarnings=verbose,'RENORM(): ');
      
    },error=function(e){stop('RENORM(): ',e$message)});
  } 
  #get data frequecy
  frequency=frequency(model$modelData[[1]]);
  
  #check TSRANGE is consistent
  if (! is.null(TSRANGE)) {
    tryCatch({
      if (! ( is.numeric(TSRANGE) && length(TSRANGE)==4 && 
              .isCompliantYP(c(TSRANGE[1],TSRANGE[2]),frequency) && 
              .isCompliantYP(c(TSRANGE[3],TSRANGE[4]),frequency) &&
              NUMPERIOD(c(TSRANGE[1],TSRANGE[2]),c(TSRANGE[3],TSRANGE[4]),frequency)>=0
      )
      ) stop();  
    },error=function(e) {stop('RENORM(): syntax error in TSRANGE: ',e$message)});
  }
  
  #periods in TSRANGE
  TSRANGEextension=NUMPERIOD(c(TSRANGE[1],TSRANGE[2]),c(TSRANGE[3],TSRANGE[4]),frequency)+1;
  
  #check TARGET 
  if (is.null(TARGET)) stop('RENORM(): "TARGET" must be defined.');
  if (! is.list(TARGET) || length(TARGET)==0) 
    stop(paste0('RENORM(): "TARGET" must be a list built of endogenous variables as names and related time series as items.'))
  
  for (idxT in names(TARGET))
    if (! idxT %in% model$vendog) 
      stop(paste0('RENORM(): TARGET "',idxT,'" is not a model endogenous variable.'))
  
  #check TARGET is consistent in TSRANGE
  for (idxT in 1:length(TARGET))
  {
    
    
    if (! is.bimets(TARGET[[idxT]])) 
      stop('RENORM(): "TARGET" must be a list of BIMETS time series: "',names(TARGET)[idxT],'" time series is not compliant.');
    if (frequency(TARGET[[idxT]])!=frequency) 
      stop('RENORM(): time series have not the same frequency. Check TARGET time series "',names(TARGET)[idxT],'".');
    
    #project TARGET on TSRANGE
    TARGET[[idxT]]=TSPROJECT(TARGET[[idxT]],TSRANGE = TSRANGE,EXTEND = TRUE,avoidCompliance = TRUE);
    
    #deal with missings
    if (any(! is.finite(coredata(TARGET[[idxT]])))) 
      stop('RENORM(): there are undefined values in TSRANGE ',paste0(TSRANGE,collapse='-'),' in TARGET time series "',names(TARGET)[idxT],'"');
    
  }
  
  #check INSTRUMENT 
  if (is.null(INSTRUMENT) || length(INSTRUMENT)==0) 
    stop(paste0('RENORM(): "INSTRUMENT" must be defined.'))
  for (idxT in 1:length(INSTRUMENT))
  {
    #check tragets in vendog list
    if (! INSTRUMENT[idxT] %in% c(model$vexog,model$vendog)) 
      stop(paste0('RENORM(): INSTRUMENT variable "',INSTRUMENT[idxT] ,
                  '" is not a model variable.'));
  }
  
  if (length(INSTRUMENT) != length(TARGET))
    stop(paste0('RENORM(): the targets count must be equal to the instruments count.'));
  
  #check CA vendog exists in model
  if (! is.null(ConstantAdjustment))
  {
    if (! is.list(ConstantAdjustment) ) 
      stop(paste0('RENORM(): "ConstantAdjustment" must be a list built of endogenous variables as names and related time series as items.'))
    
    if (length(ConstantAdjustment) >0) 
      for (idxCA in names(ConstantAdjustment))
      {
        #CA has to be a vendog
        if (! idxCA %in% model$vendog)
          stop(paste0('RENORM(): requested to add a constant adjustment to "',idxCA,'" but it is not a model endogenous variable.'))
        
        
       if (! is.bimets(ConstantAdjustment[[idxCA]])) 
          stop('RENORM(): constant adjustment must be a list of BIMETS time series: "',idxCA,'" time series is not compliant.');
        
        #CA needs same frequency as modelData
        if (frequency(ConstantAdjustment[[idxCA]])!=frequency(model$modelData[[1]])) 
          stop('RENORM(): time series do not have the same frequency. Check constant adjustment time series "',idxCA,'".');
        
        #warning about missings
        if (any(! is.finite(coredata(ConstantAdjustment[[idxCA]])))) 
          .MODEL_outputText(outputText=!quietly,'RENORM(): warning, there are undefined values in constant adjustment of the time series "',idxCA,'"\n',sep='');
        
      }
  }
  
  
  #store original INSTRUMENT, in case we need to restore after error
  INSTRUMENT_ORIGINAL=list();
  INSTRUMENT_CURRENT=list();
  for (idxI in 1:length(INSTRUMENT))
  {
    tmpI=INSTRUMENT[idxI];
    
    if (tmpI %in% model$vexog)
    {
      #instrument is vexog so get data from model$data
      INSTRUMENT_ORIGINAL[[tmpI]]=model$modelData[[tmpI]];
      
    } else if (tmpI %in% model$vendog)
    {
      #instrument is vendog so get data from CA
      if (is.null(ConstantAdjustment[[tmpI]])) 
      {
        #CA does not exist so it is zero... 
        ConstantAdjustment[[tmpI]]=TSERIES(rep(0,TSRANGEextension),START=TSRANGE[1:2],FREQ=frequency);
        INSTRUMENT_ORIGINAL[[tmpI]]=ConstantAdjustment[[tmpI]];
        #stop(paste0('RENORM(): request of endogenous variable "',tmpI,'" as INSTRUMENT but it is not in the ConstantAdjustment list'));
        
      } else 
      {
        INSTRUMENT_ORIGINAL[[tmpI]]=ConstantAdjustment[[tmpI]];
      }
    }
    else stop(paste0('RENORM(): INSTRUMENT variable "', tmpI,
                     '" is not a model exogenous variable.'));
  }
  
  #this is required otherwise R store only values and no ts... ??
  if (! is.list(model$simulation)) model$simulation=list()
  													  
  
  
  #init simulated TARGET with historical values
  for (idxN in names(TARGET))
  {
    #get data from historical
    model$simulation[[idxN]]=TSPROJECT(model$modelData[[idxN]],TSRANGE = TSRANGE,EXTEND = TRUE,avoidCompliance = TRUE);
    
    #stop if missings
    if (any(! is.finite(model$simulation[[idxN]]))) 
       stop(paste0('RENORM(): there are undefined values in TARGET "',idxN,'".'));
  }
  
  #main cycle index
  renormNIter=0;
  
  #get actual TARGETs in TARG_
  xxx_=matrix(nrow=length(TARGET),ncol=TSRANGEextension);
  for (idxR in 1:length(TARGET))
  {
    #already projected
    #xxx_[idxR,]=TSPROJECT(TARGET[[idxR]],TSRANGE=TSRANGE,EXTEND = TRUE,avoidCompliance = TRUE);
    xxx_[idxR,]=coredata(TARGET[[idxR]]);
  }
  TARG_=vector('double',length(xxx_));
  TARG_=c(xxx_);
  
  #used in convergence
  DIFF_=list();
  
  #main cycle
  for (renormNIter in 0:renormIterLimit)
  {
    
    #names on unconverged TARGETs
    unConvergedTARGET=c();
    convergenceRenorm=TRUE;
    
    #check differences in TARGETs
    for (tmpN in names(TARGET))
    {
      DIFF_[[tmpN]]=(TARGET[[tmpN]]-coredata(model$simulation[[tmpN]]))/ifelse(TARGET[[tmpN]]==0,1,TARGET[[tmpN]])
      
      #verify convergence
      if (sqrt(sum(DIFF_[[tmpN]]^2))>=renormConvergence) 
      {
        unConvergedTARGET=c(unConvergedTARGET,tmpN);
        convergenceRenorm=FALSE;
      }
    }
    
    #no convergence
    if (convergenceRenorm==FALSE && renormNIter==renormIterLimit)
    {
      
       .MODEL_outputText(outputText=!quietly,paste0('\nRENORM(): warning, no convergence in ',renormNIter,' iterations.\n'));
     
      #export uncenverged symbols
      model$renorm$unConvergedTARGET=unConvergedTARGET;
      
      break;
      
    }
    
	  if (convergenceRenorm==TRUE && renormNIter==0)
    {
      .MODEL_outputText(outputText=!quietly,paste0('\nRENORM(): all TARGET are equal to model data. This procedure will exit.\n'));
      break;
    }											  
    #convergence
    if (convergenceRenorm==TRUE)
    {
      .MODEL_outputText(outputText=!quietly,paste0('\nConvergence reached in ',renormNIter,' iterations.\n...RENORM OK\n'));
      
      #export results
      tmpL=list();
      for (idxI in 1:length(INSTRUMENT))
      {
        tmpL[[INSTRUMENT[idxI]]]=TSERIES(SINSTR_[idxI,],START=c(TSRANGE[1],TSRANGE[2]),FREQ=frequency,avoidCompliance = TRUE);
      }
      model$renorm$INSTRUMENT=tmpL;
      
      #export data used in convergence (before restore)
      model$renorm$modelData=model$modelData;
      model$renorm$ConstantAdjustment=ConstantAdjustment;
      
      break;
    }
    
     .MODEL_outputText(outputText=!quietly,paste0('\nRENORM(): iteration #',renormNIter+1,'\n'));
    
    
    
    #get current INSTRUMENT
    for (idxI in 1:length(INSTRUMENT))
    {
      tmpI=INSTRUMENT[idxI];
      
      if (tmpI %in% model$vexog)
      {
        #instrument is vexog so get data from model$data
        INSTRUMENT_CURRENT[[tmpI]]=model$modelData[[tmpI]];
        
      } else if (tmpI %in% model$vendog)
      {
        
        INSTRUMENT_CURRENT[[tmpI]]=ConstantAdjustment[[tmpI]];
        
      }
      
      #project in TSRANGE
      INSTRUMENT_CURRENT[[tmpI]]=TSPROJECT(INSTRUMENT_CURRENT[[tmpI]],TSRANGE = TSRANGE,EXTEND = TRUE,avoidCompliance = TRUE);
      xxx_[idxI,]=coredata(INSTRUMENT_CURRENT[[tmpI]]);
    }
    INSTR_=c(xxx_);
    
    tryCatch({
      
      #calc MM
      model=MULTMATRIX(model=model,
                       RENORM=TRUE,
                       TSRANGE=TSRANGE,
                       simType=simType,
                       simConvergence=simConvergence,
                       simIterLimit=simIterLimit,
                       ZeroErrorAC=ZeroErrorAC,
                       Exogenize=Exogenize,
                       ConstantAdjustment=ConstantAdjustment,
                       verbose=verbose,
                       verboseSincePeriod=verboseSincePeriod,
                       verboseVars=verboseVars,
                       TARGET=names(TARGET),
                       INSTRUMENT=INSTRUMENT,
                       MM_SHOCK=MM_SHOCK,
                       quietly=quietly,
                       avoidCompliance=TRUE,
                       ...
                       );
      
    }, error=function(err){
      #cat('\n');
      stop(paste0('\nRENORM(): error in iteration #',renormNIter,': ',err$message));
    })
    
    
    
    #get simulated TARGETs
    for (idxT in 1:length(TARGET))
    {
      #in MULTMAT we have multiple simulation column results, so [,1]
      xxx_[idxT,]=coredata(model$simulation[[names(TARGET)[idxT]]]);
    }
    TARG0_=c((xxx_));
    
    #calc delta in TARGET
    NTARG_ =TARG_-TARG0_;
    
    #get adjusted INSTRUMENTs
    tryCatch({
      #INSTR=INSTR+(MULTMAT**(-1))*NTARG
      INSTR_=as.vector(INSTR_+solve(model$MultiplierMatrix,
                                    tol=tol) %*% NTARG_);
    }, error=function(err){
      #cat('\n');
      stop(paste0('RENORM(): cannot invert multipliers matrix in iteration #',renormNIter,': ',err$message));
    })  
    
    
    SINSTR_=matrix(INSTR_,nrow=length(INSTRUMENT),ncol=TSRANGEextension)
    
    #update adjusted INSTRUMENTs
    for (idxI in 1:length(INSTRUMENT))
    {
      tmpI=INSTRUMENT[idxI];
      
      if (tmpI %in% model$vexog)
      {
        #instrument is vexog so append new data to model$data
        model$modelData[[tmpI]][[TSRANGE[1],TSRANGE[2]]]=SINSTR_[idxI,];
        
      } else if (tmpI %in% model$vendog)
      {
        
        ConstantAdjustment[[tmpI]][[TSRANGE[1],TSRANGE[2]]]=SINSTR_[idxI,];
        
      }
      
    }
    
  }#end main cycle
  
  #restore model data
  for (idxI in 1:length(INSTRUMENT))
  {
    tmpI=INSTRUMENT[idxI];
    
    if (tmpI %in% model$vexog)
    {
      #instrument is vexog so restore data to model$data
      model$modelData[[tmpI]]=INSTRUMENT_ORIGINAL[[tmpI]];
      
    } else if (tmpI %in% model$vendog)
    {
      #instrument is vendog so restore data to CA
      ConstantAdjustment[[tmpI]]=INSTRUMENT_ORIGINAL[[tmpI]];
      
    }
    
  }
  
  
  #export achieved TARGET
  tmpL=list();
  for (idxN in names(TARGET))
  {
	#probably TSPROJECT not required								
    tmpL[[idxN]]=TSPROJECT(model$simulation[[idxN]],TSRANGE = TSRANGE,EXTEND = TRUE,avoidCompliance = TRUE);
  }
  model$renorm$TARGET=tmpL;
  
  #export renorm parameters
  model$renorm[['__RENORM_PARAMETERS__']]$renormIterLimit=renormIterLimit;
  model$renorm[['__RENORM_PARAMETERS__']]$TSRANGE=TSRANGE;
  model$renorm[['__RENORM_PARAMETERS__']]$TARGET=TARGET;
  model$renorm[['__RENORM_PARAMETERS__']]$INSTRUMENT=INSTRUMENT;
  #model$renorm[['__RENORM_PARAMETERS__']]$simType=simType;
  
  
  return(model)
}

# MULTMATRIX code ---------------------------------------

MULTMATRIX <- function(model=NULL,
                       TSRANGE=NULL,
                       simType='DYNAMIC',
                       simConvergence=0.01,
                       simIterLimit=100,
                       ZeroErrorAC=FALSE,
                       Exogenize=NULL,
                       ConstantAdjustment=NULL,
                       verbose=FALSE,
                       verboseSincePeriod=0,
                       verboseVars=NULL,
                       TARGET=NULL,
                       INSTRUMENT=NULL,
                       MM_SHOCK=0.00001,
                       quietly=FALSE,
                       avoidCompliance=FALSE,
                       ...)
{
  tryCatch({
    
    
    model=SIMULATE(model=model,
                   TSRANGE=TSRANGE,
                   simType=simType,
                   simConvergence=simConvergence,
                   simIterLimit=simIterLimit,
                   ZeroErrorAC=ZeroErrorAC,
                   Exogenize=Exogenize,
                   ConstantAdjustment=ConstantAdjustment,
                   verbose=verbose,
                   verboseSincePeriod=verboseSincePeriod,
                   verboseVars=verboseVars,
                   MULTMATRIX=TRUE,
                   TARGET=TARGET,
                   INSTRUMENT=INSTRUMENT,
                   MM_SHOCK=MM_SHOCK,
                   quietly=quietly,
                   avoidCompliance=avoidCompliance,
                   ...);
    
  }, error=function(err){
    #cat('\n');
    stop('\n',err$message)
  })
  return(model)
}

# OPTIMIZE code ---------------------------------------

OPTIMIZE <- function(model=NULL,
                     TSRANGE=NULL,
                     simType='DYNAMIC',
                     simConvergence=0.01,
                     simIterLimit=100,
                     ZeroErrorAC=FALSE,
                     Exogenize=NULL,
                     ConstantAdjustment=NULL,
                     verbose=FALSE,
                     verboseSincePeriod=0,
                     verboseVars=NULL,
                     StochReplica=100,
                     StochSeed=NULL,
                     OptimizeBounds=NULL,
                     OptimizeRestrictions=NULL,
                     OptimizeFunctions=NULL,
                     quietly=FALSE,
                     RESCHECKeqList=NULL,
                     avoidCompliance=FALSE,
                     ...)
{
  
  
  tryCatch({
    
    
    model=SIMULATE(model=model,
                   OPTIMIZE=TRUE,
                   TSRANGE=TSRANGE,
                   simType=simType,
                   simConvergence=simConvergence,
                   simIterLimit=simIterLimit,
                   ZeroErrorAC=ZeroErrorAC,
                   Exogenize=Exogenize,
                   ConstantAdjustment=ConstantAdjustment,
                   verbose=verbose,
                   verboseSincePeriod=verboseSincePeriod,
                   verboseVars=verboseVars,
                   StochReplica=StochReplica,
                   StochSeed=StochSeed,
                   OptimizeBounds=OptimizeBounds,
                   OptimizeRestrictions=OptimizeRestrictions,
                   OptimizeFunctions=OptimizeFunctions,
                   quietly=quietly,
                   RESCHECKeqList=RESCHECKeqList,
                   avoidCompliance=avoidCompliance,
                   ...);
    
  }, error=function(err){
    #cat('\n');
    stop('\n',err$message)
  })
  return(model)
}


# STOCHSIMULATE code ---------------------------------------

STOCHSIMULATE <- function(model=NULL,
                          TSRANGE=NULL,
                          simType='DYNAMIC',
                          simConvergence=0.01,
                          simIterLimit=100,
                          ZeroErrorAC=FALSE,
                          Exogenize=NULL,
                          ConstantAdjustment=NULL,
                          verbose=FALSE,
                          verboseSincePeriod=0,
                          verboseVars=NULL,
                          StochStructure=NULL,
                          StochReplica=100,
                          StochSeed=NULL,
                          quietly=FALSE,
                          RESCHECKeqList=NULL,
                          avoidCompliance=FALSE,
                          ...)
{
  tryCatch({
    
    
    model=SIMULATE(model=model,
                   STOCHSIMULATE=TRUE,
                   TSRANGE=TSRANGE,
                   simType=simType,
                   simConvergence=simConvergence,
                   simIterLimit=simIterLimit,
                   ZeroErrorAC=ZeroErrorAC,
                   Exogenize=Exogenize,
                   ConstantAdjustment=ConstantAdjustment,
                   verbose=verbose,
                   verboseSincePeriod=verboseSincePeriod,
                   verboseVars=verboseVars,
                   StochStructure=StochStructure,
                   StochReplica=StochReplica,
                   StochSeed=StochSeed,
                   quietly=quietly,
                   RESCHECKeqList=RESCHECKeqList,
                   avoidCompliance=avoidCompliance,
                   ...);
    
  }, error=function(err){
    #cat('\n');
    stop('\n',err$message)
  })
  return(model)
}

# SIMULATE code ---------------------------------------

SIMULATE <- function(model=NULL,
                     TSRANGE=NULL,
                     simType='DYNAMIC',
                     simConvergence=0.01,
                     simIterLimit=100,
                     ZeroErrorAC=FALSE,
                     BackFill=0,
                     Exogenize=NULL,
                     ConstantAdjustment=NULL,
                     verbose=FALSE,
                     verboseSincePeriod=0,
                     verboseVars=NULL,
                     MULTMATRIX=FALSE,
                     RENORM=FALSE,
                     TARGET=NULL,
                     INSTRUMENT=NULL,
                     MM_SHOCK=0.00001,
                     STOCHSIMULATE=FALSE,
                     StochStructure=NULL,
                     StochReplica=100,
                     StochSeed=NULL,
                     OPTIMIZE=FALSE,
                     OptimizeBounds=NULL,
                     OptimizeRestrictions=NULL,
                     OptimizeFunctions=NULL,
                     quietly=FALSE,
                     RESCHECKeqList=NULL,
                     avoidCompliance=FALSE,
                     ...)
{ 
   
  
  #get regex definitions
  regExDefs=.RegExGlobalDefinition();
  reservedKeyw=regExDefs$reservedKeyw;
  symOnEqCleaner=regExDefs$symOnEqCleaner;
  symOnIfCleaner=regExDefs$symOnIfCleaner;
  allowedCharOnName=regExDefs$allowedCharOnName;
  charsOnNumbs=regExDefs$charsOnNumbs;
  charsOnNumWithSign=regExDefs$charsOnNumWithSign;
  strongCharOnNumbs=regExDefs$strongCharOnNumbs;
  strongCharOnNumbsWithSign=regExDefs$strongCharOnNumbsWithSign;
  allowedLhsEQfuns=regExDefs$allowedLhsEQfuns;
  allowedLhsEQfunsPub=regExDefs$allowedLhsEQfunsPub;
  
  #parent function call
  callerName='SIMULATE(): '
  
  if (! is.logical(MULTMATRIX) || is.na(MULTMATRIX)) stop('SIMULATE(): "MULTMATRIX" must be logical.');
  if (! is.logical(RENORM) || is.na(RENORM)) stop('SIMULATE(): "RENORM" must be logical.');
  if (! is.logical(STOCHSIMULATE) || is.na(STOCHSIMULATE)) stop('SIMULATE(): "STOCHSIMULATE" must be logical.');
  if (! is.logical(OPTIMIZE) || is.na(OPTIMIZE)) stop('SIMULATE(): "OPTIMIZE" must be logical.');
  
  if (MULTMATRIX)
  {
    if (RENORM)
    { 
      callerName='RENORM(): '
      
    } else 
    {
      callerName='MULTMATRIX(): '
    }
  }
  
  if (STOCHSIMULATE) callerName='STOCHSIMULATE(): '
  if (OPTIMIZE) callerName='OPTIMIZE(): '
    
 
  
  #check arguments
  if (is.null(model) ) stop(callerName,'NULL model.');
  if (is.null(TSRANGE) ) stop(callerName,'"TSRANGE" must be defined.');
  if (is.null(class( model )) || !(inherits( model ,'BIMETS_MODEL')) )
    stop(callerName,'model must be instance of BIMETS_MODEL class.');
  if (! (
    is.numeric(simIterLimit) &&  (simIterLimit > 0) 
  ) ) stop(callerName,'"simIterLimit" must be a positive number.');
  if (! (
    is.numeric(BackFill) &&  (BackFill >= 0) &&  (BackFill %% 1 == 0)
  ) ) stop(callerName,'"BackFill" must be zero or a positive integer number.');
  if (! (
    is.numeric(simConvergence) && (simConvergence > 0) 
  ) ) stop(callerName,'"simConvergence" must be a positive number.');
   if (! is.character(simType) || ! toupper(simType) %in% c('DYNAMIC','STATIC','RESCHECK','FORECAST')) 
    stop(callerName,'"simType" must be DYNAMIC, STATIC, FORECAST or RESCHECK');
	simType=toupper(simType);
  					   
  if (! (is.numeric(MM_SHOCK) && is.finite(MM_SHOCK) && MM_SHOCK>0)) 
    stop(callerName,'"MM_SHOCK" must be numeric and positive.')
  
  if (! is.logical(verbose) || is.na(verbose)) stop(callerName,'"verbose" must be logical.');
  if (! is.logical(quietly) || is.na(quietly)) stop(callerName,'"quietly" must be logical.');
  
   if (MULTMATRIX && simType=='RESCHECK')																			   
    stop(callerName,'a RESCHECK simulation is not allowed in a MULTMATRIX() operation.')
  
  if (! (
    is.numeric(verboseSincePeriod) &&  (verboseSincePeriod >= 0) 
  ) ) stop(callerName,'"verboseSincePeriod" must be a positive number.');
  
  if (MULTMATRIX && STOCHSIMULATE) stop(callerName,'if "STOCHSIMULATE" is TRUE then "MULTMATRIX" must be FALSE.');
	if (OPTIMIZE && STOCHSIMULATE) stop(callerName,'if "STOCHSIMULATE" is TRUE then "OPTIMIZE" must be FALSE.');
	if (MULTMATRIX && OPTIMIZE) stop(callerName,'if "OPTIMIZE" is TRUE then "MULTMATRIX" must be FALSE.');
  if (verbose) quietly=FALSE;
  
  if (! avoidCompliance)
  {
    #check model data
    tryCatch({
      
      .CHECK_MODEL_DATA(model,showWarnings=verbose,callerName);
      
    },error=function(e){stop(callerName,'',e$message)});
  }
  
  #get data frequency
  frequency=frequency(model$modelData[[1]]);
  
  #check TSRANGE is consistent
  if (! is.null(TSRANGE)) {
    tryCatch({
      if (! ( is.numeric(TSRANGE) && length(TSRANGE)==4 && 
              .isCompliantYP(c(TSRANGE[1],TSRANGE[2]),frequency) && 
              .isCompliantYP(c(TSRANGE[3],TSRANGE[4]),frequency) &&
              NUMPERIOD(c(TSRANGE[1],TSRANGE[2]),c(TSRANGE[3],TSRANGE[4]),frequency)>=0
      )
      ) stop();  
    },error=function(e) {stop(callerName,'syntax error in TSRANGE: ',e$message)});
  }
  
  #check exoginanda vendog exists in model
  if (! is.null(Exogenize))
  {
    if (! is.list(Exogenize) ) 
      stop(callerName,'"Exogenize" must be a list built of endogenous variables as names and related TSRANGE as items.')
    
    
    if (length(names(Exogenize)) != length(unique(names(Exogenize)))) stop(callerName,'there are duplicated names in "Exogenize".');
    
    if (length(base::setdiff(names(Exogenize),model$vendog)) >0) 
      stop(callerName,'request to exogenize "',
                  paste0(base::setdiff(names(Exogenize),model$vendog),collapse=', '),
                  '", not endogenous variable(s) of the model.')
    
    
  }
  
  #check CA vendog exists in model
  if (! is.null(ConstantAdjustment))
  {
    if (! is.list(ConstantAdjustment) ) 
      stop(callerName,'"ConstantAdjustment" must be a list built of endogenous variables as names and related time series as items.')
    
    
    
    if (length(base::setdiff(names(ConstantAdjustment),model$vendog)) >0) 
      stop(callerName,'request to add a constant adjustment to "',
                  paste0(base::setdiff(names(ConstantAdjustment),model$vendog),collapse=', '),
                  '", not endogenous variable(s) of the model.')
    
    
  }
  
  
  #check RESCHECKeqList vendog exists in model 
  if (! is.null(RESCHECKeqList))
  {
    if (simType!='RESCHECK')
    {
      .MODEL_outputText(outputText=!quietly,paste0(callerName,'simulation of type "',simType,'" requested. "RESCHECKeqList" will be set to NULL.\n'));
      RESCHECKeqList=NULL;
    } else
    {
	if (! is.character(RESCHECKeqList) ) 
      stop(callerName,'"RESCHECKeqList" must be a character array built of endogenous variables names.')
    
    if (length(base::setdiff((RESCHECKeqList),model$vendog)) >0) 
      stop(callerName,'request to RESCHECK the following "',
                  paste0(base::setdiff((RESCHECKeqList),model$vendog),collapse=', '),
                  '", not endogenous variable(s) of the model.')
    }
  } 
  
  #check (Exogenize) is consistent
  if (length(Exogenize)>0) 
    for (idxEL in 1:length(Exogenize))
    {
      if (is.logical(Exogenize[[idxEL]]) && ! is.na(Exogenize[[idxEL]])) 
      {
        if (Exogenize[[idxEL]]!=TRUE) 
          stop(callerName,'syntax error in Exogenize TSRANGE of "',
                      names(Exogenize)[idxEL],'".')
        
        #set full TSRANGE if Expogenize is TRUE
        Exogenize[[idxEL]]=TSRANGE;
        next;
      }
      
      #check TSRANGE
      tryCatch({
        if (! ( is.numeric(Exogenize[[idxEL]]) && length(Exogenize[[idxEL]])==4 && 
                .isCompliantYP(c(Exogenize[[idxEL]][1],Exogenize[[idxEL]][2]),frequency) && 
                .isCompliantYP(c(Exogenize[[idxEL]][3],Exogenize[[idxEL]][4]),frequency) &&
                NUMPERIOD(c(Exogenize[[idxEL]][1],Exogenize[[idxEL]][2]),
                          c(Exogenize[[idxEL]][3],Exogenize[[idxEL]][4]),frequency)>=0
        )
        ) stop();  
      },error=function(e) {stop(callerName,'syntax error in Exogenize TSRANGE of "',
                                       names(Exogenize)[idxEL],'". ',e$message)
      });
    } 
  
  #sim steps count
  simSteps=NUMPERIOD(TSRANGE[1:2],TSRANGE[3:4],frequency)+1;
  
  
  #check (ConstantAdjustment) is consistent
  if (length(ConstantAdjustment)>0) 
    for (idxCA in 1:length(ConstantAdjustment))
    {
      
      if (! is.bimets(ConstantAdjustment[[idxCA]])) 
        stop(callerName,'"ConstantAdjustment" must be a list of BIMETS time series: "',names(ConstantAdjustment)[idxCA],'" time series is not compliant.');
      if (frequency(ConstantAdjustment[[idxCA]])!=frequency(model$modelData[[1]])) 
        stop(callerName,'time series must have the same frequency. Check constant adjustment time series "',names(ConstantAdjustment)[idxCA],'".');
      if (verbose && any(! is.finite(coredata(ConstantAdjustment[[idxCA]])))) .MODEL_outputText(outputText = !quietly,callerName,'warning, there are undefined values in constant adjustment time series "',names(ConstantAdjustment)[idxCA],'".\n',sep='');
      
    }
  
  #check TARGET if MM is T
  if (MULTMATRIX==TRUE && (is.null(TARGET) || length(TARGET)==0) )
    stop(callerName,'impact multipliers matrix requested but TARGET names are NULL.')
  
  if (MULTMATRIX==TRUE && length(TARGET)>0) 
    if (length(base::setdiff(TARGET,model$vendog))>0)
      stop(callerName,'TARGET variables "',paste0(base::setdiff(TARGET,model$vendog),collapse=', ') ,
                  '" are not endogenous variables of the model.')
  
  #check INSTRUMENT if MM is T
  if (MULTMATRIX==TRUE && (is.null(INSTRUMENT) || length(INSTRUMENT)==0) )
    stop(callerName,'impact multipliers matrix requested but INSTRUMENT names are NULL.')
  
  #vendog variables that are used as instruments
  instrumentalVendogs=NULL;
  
  if (anyDuplicated(INSTRUMENT)>0) stop(callerName,'there are duplicated entries on INSTRUMENT namelist: "',INSTRUMENT[anyDuplicated(INSTRUMENT)],'".')
  if (anyDuplicated(TARGET)>0) stop(callerName,'there are duplicated entries on TARGET namelist: "',TARGET[anyDuplicated(TARGET)],'".')
  
  if (MULTMATRIX==TRUE && length(INSTRUMENT)>0) 
  { 
    
    instrumentalVendogs=base::intersect(INSTRUMENT,model$vendog);
    
    for (idxT in 1:length(INSTRUMENT))
    {
      
      #if instrument is endogenous then we use addfactor
      if (INSTRUMENT[idxT] %in% model$vendog) 
      {
        INSTRUMENT[idxT]=paste0(INSTRUMENT[idxT],'__ADDFACTOR');
        next;
      }
      
      #check tragets in vendog list
      if (! INSTRUMENT[idxT] %in% model$vexog) stop(callerName,'INSTRUMENT variable "',INSTRUMENT[idxT] ,
                                                           '" is not a model variable.')
    }
  }
  
  #set replica if MULTMATRIX selected
  if (MULTMATRIX==TRUE) 
  {
    replica=length(INSTRUMENT)*simSteps+1;
  } else replica=1;
  
  #check intersection between Simulation TSRANGE and Exogenize TSRANGE
  #remove items whose exogination TSRANGE is outside Simulation TSRANGE
  idxToBeRemoved=c();
  if (length(Exogenize)>0) 
    for (idxEL in 1:length(Exogenize))
    {
      if (NUMPERIOD(c(Exogenize[[idxEL]][1],Exogenize[[idxEL]][2]),
                    c(TSRANGE[1],TSRANGE[2]),frequency)>0)
      {
        
        if (NUMPERIOD(c(Exogenize[[idxEL]][3],Exogenize[[idxEL]][4]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxEL);
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, exogenize TSRANGE of "',names(Exogenize)[idxEL],'" does not intersect with simulation TSRANGE.\n'));
          
        } 
        
      }
      
      if (NUMPERIOD(c(Exogenize[[idxEL]][3],Exogenize[[idxEL]][4]),
                    c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
      {
        
        if (NUMPERIOD(c(Exogenize[[idxEL]][1],Exogenize[[idxEL]][2]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxEL);
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, exogenize TSRANGE of "',names(Exogenize)[idxEL],'" does not intersect with simulation TSRANGE.\n'));
          
        }
      }
    }
  
  if (length(idxToBeRemoved)>0) Exogenize=Exogenize[-idxToBeRemoved]
  
  if (length(Exogenize)>0) 
    for (idxEL in 1:length(Exogenize))
    {
      if (NUMPERIOD(c(Exogenize[[idxEL]][1],Exogenize[[idxEL]][2]),
                    c(TSRANGE[1],TSRANGE[2]),frequency)>0)
      {
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, exogenize TSRANGE of "',names(Exogenize)[idxEL],'" starts before the simulation TSRANGE.\n'));
        Exogenize[[idxEL]][1]=TSRANGE[1];
        Exogenize[[idxEL]][2]=TSRANGE[2];
      }
      
      if (NUMPERIOD(c(Exogenize[[idxEL]][3],Exogenize[[idxEL]][4]),
                    c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
      {
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, exogenize TSRANGE of "',names(Exogenize)[idxEL],'" ends after the simulation TSRANGE.\n'));
        Exogenize[[idxEL]][3]=TSRANGE[3];
        Exogenize[[idxEL]][4]=TSRANGE[4];
      }
    }
  

  
   
  # check STOCHSIMULATE ------------------------------------------------------------
  if (STOCHSIMULATE)
  {
    
    #check arguments
    if (!is.list(StochStructure) || is.null(names(StochStructure))) stop(callerName,'"StochStructure" must be a named list.');
    if (length(names(StochStructure)) != length(unique(names(StochStructure)))) stop(callerName,'there are duplicated names in "StochStructure"');
   
    for (idxN in names(StochStructure))
    {
      if (! idxN %in% c(model$vendog,model$vexog))
        stop(callerName,'"StochStructure" has a named element "',idxN,'" that is not a model variable.')
      
      if (! is.list(StochStructure[[idxN]]) 
          || is.null(names(StochStructure[[idxN]])) 
          || length(StochStructure[[idxN]])!=3
          || all(sort(names(StochStructure[[idxN]])) != c('PARS','TSRANGE','TYPE')))
        stop(callerName,'"StochStructure$',idxN,'" must be a named list built of the following 3 components: TSRANGE, TYPE, PARS.')
      
      
      if (is.logical(StochStructure[[idxN]]$TSRANGE) && ! is.na(StochStructure[[idxN]]$TSRANGE)) 
      {
        if (StochStructure[[idxN]]$TSRANGE!=TRUE) 
          stop(callerName,'"StochStructure$',idxN,'$TSRANGE" must be TRUE or a 4 element integer array.')
        
        #set full TSRANGE if StochStructure$idxN$TSRANGE is TRUE
        StochStructure[[idxN]]$TSRANGE=TSRANGE;
        
      } else
      { #check TSRANGE
        tryCatch({
          if (! ( is.numeric(StochStructure[[idxN]]$TSRANGE) && length(StochStructure[[idxN]]$TSRANGE)==4 && 
                  .isCompliantYP(c(StochStructure[[idxN]]$TSRANGE[1],StochStructure[[idxN]]$TSRANGE[2]),frequency) && 
                  .isCompliantYP(c(StochStructure[[idxN]]$TSRANGE[3],StochStructure[[idxN]]$TSRANGE[4]),frequency) &&
                  NUMPERIOD(c(StochStructure[[idxN]]$TSRANGE[1],StochStructure[[idxN]]$TSRANGE[2]),
                            c(StochStructure[[idxN]]$TSRANGE[3],StochStructure[[idxN]]$TSRANGE[4]),frequency)>=0
          )
          ) stop();  
        },error=function(e) {stop(callerName,'syntax error in "StochStructure$',idxN,'$TSRANGE". ',e$message)
        });
        
      }
    }
    
    idxToBeRemoved=c();
    for (idxN in 1:length(StochStructure))
    {
        elemName=names(StochStructure)[idxN];
       
        #check intersection between StochStructure TSRANGE and Simulation TSRANGE
        #remove items whose TSRANGE is outside Simulation TSRANGE
            if (NUMPERIOD(c(StochStructure[[idxN]]$TSRANGE[1],StochStructure[[idxN]]$TSRANGE[2]),
                          c(TSRANGE[1],TSRANGE[2]),frequency)>0)
            {
              
              if (NUMPERIOD(c(StochStructure[[idxN]]$TSRANGE[3],StochStructure[[idxN]]$TSRANGE[4]),
                            c(TSRANGE[1],TSRANGE[2]),frequency)>0)
              {
                idxToBeRemoved=c(idxToBeRemoved,idxN);
                .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "StochStructure$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n'));
                
              } 
              
            }
            
            if (NUMPERIOD(c(StochStructure[[idxN]]$TSRANGE[3],StochStructure[[idxN]]$TSRANGE[4]),
                          c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
            {
              
              if (NUMPERIOD(c(StochStructure[[idxN]]$TSRANGE[1],StochStructure[[idxN]]$TSRANGE[2]),
                            c(TSRANGE[3],TSRANGE[4]),frequency)<0)
              {
                idxToBeRemoved=c(idxToBeRemoved,idxN);
                .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "StochStructure$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n'));
                
              }
            }
          
        
      
    }
    
    if (length(idxToBeRemoved)>0) StochStructure=StochStructure[-idxToBeRemoved]
    
    if (length(StochStructure)>0) 
    {  for (idxN in 1:length(StochStructure))
      {
        if (NUMPERIOD(c(StochStructure[[idxN]]$TSRANGE[1],StochStructure[[idxN]]$TSRANGE[2]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "StochStructure$',names(StochStructure)[idxN],'$TSRANGE" starts before the simulation TSRANGE\n'));
          StochStructure[[idxN]]$TSRANGE[1]=TSRANGE[1];
          StochStructure[[idxN]]$TSRANGE[2]=TSRANGE[2];
        }
        
        if (NUMPERIOD(c(StochStructure[[idxN]]$TSRANGE[3],StochStructure[[idxN]]$TSRANGE[4]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "StochStructure$',names(StochStructure)[idxN],'$TSRANGE" ends after the simulation TSRANGE\n'));
          StochStructure[[idxN]]$TSRANGE[3]=TSRANGE[3];
          StochStructure[[idxN]]$TSRANGE[4]=TSRANGE[4];
        }
      }
    
      for (idxN in names(StochStructure))
      {
        if (! is.character(StochStructure[[idxN]]$TYPE) 
            || ! StochStructure[[idxN]]$TYPE %in% c('NORM','UNIF')
        ) stop(callerName,'"StochStructure$',idxN,'$TYPE" must be NORM or UNIF.')
        
        if (StochStructure[[idxN]]$TYPE=='UNIF'
            || StochStructure[[idxN]]$TYPE=='NORM')
        {
          if (any(is.null(StochStructure[[idxN]]$PARS)) 
              || any(! is.finite(StochStructure[[idxN]]$PARS))
              || length(StochStructure[[idxN]]$PARS) !=2) 
            stop(callerName,'"StochStructure$',idxN,'$PARS" must be a 2 elements finite numerical array.')
        }
      }
    
    }
    
    if (is.null(StochReplica) 
        || ! is.finite(StochReplica)
        || StochReplica < 1
        || StochReplica %% 1 != 0 )
      stop(callerName,'"StochReplica" must be a positive integer.')
    
    if (! is.null(StochSeed))
    {
      if(! is.finite(StochSeed))
      stop(callerName,'"StochSeed" must be a finite number.')
      
      #set provided seed into the random generator
      set.seed(StochSeed);
       
    }
    
    #set replica if STOCHSIMULATE selected
    replica=StochReplica+1;
    
  }#end if STOCHSIMULATE
  
 
  
  
  
  
  
  #a local env will contains model time series in assign(), get() and eval()
  localE = new.env();
  
  #we need at least 1 lag in order to start forecast 
  model_max_lag=max(model$max_lag,1);
  
  #extend TSRANGE by max lag required by model
  newStart=normalizeYP(c(TSRANGE[1],TSRANGE[2]-model_max_lag),frequency);
  EXTENDED_TSRANGE=TSRANGE;
  EXTENDED_TSRANGE[1]=newStart[1];
  EXTENDED_TSRANGE[2]=newStart[2];
  
  extSimSteps=NUMPERIOD(EXTENDED_TSRANGE[1:2],EXTENDED_TSRANGE[3:4],frequency)+1;
  
  
  # check coefficients exist -----------------------------------------
  
  #create sublists of requested RESCHECK
  RESCHECK_behaviorals=NULL;
  RESCHECK_identities=NULL;
  
  if (!is.null(RESCHECKeqList)) 
  {
    RESCHECK_behaviorals=base::intersect(RESCHECKeqList,names(model$behaviorals));
    RESCHECK_identities =base::intersect(RESCHECKeqList,names(model$identities));
    
    #empty intersect returns character(0) we need a NULL
    if (length(RESCHECK_behaviorals)==0) RESCHECK_behaviorals=NULL;
    if (length(RESCHECK_identities)==0)  RESCHECK_identities=NULL;
    
  }
  
  #check behavioral coefficients
  if (length(model$behaviorals)>0) 
  {  if (is.null(RESCHECKeqList)) 
    { #user requested a full simulation
      for (idx in 1:length(model$behaviorals)) 
      {
        #check behavioral has coefficients defined and consistent
        if (
          (length(model$behaviorals[[idx]]$coefficients) != length(model$behaviorals[[idx]]$eqCoefficientsNames)) ||
          any( !is.finite(model$behaviorals[[idx]]$coefficients))
        )
        {
          stop(callerName,'please check coefficients definition in behavioral "', 
                      names(model$behaviorals)[[idx]],'" #',idx,' or rerun ESTIMATE().')
        }    
      }
    } else
    {
      #user requested a selection of endogenous rescheck
      if (length(RESCHECK_behaviorals)>0)
        for (idx in 1:length(RESCHECK_behaviorals))
        {
          #check behavioral has coefficients defined and consistent
          if (
            (length(model$behaviorals[[RESCHECK_behaviorals[idx]]]$coefficients) != length(model$behaviorals[[RESCHECK_behaviorals[idx]]]$eqCoefficientsNames)) ||
            any( !is.finite(model$behaviorals[[RESCHECK_behaviorals[idx]]]$coefficients))
          )
          {
            stop(callerName,'please check coefficients definition in behavioral "', 
                        RESCHECK_behaviorals[idx],'" or rerun ESTIMATE().')
          }    
        }
    }
  }
  
  
  #use local var for speedup
  model_vendog=model$vendog;
  model_vexog=model$vexog;
  model_names_behavioral=names(model$behaviorals);
  model_names_identity=names(model$identities);
  model_fullComponentList=c(model$vendog,model$vexog);
  
 if (! is.null(verboseVars) &&
      (! is.character(verboseVars) || ! all(verboseVars %in% model_fullComponentList)))
      stop(callerName,'"verboseVars" must be a character array built of model variables names.')
  
  
  
  #user requested a selection of endogenous rescheck
  if (!is.null(RESCHECKeqList))
  {
    model_fullComponentList=c()
    
    
    if (length(RESCHECK_behaviorals)>0)
      for (idx in 1:length(RESCHECK_behaviorals))
      {
        #add local components to list of variables to be proxied
        model_fullComponentList=c(model_fullComponentList,model$behaviorals[[RESCHECK_behaviorals[idx]]]$eqComponentsNames)
      }
    
    if (length(RESCHECK_identities)>0)
      for (idx in 1:length(RESCHECK_identities))
      {
        #add local components to list of variables to be proxied
        model_fullComponentList=c(model_fullComponentList,model$identities[[RESCHECK_identities[idx]]]$eqComponentsNames)
      }
    
    model_vendog=base::intersect(model$vendog,model_fullComponentList);
    model_vexog=base::intersect(model$vexog,model_fullComponentList);
    model_names_behavioral=base::intersect(names(model$behaviorals),RESCHECK_behaviorals);
    model_names_identity=base::intersect(names(model$identities),RESCHECK_identities);
	if (is.null(verboseVars))
    {
      verboseVars=model_fullComponentList;
    } else {
      verboseVars=base::intersect(verboseVars,model_fullComponentList);
      
      if (length(verboseVars)==0) 
      {
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "verboseVars" is not coherent and will be set equal to "RESCHECKeqList".\n'));
        verboseVars=model_fullComponentList;
      }
    }
  }
  
  #forecast does not propagate vendog with IF condition... ??
  #model_vendog_forecast contains all vendog but those with IF>
  model_vendog_forecast=c();
  for (idx in 1:length(model_vendog))
  {
    
    if (model_vendog[idx] %in% names(model$identities))
    {
      if (model$identities[[model_vendog[idx]]]$hasIF==FALSE)
      {
        #no IF so add...
        model_vendog_forecast=c(model_vendog_forecast,model_vendog[idx]);
      }
    }
    else
    {
      #we are in behaviorals so no IF allowed
      model_vendog_forecast=c(model_vendog_forecast,model_vendog[idx]);
    }
  }
  
  #get exogenized vendog with IF condition (vendog_forecast UNION vendog_exogenized = vendog)
  model_vendog_exogenized = base::setdiff(model_vendog,model_vendog_forecast);
  
  
  # check OPTIMIZE ------------------------------------------------------------
  if (OPTIMIZE)
  {
    
    if (is.null(StochReplica) 
        || ! is.finite(StochReplica)
        || StochReplica < 1
        || StochReplica %% 1 != 0 )
      stop(callerName,'"StochReplica" must be a positive integer.')
    
    if (! is.null(StochSeed))
    {
      if(! is.finite(StochSeed))
        stop(callerName,'"StochSeed" must be a finite number.')
      
      #set provided seed into the random generator
      set.seed(StochSeed);
      
    }
    
    #set replica if OPTIMIZE selected
    replica=StochReplica+1;
    
    
    #check argument OptimizeBounds
    if (!is.list(OptimizeBounds) || is.null(names(OptimizeBounds))) stop(callerName,'"OptimizeBounds" must be a named list.');
    if (length(names(OptimizeBounds)) != length(unique(names(OptimizeBounds)))) stop(callerName,'there are duplicated names in "OptimizeBounds".');
    
    for (idxN in names(OptimizeBounds))
    {
      if (! idxN %in% c(model$vendog,model$vexog))
        stop(callerName,'"OptimizeBounds" has a named element "',idxN,'" that is not a model variable.')
      
      if (! is.list(OptimizeBounds[[idxN]]) 
          || is.null(names(OptimizeBounds[[idxN]])) 
          || length(OptimizeBounds[[idxN]])!=2
          || all(sort(names(OptimizeBounds[[idxN]])) != c('BOUNDS','TSRANGE')))
        stop(callerName,'"OptimizeBounds$',idxN,'" must be a named list built of the following 2 components: TSRANGE, BOUNDS')
      
      
      if (is.logical(OptimizeBounds[[idxN]]$TSRANGE) && ! is.na(OptimizeBounds[[idxN]]$TSRANGE)) 
      {
        if (OptimizeBounds[[idxN]]$TSRANGE!=TRUE) 
          stop(callerName,'"OptimizeBounds$',idxN,'$TSRANGE" must be TRUE or a 4 element integer array.')
        
        #set full TSRANGE if OptimizeBounds$idxN$TSRANGE is TRUE
        OptimizeBounds[[idxN]]$TSRANGE=TSRANGE;
        
      } else
      { #check TSRANGE
        tryCatch({
          if (! ( is.numeric(OptimizeBounds[[idxN]]$TSRANGE) && length(OptimizeBounds[[idxN]]$TSRANGE)==4 && 
                  .isCompliantYP(c(OptimizeBounds[[idxN]]$TSRANGE[1],OptimizeBounds[[idxN]]$TSRANGE[2]),frequency) && 
                  .isCompliantYP(c(OptimizeBounds[[idxN]]$TSRANGE[3],OptimizeBounds[[idxN]]$TSRANGE[4]),frequency) &&
                  NUMPERIOD(c(OptimizeBounds[[idxN]]$TSRANGE[1],OptimizeBounds[[idxN]]$TSRANGE[2]),
                            c(OptimizeBounds[[idxN]]$TSRANGE[3],OptimizeBounds[[idxN]]$TSRANGE[4]),frequency)>=0
          )
          ) stop();  
        },error=function(e) {stop(callerName,'syntax error in "OptimizeBounds$',idxN,'$TSRANGE". ',e$message)
        });
        
      }
    }
    
    idxToBeRemoved=c();
    for (idxN in 1:length(OptimizeBounds))
    {
      elemName=names(OptimizeBounds)[idxN];
      
      #check intersection between OptimizeBounds TSRANGE and Simulation TSRANGE
      #remove items whose TSRANGE is outside Simulation TSRANGE
      if (NUMPERIOD(c(OptimizeBounds[[idxN]]$TSRANGE[1],OptimizeBounds[[idxN]]$TSRANGE[2]),
                    c(TSRANGE[1],TSRANGE[2]),frequency)>0)
      {
        
        if (NUMPERIOD(c(OptimizeBounds[[idxN]]$TSRANGE[3],OptimizeBounds[[idxN]]$TSRANGE[4]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxN);
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeBounds$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n'));
          
        } 
        
      }
      
      if (NUMPERIOD(c(OptimizeBounds[[idxN]]$TSRANGE[3],OptimizeBounds[[idxN]]$TSRANGE[4]),
                    c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
      {
        
        if (NUMPERIOD(c(OptimizeBounds[[idxN]]$TSRANGE[1],OptimizeBounds[[idxN]]$TSRANGE[2]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxN);
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeBounds$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n'));
          
        }
      }
      
    }
    
    if (length(idxToBeRemoved)>0) OptimizeBounds=OptimizeBounds[-idxToBeRemoved]
    
    if (length(OptimizeBounds)>0) 
    {  
      for (idxN in 1:length(OptimizeBounds))
      {
        if (NUMPERIOD(c(OptimizeBounds[[idxN]]$TSRANGE[1],OptimizeBounds[[idxN]]$TSRANGE[2]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeBounds$',names(OptimizeBounds)[idxN],'$TSRANGE" starts before the simulation TSRANGE.\n'));
          OptimizeBounds[[idxN]]$TSRANGE[1]=TSRANGE[1];
          OptimizeBounds[[idxN]]$TSRANGE[2]=TSRANGE[2];
        }
        
        if (NUMPERIOD(c(OptimizeBounds[[idxN]]$TSRANGE[3],OptimizeBounds[[idxN]]$TSRANGE[4]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeBounds$',names(OptimizeBounds)[idxN],'$TSRANGE" ends after the simulation TSRANGE.\n'));
          OptimizeBounds[[idxN]]$TSRANGE[3]=TSRANGE[3];
          OptimizeBounds[[idxN]]$TSRANGE[4]=TSRANGE[4];
        }
      }
      
      for (idxN in names(OptimizeBounds))
      {
        
        if (any(is.null(OptimizeBounds[[idxN]]$BOUNDS)) 
            || any(! is.finite(OptimizeBounds[[idxN]]$BOUNDS))
            || length(OptimizeBounds[[idxN]]$BOUNDS) !=2) 
          stop(callerName,'"OptimizeBounds$',idxN,'$BOUNDS" must be a 2 elements finite numerical array.')
        
        if (OptimizeBounds[[idxN]]$BOUNDS[1] >= OptimizeBounds[[idxN]]$BOUNDS[2]) 
          stop(callerName,'"OptimizeBounds$',idxN,'$BOUNDS[2]" must be greater than "OptimizeBounds$',idxN,'$BOUNDS[1]".')
        
        
      }
      
    } else {
      stop(callerName,'"OptimizeBounds" are null in the simulation TSRANGE.')
    }
    
    # end check OptimizeBounds
    
    
    
    #check argument OptimizeRestrictions
    if (! is.null(OptimizeRestrictions))
    {
      if (!is.list(OptimizeRestrictions)) stop(callerName,'"OptimizeRestrictions" must be a named list.');
      if (length(names(OptimizeRestrictions)) != length(unique(names(OptimizeRestrictions)))) stop(callerName,'there are duplicated names in "OptimizeRestrictions".');
      
      for (idxN in names(OptimizeRestrictions))
      {
        
        if (! is.list(OptimizeRestrictions[[idxN]]) 
            || is.null(names(OptimizeRestrictions[[idxN]])) 
            || length(OptimizeRestrictions[[idxN]])!=2
            || all(sort(names(OptimizeRestrictions[[idxN]])) != c('INEQUALITY','TSRANGE')))
          stop(callerName,'"OptimizeRestrictions$',idxN,'" must be a named list built of the following 2 components: TSRANGE, INEQUALITY.')
        
        
        if (is.logical(OptimizeRestrictions[[idxN]]$TSRANGE) && ! is.na(OptimizeRestrictions[[idxN]]$TSRANGE)) 
        {
          if (OptimizeRestrictions[[idxN]]$TSRANGE!=TRUE) 
            stop(callerName,'"OptimizeRestrictions$',idxN,'$TSRANGE" must be TRUE or a 4 element integer array.')
          
          #set full TSRANGE if OptimizeRestrictions$idxN$TSRANGE is TRUE
          OptimizeRestrictions[[idxN]]$TSRANGE=TSRANGE;
          
        } else
        { #check TSRANGE
          tryCatch({
            if (! ( is.numeric(OptimizeRestrictions[[idxN]]$TSRANGE) && length(OptimizeRestrictions[[idxN]]$TSRANGE)==4 && 
                    .isCompliantYP(c(OptimizeRestrictions[[idxN]]$TSRANGE[1],OptimizeRestrictions[[idxN]]$TSRANGE[2]),frequency) && 
                    .isCompliantYP(c(OptimizeRestrictions[[idxN]]$TSRANGE[3],OptimizeRestrictions[[idxN]]$TSRANGE[4]),frequency) &&
                    NUMPERIOD(c(OptimizeRestrictions[[idxN]]$TSRANGE[1],OptimizeRestrictions[[idxN]]$TSRANGE[2]),
                              c(OptimizeRestrictions[[idxN]]$TSRANGE[3],OptimizeRestrictions[[idxN]]$TSRANGE[4]),frequency)>=0
            )
            ) stop();  
          },error=function(e) {stop(callerName,'syntax error in "OptimizeRestrictions$',idxN,'$TSRANGE". ',e$message)
          });
          
        }
      }
      
      idxToBeRemoved=c();
      for (idxN in 1:length(OptimizeRestrictions))
      {
        elemName=names(OptimizeRestrictions)[idxN];
        
        #check intersection between OptimizeRestrictions TSRANGE and Simulation TSRANGE
        #remove items whose TSRANGE is outside Simulation TSRANGE
        if (NUMPERIOD(c(OptimizeRestrictions[[idxN]]$TSRANGE[1],OptimizeRestrictions[[idxN]]$TSRANGE[2]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          
          if (NUMPERIOD(c(OptimizeRestrictions[[idxN]]$TSRANGE[3],OptimizeRestrictions[[idxN]]$TSRANGE[4]),
                        c(TSRANGE[1],TSRANGE[2]),frequency)>0)
          {
            idxToBeRemoved=c(idxToBeRemoved,idxN);
            .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeRestrictions$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n'));
            
          } 
          
        }
        
        if (NUMPERIOD(c(OptimizeRestrictions[[idxN]]$TSRANGE[3],OptimizeRestrictions[[idxN]]$TSRANGE[4]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
        {
          
          if (NUMPERIOD(c(OptimizeRestrictions[[idxN]]$TSRANGE[1],OptimizeRestrictions[[idxN]]$TSRANGE[2]),
                        c(TSRANGE[3],TSRANGE[4]),frequency)<0)
          {
            idxToBeRemoved=c(idxToBeRemoved,idxN);
            .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeRestrictions$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n'));
            
          }
        }
        
      }
      
      if (length(idxToBeRemoved)>0) OptimizeRestrictions=OptimizeRestrictions[-idxToBeRemoved]
      
      if (length(OptimizeRestrictions)>0) 
      {  
        for (idxN in 1:length(OptimizeRestrictions))
        {
          if (NUMPERIOD(c(OptimizeRestrictions[[idxN]]$TSRANGE[1],OptimizeRestrictions[[idxN]]$TSRANGE[2]),
                        c(TSRANGE[1],TSRANGE[2]),frequency)>0)
          {
            .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeRestrictions$',names(OptimizeRestrictions)[idxN],'$TSRANGE" starts before the simulation TSRANGE.\n'));
            OptimizeRestrictions[[idxN]]$TSRANGE[1]=TSRANGE[1];
            OptimizeRestrictions[[idxN]]$TSRANGE[2]=TSRANGE[2];
          }
          
          if (NUMPERIOD(c(OptimizeRestrictions[[idxN]]$TSRANGE[3],OptimizeRestrictions[[idxN]]$TSRANGE[4]),
                        c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
          {
            .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeRestrictions$',names(OptimizeRestrictions)[idxN],'$TSRANGE" ends after the simulation TSRANGE.\n'));
            OptimizeRestrictions[[idxN]]$TSRANGE[3]=TSRANGE[3];
            OptimizeRestrictions[[idxN]]$TSRANGE[4]=TSRANGE[4];
          }
        }
        
        #check there's no overlapping in Restrictions TSRANGEs
        if (length(OptimizeRestrictions)>1)
        {
          
          for (idxN in 1:(length(OptimizeRestrictions)-1))
          {
            for (idxM in (idxN+1):length(OptimizeRestrictions))
            {
              
              if (NUMPERIOD(c(OptimizeRestrictions[[idxN]]$TSRANGE[3],OptimizeRestrictions[[idxN]]$TSRANGE[4]),
                            c(OptimizeRestrictions[[idxM]]$TSRANGE[1],OptimizeRestrictions[[idxM]]$TSRANGE[2])
                            ,frequency)<1
                  &&
                  NUMPERIOD(c(OptimizeRestrictions[[idxN]]$TSRANGE[3],OptimizeRestrictions[[idxN]]$TSRANGE[4]),
                            c(OptimizeRestrictions[[idxM]]$TSRANGE[3],OptimizeRestrictions[[idxM]]$TSRANGE[4])
                            ,frequency)>-1)
                stop(callerName,'"OptimizeRestrictions$',names(OptimizeRestrictions)[idxN],'$TSRANGE" overlaps with "OptimizeRestrictions$',names(OptimizeRestrictions)[idxM],'$TSRANGE".')
              
              if (NUMPERIOD(c(OptimizeRestrictions[[idxN]]$TSRANGE[1],OptimizeRestrictions[[idxN]]$TSRANGE[2]),
                            c(OptimizeRestrictions[[idxM]]$TSRANGE[1],OptimizeRestrictions[[idxM]]$TSRANGE[2])
                            ,frequency)<1
                  &&
                  NUMPERIOD(c(OptimizeRestrictions[[idxN]]$TSRANGE[1],OptimizeRestrictions[[idxN]]$TSRANGE[2]),
                            c(OptimizeRestrictions[[idxM]]$TSRANGE[3],OptimizeRestrictions[[idxM]]$TSRANGE[4])
                            ,frequency)>-1)
                stop(callerName,'"OptimizeRestrictions$',names(OptimizeRestrictions)[idxN],'$TSRANGE" overlaps with "OptimizeRestrictions$',names(OptimizeRestrictions)[idxM],'$TSRANGE".')
              
            }
          }
        }
        
        for (idxN in names(OptimizeRestrictions))
        {
          
          if (any(is.null(OptimizeRestrictions[[idxN]]$INEQUALITY)) 
              || any(! is.character(OptimizeRestrictions[[idxN]]$INEQUALITY))
              || length(OptimizeRestrictions[[idxN]]$INEQUALITY) !=1) 
            stop(callerName,'"OptimizeRestrictions$',idxN,'$INEQUALITY" must be a string.')
          
          #transform inequality raw text into suitable expression
          outIE=OptimizeRestrictions[[idxN]]$INEQUALITY
          
          #check double logical operator
          if (grepl('&&',outIE)) stop(callerName,'INEQUALITY definition "',outIE,'" in "OptimizeRestrictions$',idxN,'" cannot contain "&&". Please consider using the single operator "&".');
          if (grepl('\\|\\|',outIE)) stop(callerName,'INEQUALITY definition "',outIE,'" in "OptimizeRestrictions$',idxN,'" cannot contain "||". Please consider using the single operator "|".');
          
           
          
          #check unknown funs names
          unknownFuns=.unknownFunsNames(outIE,reservedKeyw)
          if (length(unknownFuns)>0) stop(callerName,'unsupported functions in "OptimizeRestrictions$',idxN,'": ',
                                          paste0(paste0(unknownFuns,'()'),collapse=', '));
          
          #extract components names from restriction
          namesOnRestr=strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnIfCleaner,' ',outIE,ignore.case=TRUE)),'\\s+')[[1]];
          
          #remove numbers
          rmvNumOnRSIdx=c();
          for (idxNamesOnR in 1:length(namesOnRestr)) {
            if (length(grep(strongCharOnNumbs,namesOnRestr[idxNamesOnR]))>0) 
            {
              rmvNumOnRSIdx=c(rmvNumOnRSIdx,idxNamesOnR);
              
            }
          }
          if (length(rmvNumOnRSIdx)>0) namesOnRestr=namesOnRestr[-rmvNumOnRSIdx];
          
          
          #check restriction condition is time series
          if (length(namesOnRestr)==0) stop(callerName,'syntax error in INEQUALITY definition "',outIE,'" in "OptimizeRestrictions$',idxN,'". Logical condition must contain time series.')
          
          
          #check components name
          for (idxNOR in 1:length(namesOnRestr)) {
            if (length(grep(allowedCharOnName,namesOnRestr[idxNOR]))==0) 
            {
              stop(callerName,'syntax error in INEQUALITY definition "',outIE,'" in "OptimizeRestrictions$',idxN,'". The following symbol cannot be a variable name: "',namesOnRestr[idxNOR],'".')
              
            }
          }
          
          if (any(grepl('__',namesOnRestr)))
            stop(callerName,'variable names in INEQUALITY definition "',outIE,'" in "OptimizeRestrictions$',idxN,'" cannot contain double underscore "__". Please change the following variable names: ',paste0(namesOnRestr[which(grepl('__',namesOnRestr))],collapse=', '))
          
       
          
          for (idxNOR in 1:length(namesOnRestr)) {
            if (! namesOnRestr[idxNOR] %in% model_fullComponentList) 
            {
              stop(callerName,'unknown symbol in INEQUALITY definition "',outIE,'" in "OptimizeRestrictions$',idxN,'". The variable "',namesOnRestr[idxNOR],'" is not a model variable.',
                          ifelse(! is.null(RESCHECKeqList),paste0(' Please note that RESCHECKeqList="',paste(RESCHECKeqList,collapse=', '),'"'),'')
                          )
              
            }
          }
          
          
          outIE=gsub('<-','< -',outIE);	
          
          if (! grepl('(==|<|>|>=|<=|!=|&|\\|)',outIE))
            stop(callerName,'syntax error in INEQUALITY definition "',outIE,'" in "OptimizeRestrictions$',idxN,'". No logical operators found.')
          
          
          outIE=.MODEL_MOD_FUNC_NAMES(outIE)
          outIE=.appendAFtoEndVars(outIE,model$vendog)
          outIE=.appendIndexToVars(outIE,c(model$vexog,
                                           paste0(model$vendog,'__ADDFACTOR'))
                                   ,'0');
          
          #explode model funs
          s=.explodeTSLAG(outIE)
          s=.explodeTSDELTA(s$output)
          s=.explodeTSDELTAP(s$output)
          s=.explodeTSDELTALOG(s$output)
          s=.explodeMTOT(s$output)
          s=.explodeMAVE(s$output)
          
          outIE=s$output
          
          #stop if the inequality requires lagging time series more than model equations
          if (-.getLowerLag(outIE)>model$max_lag) 
            stop(callerName,'"OptimizeRestrictions$',idxN,'$INEQUALITY" expression "',OptimizeRestrictions[[idxN]]$INEQUALITY,'" requires to lagging time series more than model equations do.')
          
          #add model lag to inequality expression
          max_lag_sim=max(model$max_lag,1)  
          outIE=.addToIndexInString(outIE,max_lag_sim+1)
          
          #append to list
          tryCatch({
            OptimizeRestrictions[[idxN]]$inExpr=parse(text=outIE)
          },error=function(err) stop(callerName,'syntax error in INEQUALITY definition "',OptimizeRestrictions[[idxN]]$INEQUALITY,'" in "OptimizeRestrictions$',idxN,'".')
          
          )
        }
        
        
      } else {
        
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeRestrictions" are null in the simulation TSRANGE.\n'));
        
      }
    }
     
    # end check OptimizeRestrictions
    
    #check argument OptimizeFunctions
    if (!is.list(OptimizeFunctions) || is.null(names(OptimizeFunctions))) stop(callerName,'"OptimizeFunctions" must be a named list.');
    if (length(names(OptimizeFunctions)) != length(unique(names(OptimizeFunctions)))) stop(callerName,'there are duplicated names in "OptimizeFunctions".');
    
    for (idxN in names(OptimizeFunctions))
    {
      
      if (! is.list(OptimizeFunctions[[idxN]]) 
          || is.null(names(OptimizeFunctions[[idxN]])) 
          || length(OptimizeFunctions[[idxN]])!=2
          || all(sort(names(OptimizeFunctions[[idxN]])) != c('FUNCTION','TSRANGE')))
        stop(callerName,'"OptimizeFunctions$',idxN,'" must be a named list built of the following 2 components: FUNCTION, TSRANGE.')
      
      
      if (is.logical(OptimizeFunctions[[idxN]]$TSRANGE) && ! is.na(OptimizeFunctions[[idxN]]$TSRANGE)) 
      {
        if (OptimizeFunctions[[idxN]]$TSRANGE!=TRUE) 
          stop(callerName,'"OptimizeFunctions$',idxN,'$TSRANGE" must be TRUE or a 4 element integer array.')
        
        #set full TSRANGE if OptimizeFunctions$idxN$TSRANGE is TRUE
        OptimizeFunctions[[idxN]]$TSRANGE=TSRANGE;
        
      } else
      { #check TSRANGE
        tryCatch({
          if (! ( is.numeric(OptimizeFunctions[[idxN]]$TSRANGE) && length(OptimizeFunctions[[idxN]]$TSRANGE)==4 && 
                  .isCompliantYP(c(OptimizeFunctions[[idxN]]$TSRANGE[1],OptimizeFunctions[[idxN]]$TSRANGE[2]),frequency) && 
                  .isCompliantYP(c(OptimizeFunctions[[idxN]]$TSRANGE[3],OptimizeFunctions[[idxN]]$TSRANGE[4]),frequency) &&
                  NUMPERIOD(c(OptimizeFunctions[[idxN]]$TSRANGE[1],OptimizeFunctions[[idxN]]$TSRANGE[2]),
                            c(OptimizeFunctions[[idxN]]$TSRANGE[3],OptimizeFunctions[[idxN]]$TSRANGE[4]),frequency)>=0
          )
          ) stop();  
        },error=function(e) {stop(callerName,'syntax error in "OptimizeFunctions$',idxN,'$TSRANGE". ',e$message)
        });
        
      }
    }
    
    idxToBeRemoved=c();
    for (idxN in 1:length(OptimizeFunctions))
    {
      elemName=names(OptimizeFunctions)[idxN];
      
      #check intersection between OptimizeFunctions TSRANGE and Simulation TSRANGE
      #remove items whose TSRANGE is outside Simulation TSRANGE
      if (NUMPERIOD(c(OptimizeFunctions[[idxN]]$TSRANGE[1],OptimizeFunctions[[idxN]]$TSRANGE[2]),
                    c(TSRANGE[1],TSRANGE[2]),frequency)>0)
      {
        
        if (NUMPERIOD(c(OptimizeFunctions[[idxN]]$TSRANGE[3],OptimizeFunctions[[idxN]]$TSRANGE[4]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxN);
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeFunctions$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n'));
          
        } 
        
      }
      
      if (NUMPERIOD(c(OptimizeFunctions[[idxN]]$TSRANGE[3],OptimizeFunctions[[idxN]]$TSRANGE[4]),
                    c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
      {
        
        if (NUMPERIOD(c(OptimizeFunctions[[idxN]]$TSRANGE[1],OptimizeFunctions[[idxN]]$TSRANGE[2]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxN);
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeFunctions$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n'));
          
        }
      }
      
    }
    
    if (length(idxToBeRemoved)>0) OptimizeFunctions=OptimizeFunctions[-idxToBeRemoved]
    
    if (length(OptimizeFunctions)>0) 
    {  
      for (idxN in 1:length(OptimizeFunctions))
      {
        if (NUMPERIOD(c(OptimizeFunctions[[idxN]]$TSRANGE[1],OptimizeFunctions[[idxN]]$TSRANGE[2]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeFunctions$',names(OptimizeFunctions)[idxN],'$TSRANGE" starts before the simulation TSRANGE.\n'));
          OptimizeFunctions[[idxN]]$TSRANGE[1]=TSRANGE[1];
          OptimizeFunctions[[idxN]]$TSRANGE[2]=TSRANGE[2];
        }
        
        if (NUMPERIOD(c(OptimizeFunctions[[idxN]]$TSRANGE[3],OptimizeFunctions[[idxN]]$TSRANGE[4]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeFunctions$',names(OptimizeFunctions)[idxN],'$TSRANGE" ends after the simulation TSRANGE.\n'));
          OptimizeFunctions[[idxN]]$TSRANGE[3]=TSRANGE[3];
          OptimizeFunctions[[idxN]]$TSRANGE[4]=TSRANGE[4];
        }
      }
      
      #check there's no overlapping in Restrictions TSRANGEs
      if (length(OptimizeFunctions)>1)
      {
        
        for (idxN in 1:(length(OptimizeFunctions)-1))
        {
          for (idxM in (idxN+1):length(OptimizeFunctions))
          {
            
            if (NUMPERIOD(c(OptimizeFunctions[[idxN]]$TSRANGE[3],OptimizeFunctions[[idxN]]$TSRANGE[4]),
                          c(OptimizeFunctions[[idxM]]$TSRANGE[1],OptimizeFunctions[[idxM]]$TSRANGE[2])
                          ,frequency)<1
                &&
                NUMPERIOD(c(OptimizeFunctions[[idxN]]$TSRANGE[3],OptimizeFunctions[[idxN]]$TSRANGE[4]),
                          c(OptimizeFunctions[[idxM]]$TSRANGE[3],OptimizeFunctions[[idxM]]$TSRANGE[4])
                          ,frequency)>-1)
              stop(callerName,'"OptimizeFunctions$',names(OptimizeFunctions)[idxN],'$TSRANGE" overlaps with "OptimizeFunctions$',names(OptimizeFunctions)[idxM],'$TSRANGE".')
            
            if (NUMPERIOD(c(OptimizeFunctions[[idxN]]$TSRANGE[1],OptimizeFunctions[[idxN]]$TSRANGE[2]),
                          c(OptimizeFunctions[[idxM]]$TSRANGE[1],OptimizeFunctions[[idxM]]$TSRANGE[2])
                          ,frequency)<1
                &&
                NUMPERIOD(c(OptimizeFunctions[[idxN]]$TSRANGE[1],OptimizeFunctions[[idxN]]$TSRANGE[2]),
                          c(OptimizeFunctions[[idxM]]$TSRANGE[3],OptimizeFunctions[[idxM]]$TSRANGE[4])
                          ,frequency)>-1)
              stop(callerName,'"OptimizeFunctions$',names(OptimizeFunctions)[idxN],'$TSRANGE" overlaps with "OptimizeFunctions$',names(OptimizeFunctions)[idxM],'$TSRANGE".')
            
          }
        }
      }
      
      for (idxN in names(OptimizeFunctions))
      {
        
        if (any(is.null(OptimizeFunctions[[idxN]]$FUNCTION)) 
            || any(! is.character(OptimizeFunctions[[idxN]]$FUNCTION))
            || length(OptimizeFunctions[[idxN]]$FUNCTION) !=1) 
          stop(callerName,'"OptimizeFunctions$',idxN,'$FUNCTION" must be a string.')
        
        #transform fun raw text into suitable expression
        outFUN=OptimizeFunctions[[idxN]]$FUNCTION
        
        
        #check unknown funs names
        unknownFuns=.unknownFunsNames(outFUN,reservedKeyw)
        if (length(unknownFuns)>0) stop(callerName,'unsupported functions in "OptimizeFunctions$',idxN,'": ',
                                        paste0(paste0(unknownFuns,'()'),collapse=', '));
        
        
        #extract components names from restriction
        namesOnFun=strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnEqCleaner,' ',outFUN,ignore.case=TRUE)),'\\s+')[[1]];
        
        #remove numbers
        rmvNumOnFunIdx=c();
        for (idxNamesOnF in 1:length(namesOnFun)) {
          if (length(grep(strongCharOnNumbs,namesOnFun[idxNamesOnF]))>0) 
          {
            rmvNumOnFunIdx=c(rmvNumOnFunIdx,idxNamesOnF);
            
          }
        }
        if (length(rmvNumOnFunIdx)>0) namesOnFun=namesOnFun[-rmvNumOnFunIdx];
        
        
        #check function is time series
        if (length(namesOnFun)==0) stop(callerName,'syntax error in FUNCTION definition "',outFUN,'" in "OptimizeFunction$',idxN,'". Function definition must contain time series.')
        
        
        #check components name
        for (idxNOF in 1:length(namesOnFun)) {
          if (length(grep(allowedCharOnName,namesOnFun[idxNOF]))==0) 
          {
            stop(callerName,'syntax error in FUNCTION definition "',outFUN,'" in "OptimizeFunction$',idxN,'". The following symbol cannot be a variable name: "',namesOnFun[idxNOF],'".')
            
          }
        }
        
        if (any(grepl('__',namesOnFun)))
          stop(callerName,'variable names in FUNCTION definition "',outFUN,'" in "OptimizeFunction$',idxN,'" cannot contain double underscore "__". Please change the following variable names: ',paste0(namesOnFun[which(grepl('__',namesOnFun))],collapse=', '))
        
        
        
        for (idxNOF in 1:length(namesOnFun)) {
          if (! namesOnFun[idxNOF] %in% model_fullComponentList) 
          {
            stop(callerName,'unknown symbol in FUNCTION definition "',outFUN,'" in "OptimizeFunction$',idxN,'". The variable "',namesOnFun[idxNOF],'" is not a model variable.',
                        ifelse(! is.null(RESCHECKeqList),paste0(' Please note that RESCHECKeqList="',paste(RESCHECKeqList,collapse=', '),'"'),'')
            )
            
          }
        }
        
        if ( grepl('(=|<|>|>=|<=|!=|&|\\|)',outFUN))
          stop(callerName,'syntax error in FUNCTION definition "',outFUN,'" in OptimizeFunctions$',idxN,'. No logical nor assignment operators allowed in function definition.')
        
        
        outFUN=.MODEL_MOD_FUNC_NAMES(outFUN)
        outFUN=.appendIndexToVars(outFUN,c(model$vexog,
                                           model$vendog)
                                  ,'0');
        
        #explode model funs
        s=.explodeTSLAG(outFUN)
        
        s=.explodeTSDELTA(s$output)
        s=.explodeTSDELTAP(s$output)
        s=.explodeTSDELTALOG(s$output)
        s=.explodeMTOT(s$output)
        s=.explodeMAVE(s$output)
        
        outFUN=s$output
        
        #stop if the function requires lagging time series more than model equations
        if (-.getLowerLag(outFUN)>model$max_lag) 
          stop(callerName,'"OptimizeFunctions$',idxN,'$FUNCTION" expression "',OptimizeFunctions[[idxN]]$FUNCTION,'" requires to lagging time series more than model equations do.')
        
        #add model lag to function expression
        max_lag_sim=max(model$max_lag,1)  
        outFUN=.addToIndexInString(outFUN,max_lag_sim+1)
        
        #append to list
        tryCatch({
          OptimizeFunctions[[idxN]]$funExpr=parse(text=outFUN)
        },error=function(err) stop(callerName,'syntax error in FUNCTION definition "',OptimizeFunctions[[idxN]]$FUNCTION,'" in "OptimizeFunctions$',idxN,'".')
        
        )
        
        
        
      }
      
      
    } else {
      
      stop(callerName,'"OptimizeFunctions" is null in the simulation TSRANGE.\n')
      
    }
    
    # end check OptimizeFunctions
    
  }#end if OPTIMIZE
  
  
  # create proxies -----------------------------------
  
  tryCatch(
    {
      
      #create local proxies for all references 
      #(component names used in eq) in behaviorals/identities
      #and stores as ORIGINAL
      
      for (tempName in model_fullComponentList)
      {
        #project ts in ext TSRANGE
        tsValues=TSPROJECT(model$modelData[[tempName]],
                           TSRANGE=EXTENDED_TSRANGE,
                           ARRAY=TRUE,
                           EXTEND=TRUE,
                           avoidCompliance = TRUE);
        
        #buils matrices (in case of parallel sims)
        tempValues=matrix(tsValues,ncol=replica,nrow=extSimSteps);
        
        #create proxy for full original time series (on EXTENDED_TSRANGE)
        assign(paste0(tempName,'__ORIGINAL'),tempValues
               ,envir = localE
        );
        
        #__ORIGINAL wont be modified during sim, next one will be sliding windowed
        #assign(paste0(tempName),tempValues
        #       ,envir = localE
        #);
        
      }
      
    },error=function(e){
      stop(callerName,'model data must contain time series "',tempName,'".');
    });
  
  
  tryCatch(
    {
      #add factor all set to 0
      tsValues=rep(0,extSimSteps);
      tempValues=matrix(tsValues,ncol=replica,nrow=extSimSteps);
      
      for (tempName in paste0(model_vendog,'__ADDFACTOR'))
      {
        
        #create proxy for full original time series (on EXTENDED_TSRANGE)
        assign(paste0(tempName,'__ORIGINAL'),tempValues
               ,envir = localE
        );
        
        #this is needed if this time series is not in CA, INSTRUMENT or Stochsim
        assign(paste0(tempName),tempValues
               ,envir = localE
        );
       
      } 
      
    },error=function(e){
      stop(callerName,'cannot initialize the constant adjustment of "',tempName,'".');
    });
  
  tryCatch(
    {
      #... create proxies for Constant Adjustment
      if (length(ConstantAdjustment)>0) 
        for (idxAF in 1:length(ConstantAdjustment))
        {
          
          tempName=paste0(names(ConstantAdjustment)[idxAF],'__ADDFACTOR');
           
          #assign from input list to local environment
          #project to ext TSRANGE
          tsValues=TSPROJECT(ConstantAdjustment[[idxAF]],
                             TSRANGE=EXTENDED_TSRANGE,
                             ARRAY=TRUE,
                             EXTEND=TRUE,
                             avoidCompliance = TRUE);
          
          #replace NA with 0 (EXTEND will insert NA)
          tsValues[which(is.na(tsValues))]=0;
          
          tempValues=matrix(tsValues,ncol=replica,nrow=extSimSteps);
          
          #create proxy for full original time series (on EXTENDED_TSRANGE)
          assign(paste0(tempName,'__ORIGINAL'),tempValues
                 ,envir = localE
          );
          
          #assign(paste0(tempName),tempValues
          #       ,envir = localE
          #);
          
          
          
        } 
      
    },error=function(e){
      stop(callerName,'"ConstantAdjustment" must contain time series "',tempName,'".');
    });
  
  
  #if multipliers requested then modify INSTRUMENTS columns with SHOCKs...
  if (MULTMATRIX) 
  { 
    tmpI=0;
    
    for (idxI in paste0(INSTRUMENT, '__ORIGINAL'))
    { 
      
      #we need INSTRUMENT column index in matrix MM
      tmpI=tmpI+1;
      
      #get instrument
      tmpM=get(idxI,envir=localE);
      
      for (idxR in 1:simSteps)
      {
        #shock related column
        #get base value
        tmpV=tmpM[model_max_lag+idxR,1];
        
        #choice delta
        tmpD=ifelse(tmpV<1,MM_SHOCK,tmpV*MM_SHOCK);
        
        #apply shock to related column
        #each matrix has simsteps x INSTRUMENT columns count
        #as well as "replica"
        tmpM[model_max_lag+idxR,1+(tmpI-1)*simSteps+idxR]=tmpV+tmpD;
        
        #save back
        assign(idxI,tmpM,envir=localE);
        
      }
      
    } 
  }
  
 
  
  # check missings and lengths --------------------------------
  
   #strict Missing check
	if (! quietly)
  {
      #leadingTSRANGE  = EXTENDED_TSRANGE - TSRANGE
	  leadingTSRANGE=EXTENDED_TSRANGE;
	  leadingTSRANGE[3:4]=normalizeYP(c(TSRANGE[1],TSRANGE[2]-1),f = frequency);
	  leadingSteps=extSimSteps-simSteps;
    if (length(model_fullComponentList)>0) 
      for (idxName in (model_fullComponentList))    
      {
        tmpProxy=get(paste0(idxName,'__ORIGINAL')
                     ,envir=localE
        );
        
        #verify there are no missing values in first max_lag values
        localMissingIndexes=which(! is.finite(tmpProxy[(1:model_max_lag),]));
        
         #index in GETDATE accounts for replicas
        if (length(localMissingIndexes)>0 )
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, ',ifelse(idxName %in% model_vendog,'endogenous','exogenous'),' variable "',idxName,'" is not fully defined in extended TSRANGE. There are undefined values at year-period ',
                      paste0(date2yp(as.Date(
                        GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=leadingTSRANGE,EXTEND=TRUE,avoidCompliance = TRUE),
                                #localMissingIndexes[length(localMissingIndexes)/replica],avoidCompliance = TRUE)
                                (localMissingIndexes[length(localMissingIndexes)]-1) %% leadingSteps+1,avoidCompliance = TRUE)
                      ),f=frequency),collapse = '-'),'.\n'));
        
        #do not really know when this can happen...
        if (length(tmpProxy)!=replica*extSimSteps) 
          stop(callerName,'wrong length in TSRANGE projected time series "',idxName,'".')
        
      }
    
    if (length(ConstantAdjustment)>0) 
      for (idxCA in names(ConstantAdjustment))
      {
        
        tmpProxy=get(paste0(idxCA,'__ADDFACTOR', '__ORIGINAL')
                     ,envir=localE
        );
        
        #verify there are no missing values
        localMissingIndexes=which(! is.finite(tmpProxy[(1:model_max_lag),]));
        
         if (length(localMissingIndexes)>0 )
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, constant adjustment of endogenous variable "',idxCA,'" is not fully defined in extended TSRANGE. There are undefined values at year-period ',
                      paste0(date2yp(as.Date(
                        GETDATE(TSPROJECT(ConstantAdjustment[[idxCA]],TSRANGE=leadingTSRANGE,EXTEND=TRUE),
                                #localMissingIndexes[length(localMissingIndexes)/replica],avoidCompliance = TRUE)
                                (localMissingIndexes[length(localMissingIndexes)]-1) %% leadingSteps+1,avoidCompliance = TRUE)
                      ),f=frequency),collapse = '-'),'.\n'));
        
        #do not really know when this can happen...
        if (length(tmpProxy)!=replica*extSimSteps) 
          stop(callerName,'wrong length in TSRANGE projected constant adjusted time series "',names(ConstantAdjustment)[idxCA],'".')
        
      }
  } #end strictMissingsCheck
  
  #check missing and length in vexog time series inside TSRANGE  
  if (length(model_vexog)>0) 
    for (idxName in (model_vexog))    
    {
      
      tmpProxy=get(paste0(idxName,'__ORIGINAL')
                   ,envir=localE
      );
      
      #verify there are no missing values
      #we cut first max_lag values that could be unused
      localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),]));
       
      #index in GETDATE accounts for replicas
      if (length(localMissingIndexes)>0 )
        stop(callerName,'exogenous variable "',idxName,'" is not fully defined in the TSRANGE. There are undefined values at year-period ',
                    paste0(date2yp(as.Date(
                      GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=TSRANGE,EXTEND=TRUE,avoidCompliance = TRUE),
                              #localMissingIndexes[length(localMissingIndexes)/replica],avoidCompliance = TRUE)
                              (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1,avoidCompliance = TRUE)
                    ),f=frequency),collapse = '-'),'\n')
      
      #do not really know when this can happen...
      if (length(tmpProxy)!=replica*extSimSteps) 
        stop(callerName,'wrong length in TSRANGE projected time series "',idxName,'".')
      
    }
  
  #check missing and length in vexogenized time series inside TSRANGE  
  if (length(model_vendog_exogenized)>0) 
    for (idxName in (model_vendog_exogenized))
    {
      
      tmpProxy=get(paste0(idxName,'__ORIGINAL')
                   ,envir=localE
      );
      
      #verify there are no missing values
      localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),]));
      
      
      if (length(localMissingIndexes)>0  ) 
        stop(callerName,'exogenized variable "',idxName,'" is not fully defined in the TSRANGE. There are undefined values at year-period ',
                    paste0(date2yp(as.Date(
                      GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=TSRANGE,EXTEND=TRUE,avoidCompliance = TRUE),
                              #localMissingIndexes[length(localMissingIndexes)/replica],avoidCompliance = TRUE)
                              (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1,avoidCompliance = TRUE)
                    ),f=frequency),collapse = '-'),'.\n')
      
      #do not really know when this can happen...
      if (length(tmpProxy)!=replica*extSimSteps) 
        stop(callerName,'wrong length in TSRANGE projected time series "',idxName,'".')
      
    }
  
  #check missing and length in vendog time series inside TSRANGE  
  if (length(model_vendog)>0) 
    for (idxName in (model_vendog))
    { 
      
      tmpProxy=get(paste0(idxName, '__ORIGINAL')
                   ,envir=localE
      );
      
      #verify there are no missing values 
      localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),]));
      
      #in DYNAMIC and FORECAST sim we can allow missings in trailing obs
      if (length(localMissingIndexes)>0  ) 
      { if (simType != 'FORECAST') .MODEL_outputText(outputText = !quietly,
                              paste0('\n',callerName,'warning, endogenous variable "',idxName,'" is not fully defined in TSRANGE. There are undefined values at year-period ',
                                paste0(date2yp(as.Date(
                                  GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=TSRANGE,EXTEND=TRUE,avoidCompliance = TRUE),
                                          #localMissingIndexes[length(localMissingIndexes)/replica],avoidCompliance = TRUE)
                                          (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1,avoidCompliance = TRUE)
                                  ),f=frequency),collapse = '-'),'\n'));
        
        if (simType=='DYNAMIC')
        {
          .MODEL_outputText(outputText = !quietly,paste0('\nSimulation will continue replicating last observation in time series "', idxName,'" over the full TSRANGE. Use the FORECAST option to avoid this message.\n'));
        }
        
        if (simType=='DYNAMIC' || simType == 'FORECAST')
        {
          
		      #here we propagate last known finite observation if any trailing missing
          localMissingIndexes=which(! is.finite(tmpProxy));
          												   
          #should never happen
          if (length(localMissingIndexes)==length(tmpProxy)) 
            stop(callerName,'all endogenous time series "', idxName,'" values are undefined in the extended TSRANGE. Cannot initialize the simulation.')
          
          #get last known finite observation
          lastKnownIndex=max(which(is.finite(rowSums(tmpProxy))));
          
         #all obs, if lastKnownIndex is last row of tmpProxy then skip
         if (replica*lastKnownIndex < length(tmpProxy))
          {
            #assign finite values it to trailing missings (and consider replica)
            lastKnownObservation=tmpProxy[drop=FALSE,lastKnownIndex,];
            lastKnownObservationM=c();
            for (tmpIdx in 1:length(lastKnownIndex:(length(tmpProxy)/replica))) lastKnownObservationM=rbind(lastKnownObservationM,lastKnownObservation);
            
            tmpProxy[lastKnownIndex:(length(tmpProxy)/replica),]=lastKnownObservationM;
            
            #update original and current time series
            assign(paste0(idxName,'__ORIGINAL'),tmpProxy
                   ,envir = localE
            );
            
            #assign(idxName,tmpProxy
            #       ,envir = localE
            #);
            
          }
          
          localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),]));
          
          #check all missings are gone (may be we have NA inside TSRANGE but latest obs are ok)
           if (length(localMissingIndexes)>0  ) 
            stop(paste0('\n',callerName,'endogenous variable "',idxName,'" is not fully defined in TSRANGE. There are undefined values at year-period ',
                        paste0(date2yp(as.Date(
                          GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=TSRANGE,EXTEND=TRUE),
                                  #localMissingIndexes[length(localMissingIndexes)/replica],avoidCompliance = TRUE)
                                  (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1,avoidCompliance = TRUE)
                                  ),f=frequency),collapse = '-'),'.\n'));
          
        }
        else
        {
          #missings are not allowed on STATIC or RESCHECK
          stop(callerName,'endogenous variable "',idxName,'" is not fully defined in TSRANGE. There are undefined values at year-period ',
                      paste0(date2yp(as.Date(
                        GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=TSRANGE,EXTEND=TRUE),
                                #localMissingIndexes[length(localMissingIndexes)/replica],avoidCompliance = TRUE)
                                (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1,avoidCompliance = TRUE)
                      ),f=frequency),collapse = '-'),'.\n','Simulation of type "',simType,'" will stop.')
        }
      }
      
      #do not really know when this can happen...
      if (length(tmpProxy)!=replica*extSimSteps) 
        stop(callerName,'wrong length in TSRANGE projected time series "',idxName,'".')
      
    }
  
  #check missing and length in CA time series inside TSRANGE  
  if (length(ConstantAdjustment)>0) 
    for (idxCA in names(ConstantAdjustment))
    {
      
      tmpProxy=get(paste0(idxCA,'__ADDFACTOR', '__ORIGINAL')
                   ,envir=localE
      );
      
      #verify there are no missing values
      localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),]));
      
      if (length(localMissingIndexes)>0 )
        stop(callerName,'constant adjustment of endogenous variable "',idxCA,'" is not fully defined in the TSRANGE. There are undefined values at year-period ',
                    paste0(date2yp(as.Date(
                      GETDATE(TSPROJECT(ConstantAdjustment[[idxCA]],TSRANGE=TSRANGE,EXTEND=TRUE),
                              #localMissingIndexes[length(localMissingIndexes)/replica],avoidCompliance = TRUE)
                              (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1,avoidCompliance = TRUE)
                              ),f=frequency),collapse = '-'),'.\n')
      
      #do not really know when this can happen...
      if (length(tmpProxy)!=replica*extSimSteps) 
        stop(callerName,'wrong length in TSRANGE projected constant adjusted time series "',names(ConstantAdjustment)[idxCA],'".')
      
    }
  
  #backupped and later saved in simulate_parameters
  Exogenize_original=Exogenize;
  
  #print messages....
  if (length(Exogenize) >0) 
    for (idxEL in names(Exogenize))
    {
      
      .MODEL_outputText(outputText = !quietly,paste0(callerName,'endogenous variable "',
                                                     idxEL,
                                                     '" has been exogenized from year-period ',
                                                     paste0(Exogenize[[idxEL]][1:2],collapse='-'),
                                                     ' to ',
                                                     paste0(Exogenize[[idxEL]][3:4],collapse='-'),'.\n'));
      
      #convert to indexes
      Exogenize[[idxEL]]  = c(
        (NUMPERIOD(TSRANGE[1:2],Exogenize[[idxEL]][1:2],frequency)+1):
          (NUMPERIOD(TSRANGE[1:2],Exogenize[[idxEL]][3:4],frequency)+1)
      )
      
    }
  
  if (length(ConstantAdjustment) >0) 
    for (idxCA in names(ConstantAdjustment))
    {
      
      tryCatch(
         {.MODEL_outputText(outputText = !quietly,paste0(callerName,'endogenous variable "',
                                                        idxCA,
                                                        '" has a constant adjustment from year-period ',
                                                        paste0(start(TSPROJECT(ConstantAdjustment[[idxCA]],TSRANGE=EXTENDED_TSRANGE,avoidCompliance = TRUE)),collapse='-'),
                                                        ' to ',
                                                        paste0(end(TSPROJECT(ConstantAdjustment[[idxCA]],TSRANGE=EXTENDED_TSRANGE,avoidCompliance = TRUE)),collapse='-'),'.\n'));
        },error=function(e){
          
          .MODEL_outputText(outputText = !quietly,
                            paste0(callerName,'warning, constant adjustment "',idxCA,'" does not intersect with simulation TSRANGE.\n'));
          
        }
      );
    }
  
  if (length(Exogenize)>0) 
    if (MULTMATRIX && length(base::intersect(names(Exogenize),TARGET))>0)
    {
      .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, the following TARGET have been exogenized, therefore related multiplier will be zero: ',paste(base::intersect(names(Exogenize),TARGET),collapse =', '),'.\n')) 
    }
  
  #backupped and later saved in simulate_parameters
  StochStructure_original= StochStructure;
 
  
  #print messages....
  if (STOCHSIMULATE && length(StochStructure)>0) 
    for (idxSS in names(StochStructure))
    {
      .MODEL_outputText(outputText = !quietly,paste0(callerName,'',ifelse(idxSS %in% model_vendog,'endogenous variable "','exogenous variable "'),
                                                     idxSS,
                                                     '" has been ',StochStructure[[idxSS]]$TYPE,' perturbed from year-period ',
                                                     paste0(StochStructure[[idxSS]]$TSRANGE[1:2],collapse='-'),
                                                     ' to ',
                                                     paste0(StochStructure[[idxSS]]$TSRANGE[3:4],collapse='-'),'.\n'));
      
      #convert to indexes
      StochStructure[[idxSS]]$TSRANGE  = c(
        (NUMPERIOD(EXTENDED_TSRANGE[1:2],StochStructure[[idxSS]]$TSRANGE[1:2],frequency)+1):
          (NUMPERIOD(EXTENDED_TSRANGE[1:2],StochStructure[[idxSS]]$TSRANGE[3:4],frequency)+1)
      )
      
    }
  
  if (OPTIMIZE)
  {
    if(length(OptimizeBounds)>0) 
    {
      for (idxSS in names(OptimizeBounds))
      {
      .MODEL_outputText(outputText = !quietly,paste0(callerName,'optimization boundaries for the ',ifelse(idxSS %in% model_vendog,'add-factor of endogenous variable "','exogenous variable "'),
                                                     idxSS,
                                                     '" are (',paste0(OptimizeBounds[[idxSS]]$BOUNDS[1:2],collapse=','),') from year-period ',
                                                     paste0(OptimizeBounds[[idxSS]]$TSRANGE[1:2],collapse='-'),
                                                     ' to ',
                                                     paste0(OptimizeBounds[[idxSS]]$TSRANGE[3:4],collapse='-'),'.\n'));
      
      #convert to indexes
      OptimizeBounds[[idxSS]]$TSRANGE  = c(
        (NUMPERIOD(EXTENDED_TSRANGE[1:2],OptimizeBounds[[idxSS]]$TSRANGE[1:2],frequency)+1):
          (NUMPERIOD(EXTENDED_TSRANGE[1:2],OptimizeBounds[[idxSS]]$TSRANGE[3:4],frequency)+1)
      )
      
      }
    }

    if(length(OptimizeRestrictions)>0) 
    {
      for (idxSS in names(OptimizeRestrictions))
      {
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'optimization restriction "',
                                                       idxSS,'" is active from year-period ',
                                                       paste0(OptimizeRestrictions[[idxSS]]$TSRANGE[1:2],collapse='-'),
                                                       ' to ',
                                                       paste0(OptimizeRestrictions[[idxSS]]$TSRANGE[3:4],collapse='-'),'.\n'));
        #convert to indexes
        OptimizeRestrictions[[idxSS]]$TSRANGE  = c(
          (NUMPERIOD(EXTENDED_TSRANGE[1:2],OptimizeRestrictions[[idxSS]]$TSRANGE[1:2],frequency)+1):
            (NUMPERIOD(EXTENDED_TSRANGE[1:2],OptimizeRestrictions[[idxSS]]$TSRANGE[3:4],frequency)+1)
        )
        
      }
    }
    
    if(length(OptimizeFunctions)>0) 
    {
      for (idxSS in names(OptimizeFunctions))
      {
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'optimization objective function "',
                                                       idxSS,'" is active from year-period ',
                                                       paste0(OptimizeFunctions[[idxSS]]$TSRANGE[1:2],collapse='-'),
                                                       ' to ',
                                                       paste0(OptimizeFunctions[[idxSS]]$TSRANGE[3:4],collapse='-'),'.\n'));
        #convert to indexes
        OptimizeFunctions[[idxSS]]$TSRANGE  = c(
          (NUMPERIOD(EXTENDED_TSRANGE[1:2],OptimizeFunctions[[idxSS]]$TSRANGE[1:2],frequency)+1):
            (NUMPERIOD(EXTENDED_TSRANGE[1:2],OptimizeFunctions[[idxSS]]$TSRANGE[3:4],frequency)+1)
        )
        
      }
    }
    
    
  }
  
  
  
  
  # add stoch noise ----------------------------------------------------------------------------------------
  
  #add noise to selected vars
  if (STOCHSIMULATE)
  {
    #store perturbed instruments
    INSTRUMENT_MM=list();
    
    if (length(StochStructure)>0) 
    { 
        
      for (idxN in names(StochStructure))
      {
        
        if (StochStructure[[idxN]]$TYPE=='UNIF')
          noise=matrix(
            stats::runif(
              length(StochStructure[[idxN]]$TSRANGE)*(replica-1),
              min=StochStructure[[idxN]]$PARS[1],
              max=StochStructure[[idxN]]$PARS[2])
            ,nrow=length(StochStructure[[idxN]]$TSRANGE)
          )
        
        
        if (StochStructure[[idxN]]$TYPE=='NORM')
          noise=matrix(
            stats::rnorm(
              length(StochStructure[[idxN]]$TSRANGE)*(replica-1),
              mean=StochStructure[[idxN]]$PARS[1],
              sd=StochStructure[[idxN]]$PARS[2])
            ,nrow=length(StochStructure[[idxN]]$TSRANGE)
          )
        
        if (idxN %in% model_vendog)
        {
          #get un-noised time series
          tmpNoised_ORIG=get(paste0(idxN,'__ADDFACTOR','__ORIGINAL'),envir = localE);
          #tmpNoised=get(paste0(idxN,'__ADDFACTOR'),envir = localE);
          
        } else if(idxN %in% model_vexog)
        {
          #get un-noised time series
          tmpNoised_ORIG=get(paste0(idxN,'__ORIGINAL'),envir = localE);
          #tmpNoised=get(paste0(idxN),envir = localE);
          
        } else if (! is.null(RESCHECKeqList) && length(base::intersect(idxN,model_fullComponentList))==0)
        {
          #with RESCHECK user can select eq to be simulated
          #so idxN could be not in vendog nor in vexog
          .MODEL_outputText(outputText =!quietly,paste0(callerName,'warning, "',idxN,'" appears in "StochStructure" but it is not required in order to perform the simulations requested in "RESCHECKeqList". It will be skipped.\n'));
          next;
          
        } else stop(callerName,'unknown error while perturbing time series "',idxN,'".')
        
        #add noise to matrix data
        tmpNoised_ORIG[StochStructure[[idxN]]$TSRANGE,2:replica]=tmpNoised_ORIG[StochStructure[[idxN]]$TSRANGE,2:replica]+noise;
        
        if (idxN %in% model_vendog)
        {
          #save noised ts
          assign(paste0(idxN,'__ADDFACTOR','__ORIGINAL'),tmpNoised_ORIG,envir = localE);
          
        } else if(idxN %in% model_vexog)
        {
          #save noised ts
          assign(paste0(idxN,'__ORIGINAL'),tmpNoised_ORIG,envir = localE);
          
        }  else stop(callerName,'unknown error while storing perturbed time series "',idxN,'".')
        
        #store perturbed ts for output
        INSTRUMENT_MM[[idxN]]=tmpNoised_ORIG[drop=FALSE,-(1:model_max_lag),]
        
      }
    }
  }
  
  # populate optimization search domain --------------------------------------------------------------
  #populate search domain
  if (OPTIMIZE)
  {
    #store perturbed instruments
    INSTRUMENT_MM=list();
    #columns to be kept after optimization restrictions
    optColsToBeKept=rep(TRUE,replica-1);
    
    #optimize function results
    optFunResults=rep(0,replica-1);
    
    #optimize fun time series
    optFunTSMM=c()
    optFunTS=NULL
    
    if (length(OptimizeBounds)>0) 
    { 
      
      for (idxN in names(OptimizeBounds))
      {
        
        noise=matrix(
          stats::runif(
            length(OptimizeBounds[[idxN]]$TSRANGE)*(replica-1),
            min=OptimizeBounds[[idxN]]$BOUNDS[1],
            max=OptimizeBounds[[idxN]]$BOUNDS[2])
          ,nrow=length(OptimizeBounds[[idxN]]$TSRANGE)
        )
          
          if (idxN %in% model_vendog)
          {
            #get un-noised time series
            tmpNoised_ORIG=get(paste0(idxN,'__ADDFACTOR','__ORIGINAL'),envir = localE);
            #tmpNoised=get(paste0(idxN,'__ADDFACTOR'),envir = localE);
            
          } else if (idxN %in% model_vexog)
          {
            #get un-noised time series
            tmpNoised_ORIG=get(paste0(idxN,'__ORIGINAL'),envir = localE);
            #tmpNoised=get(paste0(idxN),envir = localE);
            
          } else if (! is.null(RESCHECKeqList) && length(base::intersect(idxN,model_fullComponentList))==0)
          {
            #with RESCHECK user can select eq to be simulated
            #so idxN could be not in vendog nor in vexog
            .MODEL_outputText(outputText =!quietly,paste0(callerName,'warning, "',idxN,'" appears in "OptimizeBounds" but it is not required in order to perform the simulations requested in "RESCHECKeqList". It will be skipped.\n'));
            next;
            
          } else stop(callerName,'unknown error while populating time series "',idxN,'".')
          
          #populate matrix data
          #tmpNoised_ORIG[OptimizeBounds[[idxN]]$TSRANGE,2:replica]=tmpNoised_ORIG[OptimizeBounds[[idxN]]$TSRANGE,2:replica]+noise;
          tmpNoised_ORIG[OptimizeBounds[[idxN]]$TSRANGE,2:replica]=noise;
          
          if (idxN %in% model_vendog)
          {
            #save populated ts
            assign(paste0(idxN,'__ADDFACTOR','__ORIGINAL'),tmpNoised_ORIG,envir = localE);
            
          } else if (idxN %in% model_vexog)
          {
            #save populated ts
            assign(paste0(idxN,'__ORIGINAL'),tmpNoised_ORIG,envir = localE);
            
          }  else stop(callerName,'unknown error while storing populated time series "',idxN,'".')
          
          #store perturbed ts for output
          INSTRUMENT_MM[[idxN]]=tmpNoised_ORIG[drop=FALSE,-(1:model_max_lag),]
          
          
      }
    }
  }
  
  #temp list with sim results
  simulation=list();
  
  .MODEL_outputText(outputText=!quietly,"\n");
  
  for (currentVendog in model_names_behavioral)
  { #cycle in endogenous
    
    #get eq components names and assign into them the behavioral coefficient values
    
    currentBehavioral=model$behaviorals[[currentVendog]];
    
    #get names of coefficients for current behavioral endogenous
    localCoefficientNames=currentBehavioral$eqCoefficientsNames;
    
    #get values of coefficients for current behavioral endogenous
    localCoefficientValues=c(currentBehavioral$coefficients);      
    
    #check there is a coefficient value for every coefficient name
    if (length(localCoefficientNames) != length(localCoefficientValues)) 
      stop(callerName,'coefficients count error in behavioral "',currentVendog,'".')
    
    #assign values to coefficients
    for (idxA in 1:length(localCoefficientValues))
    {
      assign(paste0(currentVendog,'__',localCoefficientNames[idxA])
             ,localCoefficientValues[idxA]
             ,envir = localE
      );
    } 
    
    #assign values to error coefficients
    if ( ! is.null(currentBehavioral$errorType) && currentBehavioral$errorType=='AUTO')
    {  
      for (idxEC in 1:currentBehavioral$errorDim)
      {
        #check if rhos have to be zeroed
        if (ZeroErrorAC)
        {
          assign(paste0(currentVendog,'__RHO__',idxEC)
                 ,0
                 ,envir = localE
          );
        }
        else
        {
          assign(paste0(currentVendog,'__RHO__',idxEC)
                 ,currentBehavioral$errorCoefficients[idxEC]
                 ,envir = localE
          );
        }
        
      }
      
    }# end deal with AUTO
    
    
    
  }#for in model_names_behaviorals
  
  # build sim expressions -------------------------------------------------------
  
  if (! simType =='RESCHECK')
  {#full simultaion required
    
    #array of expressions used in convergence algos
    vpre_expressions=vector('list',simSteps);
    vsim_expressions=vector('list',simSteps);
    vfeed_expressions=vector('list',simSteps);
    vpost_expressions=vector('list',simSteps);
    
    #build expressions arrays
    
    #gauss support variables, needed for convergence check on feedback variables
    assign('VFEED__NEXT__X__BIMETS__MODEL',
           matrix(ncol=replica,nrow=length(model$vfeed))
           ,envir=localE
    );
    assign('VFEED__X__BIMETS__MODEL', 
           matrix(ncol=replica,nrow=length(model$vfeed))
           ,envir=localE
    );
    
    #LAST__EVAL__EQ used in debug, it keeps name of last evaluated eq during sim
    assign('LAST__EVAL__EQ', 
           'unknown'
           ,envir=localE
    );
    
    #convergence expression (max %diff between two iterations in vfeed)
    convExpr=parse(text='
                   max(
                   abs(
                   (VFEED__NEXT__X__BIMETS__MODEL - VFEED__X__BIMETS__MODEL))/
                   ifelse(abs(VFEED__X__BIMETS__MODEL)>1,abs(VFEED__X__BIMETS__MODEL),1) 
                   ) < CONVERGE__BIMETS__MODEL');
   
    
    #build vpre expression
    if (length(model$vpre)>0) 
    { 
      for (tmpV in (model$vpre))
      {
        #init tmp var
        tmp_expressions=c();
        
        #append sim expression
        if (tmpV %in% names(model$behaviorals))
        {
          tmp_expressions=model$behaviorals[[tmpV]]$eqSimExp
        } else
        {
          tmp_expressions=model$identities[[tmpV]]$eqSimExp
        }
        
        #append check on result
        tmp_expressions=c(parse(text=paste0('LAST__EVAL__EQ="',tmpV,'";')),tmp_expressions,parse(text=paste0('if (any(! is.finite(',tmpV,'[',model_max_lag+1,',]))) stop(\'Uncomputable solution or numeric overflow while evaluating \"',tmpV,'\".\');')))
        
        #update vpre expressions, and deal with exonization
        if (! tmpV %in% names(Exogenize)) 
        {
          for (idxSP in 1:simSteps)
          {
            vpre_expressions[[idxSP]]=c(vpre_expressions[[idxSP]],tmp_expressions);   
          } 
        } else 
        {
          for (idxSP in 1:simSteps)
          {
            if (! idxSP %in% Exogenize[[tmpV]])  vpre_expressions[[idxSP]]=c(vpre_expressions[[idxSP]],tmp_expressions);   
          } 
        }
      }}#end vpre
    
    #get index of first vfeed in vsim
    if (length(model$vsim)>0)
    {
      vfeedIndexInVsim=which(model$vsim == model$vfeed[[1]]);
      if (length(vfeedIndexInVsim)==0) stop(callerName,'unknown error while splitting vsim vs vfeed.')
    }
    
    
    # gauss -----------------------------------------------------
    
    #build vsim expression
    if (length(model$vsim)>0) 
    { 
      for (idxE in 1:length(model$vsim))
      {
        #init tmp var
        tmp_expressions=c();
        tmpV=model$vsim[idxE];
        
        #save previous vfeed values (we need for convergence test)
        if (idxE>=vfeedIndexInVsim) 
        {
          tmp_expressions=parse(text=paste0('VFEED__X__BIMETS__MODEL[',1+idxE-vfeedIndexInVsim,',]=',tmpV,'[',model_max_lag+1,',]'))
          
          for (idxSP in 1:simSteps) vsim_expressions[[idxSP]]=c(vsim_expressions[[idxSP]],tmp_expressions);
          
        }
        
        tmp_expressions=c();
        
        #append sim expression
        if (tmpV %in% names(model$behaviorals))
        {
          tmp_expressions=c(tmp_expressions,model$behaviorals[[tmpV]]$eqSimExp);
          
        } else
        {
          tmp_expressions=c(tmp_expressions,model$identities[[tmpV]]$eqSimExp)
        }
        
        #gauss
        #store vsim expressions
        tmp_expressions=c(parse(text=paste0('LAST__EVAL__EQ="',tmpV,'";')),tmp_expressions,parse(text=paste0('if (any(! is.finite(',tmpV,'[',model_max_lag+1,',]))) stop(\'Uncomputable solution or numeric overflow while evaluating \"',tmpV,'\".\');')))
        
        #deal with exogenization
        if (! tmpV %in% names(Exogenize)) 
        {
          for (idxSP in 1: simSteps)
          {
            vsim_expressions[[idxSP]]=c(vsim_expressions[[idxSP]],tmp_expressions);   
          } 
        } else 
        {
          for (idxSP in 1:simSteps)
          {
            if (! idxSP %in% Exogenize[[tmpV]])  vsim_expressions[[idxSP]]=c(vsim_expressions[[idxSP]],tmp_expressions);   
          } 
        }
        
        #store next vfeed values (we need for convergence test)
        if (idxE>=vfeedIndexInVsim) 
        {
          tmp_expressions=parse(text=paste0('VFEED__NEXT__X__BIMETS__MODEL[',1+idxE-vfeedIndexInVsim,',]=',tmpV,'[',model_max_lag+1,',]'))
          
          for (idxSP in 1: simSteps) vsim_expressions[[idxSP]]=c(vsim_expressions[[idxSP]],tmp_expressions);
        }
        
      }
    }
    
    
    
    #build vpost expression
    if (length(model$vpost)>0) 
    { 
      for (tmpV in (model$vpost))
      {
        #init tmp vars
        tmp_expressions=c();
        
        #append sim expr
        if (tmpV %in% names(model$behaviorals))
        {
          tmp_expressions=model$behaviorals[[tmpV]]$eqSimExp
        } else
        {
          tmp_expressions=model$identities[[tmpV]]$eqSimExp
        }
        
        tmp_expressions=c(parse(text=paste0('LAST__EVAL__EQ="',tmpV,'";')),tmp_expressions,parse(text=paste0('if (any(! is.finite(',tmpV,'[',model_max_lag+1,',]))) stop(\'Uncomputable solution or numeric overflow while evaluating \"',tmpV,'\".\');')))
        
        #deal with exonization
        if (! tmpV %in% names(Exogenize)) 
        {
          for (idxSP in 1: simSteps)
          {
            vpost_expressions[[idxSP]]=c(vpost_expressions[[idxSP]],tmp_expressions);   
          } 
        } else 
        {
          for (idxSP in 1: simSteps)
          {
            if (! idxSP %in% Exogenize[[tmpV]])  vpost_expressions[[idxSP]]=c(vpost_expressions[[idxSP]],tmp_expressions);   
          } 
        }
      }
    }
    
    
    #assign to local var sim arguments
    assign('ITERLIMIT__BIMETS__MODEL',simIterLimit
           ,envir=localE
    );
    assign('CONVERGE__BIMETS__MODEL',simConvergence/100
           ,envir=localE
    );
    
  }#not RESCHECK
  
  
   
  if (simType!='RESCHECK' && length(model$vsim)==0) 
    .MODEL_outputText(sep='',outputText=!quietly,callerName,'warning, the incidence graph is not cyclic. There is no convergence to be achieved.\n')
  
  
  # main loop --------------------------------------------------------------
  
  
  tryCatch({
    
    for (simIterLoopIdx in 1:simSteps)
    {
      
      if (MULTMATRIX)
      {.MODEL_outputText(outputText=!quietly,paste0('\r','Multiplier Matrix:'),sprintf("%10.2f",simIterLoopIdx*100/simSteps),'%');
      } else if (STOCHSIMULATE)
      {.MODEL_outputText(outputText=!quietly,paste0('\r','Stochastic Simulation:'),sprintf("%10.2f",simIterLoopIdx*100/simSteps),'%'); 
      } else if (OPTIMIZE)
      {.MODEL_outputText(outputText=!quietly,paste0('\r','Optimize:'),sprintf("%10.2f",simIterLoopIdx*100/simSteps),'%'); 
      } else
      {.MODEL_outputText(outputText=!quietly,paste0('\r','Simulation:'),sprintf("%10.2f",simIterLoopIdx*100/simSteps),'%');}
      
       
      if (simType != 'RESCHECK' && (verbose)) {cat('\n');}
      
      #create local proxies for all references 
      #(components names used in eq) in  behaviorals/identities
      #in moving window (with range maxlag) starting from idx
      for (tempName in (model_fullComponentList))
      {
        
        assign(tempName,get(paste0(tempName,'__ORIGINAL')
                            ,envir=localE
        )[drop=FALSE,simIterLoopIdx+(0:model_max_lag),]
        , envir = localE
        );
        
      }
      
      #proxy for explicit constant adjustments
      if (length(ConstantAdjustment)>0) 
        for (tempName in paste0(names(ConstantAdjustment),'__ADDFACTOR'))
        {
          
          assign(tempName,get(paste0(tempName,'__ORIGINAL')
                              ,envir=localE
          )[drop=FALSE,simIterLoopIdx+(0:model_max_lag),]
          , envir = localE
          );
          
        } 
      
      #proxy for vendog constant adjustment as INSTRUMENT 
      #vendog can be in INSTRUMENT (i.e. MULTMATRIX) but not in CA list
      if (length(instrumentalVendogs)>0) 
        for (tempName in paste0(instrumentalVendogs,'__ADDFACTOR'))
        {
          
          assign(tempName,get(paste0(tempName,'__ORIGINAL')
                              ,envir=localE
          )[drop=FALSE,simIterLoopIdx+(0:model_max_lag),]
          , envir = localE
          );
          
        } 
      
      
      #proxy for vendog noised in stocastich simulation 
      if (STOCHSIMULATE && length(StochStructure)>0)
      {
        
        noised_vendog=base::intersect(names(StochStructure),model_vendog);
        
        if (length(noised_vendog)>0)
          for (tempName in paste0(noised_vendog,'__ADDFACTOR'))
          {
            assign(tempName,get(paste0(tempName,'__ORIGINAL')
                                ,envir=localE
            )[drop=FALSE,simIterLoopIdx+(0:model_max_lag),]
            , envir = localE
            );
          }
      }
      
      #proxy for vendog noised in optimize simulation 
      if (OPTIMIZE && length(OptimizeBounds)>0)
      {
        
        
        optimized_vendog=base::intersect(names(OptimizeBounds),model_vendog);
        
        if (length(optimized_vendog)>0)
          for (tempName in paste0(optimized_vendog,'__ADDFACTOR'))
          {
            assign(tempName,get(paste0(tempName,'__ORIGINAL')
                                ,envir=localE
            )[drop=FALSE,simIterLoopIdx+(0:model_max_lag),]
            , envir = localE
            );
          }
      }
      
      # check optimize restrictions --------------------------------------------------
      
      if (OPTIMIZE && length(OptimizeRestrictions)>0)
      {
        
        for (idxIE in 1:length(OptimizeRestrictions))
        {
          
          
          #check if we are inside restriction tsrange
          if ((model_max_lag+simIterLoopIdx) %in% OptimizeRestrictions[[idxIE]]$TSRANGE)
          {
            
            
            tryCatch({
              
            #browser()
              
            eval(parse(text=paste0('LAST__EVAL__EQ="OptimizeRestrictions$',names(OptimizeRestrictions)[idxIE],'";')),envir = localE)
            
           
            currentRestrictionResults=eval(OptimizeRestrictions[[idxIE]]$inExpr,envir = localE)
            
            if (length(currentRestrictionResults)==0 || any(is.na(currentRestrictionResults)) || ! all(is.logical(currentRestrictionResults))) stop('restriction must be a computable inequality.')
            
            if (length(optColsToBeKept) != length(currentRestrictionResults[-1])) stop('unknown error.')
            optColsToBeKept=optColsToBeKept & currentRestrictionResults[-1]
            
            },error=function(err) stop(paste0('Cannot evaluate "OptimizeRestrictions$',
                                              names(OptimizeRestrictions)[idxIE],
                                              '", inequality "',
                                              OptimizeRestrictions[[idxIE]]$INEQUALITY,'": ',err$message))
            ,warning=function(warn) cat(paste0('\n',callerName,'warning on evaluating "OptimizeRestrictions$',
                                               names(OptimizeRestrictions)[idxIE],': ',warn$message),'\n')
            )
            
          }
        }
      }
      
      # rescheck loop ---------------------------------------
      
      
      
      if (simType=='RESCHECK') 
      {
        tryCatch(
          {    
            
            
            #cycle behaviorals
            for (currentVendog in model_names_behavioral)
            { #cycle in endogenous
              
            
              #get var name
              #currentVendog=model_names_behavioral[idxB]
              
              #backup historical value
              backupVendog=get(currentVendog,envir = localE)
              
              #if not exoginated eval expression
              if ( !   (  (simIterLoopIdx %in% Exogenize[[currentVendog]])) ) 
                eval(c(parse(text=paste0('LAST__EVAL__EQ="',currentVendog,'";')),model$behaviorals[[currentVendog]]$eqSimExp),envir=localE);
              
              #get evaluated current value
              tmpResCheck=get(currentVendog
                              ,envir = localE
              )[drop=FALSE,model_max_lag+1,];
              
              #restore vendog (may be used in other eqs)
              assign(currentVendog,backupVendog,envir = localE);
              
               #check missings     
              localMissingIndexes=any(! is.finite(tmpResCheck));
              if (localMissingIndexes) 
                stop(paste0('Uncomputable solution or numeric overflow in residual checking of behavioral EQ "',
                            model$behaviorals[[currentVendog]]$eq,
                            '" at year-period ',paste0(
                              normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),frequency)
                              ,collapse = '-'),
                            '.\nCheck equation stability, or lagged time series span.',
                            ifelse(! is.null(model$behaviorals[[currentVendog]]$errorRaw),paste0('\nBehavioral has "ERROR> ',model$behaviorals[[currentVendog]]$errorRaw,'", that lags the whole equation.'),''),
                            ifelse(! is.null(model$behaviorals[[currentVendog]]$pdlRaw),paste0('\nBehavioral has "PDL> ',model$behaviorals[[currentVendog]]$pdlRaw,'", that lags the whole regressor.'),'')
                ));
              
              #append results
              simulation[[currentVendog]]=rbind(simulation[[currentVendog]],tmpResCheck);
              
            } #end cycle behaviorals
            
          },error=function(err){
            stop(paste0(err$message))}
        );#end trycatch
        
        
        tryCatch(
          {   
            #cycle indentities
            for (currentVendog in model_names_identity)
            { #cycle in endogenous
               
              
              
              #backup historical value  
              backupVendog=get(currentVendog,envir = localE);
              
              #if not exogenated eval expression
              if ( ! (  (simIterLoopIdx %in% Exogenize[[currentVendog]])) ) 
                eval(c(parse(text=paste0('LAST__EVAL__EQ="',currentVendog,'";')),model$identities[[currentVendog]]$eqSimExp),envir=localE);
                
              #get evaluated current value
              tmpResCheck=get(currentVendog,envir = localE
              )[drop=FALSE,model_max_lag+1,];
              
              #restore historical  
              assign(currentVendog,backupVendog,envir = localE);
              
              #check missings  
              localMissingIndexes=any(! is.finite(tmpResCheck));
              if (localMissingIndexes) 
                stop(paste0('Uncomputable solution or numeric overflow in residual checking of identity EQ "',model$identities[[currentVendog]]$eqFull,
                            '" at year-period ',paste0(
                              normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),frequency)
                              ,collapse = '-'),
                            '.\nCheck equation stability or lagged time series span.'
                ));
              
              #append results 
              simulation[[currentVendog]]=rbind(simulation[[currentVendog]],tmpResCheck); 
              
            }#for in vendog
            
          },error=function(err){
           stop(paste0(err$message))
          }
        );
        
      }#end is RESCHECK
      
      # convergence loop ------------------------------------------
      
      if (simType != 'RESCHECK') 
      {
        #if FORECAST is requested, value in first iter is the previous simulated one
        #(previuos historical in first period)
        if (simType=='FORECAST') 
          for (tempName in (model_vendog_forecast))
          {
            
			#if any error we save current vendog
            eval(parse(text=paste0('LAST__EVAL__EQ="',tempName,'";')),envir=localE);
            #NEEDED...deal with exogenization...
            if (  
              (simIterLoopIdx %in% Exogenize[[tempName]])) 
              next;
            
            #get vendog values
            tmpValues=get(tempName,envir=localE);
            
            #forecast in first period needs previous historical data that can be missing
            if (simIterLoopIdx==1 && any(! is.finite(tmpValues[model_max_lag,] )))
              stop(callerName,'cannot use FORECAST with endogenous variable "',tempName,'" that has a missing value in its last historical observation prior to TSRANGE.')
            
            #copy previous simulated (historical if first period) to current
            tmpValues[model_max_lag+1,]=tmpValues[drop=FALSE,model_max_lag,];
            
            #update time series
            assign(tempName,tmpValues,envir = localE);
            
          } 
        
        #eval vpre expression
        eval(vpre_expressions[[simIterLoopIdx]]
             ,envir=localE
        );
        
        #flag for convergence
        #flagConvergence=0;
        flagConvergence=ifelse(length(model$vsim)>0,0,1);
        
        #iterations index
        idxIter=0;
        
        #print stuff if required
        if (verbose && verboseSincePeriod<=simIterLoopIdx)
        {
          cat('\nVPRE eval.:\n');
          cat('VPRE vars: ',paste0(model$vpre,collapse=', '),'\n');
          
          
          if (length(ConstantAdjustment)>0)
          {
            cat('CONSTANT ADJUSTMENTS:\n');
            for (tmpV in names(ConstantAdjustment)) 
            {  
              if (length(verboseVars)==0 || tmpV %in% verboseVars)
              {
                cat(paste0(
                  'Prd. ',
                  sprintf("%4i",simIterLoopIdx),', ',
                  paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
                  'Iter. ',
                  sprintf("%4i",idxIter),' ',
                  sprintf("%12s CA",tmpV),' ='))
                
                for (idxR in 1:replica)
                  cat(sprintf("%20.15g",get(paste0(tmpV,'__ADDFACTOR')
                                            ,envir = localE
                  )[model_max_lag+1,idxR]));
                cat('\n');
              }
              
            }
          }
          
          if (length(model$vexog)>0)
          {
            
            cat('VEXOGS:\n');
            for (tmpV in c(model$vexog)) 
            {  
              if (length(verboseVars)==0 || tmpV %in% verboseVars)
              {
                cat(paste0(
                  'Prd. ',
                  sprintf("%4i",simIterLoopIdx),', ',
                  paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
                  'Iter. ',
                  sprintf("%4i",idxIter),' ',
                  sprintf("%15s",tmpV),' ='))
                
                for (idxR in 1:replica)
                  cat(sprintf("%20.15g",get(tmpV
                                            ,envir = localE
                  )[model_max_lag+1,idxR]));
                cat('\n');
              }
              
            }
          }
          cat('VENDOGS:\n');
          for (tmpV in c(model$vpre,model$vsim,model$vpost)) 
          {  
            if (length(verboseVars)==0 || tmpV %in% verboseVars)
            {
              cat(paste0(
                'Prd. ',
                sprintf("%4i",simIterLoopIdx),', ',
                paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
                'Iter. ',
                sprintf("%4i",idxIter),' ',
                sprintf("%15s",tmpV),' ='))
              
              for (idxR in 1:replica)
                cat(sprintf("%20.15g",get(tmpV
                                         ,envir = localE
                )[model_max_lag+1,idxR]));
              cat('\n');
            }
            
          }
        }
        
        #main sim stuff... cycle till convergence or max iter limit
        #WARNING: THIS FOR-CYCLE MUST BE SUPER FAST
        if ( length(model$vsim)>0)
          for (idxIter in 1:get('ITERLIMIT__BIMETS__MODEL',envir=localE))
          { 
            
            #eval vsim expressions
            eval(vsim_expressions[[simIterLoopIdx]],envir=localE);
            
            if (verbose && verboseSincePeriod<=simIterLoopIdx)
            {
              cat('\nVSIM eval.:\n');
              cat('VSIM vars: ',paste0(model$vsim,collapse=', '),'\n');
              if (length(ConstantAdjustment)>0)
              {
                cat('CONSTANT ADJUSTMENTS:\n');
                for (tmpV in names(ConstantAdjustment)) 
                {  
                  if (length(verboseVars)==0 || tmpV %in% verboseVars)
                  {
                    cat(paste0(
                      'Prd. ',
                      sprintf("%4i",simIterLoopIdx),', ',
                      paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
                      'Iter. ',
                      sprintf("%4i",idxIter),' ',
                      sprintf("%12s CA",tmpV),' ='))
                    
                    for (idxR in 1:replica)
                      cat(sprintf("%20.15g",get(paste0(tmpV,'__ADDFACTOR')
                                                ,envir = localE
                      )[model_max_lag+1,idxR]));
                    cat('\n');
                  }
                  
                }
              }
              if (length(model$vexog)>0)
              {
                cat('VEXOGS:\n');
                for (tmpV in c(model$vexog)) 
                {  
                  if (length(verboseVars)==0 || tmpV %in% verboseVars)
                  {
                    cat(paste0(
                      'Prd. ',
                      sprintf("%4i",simIterLoopIdx),', ',
                      paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
                      'Iter. ',
                      sprintf("%4i",idxIter),' ',
                      sprintf("%15s",tmpV),' ='))
                    
                    for (idxR in 1:replica)
                      cat(sprintf("%20.15g",get(tmpV
                                                ,envir = localE
                      )[model_max_lag+1,idxR]));
                    cat('\n');
                  }
                  
                }
              }
              cat('VENDOGS:\n');
              for (tmpV in c(model$vpre,model$vsim,model$vpost)) 
              {  
                if (length(verboseVars)==0 || tmpV %in% verboseVars)
                {
                  cat(paste0(
                    'Prd. ',
                    sprintf("%4i",simIterLoopIdx),', ',
                    paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
                    'Iter. ',
                    sprintf("%4i",idxIter),' ',
                    sprintf("%15s",tmpV),' ='))
                  
                  for (idxR in 1:replica)
                    cat(sprintf("%20.15g",get(tmpV
                                              ,envir = localE
                    )[model_max_lag+1,idxR]));
                  cat('\n');
                }
              }
            }
            
            #eval convergence expression 
            if (eval(convExpr,envir=localE))
            {
              #if convergence...exit loop  
              flagConvergence=1;
              break;
            }
            
          }#main convergence vsim for
        
        #eval vpost expressions
        eval(vpost_expressions[[simIterLoopIdx]],envir=localE);
        
        
        if (verbose && verboseSincePeriod<=simIterLoopIdx)
        {
          cat('\nVPOST eval.:\n');
          cat('VPOST vars: ',paste0(model$vpost,collapse=', '),'\n');
          if (length(ConstantAdjustment)>0)
          {
            cat('CONSTANT ADJUSTMENTS:\n');
            for (tmpV in names(ConstantAdjustment)) 
            {  
              if (length(verboseVars)==0 || tmpV %in% verboseVars)
              {
                cat(paste0(
                  'Prd. ',
                  sprintf("%4i",simIterLoopIdx),', ',
                  paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
                  'Iter. ',
                  sprintf("%4i",idxIter),' ',
                  sprintf("%12s CA",tmpV),' ='))
                
                for (idxR in 1:replica)
                  cat(sprintf("%20.15g",get(paste0(tmpV,'__ADDFACTOR')
                                            ,envir = localE
                  )[model_max_lag+1,idxR]));
                cat('\n');
              }
              
            }
          }
          if (length(model$vexog)>0)
          {
            cat('VEXOGS:\n');
            for (tmpV in c(model$vexog)) 
            {  
              if (length(verboseVars)==0 || tmpV %in% verboseVars)
              {
                cat(paste0(
                  'Prd. ',
                  sprintf("%4i",simIterLoopIdx),', ',
                  paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
                  'Iter. ',
                  sprintf("%4i",idxIter),' ',
                  sprintf("%15s",tmpV),' ='))
                
                for (idxR in 1:replica)
                  cat(sprintf("%20.15g",get(tmpV
                                            ,envir = localE
                  )[model_max_lag+1,idxR]));
                cat('\n');
              }
              
            }
          }
          cat('VENDOGS:\n');
          for (tmpV in c(model$vpre,model$vsim,model$vpost)) 
          { 
            if (length(verboseVars)==0 || tmpV %in% verboseVars)
            {
              cat(paste0(
                'Prd. ',
                sprintf("%4i",simIterLoopIdx),', ',
                paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
                'Iter. ',
                sprintf("%4i",idxIter),' ',
                sprintf("%15s",tmpV),' ='))
              
              for (idxR in 1:replica)
                cat(sprintf("%20.15g",get(tmpV
                                         ,envir = localE
                )[model_max_lag+1,idxR]));
              cat('\n');
            }
          }
        }
        
        #...messages
        if (flagConvergence==1) 
        {
          if (verbose) cat(paste0('Prd. ',sprintf("%4i",simIterLoopIdx),', ',
                                  paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' convergence reached in iter ',idxIter,'\n'));
        } else
        {
          #if (verbose) 
          .MODEL_outputText(outputText = !quietly,paste0('\n',callerName,'warning, at prd ',sprintf("%4i",simIterLoopIdx),' no convergence in ',idxIter,' iterations.\n'));
       }
        
      }#not RESCHECK
      
      
      #store simulated values
      tmpIdx=simIterLoopIdx+model_max_lag;
      
      for (currentVendog in (model_vendog))
      { #cycle in endogenous
        
        #get simulated values
        tmpValue = get(currentVendog,envir=localE)[drop=FALSE,1+model_max_lag,];
        
        #store in simulation list (each row is a sim period)
        if (simType!='RESCHECK')  
          simulation[[currentVendog]]=rbind(simulation[[currentVendog]],tmpValue);
        
        #if dynamic or forecast sim type is requested
        #then store simulated values also as lagged values in ORIGINAL vendogs
        if (simType=='DYNAMIC' || simType=='FORECAST')
        { 
          #get ORIGINAL values
          tmpArr = get(paste0(currentVendog,'__ORIGINAL'),envir=localE);
          
          #insert simulated value
          tmpArr[tmpIdx,] = tmpValue;
          
          #store in ORIGNAL for next iterations
          assign(paste0(currentVendog,'__ORIGINAL'),tmpArr,envir=localE);
          
        }
        
      }#end for save simulate observation
      
      
      # deal with optimize function -------------------------------------
      
      if (OPTIMIZE)
      {
        optFunTSRow=rep(NA,replica-1);
        
        for (idxIE in 1:length(OptimizeFunctions))
        {
          
          #check if we are inside optimize function tsrange
          if ((model_max_lag+simIterLoopIdx) %in% OptimizeFunctions[[idxIE]]$TSRANGE)
          {
            
            
            tryCatch({
              
             
              #in RESCHECK localE does not contain simulated ts
              if (simType=='RESCHECK')
              {
                #backup env
                localE_BK=localE
                
                #store simulated in localE
                for (idxS in names(simulation))
                {
                  tempM=get(idxS,localE)
                  
                  if ( (length((model_max_lag+2-simIterLoopIdx)  : (model_max_lag+1)) != dim(simulation[[idxS]])[1]) ||
                       (dim(tempM)[2] != dim(simulation[[idxS]])[2] ))
                    stop('error in adjusting localE in RESCHECK.')
                    
                  tempM[(model_max_lag+2-simIterLoopIdx)  : (model_max_lag+1),]=simulation[[idxS]]
                  
                  assign(idxS,tempM,localE)
                  
                }
                
              }
              
              
              
              eval(parse(text=paste0('LAST__EVAL__EQ="OptimizeFunctions$',names(OptimizeFunctions)[idxIE],'";')),envir = localE)
              currentFunctionResults=eval(OptimizeFunctions[[idxIE]]$funExpr,envir = localE)
              
              #if (length(currentFunctionResults)==0 ||  ! is.finite(currentFunctionResults)) stop('function must be computable.')
              if (length(currentFunctionResults)==0 ) stop('function must be computable.')
              
              if (length(optFunResults) != length(currentFunctionResults[-1])) stop('unknown error.')
              
              #first unperturbed column must be removed
              optFunResults=optFunResults + currentFunctionResults[-1]
              optFunTSRow=currentFunctionResults[-1]
              
              if (simType=='RESCHECK')
              {
                #restore env
                localE=localE_BK
              }
              
            },error=function(err) stop(paste0('Cannot evaluate "OptimizeFunctions$',
                                              names(OptimizeFunctions)[idxIE],
                                              '", FUNCTION definition "',
                                              OptimizeFunctions[[idxIE]]$FUNCTION,'": ',err$message))
            ,warning=function(warn) cat(paste0('\n',callerName,'warning on evaluating "OptimizeFunctions$',
                                               names(OptimizeFunctions)[idxIE],': ',warn$message),'\n')
            )
            
          }#end if inside opt fun tsrange
          
        }# end cycle opt funs
        
        optFunTSMM=rbind(optFunTSMM,optFunTSRow)
        rownames(optFunTSMM)=NULL
      }
      
    }#main cycle
    
    
    
  },error=function(err)
  {
    #intercept sim algo errors
    
    if (!exists('idxIter')) idxIter=0;
    if (!exists('simIterLoopIdx')) simIterLoopIdx=1;
    if (verbose) 
    {
      cat('\nERROR SNAPSHOT:\n');
      if (length(ConstantAdjustment)>0)
      {
        cat('CONSTANT ADJUSTMENTS:\n');
        for (tmpV in names(ConstantAdjustment)) 
        {  
          if (length(verboseVars)==0 || tmpV %in% verboseVars)
          {
            cat(paste0(
              'Prd. ',
              sprintf("%4i",simIterLoopIdx),', ',
              paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
              'Iter. ',
              sprintf("%4i",idxIter),' ',
              sprintf("%12s CA",tmpV),' ='))
            
            for (idxR in 1:replica)
              cat(sprintf("%20.15g",get(paste0(tmpV,'__ADDFACTOR')
                                        ,envir = localE
              )[model_max_lag+1,idxR]));
            cat('\n');
          }
          
        }
      }
      if (length(model$vexog)>0)
      {
        cat('VEXOGS:\n');
        for (tmpV in c(model$vexog)) 
        {  
          if (length(verboseVars)==0 || tmpV %in% verboseVars)
          {
            cat(paste0(
              'Prd. ',
              sprintf("%4i",simIterLoopIdx),', ',
              paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
              'Iter. ',
              sprintf("%4i",idxIter),' ',
              sprintf("%15s",tmpV),' ='))
            
            for (idxR in 1:replica)
              cat(sprintf("%20.15g",get(tmpV
                                        ,envir = localE
              )[model_max_lag+1,idxR]));
            cat('\n');
          }
          
        }
      }
      cat('VENDOGS:\n');
      for (tmpV in c(model$vpre,model$vsim,model$vpost)) 
      { 
        if (length(verboseVars)==0 || tmpV %in% verboseVars)
        {
          cat(paste0(
            'Prd. ',
            sprintf("%4i",simIterLoopIdx),', ',
            paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=model$frequency),collapse='-'),' ',
            'Iter. ',
            sprintf("%4i",idxIter),' ',
            sprintf("%15s",tmpV),' ='))
          
          for (idxR in 1:replica)
            cat(sprintf("%20.15g",get(tmpV
                                     ,envir = localE
            )[model_max_lag+1,idxR]));
          cat('\n');
          
         
        }
      }
    }
    
    stop(paste0('\n',callerName,'error in simulation of type "',simType,'" at year-period ',
                paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=frequency),collapse='-'),
                ' (simulation period ',simIterLoopIdx,') in iteration ',idxIter,' while evaluating "',
                get('LAST__EVAL__EQ',envir=localE),'".\n',err$message,
                ifelse(quietly==FALSE,'','\nDisable "quietly" in order to get insights on lagged missings that could impact on computation.'),
                ifelse(simType !='RESCHECK','\nTry a "RESCHECK" simulation in order to check initial equations stability.',''),
                ifelse(verbose==TRUE,'','\nEnable "verbose" in order to get more details.'),
                ifelse(STOCHSIMULATE,'\nPlease note that in STOCHSIMULATE() each realization must be computable in order to avoid errors.',''),
                ifelse(OPTIMIZE,'\nPlease note that in OPTIMIZE() each realization must be computable in order to avoid errors.','')
                ))
  });
  
  
  # build MULT MATRIX ------------------------------------------------------------
  
  if (MULTMATRIX==TRUE)
  {
    
    lenT=length(TARGET)
    lenI=length(INSTRUMENT)
    
    MultiplierMatrix=matrix(nrow=lenT*simSteps,
                            ncol=lenI*simSteps);
    
    
    #assign names to column 
    tmpColNames=vector('character',length=simSteps*lenI);
    tmpI=0;
    for (idxIS in 1:simSteps)
    {
      for (idxI in 1:lenI)
      {
        tmpI=tmpI+1;
        tmpColNames[tmpI]=paste0(INSTRUMENT[idxI],'_',idxIS);
      }
    }
    
    #assign names to rows
    tmpRowNames=vector('character',length=simSteps*lenT);
    tmpI=0;
    for (idxTS in 1:simSteps)
    {
      for (idxT in 1:lenT)
      {
        tmpI=tmpI+1;
        tmpRowNames[tmpI]=paste0(TARGET[idxT],'_',idxTS);
      }
    }
    
    colnames(MultiplierMatrix)=tmpColNames;
    rownames(MultiplierMatrix)=tmpRowNames;
    
    tmpIA=list()
    for (idxI in 1:lenI)
    {
      tmpIA[[idxI]]=get(paste0(INSTRUMENT[[idxI]],'__ORIGINAL'),envir=localE);
    }
    
    #build matrix MM
    for (idxT in 1:lenT)
    {
      tmpT=simulation[[TARGET[idxT]]];
      
      for (idxTS in 1:simSteps)
      {
        for (idxI in 1:lenI)
        {
          tmpI=tmpIA[[idxI]];
          
          for (idxIS in 1:simSteps)
          {
            
            
            rowT=idxTS;
            colT=1+(idxI-1)*simSteps+idxIS;
            
            dy=tmpT[rowT,colT]-tmpT[rowT,1];
            
            rowI=model_max_lag+idxIS;
            colI=1+(idxI-1)*simSteps+idxIS;
            
            dx=tmpI[rowI,colI]-tmpI[rowI,1]
            
            
            rowMM=idxT+(idxTS-1)*lenT;
            colMM=idxI+(idxIS-1)*lenI;
            
            
            #dx cannot be 0 because of shocks
            MultiplierMatrix[rowMM,colMM]=ifelse(dy==0,0,dy/dx)
            
            
          }
        }
      }
    }
    
    model$MultiplierMatrix=MultiplierMatrix;
  }
  
  
  .MODEL_outputText(outputText=!quietly,'\n');
 
  # post SIMULATE -------------------------------------------------------------
  
  #convert results to time series if replica==1
  if (length(simulation)>0)
  {
    if (replica==1)
    {
      for (idxSL in 1:length(simulation))
      {
        simulation[[idxSL]]=TSERIES(simulation[[idxSL]],
                                    START=TSRANGE[1:2],
                                    FREQ=frequency,
                                    avoidCompliance = TRUE);
      }
    } else
    {
     
      #if MULTMATRIX keep as simulation the first column (the no-shocked one)
      if (MULTMATRIX)
      {
        #store simulation list as matrix in model
        simulation_MM=simulation;
      
        for (idxSL in 1:length(simulation))
        {
          
          simulation[[idxSL]]=TSERIES(simulation[[idxSL]][,1],
                                    START=TSRANGE[1:2],
                                    FREQ=frequency,
                                    avoidCompliance = TRUE);
        }
      }#end multmatrix
      
      
      if (STOCHSIMULATE)
      {
        
        #store simulation list as matrix in model
        simulation_MM=simulation;
        #stochastic_simulation=simulation;
        stochastic_simulation=list();
        
        #for (idxSL in 1:length(simulation))
        for (idxSL in names(simulation))
        {
          simulation[[idxSL]]=TSERIES(simulation_MM[[idxSL]][,1],
                                      START=TSRANGE[1:2],
                                      FREQ=frequency,
                                      avoidCompliance = TRUE);
          
          #mean all rows but first (not noised)
          stochastic_simulation[[idxSL]]=list();
          stochastic_simulation[[idxSL]]$mean=TSERIES(rowMeans(simulation_MM[[idxSL]][,-1,drop=FALSE]),
                                      START=TSRANGE[1:2],
                                      FREQ=frequency,
                                      avoidCompliance = TRUE); 
           
          #sd all rows but first
          stochastic_simulation[[idxSL]]$sd=TSERIES(apply(simulation_MM[[idxSL]][,-1,drop=FALSE],1,sd),
                                                      START=TSRANGE[1:2],
                                                      FREQ=frequency,
                                                      avoidCompliance = TRUE); 
        }
      }#end stochmiulation
      
      if (OPTIMIZE)
      {
         
          #store simulation list as matrix in model
          simulation_MM=simulation;
          
          optimize=list();
          
          optimize$modelData=model$modelData
          optimize$ConstantAdjustment=ConstantAdjustment
          
          for (idxSL in names(simulation))
          {
            simulation[[idxSL]]=TSERIES(simulation_MM[[idxSL]][,1],
                                        START=TSRANGE[1:2],
                                        FREQ=frequency,
                                        avoidCompliance = TRUE);
           
          }
          
       
        if (length(optColsToBeKept) != length(optFunResults)) stop(callerName,'error length mismatch in optimize function results.')
       
        #deal with uncomputable objective function
        optColsToBeKept = optColsToBeKept & is.finite(optFunResults)
        optColsToBeKeptCount=length(which(optColsToBeKept))
        
        
        #proxy optimize function results
        optFunResultsFiltered=optFunResults
        optFunResultsFiltered[!optColsToBeKept]=NA
        
        
        
        
        #init max and which.max and stats
        optFunMax=NULL
        optFunMaxIdx=NULL
        optFunAve=NULL
        optFunSd=NULL
        
        #init list of opt instruments
        OPT_INSTRUMENT=list()
         
        if ( optColsToBeKeptCount > 0)
        {
          
            if (! quietly) cat(callerName,'',optColsToBeKeptCount,' out of ',replica-1,
                           ' objective function realizations (',format(round(optColsToBeKeptCount*100/(replica-1),0),nsmall=0),'%) are finite',ifelse(length(OptimizeRestrictions)>0,' and verify the provided restrictions',''),'.\n',sep='')
          
          #get max value
          optFunMax=max(optFunResultsFiltered,na.rm = TRUE)
          optFunMaxIdx=which.max(optFunResultsFiltered)
          
          
          optFunAve=base::mean(optFunResultsFiltered,na.rm = TRUE)
          optFunSd=stats::sd(optFunResultsFiltered,na.rm = TRUE)
          
          #get instruments optimus realizations
          for (idxI in names(OptimizeBounds))
          {
            
            
            
            #get maximizing realization for idxI
            if (idxI %in% model_vexog)
            {
              tempINST=get(paste0(idxI,'__ORIGINAL'),envir=localE)
            }
            else if (idxI %in% model_vendog)
            {
              tempINST=get(paste0(idxI,'__ADDFACTOR','__ORIGINAL'),envir=localE)
            } else {
              stop(callerName,'error in "optimize$INSTRUMENT" definition. "',idxI,'" is not a model variable.')
            }
            
            tempINSTcol=tempINST[,optFunMaxIdx+1]
            
            #export opt instrument
            OPT_INSTRUMENT[[idxI]]=TSERIES(tempINSTcol[(model_max_lag+1):length(tempINSTcol)],START=TSRANGE[1:2],FREQ=frequency)
            
            #build modelData and ConstantAdj that allow fun maximum 
           
            
            if (idxI %in% model_vexog)
            {
              optimize$modelData[[idxI]]=TSMERGE(OPT_INSTRUMENT[[idxI]],optimize$modelData[[idxI]],avoidCompliance = TRUE)
            }
            else if (idxI %in% model_vendog)
            {
              
              if (is.null(optimize$ConstantAdjustment[[idxI]]))
              {
                optimize$ConstantAdjustment[[idxI]]=OPT_INSTRUMENT[[idxI]]
                
              } else {
                
                optimize$ConstantAdjustment[[idxI]]=TSMERGE(OPT_INSTRUMENT[[idxI]],optimize$ConstantAdjustment[[idxI]],avoidCompliance = TRUE)
              }
                
            } else {
              stop(callerName,'error in "optimize$modelData" definition. "',idxI,'" is not a model variable.')
            }
            
          }
          
          optFunTS =TSERIES(optFunTSMM[,optFunMaxIdx],
                            START=TSRANGE[1:2],
                            FREQ=frequency,
                            avoidCompliance = TRUE);
          
          
        } else {
            if (! quietly) cat(callerName,'warning, none of the computed realizations are finite and verify the provided restrictions. Try to review inequalities and objective functions definition or try to increase the "StochReplica" argument value. OPTIMIZE() will exit with no solution.\n',sep='')
        }
        
        #export stuff
        #(modelData and ConstantAdjustment already exported)
        optimize$optFunResults=optFunResults
        #optimize$optFunResultsFiltered=optFunResultsFiltered
        optimize$realizationsToKeep=optColsToBeKept
        optimize$optFunMax=optFunMax
        #optimize$optFunMaxIdx=optFunMaxIdx
        optimize$INSTRUMENT=OPT_INSTRUMENT
        optimize$optFunTS=optFunTS
        optimize$optFunSd=optFunSd
        optimize$optFunAve=optFunAve
        
        
        
      }#end optimize
      
    } #end replica > 1
    
    #store simulation list in model
    #model$simulation=simulation;
  }
  
  
  # backfill -----------------------------------------------
  
  #deal with backfill
  
  if (BackFill>0 )
  {
    .MODEL_outputText(outputText = !quietly,paste0('\n',callerName,'"BackFill" enabled.\nSolutions will include up to ',BackFill,' periods (if available) of historical data starting at year-period ',paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]-BackFill),frequency),collapse = '-'),'\n'));
     
   
    #extend simulation
    if (length(simulation)>0) 
      for (idxRC in names(simulation))
      {
        #get original time series
        originalTs=model$modelData[[idxRC]];
        
        #get original start
        originalStart=start(originalTs);
        
        #get max between requested and available observations
        periodsToAdd=min(NUMPERIOD(originalStart,TSRANGE[1:2],frequency),BackFill);
        if (periodsToAdd<1) next;
        
        #get new starting point
        newStart=normalizeYP(c(TSRANGE[1],TSRANGE[2]-periodsToAdd),frequency);
        
        #get extra values
        newStaringIndex=1+NUMPERIOD(originalStart,newStart,frequency);
        extraValues=originalTs[newStaringIndex:(newStaringIndex-1+periodsToAdd)];
        
        #saver result
        simulation[[idxRC]]=TSERIES(c(extraValues,coredata(simulation[[idxRC]]))
                                    ,START=newStart,FREQ=frequency,avoidCompliance = TRUE);
        
      }
    
    #save results
    #model$simulation=simulation;
    
    
  }
  
  #merge past simulations if any
  model$simulation=.combineList(model$simulation,simulation);
  
  if (STOCHSIMULATE)
  {
    model$simulation_MM=.combineList(model$simulation_MM,simulation_MM);
    model$INSTRUMENT_MM=.combineList(model$INSTRUMENT_MM,INSTRUMENT_MM);
    model$stochastic_simulation=.combineList(model$stochastic_simulation,stochastic_simulation);
  }
  
  if (OPTIMIZE)
  {
    model$simulation_MM=.combineList(model$simulation_MM,simulation_MM);
    model$INSTRUMENT_MM=.combineList(model$INSTRUMENT_MM,INSTRUMENT_MM);
    model$optimize=optimize
  }
  
  #export simulation parameters
  model$simulation[['__SIM_PARAMETERS__']]$TSRANGE=TSRANGE;
  model$simulation[['__SIM_PARAMETERS__']]$simType=simType;
  model$simulation[['__SIM_PARAMETERS__']]$simConvergence=simConvergence;
  model$simulation[['__SIM_PARAMETERS__']]$simIterLimit=simIterLimit;
  model$simulation[['__SIM_PARAMETERS__']]$ZeroErrorAC=ZeroErrorAC;
  model$simulation[['__SIM_PARAMETERS__']]$BackFillBackFill
  model$simulation[['__SIM_PARAMETERS__']]$Exogenize=Exogenize_original;
  model$simulation[['__SIM_PARAMETERS__']]$ConstantAdjustment=ConstantAdjustment;
  model$simulation[['__SIM_PARAMETERS__']]$MULTMATRIX=MULTMATRIX;
  model$simulation[['__SIM_PARAMETERS__']]$TARGET=TARGET;
  model$simulation[['__SIM_PARAMETERS__']]$INSTRUMENT=INSTRUMENT;
  model$simulation[['__SIM_PARAMETERS__']]$MM_SHOCK=MM_SHOCK;
  model$simulation[['__SIM_PARAMETERS__']]$RESCHECKeqList=RESCHECKeqList;
  
  if (STOCHSIMULATE)
  {
  model$stochastic_simulation[['__STOCH_SIM_PARAMETERS__']]$STOCHSIMULATE=STOCHSIMULATE;
  model$stochastic_simulation[['__STOCH_SIM_PARAMETERS__']]$StochStructure=StochStructure_original;
  model$stochastic_simulation[['__STOCH_SIM_PARAMETERS__']]$StochReplica=StochReplica;
  model$stochastic_simulation[['__STOCH_SIM_PARAMETERS__']]$StochSeed=StochSeed;
  }
  
  if (OPTIMIZE)
  {
  model$optimize[['__OPT_PARAMETERS__']]$OPTIMIZE=OPTIMIZE;
  model$optimize[['__OPT_PARAMETERS__']]$StochReplica=StochReplica;
  model$optimize[['__OPT_PARAMETERS__']]$StochSeed=StochSeed;
  model$optimize[['__OPT_PARAMETERS__']]$OptimizeBounds=OptimizeBounds;
  model$optimize[['__OPT_PARAMETERS__']]$OptimizeRestrictions=OptimizeRestrictions;
  model$optimize[['__OPT_PARAMETERS__']]$OptimizeFunctions=OptimizeFunctions;
  }
  
  if (MULTMATRIX)
  {.MODEL_outputText(outputText=!quietly,'...MULTMATRIX OK\n');
  } else if (STOCHSIMULATE)
  {.MODEL_outputText(outputText=!quietly,'...STOCHSIMULATE OK\n');
  } else if (OPTIMIZE)
  {.MODEL_outputText(outputText=!quietly,'...OPTIMIZE OK\n');
  } else
  {.MODEL_outputText(outputText=!quietly,'...SIMULATE OK\n');}
  
  
  
  
  return(model);
  
}

# PRINT SUMMARY code ------------------------------------


print.BIMETS_MODEL <- function(x=NULL, ...)
{
  model=x;
  
  if (! inherits(model, "BIMETS_MODEL")) stop("print.BIMETS_MODEL(): model object is not a BIMETS model.");
  
  
  
  cat(paste0('\nBIMETS MODEL\n'));
  cat(paste0('-----------------------------------\n'));
  cat(paste0(sprintf("%-18s",'name:'),model$modelName,'\n'));
  cat(paste0(sprintf("%-18s",'behaviorals:'),(model$totNumEqs),'\n'));
  cat(paste0(sprintf("%-18s",'identities:'),(model$totNumIds),'\n'));
  cat(paste0(sprintf("%-18s",'coefficients:'),(model$eqCoeffNum),'\n'));
  tryCatch({
    .CHECK_MODEL_DATA(model,showWarnings = FALSE);
    cat(paste0(sprintf("%-18s",'model data:'),"OK",'\n'));
  },error=function(e){cat(paste0(sprintf("%-18s",'model data:'),"not OK",'\n',e$message,"\n"));});
  
  totCoeffEstimated=0;
  if (length(model$behaviorals)>0) for (idxEq in 1:length(model$behaviorals))
  {
    totCoeffEstimated=totCoeffEstimated+length(model$behaviorals[[idxEq]]$coefficients);
  }
  
  fullyEstimated=ifelse(totCoeffEstimated<model$eqCoeffNum,FALSE,TRUE);
  cat(paste0(sprintf("%-18s",'fully estimated:'),(fullyEstimated),'\n'));
  
  
  simulated=FALSE;
  if (! is.null(model$simulation))
  {
    namesSim=names(model$simulation);
    namesSim=namesSim[-which(namesSim=='__SIM_PARAMETERS__')];
    if (length(base::setdiff(model$vendog,namesSim))==0)
      simulated=TRUE;
  }
  cat(paste0(sprintf("%-18s",'simulated:'),(simulated),'\n'));
  
  
  
}

summary.BIMETS_MODEL <- function(object,...)
{
  return(print.BIMETS_MODEL(object,...)); 
}


