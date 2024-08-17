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
# @authors: ANDREA LUCIANI 
#
# @copyright: Bank of Italy
#
# @license: GPL-3 - GNU GENERAL PUBLIC LICENSE - Version 3
#
#############################################################################

# internal core funs -----------------------------------------------------------

#add an integer to var index: varName[i,] -> varName[i+valueToAdd[j],]
#for each j in valueToAdd
.addToIndexInString <- function (inputS='',valueToAdd=0,transformLeads=FALSE,baseValue=0)
{
  #init tmp vars
  openIdxs=c()
  closeIdxs=c() 
  flagInsideSquared=FALSE
  
  #cycle in chars and get positions of squared [ and ]
  for (idxS in 1:nchar(inputS))
  {
    #get current char
    tmpChar=substr(inputS,idxS,idxS) 
    
    if (tmpChar=='[')
    {
      openIdxs=c(openIdxs,idxS) 
      flagInsideSquared=TRUE
    }
    
    if (tmpChar==']')
    {
      flagInsideSquared=FALSE
    }
    
    if (flagInsideSquared && tmpChar==',')
    {
      closeIdxs=c(closeIdxs,idxS) 
    }
    
  }
  
  if (length(openIdxs)==0) return(inputS) 
  
  tmpP=1 
  length_value=length(valueToAdd)
  outS=vector('character',length_value) 
  
  tryCatch(
    {
      #cycle every index []
      for (idxI in 1:length(openIdxs))
      { 
        #get index str
        tmpS=substr(inputS,openIdxs[idxI]+1,closeIdxs[idxI]-1) 
      
        #get index as integer
        currentI_base=as.integer(tmpS) 
        
        if (! is.finite(currentI_base)) stop() 
        
        tempSubString=substr(inputS,tmpP,openIdxs[idxI]-1)
        
        #add value to current index
        for (idxV in 1:length_value)
        {
          currentI=currentI_base+valueToAdd[idxV] 
        
          #replace index in string
          if (transformLeads && currentI>baseValue)
          {
            outS[idxV]=paste0(outS[idxV]
                              ,tempSubString
                              ,paste0('__LEAD__',currentI-baseValue,'[',baseValue)
                              )
          } else {
            outS[idxV]=paste0(outS[idxV]
                              ,tempSubString
                              ,paste0('[',currentI)
                              )
          }
        }
        
        tmpP=closeIdxs[idxI] 
      }
    },
    error=function(e)
    {stop(paste0('.addToIndexInString(): cannot evaluate index "',tmpS,'" ',e$message))}
  ) 
  
  #...create last output
  for (idxV in 1:length_value)
    outS[idxV]=paste0(outS[idxV]
              ,substr(inputS,tmpP,nchar(inputS)))
  
  return(outS) 
} 

#add _ADDFACTOR to a list of var names, e.g. myVAR -> myVAR_ADDFACTOR
.appendAFtoEndVars <- function (inputS='',vName=c(),AF='__ADDFACTOR',...)
{
  #replace varName with varName_ADDFACTOR, if varName is not a fun call
  #\\b is required to match whole word, e.g. abc != abc_def
  for (idxV in 1:length(vName))
    inputS=gsub(paste0('\\b(',
                     vName[idxV],
                     ')\\b($|[^\\(])'),
              paste0('\\1\\',AF,'\\2'),
              inputS)
  
  return(inputS) 
  
}

#add indexes to a list of var names, e.g. myVAR -> myVAR[indexN,]
.appendIndexToVars <- function (inputS='',vName=c(),indexN='',...)
{
  #replace varName with varName[indexN,], if varName is not a fun call
  for (idxV in 1:length(vName))
    inputS=gsub(paste0('\\b(',
                     vName[idxV],
                     ')\\b($|[^\\(])'),
              paste0('\\1\\[',indexN,',\\]\\2'),
              inputS)
  
  return(inputS) 
  
}

#used in PDL
.bm_tartaglia_pdl <- function(degree=1)
{
  if (degree==0) 
  {
    return(c(1,-1))
  } else 
  {
    levelUpResult=.bm_tartaglia_pdl(degree-1)     
    outF=levelUpResult[1] 
    
    for (idx in 1:(length(levelUpResult)-1))
    {
      outF=c(outF,abs(levelUpResult[idx])+abs(levelUpResult[idx+1]))
    }
    
    outF=c(outF,1) 
    
    for (idx in 1:length(outF))
    {
      if (idx %% 2 ==0) outF[idx]=-outF[idx] 
    } 
    
    return(outF) 
  }
}

#build reordering blocks from the output of the 
#Tarjan #strongly connected components algo
.buildBlocks <- function(incidence_matrix)
{
  localT=.TarjanBlocksExtraction(incidence_matrix)
  
  rownames=rownames(incidence_matrix)
  
  vpre=c()
  vblocks=list()
  
  #strongly connected nodes are inverse ordered
  #in localT
  for (idx in length(localT$lists):1)
  {
    #if single connected component put in vpre
    #else we are in a simultaneous group
    if (length(localT$lists[[idx]])==1) 
    {
      vpre=c(vpre,localT$lists[[idx]])
    } else break
  }
  
  #if not in a acyclic graph
  if (!(length(vpre)>0 && idx==1) )
  {
    idxBlock=0
    
    for (idx2 in idx:1)
    {
      #new simultaneous group for a new block
      if (length(localT$lists[[idx2]])>1)
      {
        idxBlock=idxBlock+1
        vblocks[[idxBlock]]=list()
        vblocks[[idxBlock]]$vsim=localT$lists[[idx2]]
        vblocks[[idxBlock]]$vpost=c()
      } else
      {
        #add to vpost for current block
        vblocks[[idxBlock]]$vpost=c(vblocks[[idxBlock]]$vpost,localT$lists[[idx2]])
      }
    }
  }
 
  if (length(vblocks)>0)
    for (idxB in 1:length(vblocks))
    {
      #convert indexes to var names
      if (length(vblocks[[idxB]]$vpost)>0)
        vblocks[[idxB]]$vpost=rownames[vblocks[[idxB]]$vpost]
      
      #split vsim vs vfeed in current block using related zone in incidence matrix
      tempList=.reorderEquations(incidence_matrix[vblocks[[idxB]]$vsim,vblocks[[idxB]]$vsim] )
      
      #export
      vblocks[[idxB]]$vsim=tempList$vsim
      vblocks[[idxB]]$vfeed=tempList$vfeed
      
    }
  
  #convert indexes to name
  if (length(vpre)>0) vpre=rownames[vpre]
  
  return(list(vpre=vpre,vblocks=vblocks))
  
}

#check text is single parsable expression
.checkExpression <- function(inputS)
{
  if (is.null(inputS)) return(FALSE)
  if (length(inputS)!=1) return(FALSE)
  if (! is.character(inputS)) return(FALSE)
  if (nchar(inputS)==0) return(FALSE)
  
  flagX=TRUE 
  tryCatch({parse(text=inputS);flagX=FALSE;},error=function(e){})
  if (flagX) return(FALSE) 
  
  return(TRUE)
}

#chech regressor in behavioral eq
#must not contain operators outside parenthesis
.checkExpressionIsAtomic <- function(inputS=NULL,...)
{
  if (is.null(inputS)) return(FALSE)
  if (length(inputS)!=1) return(FALSE)
  if (! is.character(inputS)) return(FALSE)
  if (nchar(inputS)==0) return(FALSE)
  
  #operators to be checked for atomicity
  dualOperators=c('\\+','\\-','<','>','<=','>=','!=','==','&','&&','\\|','\\|\\|') 
  DOcollapse=paste0('(',paste(dualOperators,collapse = '|'),')') 
  
  #check operator not in start position
  if (grepl(paste0('^',DOcollapse),inputS)) return(FALSE)
  
  #check operator not in end position
  if (grepl(paste0(DOcollapse,'$'),inputS)) return(FALSE)
  
  #check parentheses
  inputSnc=nchar(inputS)
  parCountArray=rep(0,inputSnc)
  
  #each "(" is +1 each ")" is -1
  tmpCounter=0 
  for (idx in 1:inputSnc)
  {
    actualChar=substring(inputS,idx,idx)
    if (actualChar=="(") tmpCounter=tmpCounter+1
    if (actualChar==")") tmpCounter=tmpCounter-1
   
    parCountArray[idx]=tmpCounter 
  }
 
  #get position of operators
  DOpos=as.numeric(gregexpr(DOcollapse,inputS)[[1]])
  
  #operators cannot be out of parentheses
  if (any(DOpos>0) && any(parCountArray[DOpos]==0)) return(FALSE)
  
  #check expression 
  if (! .checkExpression(inputS)) return(FALSE)
  
  return(TRUE)
}

#check bimets version a model has been built with
#if different from current bimets version issues a warning
.checkModelBimetsVersion <- function(model,callerName='.checkModelBimetsVersion')
{
  if (is.null(model$bimets_version)) # || model$bimets_version != getOption('BIMETS_VERSION'))
    warning(paste0(callerName,'(): warning, current model "',model$modelName,'" has been built with an outdated BIMETS version. Please recreate the model via the LOAD_MODEL() function.\n')) 
  
  return(NULL)
}

#combine list by names
.combineList <- function(list1=NULL,list2=NULL)
{
  if (is.null(list1)) return(list2) 
  
  if ((!is.list(list1)) || (!is.list(list2))) stop('.combineList(): inputs must be of class list().')  
  
  outList=list1 
  
  matchedNames=match(names(list2), names(list1))
  commonIndexes=which(!is.na(matchedNames))
  
  if (length(commonIndexes) > 0)
  {
    #give priority to list2 in matched names
    matchedNames=matchedNames[!is.na(matchedNames)]
    outList[matchedNames] = list2[commonIndexes]
    
    #append elements of 'list2' with unmatched names
    outList=c(outList, list2[-commonIndexes])
  }
  else
  {
    outList=c(outList, list2)
  }
  
  return(outList)
}

#move vars from local env localE into global, useful in debugging
.copyLocalEnvInToGlobal <- function(sourceE=NULL,destE=.GlobalEnv,...)
{
  if (is.null(sourceE)) 
  {
    cat('Please provide a valid environment.\n') 
  } else {
    lapply(names(sourceE),function(n){assign(n,get(n,envir=sourceE),envir=destE) }) 
  }
}

#return arguments as text (2 or more) and comma positions for the first 
#occurrence of function fName in string inputS
.explodeFunctionCall <- function (inputS='',fName='',...)
{ 
  outL=list() 
  
  #check fun name exists in string
  if (substr(fName,1,1)=='.')
  {
    fNameEx=paste0('\\.\\b(',gsub("\\.","",fName),')\\b\\(') 
  } else {
    #must move dot out of \\b
    fNameEx=paste0('\\b(',fName,')\\b\\(') 
  }
  
  #check function call exists in string
  #regexpr returns only first occurrence
  firstLoc=regexpr(fNameEx,inputS) 
  
  #if none then exit
  if (firstLoc==-1) return(outL) 
  
  #remove attributes from regex results
  attributes(firstLoc)=NULL 
  
  #init tmp vars... consider we start whit one "("
  openP=1   #for round () 
  openSP=0  #for squared [] 
  
  #outputs
  commaPos=c() 
  args=c() 
  argsStart=firstLoc+nchar(fName)+1
  argsEnd=c()
  
  #cycle in chars
  for (idxS in (argsStart):nchar(inputS))
  {  
    #get char
    tmpChar=substr(inputS,idxS,idxS) 
    
    if (tmpChar=='(')
    {
      openP=openP+1 
    } else if (tmpChar==')')
    {
      openP=openP-1 
      if (openP==0 && openSP==0) 
      {
        #we are exiting function call
        argsEnd=idxS-1 
        break 
      }
    } else if (tmpChar==',' && openP==1 && openSP==0)
    {
      #we are in comma position separating fName arguments
      commaPos=c(commaPos,idxS) 
      
    } else if (tmpChar=='[')
    {
      #we need to account also squared []
      openSP=openSP+1 
      
    } else if (tmpChar==']')
    {
      openSP=openSP-1
    }
    
  }
  
  #get arguments
  argsCount=length(commaPos)+1
  
  if (argsCount==1)
  {
    args=substr(inputS,argsStart,argsEnd)
    
  } else
    for (idxA in 1:argsCount)
    {
      #first argument
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
  args=gsub("\\s*", "", args) 
  
  #results
  outL$firstLoc=firstLoc
  outL$commaPos=commaPos
  outL$argsStart=argsStart
  outL$argsEnd=argsEnd
  outL$argsCount=argsCount
  outL$args=args
  
  return(outL)
}

#transform first MAVE in inputS into exploded expression with related lags
.explodeSingleMAVE <- function (inputS='',...)
{

  #init tmp var
  outL=list()
  outL$output=inputS 
  outL$flag=FALSE 
  
  outSE=.explodeFunctionCall(inputS,'.MODEL_MAVE') 
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0 
  
  #if not specified tslag=1
  if (outSE$argsCount==1) 
  {
    tsLag=1 
    
  } else
  {
    tryCatch({
      
      tsLag=as.integer(outSE$args[2]) 
      
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>0)) stop()
    },error=function(e){stop(paste0('.explodeSingleMAVE(): window size must be a positive integer. ',e$message))}
    )
  }
  
  outTS=outSE$args[1] 
  tempArg=outTS
  firstLoc=outSE$firstLoc
  argsEnd=outSE$argsEnd
  
  #MAVE(varName[a,],b) -> (0+varName[a,]+varName[a-1,]+...+varName[a-b,])/b
  if (tsLag>1)
  {
    outADTS=.addToIndexInString(tempArg,1-2:tsLag)
      for (idxMA in 1:length(outADTS))
        outTS=paste0(outTS,'+',outADTS[idxMA]) 
  }
  
  ncIS=nchar(inputS)
  
  if (firstLoc>1)   header=substr(inputS,1,firstLoc-1)
  else              header=''
  
  if (argsEnd<ncIS-1)   trailer=substr(inputS,argsEnd+2,ncIS)
  else                  trailer=''
  
  outL$output=paste0(header
                     ,'((',
                     outTS,
                     ')/',tsLag,')',
                     trailer)
  
  #store flag if mod has been made
  outL$flag=TRUE 
  #outL$tsLag=tsLag 
  
  return(outL)
}

#explode all MAVE()
.explodeMAVE <- function (inputS='',...)
{
  tryCatch({
    
    tmpL=.explodeSingleMAVE(inputS) 
    
    #while we have more MAVE()
    while(tmpL$flag)
    {
      tmpL=.explodeSingleMAVE(tmpL$output) 
    }
  },error=function(e){stop(paste0('.explodeMAVE(): ',e$message))})
  
  return(tmpL) 
}

#transform first MTOT in inputS into exploded expression with related lags
.explodeSingleMTOT <- function (inputS='',...)
{
 
  #init tmp var
  outL=list()
  outL$output=inputS 
  outL$flag=FALSE 
  
  outSE=.explodeFunctionCall(inputS,'.MODEL_MTOT') 
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0 
  
  #if not specified tslag=1
  if (outSE$argsCount==1) 
  {
    tsLag=1 
    
  } else
  {
    tryCatch({
       
      tsLag=as.integer(outSE$args[2]) 
      
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>0)) stop()
      
    },error=function(e){stop(paste0('.explodeSingleMTOT(): window size must be a positive integer. ',e$message))}
    )
  }
  
  outTS=outSE$args[1] 
  tempArg=outTS
  firstLoc=outSE$firstLoc
  argsEnd=outSE$argsEnd
  
  #MTOT(varName[a,],b) -> 0+varName[a,]+varName[a-1,]+...+varName[a-b,]
  if (tsLag>1)
  {
    outADTS=.addToIndexInString(tempArg,1-2:tsLag)
    for (idxMA in 1:length(outADTS))
      outTS=paste0(outTS,'+',outADTS[idxMA]) 
  }
  
  ncIS=nchar(inputS)
  
  if (firstLoc>1) header=substr(inputS,1,firstLoc-1)
  else header=''
    
  if (argsEnd<ncIS-1) trailer=substr(inputS,argsEnd+2,ncIS)
  else trailer=''
    
  outL$output=paste0(header
                     ,'((',
                     outTS,
                     '))',
                     trailer) 
  
  #store flag if mod has been made
  outL$flag=TRUE 
  #outL$tsLag=tsLag 
  
  return(outL)
}

#explode all MTOT()
.explodeMTOT <- function (inputS='',...)
{ 
  tryCatch({
    
    tmpL=.explodeSingleMTOT(inputS) 
    
    #while we have more MTOT()
    while(tmpL$flag)
    {
      tmpL=.explodeSingleMTOT(tmpL$output) 
    }
  },error=function(e){stop(paste0('.explodeMTOT(): ',e$message))})
  return(tmpL) 
  
}

#transform first TSLAG in inputS into exploded expression with related lags
.explodeSingleTSLAG <- function (inputS='',...)
{
  #init tmp vars
  outL=list()
  outL$output=inputS 
  outL$flag=FALSE  
  
  outSE=.explodeFunctionCall(inputS,'.MODEL_TSLAG') 
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0 
  
  #no lag provided so it is 1
  if (outSE$argsCount==1) 
  {
    tsLag=1 
  } else
  { #otherwise eval
    tryCatch({
      
      tsLag=as.integer(outSE$args[2])
      
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>=0)) stop()
    },error=function(e){stop(paste0('.explodeSingleTSLAG(): lag value must be a non-negative integer. ',e$message))}
    )
  }
  
  firstLoc=outSE$firstLoc
  argsEnd=outSE$argsEnd
  
  #replace varName[a,] -> varName[a-lag,]
  ncIS=nchar(inputS)
  
  if (firstLoc>1) header=substr(inputS,1,firstLoc-1)
  else header=''
  
  if (argsEnd<ncIS-1) trailer=substr(inputS,argsEnd+2,ncIS)
  else trailer=''
    
  outL$output=paste0(header
                     ,'(',
                     .addToIndexInString(outSE$args[1],-tsLag),
                     ')',
                     trailer) 
  
  #store flag if mod has been made
  outL$flag=TRUE 
  
  return(outL)
}

#explode all TSLAG()
.explodeTSLAG <- function (inputS='',...)
{ 
  tryCatch({
    
    #explode first TSLAG
    tmpL=.explodeSingleTSLAG(inputS) 
    
    #while exist a TSLAG()...
    while(tmpL$flag)
    {
      tmpL=.explodeSingleTSLAG(tmpL$output) 
    }
  },error=function(e){stop(paste0('.explodeTSLAG(): ',e$message))})
  return(tmpL) 
  
}

#transform first TSLEAD in inputS into exploded expression with related lags
.explodeSingleTSLEAD <- function (inputS='',...)
{
  
  #init tmp vars
  outL=list()
  outL$output=inputS 
  outL$flag=FALSE  
  
  outSE=.explodeFunctionCall(inputS,'.MODEL_TSLEAD') 
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLead=0 
  
  #no lag provided so it is 1
  if (outSE$argsCount==1) 
  {
    tsLead=1 
  } else
  { #otherwise eval
    tryCatch({
      
      tsLead=as.integer(outSE$args[2])
      
      if (! ( is.finite(tsLead) && tsLead %%1 ==0 && tsLead>=0)) stop()
    },error=function(e){stop(paste0('.explodeSingleTSLEAD(): lead value must be a non-negative integer. ',e$message))}
    )
  }
  
  firstLoc=outSE$firstLoc
  argsEnd=outSE$argsEnd
  
  #replace varName[a,] -> varName[a-lag,]
  ncIS=nchar(inputS)
  
  if (firstLoc>1) header=substr(inputS,1,firstLoc-1)
  else header=''
  
  if (argsEnd<ncIS-1) trailer=substr(inputS,argsEnd+2,ncIS)
  else trailer=''
  
  outL$output=paste0(header
                     ,'(',
                     .addToIndexInString(outSE$args[1],tsLead),
                     ')',
                     trailer) 
  
  #store flag if mod has been made
  outL$flag=TRUE  
  
  return(outL)
}

#explode all TSLEAD()
.explodeTSLEAD <- function (inputS='',...)
{ 
  
  tryCatch({
    
    #explode first TSLEAD
    tmpL=.explodeSingleTSLEAD(inputS) 
    
    #while exist a TSLEAD()...
    while(tmpL$flag)
    {
      tmpL=.explodeSingleTSLEAD(tmpL$output) 
    }
  },error=function(e){stop(paste0('.explodeTSLEAD(): ',e$message))})
  return(tmpL) 
  
}

#transform first DELTA in inputS into exploded expression with related lags
.explodeSingleTSDELTA <- function (inputS='',...)
{
  #init tmp var
  outL=list()
  outL$output=inputS 
  outL$flag=FALSE 
  
  outSE=.explodeFunctionCall(inputS,'.MODEL_TSDELTA') 
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0 
  
  #if no specified tslag=1
  if (outSE$argsCount==1) 
  {
    tsLag=1 
  } else
  {
    tryCatch({
      
      tsLag=as.integer(outSE$args[2])
      
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>0)) stop()
    },error=function(e){stop(paste0('.explodeSingleTSDELTA(): DELTA value must be a positive integer. ',e$message))}
    )
  }
  
  tempArg=outSE$args[1] 
  firstLoc=outSE$firstLoc
  argsEnd=outSE$argsEnd
  ncIS=nchar(inputS)
  
  if (firstLoc>1) header=substr(inputS,1,firstLoc-1)
  else header=''
  
  if (argsEnd<ncIS-1) trailer=substr(inputS,argsEnd+2,ncIS)
  else trailer=''
    
  #DELTA(varName[a,],b)=((varName[a,])-(varName[a-b,]))
  outL$output=paste0(header
                     ,'((',
                     tempArg,
                     ')-(',
                     .addToIndexInString(tempArg,-tsLag),
                     '))',
                     trailer) 
  
  #store flag if mod has been made
  outL$flag=TRUE   
  
  return(outL)
}

#explode all DELTA()
.explodeTSDELTA <- function (inputS='',...)
{ 
  tryCatch({
    
    tmpL=.explodeSingleTSDELTA(inputS) 
    
    #while we have more DELTA()
    while(tmpL$flag)
    { 
      tmpL=.explodeSingleTSDELTA(tmpL$output) 
    }
  },error=function(e){stop(paste0('.explodeTSDELTA(): ',e$message))})
  
  return(tmpL) 
  
}

#transform first DELTALOG in inputS into exploded expression with related lags
.explodeSingleTSDELTALOG <- function (inputS='',...)
{
  #init tmp var
  outL=list()
  outL$output=inputS 
  outL$flag=FALSE 
  
  outSE=.explodeFunctionCall(inputS,'.MODEL_TSDELTALOG') 
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0 
  
  #if no specified tslag=1
  if (outSE$argsCount==1) 
  {
    tsLag=1 
  } else
  {
    tryCatch({
       
      tsLag=as.integer(outSE$args[2])
      
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>0)) stop()
    },error=function(e){stop(paste0('.explodeSingleTSDELTALOG(): DELTALOG value must be a positive integer. ',e$message))}
    )
  }
  
  tempArg=outSE$args[1] 
  firstLoc=outSE$firstLoc
  argsEnd=outSE$argsEnd
  ncIS=nchar(inputS)
  
  if (firstLoc>1) header=substr(inputS,1,firstLoc-1)
  else header=''
    
  if (argsEnd<ncIS-1) trailer=substr(inputS,argsEnd+2,ncIS)
  else trailer=''
    
  #DELTALOG(varName[a,],b)=(log((varName[a,])/(varName[a-b,])))
  outL$output=paste0(header
                     ,'(log((',
                     tempArg,
                     ')/(',
                     .addToIndexInString(tempArg,-tsLag),
                     ')))',
                     trailer) 
  
  #store flag if mod has been made
  outL$flag=TRUE  
  
  return(outL)
}

#explode all DELTALOG()
.explodeTSDELTALOG <- function (inputS='',...)
{ 
  tryCatch({
    
    tmpL=.explodeSingleTSDELTALOG(inputS) 
    
    #while we have more DELTALOG()
    while(tmpL$flag)
    { 
      tmpL=.explodeSingleTSDELTALOG(tmpL$output) 
    }
  },error=function(e){stop(paste0('.explodeTSDELTALOG(): ',e$message))})
  
  return(tmpL) 
  
}

#transform first DELTAP in inputS into exploded expression with related lags
.explodeSingleTSDELTAP <- function (inputS='',...)
{
  #init tmp var
  outL=list()
  outL$output=inputS 
  outL$flag=FALSE  
  
  outSE=.explodeFunctionCall(inputS,'.MODEL_TSDELTAP') 
  
  #no fun call so exit
  if (length(outSE)==0) return(outL)
  
  tsLag=0 
  
  #if no specified lag=1
  if (outSE$argsCount==1) 
  {
    tsLag=1 
  } else
  {
    tryCatch({
       
      tsLag=as.integer(outSE$args[2])
      
      if (! ( is.finite(tsLag) && tsLag %%1 ==0 && tsLag>0)) stop()
    },error=function(e){stop(paste0('.explodeSingleTSDELTAP(): DELTAP value must be a positive integer. ',e$message))}
    )
  }
  
  tempArg=outSE$args[1] 
  firstLoc=outSE$firstLoc
  argsEnd=outSE$argsEnd
  ncIS=nchar(inputS)
  tempLagged=.addToIndexInString(tempArg,-tsLag)
  
  if (firstLoc>1) header=substr(inputS,1,firstLoc-1)
  else header=''
    
  if (argsEnd<ncIS-1) trailer=substr(inputS,argsEnd+2,ncIS)
  else trailer=''
    
  #DELTAP(varName[a,],b)=(100*((varName[a,])-(varName[a-b,]))/(varName[a-b,]))
  outL$output=paste0(header
                     ,'(100*((',
                     tempArg,
                     ')-(',
                     tempLagged,
                     '))/(',tempLagged,'))',
                     trailer
  ) 
  
  #store flag if mod has been made
  outL$flag=TRUE  
  
  return(outL)
}

#explode all DELTAP()
.explodeTSDELTAP <- function (inputS='',...)
{ 
  tryCatch({
    
    tmpL=.explodeSingleTSDELTAP(inputS) 
    
    #while we have more DELTAP()
    while(tmpL$flag)
    { 
      tmpL=.explodeSingleTSDELTAP(tmpL$output) 
    }
  },error=function(e){stop(paste0('.explodeTSDELTAP(): ',e$message))})
  
  return(tmpL) 
  
}

#extract function names from string
.extractFunsNames <- function(inputS=NULL)
{
  if (is.null(inputS)) return(character(0))
  
  out=regmatches(inputS,gregexpr('(\\w+)\\(',inputS))
  out=gsub('\\(','',out[[1]])
  return(out)
}

#return vName[x,] in inputS that has x==baseIndex
#baseIndex default to 0, i.e. y[0,]=...
.getIncidenceVendogs <- function (currentVendog,inputS='',vName=c(),baseIndex=0,...)
{
  if (length(vName)==0) return(NULL) 
 
  #remove "currentVendog=[indexN,]" and ".MODEL_VIF(currentVendog[indexN,]"
  #\\b is required to match whole word, e.g. abc != abc_def
  inputS=gsub(paste0('\\b(',currentVendog,')\\b\\[\\s*',baseIndex,'\\s*,\\s*\\]\\s*\\='),'',inputS) 
  inputS=gsub(paste0('.MODEL_VIF\\(\\s*',currentVendog,'\\[\\s*',baseIndex,'\\s*,\\s*\\]\\s*\\,'),'\\(',inputS) 
   
  trailer=paste0('\\[',baseIndex,',')
  outC=regmatches(inputS,gregexpr(paste0('(\\w+)',trailer),inputS))
  outC=gsub(trailer,'',outC[[1]])
  outC=outC[outC %in% vName]
  
  return(outC)
  
}

#retrieve minimum lag / maximium lead for all lagged/leaded vars
.getLowerLagAndHigherLead <- function (inputS='',...)
{ 
  outL=+Inf 
  outH=-Inf 
  
  openIdxs=c()
  closeIdxs=c() 
  
  #cycle in chars and get sbracket positions
  for (idxS in 1:nchar(inputS))
  {
    #get current char
    tmpChar=substr(inputS,idxS,idxS) 
    
    if (tmpChar=='[')
    {
      openIdxs=c(openIdxs,idxS) 
    }
    
    if (tmpChar==']')
    {
      closeIdxs=c(closeIdxs,idxS) 
    }
  }
  
  if (length(openIdxs) ==0) return(0) 
   
  tryCatch(
    {
      #cycle in indexes
      for (idxI in 1:length(openIdxs))
      {
        #get index as string
        tmpS=substr(inputS,openIdxs[idxI]+1,closeIdxs[idxI]-1) 
        tmpS=strsplit(tmpS,',')[[1]][1]
        
        #get index as numeric 
        currentI=as.integer(tmpS)
        
        if (! is.finite(currentI)) stop() 
        
        #get minimum
        outL=min(outL,currentI) 
        
        #get maximum
        outH=max(outH,currentI) 
        
      }},
    error=function(e)
    {stop(paste0('.getLowerLagAndHigherLead(): cannot evaluate index "',tmpS,'"'))}
    
  )
  
  out=list()
  
  out$outL=outL
  out$outH=outH
  
  return(out)
  
}

#lead a simulation expression eqSimText by 1:(simSteps-1) period starting from base value 
#and transform leaded references, e.g. var[i,] -> to var__LEAD__j[0,], i>simSteps, j <- i-simSteps
.leadEqSim <- function(currentVendog,eqSimText,baseValue,simSteps)
{ 
  outSimList=list()
 
  tempATIS=.addToIndexInString(eqSimText,1:(simSteps-1), TRUE, baseValue)
  
  outS=paste0(currentVendog,'__LEAD__')
  
  for (idxSS in 1:length(tempATIS))
  { 
    outSimList[[paste0(outS,idxSS)]]=tempATIS[idxSS]
  }
  
  return(outSimList)
}

#convert funs names in model
.MODEL_MOD_FUNC_NAMES <- function(inputS=NULL)
{
    inputS=gsub('TSLEAD\\(','LEAD(',inputS,ignore.case=TRUE) 
    inputS=gsub('LEAD\\(','.MODEL_TSLEAD(',inputS,ignore.case=TRUE) 
    inputS=gsub('TSLAG\\(','LAG(',inputS,ignore.case=TRUE) 
    inputS=gsub('LAG\\(','.MODEL_TSLAG(',inputS,ignore.case=TRUE) 
    inputS=gsub('TSDELTA\\(','.MODEL_TSDELTA(',inputS,ignore.case=TRUE) 
    inputS=gsub('DEL\\(','.MODEL_TSDELTA(',inputS,ignore.case=TRUE) 
    inputS=gsub('LOG\\(','log(',inputS,ignore.case=TRUE) 
    inputS=gsub('EXP\\(','exp(',inputS,ignore.case=TRUE) 
    inputS=gsub('ABS\\(','abs(',inputS,ignore.case=TRUE) 
    inputS=gsub('MOVAVG\\(','MAVE(',inputS,ignore.case=TRUE) 
    inputS=gsub('MAVE\\(','.MODEL_MAVE(',inputS,ignore.case=TRUE) 
    inputS=gsub('MOVSUM\\(','MTOT(',inputS,ignore.case=TRUE) 
    inputS=gsub('MTOT\\(','.MODEL_MTOT(',inputS,ignore.case=TRUE) 
    inputS=gsub('TSDELTAP\\(','.MODEL_TSDELTAP(',inputS,ignore.case=TRUE) 
    inputS=gsub('TSDELTALOG\\(','.MODEL_TSDELTALOG(',inputS,ignore.case=TRUE) 
  
  return(inputS) 
}

#wrapper for base::cat
.MODEL_outputText <- function(...,sep=' ',outputText=TRUE)
{
  if (outputText) cat(...,sep=sep) 
  return(NULL) 
}

#used in Estimate
.MODEL_MAVE <- function(x=NULL,L=1)
{
  return(MAVE(x=x, L=L, avoidCompliance=TRUE))  
}

#used in Estimate
.MODEL_MTOT <- function(x=NULL,L=1)
{
  return(MTOT(x=x, L=L, avoidCompliance=TRUE))  
}

#used in Estimate
.MODEL_TSDELTA <- function(x=NULL,L=1)
{
  return(TSDELTA(x=x, L=L, avoidCompliance=TRUE)) 
}

#used in Estimate
.MODEL_TSDELTALOG <- function(x=NULL,L=1)
{
  return(TSDELTALOG(x=x, L=L, avoidCompliance=TRUE)) 
}

#used in Estimate
.MODEL_TSDELTAP <- function(x=NULL,L=1)
{
  return(TSDELTAP(x=x, L=L, avoidCompliance=TRUE)) 
}

#used in Estimate
.MODEL_TSLAG <- function(ts,lag=1)
{ 
  return(TSLAG(ts, lag, avoidCompliance=TRUE, verbose=FALSE)) 
}

#used in Estimate
.MODEL_TSLEAD <- function(ts,lead=1)
{ 
  return(TSLEAD(ts, lead, avoidCompliance=TRUE, verbose=FALSE)) 
}

#evaluate IF> condition in simulation of identities
#e.g. vendog <- 4 if vexog>0 ==> vendog <- MODEL_VIF(vendog,vexog>0,4)
.MODEL_VIF <- function(vendog,index_which=TRUE,values)
{
  
  if (! all(is.finite(index_which))) stop('Uncomputable IF condition.')
  
  #get vendog of identity
  tmpV=vendog 
  
  #condition can be boolean or array
  if (length(index_which)>1)
  {
    tmpI=which(index_which) 
  } else {
    tmpI=index_which 
  }
  
  #RHS can be scalar or array
  if (length(values)>1)
  {
    tmpV[tmpI]=values[tmpI] 
  } else {
    tmpV[tmpI]=values 
  }
  
  return(tmpV)
}

.parseLhsEQ <- function (inputS='',allowedLhsEQfuns=NULL,...)
{
  outL=list() 
  
  #cycle in available lhs funs
  for (funName in allowedLhsEQfuns)
  {
    #search current fun name in input expression
    tmpList=.explodeFunctionCall(inputS,funName)
    
    #we return first match
    if (length(tmpList)>0 && tmpList$firstLoc==1)
    {
      outL=tmpList 
      outL$funName=funName 
      outL$raw=inputS 
      
      return(outL) 
    }
  }
  
  #no lhs fun found so return identity
  outL$funName='I' 
  outL$args=inputS 
  outL$raw=inputS 
  
  return(outL) 
}

#print verbose stuff during simulation
#do not use this fun outside main simulation cycle
.printVerboseSim <- function(phase=NULL,
                            ConstantAdjustment,
                            verboseVars,
                            frequency,
                            TSRANGE,
                            simSteps,
                            simIterLoopIdx,
                            idxB,
                            idxIter,
                            model_max_lag_1,
                            length_model_vexog,
                            model_vendog,
                            model_vexog,
                            replica,
                            localE,
                            model_vpre,
                            model_vsim,
                            model_vpost,
                            hasLeads)
{
  verboseVarsAugmented=verboseVars
  model_vendog_augmented=model_vendog
  model_vexog_augmented=model_vexog
    
  if (hasLeads && simSteps>1)
  {
    for (idx in 1:(simSteps-1))
    {
      verboseVarsAugmented=c(verboseVarsAugmented,paste0(verboseVars,'__LEAD__',idx))
      model_vexog_augmented=c(model_vexog_augmented,paste0(model_vexog,'__LEAD__',idx))
      model_vendog_augmented=c(model_vendog_augmented,paste0(model_vendog,'__LEAD__',idx))
    }
  }
  
  if (phase=='VPRE')
  {
    if(length(model_vpre)==0) return()
    verboseVarsEndoActive=verboseVarsAugmented[verboseVarsAugmented %in% model_vpre]
    
  } else if (phase=='VSIM')
  {
    if(length(model_vsim[[idxB]])==0) return()
    verboseVarsEndoActive=verboseVarsAugmented[verboseVarsAugmented %in% model_vsim[[idxB]]]
    
  } else if (phase=='VPOST')
  {
    if(length(model_vpost[[idxB]])==0) return()
    verboseVarsEndoActive=verboseVarsAugmented[verboseVarsAugmented %in% model_vpost[[idxB]]]
    
  } else if (phase=='ERROR')
  {
    verboseVarsEndoActive=verboseVarsAugmented[verboseVarsAugmented %in% model_vendog_augmented]
    
  } else if (phase=='RESCHECK')
  {
    verboseVarsEndoActive=verboseVarsAugmented[verboseVarsAugmented %in% model_vendog_augmented]
    
  }  else return()
  
  verboseVarsExog=verboseVarsAugmented[verboseVarsAugmented %in% model_vexog_augmented]
  
  if (length(verboseVarsEndoActive)==0  && length(verboseVarsExog)==0)
    return()
  
    if (phase=='VPRE')
    {
      model_vars_adj=model_vpre
    } else if (phase=='VSIM')
    {
      model_vars_adj=model_vsim[[idxB]]
    } else if (phase=='VPOST')
    {
      model_vars_adj=model_vpost[[idxB]]
    } else if (phase=='ERROR')
    {
      model_vars_adj=NULL
    } else if (phase=='RESCHECK')
    {
      model_vars_adj=NULL
    }  else return()
   
    if (hasLeads && simSteps>1  && length(model_vars_adj)>0)
    {
      for (idx in 1:length(model_vars_adj))
      {
        tmpSS=strsplit(model_vars_adj[idx],'__LEAD__')[[1]]
        tmpLHF=tmpSS[1]
        tmpRHS=ifelse(length(tmpSS)>1,as.integer(tmpSS[2]),0)
        
        model_vars_adj[idx]=paste0(tmpLHF,
                                   ' (',
                                   paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+tmpRHS),f=frequency),collapse='-'),
                                   ')')
      }
    }
    
    if (phase=='VPRE')
    {
      cat('\nVPRE. Vars: ',paste0(model_vars_adj,collapse=', '),'\n') 
    } else if (phase=='VSIM')
    {
      cat(paste0('\nVSIM, block ',idxB,', iteration ',idxIter,'. Vars: ',
                 paste0(model_vars_adj,collapse=', '),'\n'))
    } else if (phase=='VPOST')
    {
      cat(paste0('\nVPOST, block ',idxB,', iteration ',idxIter,'. Vars: ',
                 paste0(model_vars_adj,collapse=', '),'\n'))
    } else if (phase=='ERROR')
    {
      cat('\nERROR SNAPSHOT:\n')
    } else if (phase=='RESCHECK')
    {
      cat('\nRESCHECK SNAPSHOT:\n')
    } else return()
 
  doubleFormat="%20.11g"
   
  simPeriods=1
  maxReplicas=3
  printedReplicas=min(replica,maxReplicas)
   
  if (length(ConstantAdjustment)>0 )
  {
    namesCAaugmented=names(ConstantAdjustment)
    if (hasLeads && simSteps>1)
      for (idx in 1:(simSteps-1))
        namesCAaugmented=c(namesCAaugmented,paste0(names(ConstantAdjustment),'__LEAD__',idx))
    
    verboseVarsEndoActiveCA=verboseVarsEndoActive[verboseVarsEndoActive %in% namesCAaugmented]
    
    if (length(verboseVarsEndoActiveCA)>0)
    {
      cat('\tCONSTANT ADJUSTMENTS:\n') 
      for (tmpVA in verboseVarsEndoActiveCA) 
      {  
          tmpV=tmpVA
          tmpSI=0
          if (grepl('__LEAD__',tmpVA))
          {
            tmpSS=strsplit(tmpVA,'__LEAD__')[[1]]
            tmpV=tmpSS[1]
            tmpSI=as.integer(tmpSS[2])
          }
          cat(paste0(
            '\tPrd. ',
            sprintf("%4i",simIterLoopIdx+tmpSI),', ',
            paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx+tmpSI-1),f=frequency),collapse='-'),', ',
            'iter. ',
            sprintf("%4i",idxIter),' ',
            sprintf("%17s CA",tmpV),' ='))
          
          for (idxR in 1:printedReplicas)
            cat(sprintf(doubleFormat,localE[[paste0(tmpV,'__ADDFACTOR',ifelse(tmpSI>0,paste0('__LEAD__',tmpSI),''))]][  model_max_lag_1,idxR])) 
        
         if (replica>maxReplicas) cat('\t(trunc)')
          cat('\n') 
        }
    }
  }
 
  if (length(verboseVarsExog)>0)
  {
    cat('\tVEXOGS:\n') 
    for (tmpVA in verboseVarsExog)
    {  
      tmpV=tmpVA
      tmpSI=0
      if (grepl('__LEAD__',tmpVA))
      {
        tmpSS=strsplit(tmpVA,'__LEAD__')[[1]]
        tmpV=tmpSS[1]
        tmpSI=as.integer(tmpSS[2])
      }
      
      cat(paste0(
        '\tPrd. ',
        sprintf("%4i",simIterLoopIdx+tmpSI),', ',
        paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx+tmpSI-1),f=frequency),collapse='-'),', ',
        'iter. ',
        sprintf("%4i",idxIter),' ',
        sprintf("%20s",tmpV),' ='))
      
      for (idxR in 1:printedReplicas)
        cat(sprintf(doubleFormat,localE[[paste0(tmpV,ifelse(tmpSI>0,paste0('__LEAD__',tmpSI),''))]][  model_max_lag_1,idxR])) 
    
      if (replica>maxReplicas) cat('\t(trunc)')
      cat('\n') 
    }
  }
  
  if (length(verboseVarsEndoActive)>0)
  {
    cat('\tVENDOGS:\n') 
    for (tmpVA in verboseVarsEndoActive) 
    { 
      tmpV=tmpVA
      tmpSI=0
      if (grepl('__LEAD__',tmpVA))
      {
        tmpSS=strsplit(tmpVA,'__LEAD__')[[1]]
        tmpV=tmpSS[1]
        tmpSI=as.integer(tmpSS[2])
      }
      cat(paste0(
        '\tPrd. ',
        sprintf("%4i",simIterLoopIdx+tmpSI),', ',
        paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx+tmpSI-1),f=frequency),collapse='-'),', ',
        'iter. ',
        sprintf("%4i",idxIter),' ',
        sprintf("%20s",tmpV),' ='))
      
      for (idxR in 1:printedReplicas)
        cat(sprintf(doubleFormat,localE[[paste0(tmpV,ifelse(tmpSI>0,paste0('__LEAD__',tmpSI),''))]][  model_max_lag_1,idxR]))
    
      if (replica>maxReplicas) cat('\t(trunc)')
      cat('\n') 
    }
  }
}

#export main RexEx definitions
.RegExGlobalDefinition <- function()
{
  #symbols/funcs allowed on EQ
  reservedKeyw=c('LOG','TSLAG','LAG','TSLEAD','LEAD','MOVAVG','MAVE','EXP','TSDELTA','DEL','ABS','MOVSUM','MTOT','TSDELTAP','TSDELTALOG') 
  
  #list of availables lhs funs
  allowedLhsEQfuns=c('LOG','EXP','TSDELTA','DEL','TSDELTAP','TSDELTALOG') 
  allowedLhsEQfunsPub=c('LOG','EXP','TSDELTA','TSDELTAP','TSDELTALOG') 
  
  #what to remove from equations in order to obtains symbol names
  #symOnEqCleaner='(\\+|\\-|\\(|\\)|\\*|\\/|\\,|\\=|LOG\\(|LAG\\(|MAVE\\(|EXP\\(|DEL\\(|ABS\\()' 
  symOnEqCleaner=paste0('(\\+|\\-|\\(|\\)|\\*|\\/|\\,|\\^|\\=|<|>|\\.GE\\.|\\.LE\\.|\\.GT\\.|\\.LT\\.|\\.EQ\\.',paste(paste0('|',reservedKeyw,'\\('),collapse=''),')') 
  symOnIfCleaner=paste0('(\\&|\\||\\+|\\-|\\(|\\)|\\*|\\/|\\,|\\^|\\=|<|>|\\.GE\\.|\\.LE\\.|\\.GT\\.|\\.LT\\.|\\.EQ\\.',paste(paste0('|',reservedKeyw,'\\('),collapse=''),')') 
  
  #regex allowed on symbol names
  allowedCharOnNameToken='[a-zA-Z_]+[a-zA-Z0-9_]*' 
  allowedCharOnName=paste0('^',allowedCharOnNameToken,'$') 
  
  #regex allowed on numbers (we use only 'strongCharsOnNumbs')
  charsOnNumbs='([0-9]+[\\.]?|[\\.]?[0-9]+|[0-9]+\\.[0-9]+|pi)' 
  charsOnNumWithSign=paste0('(\\+|-)?',charsOnNumbs) 
  strongCharOnNumbs=paste0('^',charsOnNumbs,'$') 
  strongCharOnNumbsWithSign=paste0('^',charsOnNumWithSign,'$') 
  
  return(list(reservedKeyw=reservedKeyw,
              symOnEqCleaner=symOnEqCleaner,
              symOnIfCleaner=symOnIfCleaner,
              allowedCharOnName=allowedCharOnName,
              allowedCharOnNameToken=allowedCharOnNameToken,
              charsOnNumbs=charsOnNumbs,
              charsOnNumWithSign=charsOnNumWithSign,
              strongCharOnNumbs=strongCharOnNumbs,
              strongCharOnNumbsWithSign=strongCharOnNumbsWithSign,
              allowedLhsEQfuns=allowedLhsEQfuns,
              allowedLhsEQfunsPub=allowedLhsEQfunsPub))
}

#reorder equation evaluation sequence based on incidence matrix
.reorderEquations <- function(incidence_matrix)
{
  if (is.null(incidence_matrix))
    return(list(
      vsim=NULL,
      vfeed=NULL
    ))
 
  #we need original incidence_matrix later...
  temp_incidence_matrix=incidence_matrix 
  
  #get VFEED endogenous
  VFEED=c() 
  
  if (dim(temp_incidence_matrix)[1]>0) while(1) {
    
    flag=1
    while(flag)
    {
      flag=0
      
      #RULE #2c 
      temp_vendog=colSums(temp_incidence_matrix)
      which_temp_vendog=(temp_vendog==0)
      
      if (any(which_temp_vendog))
      {
        temp_incidence_matrix=temp_incidence_matrix[! which_temp_vendog,! which_temp_vendog, drop=F]
        
        if (dim(temp_incidence_matrix)[1]==0) break
        
        flag=1
      }
      
      #RULE #2r 
      temp_vendog=rowSums(temp_incidence_matrix)
      which_temp_vendog=(temp_vendog==0)
      
      if (any(which_temp_vendog))
      {
        temp_incidence_matrix=temp_incidence_matrix[! which_temp_vendog,! which_temp_vendog, drop=F]
        
        if (dim(temp_incidence_matrix)[1]==0) break
        flag=1
      }
      
      #RULE #3 
      #if sum column is 1 merge and remove it
      temp_vendog=colSums(temp_incidence_matrix)
      which_temp_vendog=which(temp_vendog==1)
      if (length(which_temp_vendog)>0)
      {
        for (idx in which_temp_vendog)
        {
          dest_row=which(temp_incidence_matrix[,idx]==1)
          temp_incidence_matrix[dest_row,which(temp_incidence_matrix[idx,]==1)]=1
        }
        
        for (idx in which_temp_vendog)
          if (T && temp_incidence_matrix[idx,idx]==1 )
          {
            VFEED=c(VFEED,rownames(temp_incidence_matrix)[idx])
          }
        
        temp_incidence_matrix=temp_incidence_matrix[-which_temp_vendog,-which_temp_vendog, drop=F]
        
        if (dim(temp_incidence_matrix)[1]==0) break
        flag=1
      }
      #if sum rows is 1 merge and remove it
      temp_vendog=rowSums(temp_incidence_matrix)
      which_temp_vendog=which(temp_vendog==1)
      if (length(which_temp_vendog)>0)
      {
        for (idx in which_temp_vendog)
        {
          dest_col=which(temp_incidence_matrix[idx,]==1)
          temp_incidence_matrix[which(temp_incidence_matrix[,idx]==1),dest_col]=1
        }
        
        for (idx in which_temp_vendog)
          if (temp_incidence_matrix[idx,idx]==1 )
          {            VFEED=c(VFEED,rownames(temp_incidence_matrix)[idx])
            
          }
        
        temp_incidence_matrix=temp_incidence_matrix[-which_temp_vendog,-which_temp_vendog, drop=F]
        
        if (dim(temp_incidence_matrix)[1]==0) break
        flag=1
      }
      
      #RULE #1
      rule1_active=c()
      
      for (idx in 1:dim(temp_incidence_matrix)[1])
        if (temp_incidence_matrix[idx,idx]==1)
          rule1_active=c(idx,rule1_active)
      
      if (length(rule1_active)>0)
      {
        VFEED=c(VFEED,rownames(temp_incidence_matrix)[rule1_active])
        temp_incidence_matrix=temp_incidence_matrix[-rule1_active,-rule1_active, drop=F]
        
        if (dim(temp_incidence_matrix)[1]==0) break
        flag=1
      }
      
    }#end flag
    
    #break if incidence matrix is empty
    if (dim(temp_incidence_matrix)[1]==0) break
    
    #RULE #4
    #remove most populated (#1 in rows * (times) #1 in cols)
    temp_rowSums_vendog=rowSums(temp_incidence_matrix)
    temp_colSums_vendog=colSums(temp_incidence_matrix)
    
    which_max_temp=which.max(temp_rowSums_vendog*temp_colSums_vendog)
    VFEED=c(VFEED,rownames(temp_incidence_matrix)[which_max_temp])
    
    temp_incidence_matrix=temp_incidence_matrix[-which_max_temp,-which_max_temp, drop=F]
    
    if (dim(temp_incidence_matrix)[1]==0) break
    
  }
  
  #separate VFEED from VSIM, and order VSIM
  VSIM=c()
  temp_incidence_matrix=incidence_matrix
  
  which_inactive_vendog_index=which(rownames(temp_incidence_matrix) %in% VFEED)
  
  temp_incidence_matrix=temp_incidence_matrix[-which_inactive_vendog_index,-which_inactive_vendog_index, drop=F]
  
  if (dim(temp_incidence_matrix)[1]>0) while(1){
    if (dim(temp_incidence_matrix)[1]==0) break
    
    temp_vendog=rowSums(temp_incidence_matrix)
    which_temp_vendog=(temp_vendog==0)
    
    if (any(which_temp_vendog))
    {
      VSIM=c(VSIM,rownames(temp_incidence_matrix)[which_temp_vendog]) 
      
      temp_incidence_matrix=temp_incidence_matrix[! which_temp_vendog,! which_temp_vendog, drop=F]
      
      if (dim(temp_incidence_matrix)[1]==0) break
    } else {
      stop(paste0('LOAD_MODEL(): cannot reduce incidence matrix.'))
    }
  }
  
  #append VFEED to VSIM
  VSIM=c(VSIM,VFEED) 
  
  return(list(
    vsim=VSIM, 
    vfeed=VFEED
  ))
}

#Tarjan support funs
#see Tarjan's strongly connected components algorithm
.TarjanBlocksExtraction <- function(incidence_matrix)
{
  #local supporting environment
  localT=new.env()
  
  #copy incidence matrix into envornment
  localT$incidence_matrix=incidence_matrix
  nvar=dim(incidence_matrix)[1]
  
  localT$stack=c() 
  localT$lists=list()
  
  #refs:
  #tarjanMatrix[v,1] == v.index
  #tarjanMatrix[v,2] == v.lowlink
  #tarjanMatrix[v,3] == v.onStack
  localT$tarjanMatrix=matrix(0,nrow=nvar,ncol=3)
  localT$tarjanMatrix[,1]=-1 
  localT$index=0
  
  for (vv in 1:nvar)
  {
    if (localT$tarjanMatrix[vv,1]==-1)
      .TarjanStrongConnect(vv,localT)
  }
  
  return(localT)
}

.TarjanStrongConnect <- function(vv,localT)
{
  localT$tarjanMatrix[vv,1]=localT$index
  localT$tarjanMatrix[vv,2]=localT$index
  localT$index=localT$index+1
  
  localT$stack=c(vv,localT$stack)
  localT$tarjanMatrix[vv,3]=1
  
  destvars=which(localT$incidence_matrix[,vv]==1)
  
  #we exploded recursive function by 3 levels to avoid R limitation on stack
  if (length(destvars)>0)
    for (ww in destvars)
    {
      #ww.index indefined
      if (localT$tarjanMatrix[ww,1]==-1)
      {
        #_______________________________________
        
        vv_old=vv
        ww_old=ww
        vv=ww
        
        localT$tarjanMatrix[vv,1]=localT$index
        localT$tarjanMatrix[vv,2]=localT$index
        localT$index=localT$index+1
        
        localT$stack=c(vv,localT$stack)
        localT$tarjanMatrix[vv,3]=1
        
        destvars=which(localT$incidence_matrix[,vv]==1)
        
        if (length(destvars)>0)
          for (ww in destvars)
          {
            if (localT$tarjanMatrix[ww,1]==-1)
            {
              #_______________________________________
              
              vv_old2=vv
              ww_old2=ww
              vv=ww
              
              localT$tarjanMatrix[vv,1]=localT$index
              localT$tarjanMatrix[vv,2]=localT$index
              localT$index=localT$index+1
              
              localT$stack=c(vv,localT$stack)
              localT$tarjanMatrix[vv,3]=1
              
              destvars=which(localT$incidence_matrix[,vv]==1)
              
              if (length(destvars)>0)
                for (ww in destvars)
                {
                  if (localT$tarjanMatrix[ww,1]==-1)
                  {
                    #_______________________________________
                    
                    vv_old3=vv
                    ww_old3=ww
                    vv=ww
                    
                    localT$tarjanMatrix[vv,1]=localT$index
                    localT$tarjanMatrix[vv,2]=localT$index
                    localT$index=localT$index+1
                    
                    localT$stack=c(vv,localT$stack)
                    localT$tarjanMatrix[vv,3]=1
                    
                    destvars=which(localT$incidence_matrix[,vv]==1)
                    
                    if (length(destvars)>0)
                      for (ww in destvars)
                      {
                        if (localT$tarjanMatrix[ww,1]==-1)
                        {
                          #_______________________________________
                          
                          #vv_old=vv
                          #vv=ww
                          .TarjanStrongConnect(ww,localT)
                          
                          #vv=vv_old
                          
                          #_______________________________________
                          
                          localT$tarjanMatrix[vv,2]=min(localT$tarjanMatrix[vv,2],localT$tarjanMatrix[ww,2])
                          
                        } else if (localT$tarjanMatrix[ww,3]==1)
                        {
                          localT$tarjanMatrix[vv,2]=min(localT$tarjanMatrix[vv,2],localT$tarjanMatrix[ww,1])
                        }
                      }
                    
                    if (localT$tarjanMatrix[vv,2]==localT$tarjanMatrix[vv,1])
                    {
                      listsLength=length(localT$lists)+1
                      outputList=c()
                      
                      flag=0
                      while (flag!=vv)
                      {
                        ww=localT$stack[1]
                        localT$stack=localT$stack[-1]
                        localT$tarjanMatrix[ww,3]=0
                        outputList=c(outputList,ww)
                        flag=ww
                      }
                      
                      localT$lists[[listsLength]]=outputList
                    }
                    
                    ww=ww_old3
                    vv=vv_old3
                    
                    #_______________________________________
                    
                    localT$tarjanMatrix[vv,2]=min(localT$tarjanMatrix[vv,2],localT$tarjanMatrix[ww,2])
                    
                  } else if (localT$tarjanMatrix[ww,3]==1)
                  {
                    localT$tarjanMatrix[vv,2]=min(localT$tarjanMatrix[vv,2],localT$tarjanMatrix[ww,1])
                  }
                }
              
              if (localT$tarjanMatrix[vv,2]==localT$tarjanMatrix[vv,1])
              {
                listsLength=length(localT$lists)+1
                outputList=c()
                
                flag=0
                while (flag!=vv)
                {
                  ww=localT$stack[1]
                  localT$stack=localT$stack[-1]
                  localT$tarjanMatrix[ww,3]=0
                  outputList=c(outputList,ww)
                  flag=ww
                }
                
                localT$lists[[listsLength]]=outputList
                
              }
              
              ww=ww_old2
              vv=vv_old2
              
              #_______________________________________
              
              localT$tarjanMatrix[vv,2]=min(localT$tarjanMatrix[vv,2],localT$tarjanMatrix[ww,2])
              
            } else if (localT$tarjanMatrix[ww,3]==1)
            {
              localT$tarjanMatrix[vv,2]=min(localT$tarjanMatrix[vv,2],localT$tarjanMatrix[ww,1])
            }
          }
        
        if (localT$tarjanMatrix[vv,2]==localT$tarjanMatrix[vv,1])
        {
          listsLength=length(localT$lists)+1
          outputList=c()
          
          flag=0
          while (flag!=vv)
          {
            ww=localT$stack[1]
            localT$stack=localT$stack[-1]
            localT$tarjanMatrix[ww,3]=0
            outputList=c(outputList,ww)
            flag=ww
          }
          
          localT$lists[[listsLength]]=outputList
          
        }
        
        ww=ww_old
        vv=vv_old
        
        #_______________________________________
        
        localT$tarjanMatrix[vv,2]=min(localT$tarjanMatrix[vv,2],localT$tarjanMatrix[ww,2])
        
      } else if (localT$tarjanMatrix[ww,3]==1)
      {
        localT$tarjanMatrix[vv,2]=min(localT$tarjanMatrix[vv,2],localT$tarjanMatrix[ww,1])
      }
    }
  
  if (localT$tarjanMatrix[vv,2]==localT$tarjanMatrix[vv,1])
  {
    listsLength=length(localT$lists)+1
    outputList=c()
    
    flag=0
    while (flag!=vv)
    {
      ww=localT$stack[1]
      localT$stack=localT$stack[-1]
      localT$tarjanMatrix[ww,3]=0
      outputList=c(outputList,ww)
      flag=ww
    }
    
    localT$lists[[listsLength]]=outputList
  }
}

#return list of unknown fun names, case ignored
.unknownFunsNames <- function(inputS=NULL,reservedKeyw)
{
  
  allFunNames=.extractFunsNames(inputS) 
  return( allFunNames[! (toupper(allFunNames) %in% reservedKeyw)])
}

# exported core funs -----------------------------------------------------------

# PRINT SUMMARY code -----------------------------------------------------------

print.BIMETS_MODEL <- function(x=NULL,...)
{
  model=x 
  
  if (! inherits(model, "BIMETS_MODEL")) stop("print.BIMETS_MODEL(): model object is not a BIMETS model.") 
  
  cat(paste0('\nBIMETS MODEL\n')) 
  cat(paste0('-----------------------------------\n')) 
  cat(paste0(sprintf("%-25s",'name:'),model$modelName,'\n')) 
  cat(paste0(sprintf("%-25s",'built with bimets:'),ifelse(is.null(model$bimets_version),'unknown',model$bimets_version),'\n')) 
  cat(paste0(sprintf("%-25s",'behaviorals:'),(model$totNumEqs),'\n')) 
  cat(paste0(sprintf("%-25s",'identities:'),(model$totNumIds),'\n')) 
  cat(paste0(sprintf("%-25s",'coefficients:'),(model$eqCoeffNum),'\n')) 
  
  tryCatch({
    .CHECK_MODEL_DATA(model,showWarnings=FALSE) 
    cat(paste0(sprintf("%-25s",'model data:'),"OK",'\n')) 
  },error=function(e){cat(paste0(sprintf("%-25s",'model data:'),"not OK",' :: ',e$message,"\n")) }) 
  
  is_lead=FALSE
  if (model$max_lead>0) is_lead=TRUE
  cat(paste0(sprintf("%-25s",'forward looking:'),(is_lead),'\n')) 
  
  totCoeffEstimated=0 
  if (length(model$behaviorals)>0) for (idxEq in 1:length(model$behaviorals))
  {
    totCoeffEstimated=totCoeffEstimated+length(model$behaviorals[[idxEq]]$coefficients) 
  }
  
  fullyEstimated=ifelse(totCoeffEstimated<model$eqCoeffNum,FALSE,TRUE) 
  cat(paste0(sprintf("%-25s",'fully estimated:'),(fullyEstimated),'\n')) 
  
  simulated=FALSE 
  if (! is.null(model$simulation))
  {
    namesSim=names(model$simulation) 
    namesSim=namesSim[-which(namesSim=='__SIM_PARAMETERS__')] 
    if (length(base::setdiff(model$vendog,namesSim))==0)
      simulated=TRUE 
  }
  
  cat(paste0(sprintf("%-25s",'fully simulated:'),(simulated),'\n')) 
  
  renormed=FALSE
  if (! is.null(model$renorm))
  {
    if (!is.null(model$renorm$TARGET))
      renormed=TRUE
  }
  cat(paste0(sprintf("%-25s",'renormed:'),(renormed),'\n')) 
  
  stoch_simulated=FALSE 
  if (! is.null(model$stochastic_simulation))
  {
    namesSim=names(model$stochastic_simulation) 
    namesSim=namesSim[-which(namesSim=='__STOCH_SIM_PARAMETERS__')] 
    if (length(base::setdiff(model$vendog,namesSim))==0)
      stoch_simulated=TRUE 
  }
  
  cat(paste0(sprintf("%-25s",'fully stochastic sim.:'),(stoch_simulated),'\n')) 
  
  optimized=FALSE
  if (! is.null(model$optimize))
  {
    if (!is.null(model$optimize$realizationsToKeep))
      optimized=TRUE  
  }
  
  cat(paste0(sprintf("%-25s",'optimized:'),(optimized),'\n')) 
}

summary.BIMETS_MODEL <- function(object,...)
{
  return(print.BIMETS_MODEL(object,...))  
}

# LOAD MODEL code --------------------------------------------------------------

LOAD_MODEL <- function(modelFile=NULL,
                       modelText=NULL,
                       quietly=FALSE,
                       oldStyleModel=FALSE,
                       ...)
{
  #rawData:       original text file
  #cleanModel:    rawData without MODEL, END, comments and trim spaces
  #totNumEqs:     equations count
  #totNumIds:     identities count ("same id multiple IF>" counts 1)
  #totNumCoeff:   coefficients count
  #behaviorals:   contains data of behavioral definition
  #identities:    contains data of identity definition
  #max_lag:       contains max lag required for all eqs
  #oldStyleModel  removes starting dollar signs if there 
  #               is another dollar sign at the end of the line
  
  #main output
  model=list() 
  class(model)='BIMETS_MODEL' 
  
  #get regex definitions
  regExDefs=.RegExGlobalDefinition() 
  reservedKeyw=regExDefs$reservedKeyw 
  symOnEqCleaner=regExDefs$symOnEqCleaner 
  symOnIfCleaner=regExDefs$symOnIfCleaner 
  allowedCharOnName=regExDefs$allowedCharOnName 
  allowedCharOnNameToken=regExDefs$allowedCharOnNameToken
  charsOnNumbs=regExDefs$charsOnNumbs 
  charsOnNumWithSign=regExDefs$charsOnNumWithSign 
  strongCharOnNumbs=regExDefs$strongCharOnNumbs 
  strongCharOnNumbsWithSign=regExDefs$strongCharOnNumbsWithSign 
  allowedLhsEQfuns=regExDefs$allowedLhsEQfuns 
  allowedLhsEQfunsPub=regExDefs$allowedLhsEQfunsPub 
  
  #check arg
  if (!(is.logical(quietly))) stop('LOAD_MODEL(): "quietly" must be TRUE or FALSE.')
  
  #input is a file
  if (is.null(modelText))
  {
    #file exists?
    if (is.null(modelFile) || !is.character(modelFile) || !file.exists(modelFile)) 
      stop('LOAD_MODEL(): model file "',as.character(modelFile),'" not found.') 
    modelName=modelFile 
    #read file
    tryCatch({
      rawData=readLines(modelFile,warn=FALSE) 
    },error=function(e){stop('LOAD_MODEL(): cannot read the model file. ',e$message) }) 
  } else 
  {
    #input is text
    modelName=substitute(modelText) 
    #deal with MS-Win carriage
    modelText=gsub('\r\n','\n',modelText) 
    #get lines
    rawData=strsplit(modelText,'\n')[[1]] 
  }
  
  #check consistence
  if (length(rawData)==0) stop('LOAD_MODEL(): empty model file.')
  model$rawData=rawData 
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
        next 
      #...else remove first $
      cleanModel[idxR]=gsub("^\\s*\\$", "", cleanModel[idxR]) 
    }
  }
  
  #remove empty lines
  emptyLinesIdx=which(cleanModel=='') 
  if (length(emptyLinesIdx)>0) cleanModel=cleanModel[-emptyLinesIdx] 
  if (cleanModel[1]!='MODEL' || cleanModel[length(cleanModel)]!='END') 
    stop('LOAD_MODEL(): model file must begin with MODEL and must end with END keywords.')
  
  #again trim leading/trailing spaces
  cleanModel=gsub("^\\s+|\\s+$", "", cleanModel)
  
  #remove comments (line starting with $ or COMMENT) and first/last line (MODEL|END)
  commentsIndexes=grep("^(\\$|COMMENT\\s*>)",cleanModel,ignore.case=TRUE) 
  if (length(commentsIndexes)>0) cleanModel=cleanModel[-commentsIndexes] 
  cleanModel=cleanModel[-1]
  cleanModel=cleanModel[-length(cleanModel)] 
  if (length(grep("\\$",cleanModel))>0) 
    stop('LOAD_MODEL(): dollar symbol (i.e. a comment) allowed only at the beginning of a line.\nCheck line "',cleanModel[grep("\\$",cleanModel)[1]],'"') 
  #eqs and ids num
  totNumEqs=0 
  totNumIds=0 
  
  #collect keyword indexes
  behavioralIndexes=grep("^(EQUATION|BEHAVIORAL)\\s*>",cleanModel, ignore.case=TRUE) 
  identityIndexes=grep("^IDENTITY\\s*>",cleanModel, ignore.case=TRUE) 
  eqIndexes=grep("^EQ\\s*>",cleanModel, ignore.case=TRUE) 
  coeffIndexes=grep("^COEFF\\s*>",cleanModel, ignore.case=TRUE) 
  storeIndexes=grep("^STORE\\s*>",cleanModel, ignore.case=TRUE) 
  pdlIndexes=grep("^PDL\\s*>",cleanModel, ignore.case=TRUE) 
  restrictIndexes=grep("^RESTRICT\\s*>",cleanModel, ignore.case=TRUE) 
  errorIndexes=grep("^ERROR\\s*>",cleanModel, ignore.case=TRUE) 
  ifIndexes=grep("^IF\\s*>",cleanModel, ignore.case=TRUE) 
  ivIndexes=grep("^IV\\s*>",cleanModel, ignore.case=TRUE) 
  
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
               ivIndexes)) 
  
  #sorted indexes of eq keywords
  eqIdenDefIdx=sort(c(length(cleanModel)+1,
                      behavioralIndexes,
                      identityIndexes)) 
  
  #coeff num
  totNumCoeff=0
  
  #analyze code behaviorals -----------------------------------------------------------------------------
  
  model$behaviorals=list() 
  
  #cycle and extract all behaviorals
  if (length(behavioralIndexes)>0) for(idxBI in 1:length(behavioralIndexes))
  {
    #empty tmp behavioral to be added to model list
    behavioralTmp=list()     
    
    #...we extract all code between two behavioral definitions
    #get next keyword definition index
    nextKwIdx=min(kwIdx[which(kwIdx>behavioralIndexes[idxBI])])     
    
    #get next behavioral definition index, needed to check COEFF exists
    nextDefIdx=min(eqIdenDefIdx[which(eqIdenDefIdx>behavioralIndexes[idxBI])])     
    
    #read and trim all lines of behavior definition
    behavioralRaw=paste(cleanModel[behavioralIndexes[idxBI]:(nextKwIdx-1)],collapse=' ')
    behavioralRaw=gsub("^(EQUATION|BEHAVIORAL)\\s*>", "", behavioralRaw, ignore.case=TRUE) 
    behavioralRaw=gsub("^\\s+|\\s+$", "", behavioralRaw) 
    
    #get behavioral name (split if TSRANGE defined)
    behavioralName=gsub("^\\s+|\\s+$", "",strsplit(behavioralRaw,'TSRANGE')[[1]][1])     
    if (is.na(behavioralName) || nchar(behavioralName)==0) 
      stop('LOAD_MODEL(): unknown behavioral name in line: "',cleanModel[behavioralIndexes[idxBI]],'".')  
    
    #check name is compliant with regex
    if (length(grep(allowedCharOnName,behavioralName))==0) 
      stop('LOAD_MODEL(): invalid behavioral name: "',behavioralName,'".')  
    
    #check TSRANGE inline definition
    if (length(grep('TSRANGE',behavioralRaw))>0)
    { 
      tryCatch({
        #get text after TSRANGE kw then split with spaces
        behavioralTsrange=suppressWarnings(as.numeric(strsplit(gsub("^\\s+|\\s+$", "",strsplit(behavioralRaw,'TSRANGE')[[1]][2]),'\\s+')[[1]]))  
        if (any(! is.finite(behavioralTsrange))) stop()  
        if (any(behavioralTsrange<=0)) stop()  
        if (any(behavioralTsrange %% 1 != 0)) stop()  
        if (length(behavioralTsrange)!=4) stop()  
      },error=function(e){
        
        stop('LOAD_MODEL(): invalid TSRANGE in line: "',behavioralRaw,'".')})  
      
    } else behavioralTsrange=NA      
    
    #check behavioral has the COEFF definition
    coeffLocalIdx=which(coeffIndexes %in% behavioralIndexes[idxBI]:(nextDefIdx-1))  
    if (length(coeffLocalIdx)==0) stop('LOAD_MODEL(): no COEFF definition in behavioral "',behavioralName,'".')  
    if (length(coeffLocalIdx)>1) stop('LOAD_MODEL(): multiple COEFF definitions in behavioral "',behavioralName,'".')  
    
    #read all lines of coeff definition    
    nextKwIdx=min(kwIdx[which(kwIdx>coeffIndexes[coeffLocalIdx])])  
    coeffRaw=paste(cleanModel[coeffIndexes[coeffLocalIdx]:(nextKwIdx-1)],collapse=' ')  
    coeffRaw=gsub("^COEFF\\s*>", "", coeffRaw, ignore.case=TRUE)  
    coeffRaw=gsub("^\\s+|\\s+$", "", coeffRaw)  
    
    #split and remove duplicates
    coeff=unique(strsplit(coeffRaw,'\\s+')[[1]])  
    
    #check coeef have allowed names
    if (any(! grepl(allowedCharOnName,coeff))) stop('LOAD_MODEL(): invalid name in COEFF definition "',coeffRaw,'" in behavioral "',behavioralName,'".')  
    
    coeffNum=length(coeff)  
    totNumCoeff=totNumCoeff+coeffNum  
    if (coeffNum==0) stop('LOAD_MODEL(): no COEFF definition in behavioral "',behavioralName,'".')  
    
    #check behavioral has EQ definition
    eqLocalIdx=which(eqIndexes %in% behavioralIndexes[idxBI]:(nextDefIdx-1))  
    if (length(eqLocalIdx)==0) stop('LOAD_MODEL(): no EQ definition in behavioral "',behavioralName,'".')  
    if (length(eqLocalIdx)>1) stop('LOAD_MODEL(): multiple EQ definitions in behavioral "',behavioralName,'".')  
    
    #read all lines of EQ definition
    nextKwIdx=min(kwIdx[which(kwIdx>eqIndexes[eqLocalIdx])])  
    eqRaw=paste(cleanModel[eqIndexes[eqLocalIdx]:(nextKwIdx-1)],collapse=' ')  
    eqRaw=gsub("^EQ\\s*>", "", eqRaw, ignore.case=TRUE)  
    eqRaw=gsub("\\s*", "", eqRaw)   
    
    #check unknown funs names
    unknownFuns=.unknownFunsNames(eqRaw,reservedKeyw)
    if (length(unknownFuns)>0) stop('LOAD_MODEL(): unsupported functions in EQ definition: "',eqRaw,'" in behavioral "',behavioralName,'": ',
                                    paste0(paste0(unknownFuns,'()'),collapse=', '))  
   
    #check EQ is non trivial
    if (nchar(eqRaw)==0 || !grepl('=',eqRaw))
      stop('LOAD_MODEL(): syntax error in behavioral "',behavioralName,'" in EQ definition: "',eqRaw,'".')  
    
    #trim and extract symbol names from EQ
    #we keep dependent
    namesOnEq=strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnEqCleaner,' ',eqRaw,ignore.case=TRUE)),'\\s+')[[1]]  
    
    #check EQ is non trivial
    if ( length(namesOnEq)==0 )
      stop('LOAD_MODEL(): syntax error in behavioral "',behavioralName,'" in EQ definition: "',eqRaw,'".')  
    
    #remove numbers from names
    rmvNumOnEqIdx=c()  
    for (idxNamesOnEq in 1:length(namesOnEq)) {
      if (length(grep(strongCharOnNumbs,namesOnEq[idxNamesOnEq]))>0) 
      {
        rmvNumOnEqIdx=c(rmvNumOnEqIdx,idxNamesOnEq)  
      }
    }
    if (length(rmvNumOnEqIdx)>0) namesOnEq=namesOnEq[-rmvNumOnEqIdx]  
    
    #check "=" exists
    eqRawSplitted=strsplit(eqRaw,'\\=')[[1]]  
    if (length(eqRawSplitted)!=2) stop('LOAD_MODEL(): syntax error in behavioral "',behavioralName,'" in EQ definition: "',eqRaw,'".')
    
    rhsExp=eqRawSplitted[2]  
    lhsExp=eqRawSplitted[1]  
    
    if (lhsExp=='') stop('LOAD_MODEL(): syntax error in behavioral "',behavioralName,'" in EQ definition: "',eqRaw,'".')
    
    lhsFun=NULL  
    
    #find lhs func
    tryCatch({
      lhsFun=.parseLhsEQ(lhsExp,allowedLhsEQfuns)  
    },error=function(err){
      stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" in behavioral "',behavioralName,'".')})
    
    # lhs behavioral ------------------------------------------------
    
    #check lhs fun syntax
    if (lhsFun$funName=='I')
    {
      #check EQ contains behavioral name
      if (lhsFun$args!=behavioralName)
        stop('LOAD_MODEL(): LHS of EQ definition: "',eqRaw,'" must contain only the behavioral name "',behavioralName,'" or the allowed uppercase LHS functions: ',paste0(paste0(allowedLhsEQfunsPub,'()'),collapse=', '))  
      
      if (lhsExp!=paste0(behavioralName))
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',behavioralName,'" are allowed on the LHS of EQ definition, e.g. EQ> ',behavioralName,'=...') 
      
    } else if (lhsFun$funName=='LOG')
    {
      if (lhsFun$argsCount!=1 || lhsFun$args!=behavioralName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the behavioral "',behavioralName,'". If the LOG of the endogenous variable is requested please try the following: "LOG(',behavioralName,')"')  
      
      if (lhsExp!=paste0('LOG(',behavioralName,')'))
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',behavioralName,'" are allowed on the LHS of EQ definition, e.g. EQ> LOG(',behavioralName,')=...') 
      
    } else if (lhsFun$funName=='EXP')
    {
      if (lhsFun$argsCount!=1 || lhsFun$args!=behavioralName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the behavioral "',behavioralName,'". If the EXP of the endogenous variable is requested please try the following: "EXP(',behavioralName,')"')  
      
      if (lhsExp!=paste0('EXP(',behavioralName,')'))
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',behavioralName,'" are allowed on the LHS of EQ definition, e.g. EQ> EXP(',behavioralName,')=...') 
      
    } else if (lhsFun$funName=='TSDELTA' || lhsFun$funName=='DEL')
    {
      if (lhsFun$argsCount>2 || lhsFun$args[1]!=behavioralName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the behavioral "',behavioralName,'". If the TSDELTA of the endogenous variable is requested please try the following: "TSDELTA(',behavioralName,')" or "TSDELTA(',behavioralName,',n)"')  
      
      if (lhsExp!=paste0(lhsFun$funName,'(',behavioralName,',',lhsFun$args[2],')') && lhsExp!=paste0(lhsFun$funName,'(',behavioralName,')'))
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',behavioralName,'" are allowed on the LHS of EQ definition, e.g. EQ> TSDELTA(',behavioralName,',n)=...') 
      
    } else if (lhsFun$funName=='TSDELTAP' )
    {
      if (lhsFun$argsCount>2 || lhsFun$args[1]!=behavioralName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the behavioral "',behavioralName,'". If the TSDELTAP of the endogenous variable is requested please try the following: "TSDELTAP(',behavioralName,')" or "TSDELTAP(',behavioralName,',n)"')  
      
      if (lhsExp!=paste0(lhsFun$funName,'(',behavioralName,',',lhsFun$args[2],')') && lhsExp!=paste0(lhsFun$funName,'(',behavioralName,')') )
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',behavioralName,'" are allowed on the LHS of EQ definition, e.g. EQ> TSDELTAP(',behavioralName,',n)=...') 
      
    } else if (lhsFun$funName=='TSDELTALOG' )
    {
      if (lhsFun$argsCount>2 || lhsFun$args[1]!=behavioralName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" in behavioral "',behavioralName,'". If the TSDELTALOG of the endogenous variable is requested please try the following: "TSDELTALOG(',behavioralName,')" or "TSDELTALOG(',behavioralName,',n)"')  
      
      if (lhsExp!=paste0(lhsFun$funName,'(',behavioralName,',',lhsFun$args[2],')') && lhsExp!=paste0(lhsFun$funName,'(',behavioralName,')') )
        stop('LOAD_MODEL(): syntax error in behavioral "',behavioralName,'" in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',behavioralName,'" are allowed on the LHS of EQ definition, e.g. EQ> TSDELTALOG(',behavioralName,',n)=...') 
    }
    
    #check EQ contains COEFFs
    for(coeffTmp in coeff) if (length(grep(allowedCharOnName,coeffTmp))==0) 
      stop('LOAD_MODEL(): invalid coefficient name "',coeffTmp,'" in COEFF definition of behavioral: "',behavioralName,'".')      
    for(coeffTmp in coeff) if (! (coeffTmp %in% namesOnEq) ) 
      stop('LOAD_MODEL(): coefficient "',coeffTmp,'" not found in EQ definition: "',eqRaw,'".')      
    
    if (! .checkExpression(rhsExp)) stop('LOAD_MODEL(): syntax error in behavioral: "',behavioralName,'" in RHS of EQ definition: "',eqRaw,'".')
    
    #remove coeff from EQ components names
    rmvCoeffOnEqIdx=c()  
    for (idxNamesOnEq in 1:length(namesOnEq)) {
      if (namesOnEq[idxNamesOnEq] %in% coeff) 
      {
        rmvCoeffOnEqIdx=c(rmvCoeffOnEqIdx,idxNamesOnEq)              
      }
    }
    
    if (!(all(namesOnEq[rmvCoeffOnEqIdx]==coeff))) 
      stop('LOAD_MODEL(): in behavioral "',behavioralName,'", coefficients in EQ definition: "',eqRaw,'" must appear in the same order as in the COEFF definition: "',paste(coeff,collapse=', '),'".')  
    
    if (length(rmvCoeffOnEqIdx)>0) namesOnEq=namesOnEq[-rmvCoeffOnEqIdx]  
    
    #check components name
    for (idxNOEQ in 1:length(namesOnEq)) {
      if (length(grep(allowedCharOnName,namesOnEq[idxNOEQ]))==0) 
      {
        stop('LOAD_MODEL(): syntax error in behavioral "',behavioralName,'" in EQ definition: "',eqRaw,'". The following symbol cannot be a variable name: "',namesOnEq[idxNOEQ],'".')  
      }
    }
    
    #normalize regressors
    for(coeffTmp in coeff)
    {
      #if is intercept (isolated coeff)
      if (!grepl(paste0(coeffTmp,'\\*'),rhsExp))
      {
        #convert intercept: coeff0+ -> coeff0*1+
        rhsExp=gsub(paste0(coeffTmp,'\\+'),paste0(coeffTmp,'*1+'),rhsExp)  
        #+coeff0 at the end -> +coeff0*1
        rhsExp=gsub(paste0(coeffTmp,'$'),paste0(coeffTmp,'*1'),rhsExp)  
        #coeff0 at the start -> +coeff0
        rhsExp=gsub(paste0('^',coeffTmp),paste0('+',coeffTmp),rhsExp)  
      }
      
      #in case rhs start with coeff without sign
      rhsExp=gsub(paste0('^',coeffTmp),paste0('+',coeffTmp),rhsExp) 
    }
    
    for(coeffTmp in coeff) if (length(grep(paste0('(\\+)',coeffTmp,'\\*'),rhsExp))==0 ) 
      stop('LOAD_MODEL(): in behavioral "',behavioralName,'", please provide EQ "',eqRaw,'" in form of a linear combination of coefficients: fix coeff. "',coeffTmp,'".')      
    
    #split using coeffs and get EQ regressors
    eqRegressorsNames=strsplit(rhsExp,paste0('(\\+)(',paste(coeff,collapse='|'),')\\*'))[[1]]  
    if (length(eqRegressorsNames)<2) stop('LOAD_MODEL(): in behavioral "',behavioralName,'", regressors count error in EQ definition: "',eqRaw,'"')  
    
    if (eqRegressorsNames[1]!='')
      stop('LOAD_MODEL(): in behavioral "',behavioralName,'", please provide EQ "',eqRaw,'" in form of a linear combination of coefficients.')  
    
    eqRegressorsNames=eqRegressorsNames[-1]   #remove first void element
    
    for (idxR in 1:length(eqRegressorsNames))
      if (! .checkExpressionIsAtomic(eqRegressorsNames[idxR])) 
        stop('LOAD_MODEL(): in behavioral "',behavioralName,'", please provide EQ "',eqRaw,'" in form of a linear combination of coefficients: fix regressor "',eqRegressorsNames[idxR],'".')  
    
    if (length(eqRegressorsNames)!=length(coeff)) 
      stop('LOAD_MODEL(): in behavioral "',behavioralName,'", regressors count in EQ definition: "',eqRaw,'" is different from coefficients count in COEFF definition: "',paste(coeff,collapse=', '),'".')  
    
    #check if duplicated regressor
    if (any(duplicated(eqRegressorsNames))) 
      stop('LOAD_MODEL(): in behavioral "',behavioralName,'", duplicated regressor "',paste(eqRegressorsNames[which(duplicated(eqRegressorsNames))],collapse=' '),'" in EQ definition: "',eqRaw,'".')  
    
    #check unique intercept 
    flagSC=FALSE  
    for(regrTmp in eqRegressorsNames)
    {
      if (grepl(strongCharOnNumbs,regrTmp))
      {
        if (flagSC) stop('LOAD_MODEL(): in behavioral "',behavioralName,'", multiple intercepts defined in EQ: "',eqRaw,'"' )  
        flagSC=TRUE  
      }
    }
    
    #check behavioral has STORE definition
    storeLocalIdx=which(storeIndexes %in% behavioralIndexes[idxBI]:(nextDefIdx-1))  
    storeVarName=NULL  
    storePosition=NULL  
    if (length(storeLocalIdx)>0)
    { 
      if (length(storeLocalIdx)>1) stop('LOAD_MODEL(): multiple STORE definitions in behavioral "',behavioralName,'".')  
      
      #read all lines of store definition
      nextKwIdx=min(kwIdx[which(kwIdx>storeIndexes[storeLocalIdx])])  
      storeRaw=paste(cleanModel[storeIndexes[storeLocalIdx]:(nextKwIdx-1)],collapse=' ')  
      storeRaw=gsub("^STORE\\s*>", "", storeRaw, ignore.case=TRUE)  
      storeRaw=gsub("\\s*", "", storeRaw)  
      
      #get store var name and position
      storeSplit=strsplit(storeRaw,'\\(')[[1]]  
      if (length(storeSplit)!=2) 
        stop('LOAD_MODEL(): syntax error in STORE definition: "',storeRaw,'" in behavioral "',behavioralName,'".')
      
      storeVarName=gsub("\\s*", "", storeSplit[1])  
      if (length(grep(allowedCharOnName,storeVarName))==0) 
        stop('LOAD_MODEL(): invalid variable name in STORE definition: "',storeRaw,'" in behavioral "',behavioralName,'".')  
      
      #STORE location must be positive integer
      if (length(grep("^\\s*[0]*[0-9]*\\s*\\)$",storeSplit[2]))==0) 
        stop('LOAD_MODEL(): syntax error in STORE definition: "',storeRaw,'" in behavioral "',behavioralName,'".')  
      storePosition=as.numeric(gsub('\\)','',storeSplit[2]))          
      if (storePosition<0) 
        stop('LOAD_MODEL(): unknown error on retrieving STORE position in "',storeRaw,'" in behavioral "',behavioralName,'".')  
    }
    
    #check behavioral has PDL definition
    #pdlLocalIdx contains lines indexes of pdl in behavioral
    pdlLocalIdx=which(pdlIndexes %in% behavioralIndexes[idxBI]:(nextDefIdx-1))  
    pdlRaw=NULL  
    bm_pdlMatrix=NULL        
    
    bm_pdlRestrictionMatrix=c()  
    bm_pdlRestrictionVector=c()  
    
    #backUp old coeff and eqRegressorsNames          
    coeffOriginal=coeff  
    eqRegressorsNamesOriginal=eqRegressorsNames  
    
    #we have PDL definitions
    if (length(pdlLocalIdx)>0) 
    {
      #analyze all PDL definitions
      pdlRaw='' 
      for (pdlLLidx in 1:length(pdlLocalIdx))
      {
        #read all lines of PDL definition
        nextKwIdx=min(kwIdx[which(kwIdx>pdlIndexes[pdlLocalIdx[pdlLLidx]])]) 
        #e.g. pdlRawSingle="PDL> C06 2 14 F"
        #clean...
        pdlRawSingle=paste(cleanModel[pdlIndexes[pdlLocalIdx[pdlLLidx]]:(nextKwIdx-1)],collapse=';') 
        pdlRawSingle=gsub("^PDL\\s*>", "", pdlRawSingle, ignore.case=TRUE) 
        pdlRawSingle=gsub("^\\s*|\\s*$", "", pdlRawSingle) 
        
        #e.g. pdlRaw="C07 2 14 F;C06 2 14 F;"
        pdlRaw=paste0(pdlRawSingle,';',pdlRaw) 
      }
      
      #analyze pdlRaw
      pdlComponents=strsplit(pdlRaw,';')[[1]] 
      if (length(pdlComponents)<1) 
        stop('LOAD_MODEL(): NULL length in PDL definition "',pdlRaw,'" in behavioral "',behavioralName,'".')
      
      #create empty list
      bm_pdl=list() 
      #following 'bm_pdlMatrix' matrix will have:
      #first row is 1 if restriction applies to i-th coefficient
      #second row and third row have degree and lag values
      #4th row is 1 if N 
      #5th row is 1 if F 
      
      bm_pdlMatrix=matrix(rep(0,5*length(coeff)),nrow=5,ncol=length(coeff)) 
      for (idxPC in 1:length(pdlComponents ))
      {
        #split with spaces " C06 2 14 F" -> c('C06',2,14,'F')
        pdlSingleElement=strsplit(pdlComponents[idxPC],'\\s+')[[1]] 
        if ((length(pdlSingleElement)<3) || (length(pdlSingleElement)>5)) 
          stop('LOAD_MODEL(): syntax error in PDL definition "',pdlComponents[idxPC],'" in behavioral "',behavioralName,'". Usage: PDL> coeff degree laglength [N] [F].')
        
        #check coefficient exists
        if ( ! (pdlSingleElement[1] %in% coeff)) 
          stop('LOAD_MODEL(): unknown coefficient "',pdlSingleElement[1],'" in PDL definition in behavioral "',behavioralName,'".') 
        coeffPos=which(coeff==pdlSingleElement[1]) 
        bm_pdlMatrix[1,coeffPos]=1 
        #PDL on constant term throws an error
        if (grepl(strongCharOnNumbs,eqRegressorsNames[coeffPos]))
        {
          stop('LOAD_MODEL(): cannot expand a PDL on a constant term: coeff. "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".') 
        }
        
        #deal with degree
        tmpV=as.numeric(pdlSingleElement[2])
        if ( 
          ( ! is.finite(tmpV))    ||
          (length(tmpV)==0) ||
          (!((tmpV %% 1) == 0) ) ||
          (!(tmpV >-1))
        ) stop('LOAD_MODEL(): polynomial degree must be a non-negative integer in PDL definition of coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".') 
        bm_pdlMatrix[2,coeffPos]=tmpV 
        #deal with laglength
        tmpV=as.numeric(pdlSingleElement[3])
        if ( 
          ( ! is.finite(tmpV))    ||
          (length(tmpV)==0) ||
          (!((tmpV %% 1) == 0) ) ||
          (!(tmpV >0))
        ) stop('LOAD_MODEL(): laglength must be a positive integer in PDL definition of coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".') 
        bm_pdlMatrix[3,coeffPos]=tmpV 
        if (bm_pdlMatrix[3,coeffPos]<=bm_pdlMatrix[2,coeffPos]) 
          stop('LOAD_MODEL(): laglength must be greater than polynomial degree in PDL definition of coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".')
        
        #deal with N/F				
        if (length(pdlSingleElement)>3)
        {              
          tmpV=pdlSingleElement[4]
          if ( 
            (tmpV != 'N')  &&
            (tmpV != 'F') 
          ) stop('LOAD_MODEL(): options must be "N" and/or "F" in PDL definition of coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".') 
          if (tmpV == 'N') bm_pdlMatrix[4,coeffPos]=1 
          if (tmpV == 'F') bm_pdlMatrix[5,coeffPos]=1 
        }
        
        if (length(pdlSingleElement)>4)
        {              
          tmpV=pdlSingleElement[5]
          if ( 
            (tmpV != 'N')  &&
            (tmpV != 'F') 
          ) stop('LOAD_MODEL(): options must be "N" and/or "F" in PDL definition of coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".') 
          if (tmpV == 'N') bm_pdlMatrix[4,coeffPos]=1 
          if (tmpV == 'F') bm_pdlMatrix[5,coeffPos]=1 
        }
        
        #check if PDL is 0 1 then error due to redundance
        if (bm_pdlMatrix[2,coeffPos]==0 && bm_pdlMatrix[3,coeffPos]==1) 
          stop('LOAD_MODEL(): PDL with degree 0 and length 1 can be removed: coefficient "',pdlSingleElement[1],'" in behavioral "',behavioralName,'".') 
      }		
      
      #add lagged coefficient to coefficient list 
      for (pdlMatrixIdx in 1:length(coeffOriginal)) 
      {				
        #coefficient has pdl
        if (bm_pdlMatrix[1,pdlMatrixIdx]==1)
        {
          #insert required coefficient in list of coefficients
          coeffToBeInserted=c() 
          eqCompToBeInserted=c() 
          #pdl length > 1 then insert
          if (bm_pdlMatrix[3,pdlMatrixIdx]>1)
          {
            for (idxCoeffToBeIns in 1:(bm_pdlMatrix[3,pdlMatrixIdx]-1))
            {
              #e.g. coeffToBeInserted=C06_PDL_6
              coeffToBeInserted=c(coeffToBeInserted,paste0(coeffOriginal[pdlMatrixIdx],'__PDL__',idxCoeffToBeIns)) 
              #e.g. eqCompToBeInserted=LAG(  (LAG(LOG(KSTAR))-LAG(LOG(KSTAR),2))  ,9)
              eqCompToBeInserted=c(eqCompToBeInserted,paste0('TSLAG(',eqRegressorsNamesOriginal[pdlMatrixIdx],',',idxCoeffToBeIns,')'))
            }					
          }
          
          #new coefficients and new regressors will be inserted after original ones
          idxIntoInsert=which(coeff==coeffOriginal[pdlMatrixIdx])
          
          if (idxIntoInsert<length(coeff)) coeff=c(coeff[1:idxIntoInsert],coeffToBeInserted,coeff[(idxIntoInsert+1):length(coeff)])
          else coeff=c(coeff[1:idxIntoInsert],coeffToBeInserted) 
          idxIntoInsert=which(eqRegressorsNames==eqRegressorsNamesOriginal[pdlMatrixIdx])
          #warning: adding LAGs can duplicate regressors 
          #e.g. we have a MDL EQ with regressors (i) "p" and (ii) "LAG(p,1)" in a klein model
          #if we have PDL on "p" when we expand we will have a duplicated regressor LAG(p,1).
          #Now, if we also have a PDL on second original regressors (ii) LAG(p,1)
          #we have to take the last LAG(p,1) in list as the insertion point for new regressors
          #for PDL of (ii) "LAG(p,1)" (regressor must be different in model definition so last one is ok)
          idxIntoInsert=idxIntoInsert[length(idxIntoInsert)]
          
          if (idxIntoInsert<length(eqRegressorsNames)) eqRegressorsNames=c(eqRegressorsNames[1:idxIntoInsert],eqCompToBeInserted,eqRegressorsNames[(idxIntoInsert+1):length(eqRegressorsNames)])
          else eqRegressorsNames=c(eqRegressorsNames[1:idxIntoInsert],eqCompToBeInserted) 
        }      
      }		
      
      #create PDL restriction matrix and vector (using Tartaglia)
      for (pdlMatrixIdx in 1:length(coeffOriginal)) 
      {
        #coefficient has PDL
        if (bm_pdlMatrix[1,pdlMatrixIdx]==1)
        {
          #pdllength > pdldegree+1 then insert row
          if (bm_pdlMatrix[3,pdlMatrixIdx]>bm_pdlMatrix[2,pdlMatrixIdx]+1)
          {
            #get tartaglia coefficients
            tmpTartagliaRow=.bm_tartaglia_pdl(bm_pdlMatrix[2,pdlMatrixIdx]) 
            for(tartagliaIdx in 1:(bm_pdlMatrix[3,pdlMatrixIdx]-(bm_pdlMatrix[2,pdlMatrixIdx]+1)))
            {
              #create R matrix row
              temp_row=rep(0,length(coeff)) 
              idxCoeff=which(coeff==coeffOriginal[pdlMatrixIdx])
              #insert tartaglia coeff starting from correct index
              temp_row[(idxCoeff-1+tartagliaIdx):(idxCoeff+length(tmpTartagliaRow)-2+tartagliaIdx)]=tmpTartagliaRow
              
              #e.g. bm_pdlRestrictionMatrix=(0 0 1 -2 1 0 0; 0 0 0 1 -2 1 0 )
              bm_pdlRestrictionMatrix=rbind(bm_pdlRestrictionMatrix,temp_row) 
              bm_pdlRestrictionVector=c(bm_pdlRestrictionVector,0) 
            }					
          }
          
          #check N
          #put one row in matrix with 1 on original coeff (no LAGged) e.g. c02
          if (bm_pdlMatrix[4,pdlMatrixIdx]==1 && bm_pdlMatrix[3,pdlMatrixIdx]>1)
          {			
            temp_row=rep(0,length(coeff)) 
            idxCoeff=which(coeff==coeffOriginal[pdlMatrixIdx])
            temp_row[idxCoeff]=1 
            bm_pdlRestrictionMatrix=rbind(bm_pdlRestrictionMatrix,temp_row) 
            bm_pdlRestrictionVector=c(bm_pdlRestrictionVector,0) 
          }
          
          #check F
          #put one row in matrix with 1 on last LAGged coeff e.g. LAG(C02,4)
          if (bm_pdlMatrix[5,pdlMatrixIdx]==1 && bm_pdlMatrix[3,pdlMatrixIdx]>1)
          {
            temp_row=rep(0,length(coeff)) 
            idxCoeff=which(coeff==coeffOriginal[pdlMatrixIdx])
            temp_row[idxCoeff+bm_pdlMatrix[3,pdlMatrixIdx]-1]=1 
            bm_pdlRestrictionMatrix=rbind(bm_pdlRestrictionMatrix,temp_row) 
            bm_pdlRestrictionVector=c(bm_pdlRestrictionVector,0) 
          }
          
          #check N and F and length=2 -> error
          if (bm_pdlMatrix[4,pdlMatrixIdx]==1 && bm_pdlMatrix[5,pdlMatrixIdx]==1 && bm_pdlMatrix[3,pdlMatrixIdx]==2)
          {
            stop('LOAD_MODEL(): redundant options "N" and "F" set in coefficient "',coeffOriginal[pdlMatrixIdx],'" with PDL of length 2 in behavioral "',behavioralName,'".')
          }
        }
      }  
    }#endif pdl
    
    if (! is.null(bm_pdlRestrictionMatrix)) 
    {
      rownames(bm_pdlRestrictionMatrix)=c() 
      colnames(bm_pdlRestrictionMatrix)=c() 
    }
    
    if (! is.null(bm_pdlRestrictionVector)) 
    {
      rownames(bm_pdlRestrictionVector)=c() 
      colnames(bm_pdlRestrictionVector)=c() 
    }
   
    #check behavioral has RESTRICT def
    #RESTRICT> definition can span on several lines 
    #or have multiple defs separated by ";"
    restrictLocalIdx=which(restrictIndexes %in% behavioralIndexes[idxBI]:(nextDefIdx-1)) 
    restrictRaw=NULL 
    if (length(restrictLocalIdx)>0) 
    {
      #analyze all restrict definitions
      restrictRaw='' 
      for (restrictLLidx in 1:length(restrictLocalIdx))
      {
        #read all lines of restrict definition
        nextKwIdx=min(kwIdx[which(kwIdx>restrictIndexes[restrictLocalIdx[restrictLLidx]])]) 
        restrictRawSingle=paste(cleanModel[restrictIndexes[restrictLocalIdx[restrictLLidx]]:(nextKwIdx-1)],collapse=';') 
        restrictRawSingle=gsub("^RESTRICT\\s*>", "", restrictRawSingle, ignore.case=TRUE) 
        restrictRawSingle=gsub("\\s*", "", restrictRawSingle) 
        #e.g. restrictRaw="C01+C02=0;C01+C03=0"
        restrictRaw=paste0(restrictRawSingle,';',restrictRaw) 
      }
      
      #analyze restrictRaw
      restrictComponents=strsplit(restrictRaw,';')[[1]] 
      if (length(restrictComponents)<1) stop('LOAD_MODEL(): NULL length in restriction definition "',restrictRaw,' in behavioral "',behavioralName,'".')
      
      #matrix R and vector r of restriction
      bm_matrixR=c() 
      bm_vectorR=c() 
      for (idxRestricComp in 1:length(restrictComponents))
      {
        #split equal
        restrComponentSplit=strsplit(restrictComponents[[idxRestricComp]],'\\=')[[1]]
        
        #check equal signs count
        if (length(restrComponentSplit)!=2) 
          stop('LOAD_MODEL(): syntax error in restriction "',restrictComponents[[idxRestricComp]],'" in behavioral "',behavioralName,'".\nUsage: "RESTRICT> alpha0 * coeff0 + ... + alphaN * coeffN = const".')
        
        restrictionRHS=restrComponentSplit[2] 
        restrictionLHS=restrComponentSplit[1] 
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
          stop('LOAD_MODEL(): syntax error in restriction "',restrictComponents[[idxRestricComp]],'" in behavioral "',behavioralName,'".\nUsage: "RESTRICT> alpha0 * coeff0 + ... + alphaN * coeffN = const".')
        
        #check LHS is linear comb of coeff
        regExprRes=paste0('^((',
                          charsOnNumWithSign,paste0('\\*|\\+|-)?(',
                                                    paste0(coeff,collapse='|')  ,')'),')+$')
        
        if (length(grep(regExprRes,restrictionLHS))==0) 
          stop('LOAD_MODEL(): syntax error in restriction "',restrictComponents[[idxRestricComp]],'" in behavioral "',behavioralName,'".\nUsage: "RESTRICT> alpha0 * coeff0 + ... + alphaN * coeffN = const".')
        
        bm_tmpRrow=c() 
        #exctract coeff multiplier in restriction
        for (idxCoeff in 1:length(coeff))
        {  
          tmpRegExp=gregexpr(paste0('(',charsOnNumWithSign,'\\*|\\+|-)?(',
                                   coeff[idxCoeff],')(\\+|-|$)'),restrictionLHS) 
          #e.g. localMultiplier="+C02"
          localMultiplierList=regmatches(restrictionLHS,tmpRegExp)
          
          localMultiplier=localMultiplierList[[1]]
          
          #duplicate coefficient in single restriction
          if (length(localMultiplier)>1)   
            stop('LOAD_MODEL(): syntax error in restriction "',restrictComponents[[idxRestricComp]],'" in behavioral "',behavioralName,'".\nUsage: "RESTRICT> alpha0 * coeff0 + ... + alphaN * coeffN = const".')
          
          if (length(localMultiplier)==1)
          {
            #coeff exists in restriction
            localMultiplier=gsub(paste0('(\\*|',coeff[idxCoeff],'(\\+|-)?)'),'',localMultiplier)
            
            #exctract value
            if (localMultiplier=='+' || localMultiplier=='' ) {
              localMultiplier=1 
            } else if (localMultiplier=='-')
            {
              localMultiplier=-1 
            } else {
              
              localMultiplier=as.numeric(localMultiplier) 
            }
            
          } else 
          {
            localMultiplier=0 
          }
          
          bm_tmpRrow=c(bm_tmpRrow,localMultiplier)
          
        }
        
        #e.g. C01+C02=0;C01+C03=0
        #bm_vectorR=(0;0)
        bm_vectorR=c(bm_vectorR,as.numeric(restrictionRHS))
        #e.g. bm_matrixR=(0 1 1 0 0...; 0 1 0 1 0 ...)
        bm_matrixR=rbind(bm_matrixR,bm_tmpRrow)
        
      }#end cycle component of restriction
      
      if (! is.null(bm_matrixR))
      {
        rownames(bm_matrixR)=c() 
        colnames(bm_matrixR)=c() 
      }
      
      if (! is.null(bm_vectorR))
      {
        rownames(bm_vectorR)=c() 
        colnames(bm_vectorR)=c() 
      }      
      
      behavioralTmp$matrixR=bm_matrixR 
      behavioralTmp$vectorR=bm_vectorR 
    }#endif restrict
    
    #get ERROR>
    #check behavioral has ERROR def
    errorLocalIdx=which(errorIndexes %in% behavioralIndexes[idxBI]:(nextDefIdx-1)) 
    errorRaw=NULL 
    errorType=NULL 
    errorDim=NULL 
    if (length(errorLocalIdx)>0) 
    {
      if (length(errorLocalIdx)>1) stop('LOAD_MODEL(): multiple ERROR definitions in behavioral "',behavioralName,'".') 
      errorRaw='' 
      nextKwIdx=min(kwIdx[which(kwIdx>errorIndexes[errorLocalIdx])]) 
      errorRaw=paste(cleanModel[errorIndexes[errorLocalIdx]:(nextKwIdx-1)],collapse=' ') 
      errorRaw=gsub("^ERROR\\s*>", "", errorRaw, ignore.case=TRUE) 
      errorRaw=gsub("^\\s*|\\s*$", "", errorRaw) 
      #catch auto(n) directive with n=1..9
      if (length(grep('^AUTO\\([1-9]\\)',errorRaw))>0){
        errorType='AUTO' 
        errorDim=gsub("AUTO\\(", "", errorRaw) 
        errorDim=as.numeric(gsub("\\)", "", errorDim)) 
        if ((length(errorDim)==0) || ! is.finite(errorDim) || (errorDim<1 )  || (errorDim>9 )) 
          stop('LOAD_MODEL(): syntax error in ERROR definition "',errorRaw,'" in behavioral "',behavioralName,'". Usage: ERROR> AUTO(n), n=1..9') 
      } else {
        stop('LOAD_MODEL(): syntax error in ERROR definition "',errorRaw,'" in behavioral "',behavioralName,'". Usage: ERROR> AUTO(n), n=1..9')
      }
    }
    
    #check behavioral has IV> def
    #IV can span on several lines or have several definitions separated by ";"
    ivLocalIdx=which(ivIndexes %in% behavioralIndexes[idxBI]:(nextDefIdx-1)) 
    ivRaw=NULL 
    namesOnIV=c() 
    if (length(ivLocalIdx)>0) 
    {
      #analyze all IV definitions
      ivRaw='' 
      for (ivLLidx in 1:length(ivLocalIdx))
      {
        nextKwIdx=min(kwIdx[which(kwIdx>ivIndexes[ivLocalIdx[ivLLidx]])]) 
        ivRawSingle=paste(cleanModel[ivIndexes[ivLocalIdx[ivLLidx]]:(nextKwIdx-1)],collapse='') 
        ivRawSingle=gsub("^IV\\s*>", "", ivRawSingle, ignore.case=TRUE) 
        ivRawSingle=gsub("\\s*", "", ivRawSingle) 
        ivRaw=paste0(ivRawSingle,';',ivRaw) 
      }
      
      #analyze ivRaw
      ivComponents=strsplit(ivRaw,';')[[1]] 
      if (length(ivComponents)<1) stop('LOAD_MODEL(): NULL length on IV definition "',ivRaw,' in behavioral "',behavioralName,'".')
      
      for (idxIVComp in 1:length(ivComponents))
      {
        if (! .checkExpression(ivComponents[idxIVComp]))
          stop('LOAD_MODEL(): syntax error in IV definition: "',ivComponents[idxIVComp],'" in behavioral "',behavioralName,'".')
        #check unknown funs names
        unknownFuns=.unknownFunsNames(ivComponents[idxIVComp],reservedKeyw) 
        if (length(unknownFuns)>0) stop('LOAD_MODEL(): unsupported functions in IV definition: "',ivComponents[idxIVComp],'" in behavioral "',behavioralName,'": ',
                                        paste0(paste0(unknownFuns,'()'),collapse=', ')) 
        #trim and extract symbol names from IV
        namesOnIV=c(namesOnIV,strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnEqCleaner,' ',ivComponents[idxIVComp],ignore.case=TRUE)),'\\s+')[[1]]) 
       }
      
      #remove numbers from names
      rmvNumOnIVIdx=c() 
      for (idxNamesOnIV in 1:length(namesOnIV)) {
        if (length(grep(strongCharOnNumbs,namesOnIV[idxNamesOnIV]))>0) 
        {
          rmvNumOnIVIdx=c(rmvNumOnIVIdx,idxNamesOnIV) 
        }
      }
      if (length(rmvNumOnIVIdx)>0) namesOnIV=namesOnIV[-rmvNumOnIVIdx] 
      #check names
      for (idxNOEQ in 1:length(namesOnIV)) {
        if (length(grep(allowedCharOnName,namesOnIV[idxNOEQ]))==0) 
        {
          stop('LOAD_MODEL(): syntax error in IV definition: "',ivRaw,'". The following symbol cannot be a variable name: "',namesOnIV[idxNOEQ],'" (behavioral "',behavioralName,'").') 
        }
        
        if ((grepl('__',namesOnIV[idxNOEQ])))
          stop(paste0('LOAD_MODEL(): IV definition "',namesOnIV[idxNOEQ],'" cannot contain double underscore "__" (behavioral "',behavioralName,'").'))
      }
    }
    
    behavioralTmp$eq=eqRaw 
    behavioralTmp$iv=ivRaw 
    behavioralTmp$eqCoefficientsNames=coeff 
    behavioralTmp$eqCoefficientsCount=length(coeff) 
    behavioralTmp$eqCoefficientsNamesOriginal=coeffOriginal 
    behavioralTmp$eqComponentsNames=sort(unique(namesOnEq)) 
    behavioralTmp$IVComponentsNames=sort(unique(namesOnIV)) 
    behavioralTmp$tsrange=behavioralTsrange 
    behavioralTmp$storeVarName=storeVarName 
    behavioralTmp$storePosition=storePosition 
    behavioralTmp$eqRegressorsNames=eqRegressorsNames 
    behavioralTmp$eqRegressorsNamesOriginal=eqRegressorsNamesOriginal 
    behavioralTmp$pdlRaw=pdlRaw 
    behavioralTmp$pdlMatrix=bm_pdlMatrix 
    behavioralTmp$pdlRestrictionMatrix=bm_pdlRestrictionMatrix 
    behavioralTmp$pdlRestrictionVector=bm_pdlRestrictionVector 
    behavioralTmp$restrictRaw=restrictRaw 
    behavioralTmp$errorRaw=errorRaw 
    behavioralTmp$errorType=errorType 
    behavioralTmp$errorDim=errorDim 
    behavioralTmp$lhsFun=lhsFun 
    #append to model list of behavioral
    if (! is.null(model$behaviorals[[behavioralName]])) 
      stop('LOAD_MODEL(): duplicated behavioral name "',behavioralName,'".') 
    model$behaviorals[[behavioralName]]=behavioralTmp 
    
    #add name to vendog in related position (required for ordering algo)
    #we want the same order in vendog as the one in MDL
    model$vendog[behavioralIndexes[idxBI]]=behavioralName 
    
  }# end behavioral loop
  
  
  #analyze code identities ----------------------------------------------------------------------
  
  model$identities=list() 
  if (length(identityIndexes)>0) for(idxII in 1:length(identityIndexes))
  {
    identityTmp=list() 
    #get next keyword definition index
    nextKwIdx=min(kwIdx[which(kwIdx>identityIndexes[idxII])]) 
    #read all lines of behavior definition
    identitylRaw=paste(cleanModel[identityIndexes[idxII]:(nextKwIdx-1)],collapse=' ')
    identitylRaw=gsub("^IDENTITY\\s*>", "", identitylRaw, ignore.case=TRUE) 
    identitylRaw=gsub("^\\s+|\\s+$", "", identitylRaw) 
    #get identity name
    identityName=gsub("^\\s+|\\s+$", "",identitylRaw) 
    if (nchar(identityName)==0) stop('LOAD_MODEL(): unknown identity name in line: "',cleanModel[identityIndexes[idxII]],'".') 
    
    if (length(grep(allowedCharOnName,identityName))==0) stop('LOAD_MODEL(): invalid identity name: "',identityName,'"') 
    #get next eq definition index
    nextDefIdx=min(eqIdenDefIdx[which(eqIdenDefIdx>identityIndexes[idxII])]) 
    #check identity has eq def
    eqLocalIdx=which(eqIndexes %in% identityIndexes[idxII]:(nextDefIdx-1)) 
    if (length(eqLocalIdx)==0) stop('LOAD_MODEL(): no EQ definition in identity "',identityName,'".') 
    if (length(eqLocalIdx)>1) stop('LOAD_MODEL(): multiple EQ definitions in identity "',identityName,'".') 
    #get eq
    #read all lines of EQ definition
    nextKwIdx=min(kwIdx[which(kwIdx>eqIndexes[eqLocalIdx])]) 
    eqRaw=paste(cleanModel[eqIndexes[eqLocalIdx]:(nextKwIdx-1)],collapse=' ') 
    eqRaw=gsub("^EQ\\s*>", "", eqRaw, ignore.case=TRUE) 
    eqRaw=gsub("\\s*", "", eqRaw) 
    #check unknown funs names
    unknownFuns=.unknownFunsNames(eqRaw,reservedKeyw)
    if (length(unknownFuns)>0) stop('LOAD_MODEL(): unsupported functions in identity "',identityName,'" in EQ definition: "',eqRaw,'": ',
                                    paste0(paste0(unknownFuns,'()'),collapse=', ')) 
    
    #check EQ is non trivial
    if (nchar(eqRaw)==0 || !grepl('=',eqRaw))
      stop('LOAD_MODEL(): syntax error in identity "',identityName,'" in EQ definition: "',eqRaw,'".')  
    
    #trim and extract symbol names from EQ
    #we keep dependent
    namesOnEq=strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnEqCleaner,' ',eqRaw,ignore.case=TRUE)),'\\s+')[[1]]  
    
    #check EQ is non trivial
    if ( length(namesOnEq)==0 )
      stop('LOAD_MODEL(): syntax error in identity "',identityName,'" in EQ definition: "',eqRaw,'".')  
    
    eqRawSplitted=strsplit(eqRaw,'\\=')[[1]] 
    if (length(eqRawSplitted)!=2) stop('LOAD_MODEL(): syntax error in identity "',identityName,'" in EQ definition: "',eqRaw,'".')
    
    #convert RHS text to expression
    rhsExp=eqRawSplitted[2] 
    lhsExp=eqRawSplitted[1] 
    if (lhsExp=='') stop('LOAD_MODEL(): syntax error in identity "',identityName,'" in EQ definition: "',eqRaw,'".')
    
    #find lhs func
    tryCatch({
      lhsFun=.parseLhsEQ(lhsExp,allowedLhsEQfuns) 
    },error=function(err){
      stop('LOAD_MODEL(): syntax error in identity "',identityName,'" in LHS of EQ definition: "',eqRaw,'".')})
    
    # lhs identity -------------------------------------------------------------------
    
    #check lhs fun syntax
    if (lhsFun$funName=='I')
    {
      #check EQ contains identity name
      if (lhsFun$args!=identityName)
        stop('LOAD_MODEL(): LHS of EQ definition: "',eqRaw,'" must contain only the identity name "',identityName,'" or the allowed uppercase LHS functions: ',paste0(paste0(allowedLhsEQfunsPub,'()'),collapse=', ')) 
      
      if (lhsExp!=paste0(identityName))
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',identityName,'" are allowed on the LHS of EQ definition, e.g. EQ> ',identityName,'=...') 
      
    } else if (lhsFun$funName=='LOG')
    {
      if (lhsFun$argsCount!=1 || lhsFun$args!=identityName ) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the identity "',identityName,'". If the LOG of the endogenous variable is requested please try the following: "LOG(',identityName,')"') 
      
      if (lhsExp!=paste0('LOG(',identityName,')'))
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',identityName,'" are allowed on the LHS of EQ definition, e.g. EQ> LOG(',identityName,')=...') 
      
    } else if (lhsFun$funName=='EXP')
    {
      if (lhsFun$argsCount!=1 || lhsFun$args!=identityName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the identity "',identityName,'". If the EXP of the endogenous variable is requested please try the following: "EXP(',identityName,')"') 
      
      if (lhsExp!=paste0('EXP(',identityName,')'))
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',identityName,'" are allowed on the LHS of EQ definition, e.g. EQ> EXP(',identityName,')=...') 
      
    } else if (lhsFun$funName=='TSDELTA' || lhsFun$funName=='DEL')
    {
      if (  lhsFun$argsCount>2 || lhsFun$args[1]!=identityName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the identity "',identityName,'". If the TSDELTA of the endogenous variable is requested please try the following: "TSDELTA(',identityName,')" or "TSDELTA(',identityName,',n)"') 
    
      if (lhsExp!=paste0(lhsFun$funName,'(',identityName,',',lhsFun$args[2],')') && lhsExp!=paste0(lhsFun$funName,'(',identityName,')'))
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',identityName,'" are allowed on the LHS of EQ definition, e.g. EQ> TSDELTA(',identityName,',n)=...') 
      
    } else if (lhsFun$funName=='TSDELTAP' )
    {
      if (  lhsFun$argsCount>2 || lhsFun$args[1]!=identityName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the identity "',identityName,'". If the TSDELTAP of the endogenous variable is requested please try the following: "TSDELTAP(',identityName,')" or "TSDELTAP(',identityName,',n)"') 
      
      if (lhsExp!=paste0(lhsFun$funName,'(',identityName,',',lhsFun$args[2],')') && lhsExp!=paste0(lhsFun$funName,'(',identityName,')') )
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',identityName,'" are allowed on the LHS of EQ definition, e.g. EQ> TSDELTAP(',identityName,',n)=...') 
      
    } else if (lhsFun$funName=='TSDELTALOG' )
    {
      if (  lhsFun$argsCount>2 || lhsFun$args[1]!=identityName) 
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'" of the identity "',identityName,'". If the TSDELTALOG of the endogenous variable is requested please try the following: "TSDELTALOG(',identityName,')" or "TSDELTALOG(',identityName,',n)"') 
      
      if (lhsExp!=paste0(lhsFun$funName,'(',identityName,',',lhsFun$args[2],')') && lhsExp!=paste0(lhsFun$funName,'(',identityName,')') )
        stop('LOAD_MODEL(): syntax error in LHS of EQ definition: "',eqRaw,'". Only the identity function or a single transformation of the endogenous variable "',identityName,'" are allowed on the LHS of EQ definition, e.g. EQ> TSDELTALOG(',identityName,',n)=...') 
    }
    
    if (! .checkExpression(rhsExp)) stop('LOAD_MODEL(): syntax error in identity "',identityName,'" in RHS of EQ definition: "',eqRaw,'".')
    
    #remove numbers
    rmvNumOnEqIdx=c() 
    for (idxNamesOnEq in 1:length(namesOnEq)) {
      if (length(grep(strongCharOnNumbs,namesOnEq[idxNamesOnEq]))>0) 
      {
        rmvNumOnEqIdx=c(rmvNumOnEqIdx,idxNamesOnEq) 
      }
    }
    if (length(rmvNumOnEqIdx)>0) namesOnEq=namesOnEq[-rmvNumOnEqIdx] 
    #check components name
    for (idxNOEQ in 1:length(namesOnEq)) {
      if (length(grep(allowedCharOnName,namesOnEq[idxNOEQ]))==0) 
      {
        stop('LOAD_MODEL(): syntax error in identity "',identityName,'" in EQ definition: "',eqRaw,'". The following symbol cannot be a variable name: "',namesOnEq[idxNOEQ],'".') 
      }
    }
    
    #check identity has IF def
    ifLocalIdx=which(ifIndexes %in% identityIndexes[idxII]:(nextDefIdx-1)) 
    ifRaw=NULL 
    identityTmp$hasIF=FALSE 
    if (length(ifLocalIdx)>0) 
    {
      if (length(ifLocalIdx)>1) stop('LOAD_MODEL(): multiple IF definitions in identity "',identityName,'".') 
      ifRaw='' 
      nextKwIdx=min(kwIdx[which(kwIdx>ifIndexes[ifLocalIdx])]) 
      ifRaw=paste(cleanModel[ifIndexes[ifLocalIdx]:(nextKwIdx-1)],collapse=' ') 
      ifRaw=gsub("^IF\\s*>", "", ifRaw, ignore.case=TRUE) 
      ifRaw=gsub("^\\s*|\\s*$", "", ifRaw) 
      #check for unknown funs
      unknownFuns=.unknownFunsNames(ifRaw,reservedKeyw)
      if (length(unknownFuns)>0) stop('LOAD_MODEL(): unsupported functions in IF definition: "',ifRaw,'" in identity "',identityName,'": ',
                                      paste0(paste0(unknownFuns,'()'),collapse=', ')) 
      #check double logical operator
      if (grepl('&&',ifRaw)) stop('LOAD_MODEL(): IF definition: "',ifRaw,'" in identity "',identityName,'" cannot contain "&&". Please consider using the single operator "&"') 
      if (grepl('\\|\\|',ifRaw)) stop('LOAD_MODEL(): IF definition: "',ifRaw,'" in identity "',identityName,'" cannot contain "||". Please consider using the single operator "|"') 
      #extract components names from IF
      namesOnIf=strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnIfCleaner,' ',ifRaw,ignore.case=TRUE)),'\\s+')[[1]] 
      
      #remove numbers
      rmvNumOnIfIdx=c() 
      for (idxNamesOnIf in 1:length(namesOnIf)) {
        if (length(grep(strongCharOnNumbs,namesOnIf[idxNamesOnIf]))>0) 
        {
          rmvNumOnIfIdx=c(rmvNumOnIfIdx,idxNamesOnIf) 
        }
      }
      if (length(rmvNumOnIfIdx)>0) namesOnIf=namesOnIf[-rmvNumOnIfIdx] 
      #check IF condition is time series
      if (length(namesOnIf)==0) stop(paste0('LOAD_MODEL(): syntax error in IF condition "',ifRaw,'" in identity "',identityName,'". Logical condition must contain time series.')) 
      #check components name
      for (idxNOEQ in 1:length(namesOnIf)) {
        if (length(grep(allowedCharOnName,namesOnIf[idxNOEQ]))==0) 
        {
          stop(paste0('LOAD_MODEL(): syntax error in IF definition: "',ifRaw,'" in identity "',identityName,'". The following symbol cannot be a variable name: "',namesOnIf[idxNOEQ],'".')) 
        }
      }
      
      namesOnEq=c(namesOnEq,namesOnIf) 
    }	
    
    #store stuff with trailing ";"
    identityTmp$eqRaw=paste0(eqRaw,';') 
    
    #we must deal with multiple lhsFun due to IF conditions
    #e.g. __IF__ (x<0) __THEN__ EXP(y)=x; __IF__(x>=0) __THEN__ DELTAP(y)=x
    #we need a list of lists
    identityTmp$multipleLhsFun=list(lhsFun) 
    
    #build identity eq as __IF__ (condition) __THEN__ EQ
    if (! is.null(ifRaw)) 
    {
      tmpIfRaw=ifRaw 
      tmpIfRaw=gsub('\\.EQ\\.',' == ',tmpIfRaw) 
      tmpIfRaw=gsub('\\.NE\\.',' != ',tmpIfRaw) 
      tmpIfRaw=gsub('\\.GE\\.',' >= ',tmpIfRaw) 
      tmpIfRaw=gsub('\\.LE\\.',' <= ',tmpIfRaw) 
      tmpIfRaw=gsub('\\.GT\\.',' > ',tmpIfRaw) 
      tmpIfRaw=gsub('\\.LT\\.',' < ',tmpIfRaw) 
      tmpIfRaw=gsub('<-','< -',tmpIfRaw) 
     
       if (! grepl('(==|<|>|>=|<=|!=|&|\\|)',tmpIfRaw))
        stop(paste0('LOAD_MODEL(): syntax error in IF condition "',ifRaw,'" in identity "',identityName,'". No logical operator found.'))
      
      if (! .checkExpression(tmpIfRaw)) stop('LOAD_MODEL(): syntax error in IF definition: "',tmpIfRaw,'" in identity "',identityName,'".')
      
      identityTmp$eqFull=paste0('__IF__ (',ifRaw,') __THEN__ ',identityTmp$eqRaw) 
      identityTmp$ifCondition=paste0(tmpIfRaw,';') 
      identityTmp$hasIF=TRUE 
    } 
    else
    {
      identityTmp$ifCondition=paste0('__TRUE__;') 
      identityTmp$eqFull=identityTmp$eqRaw 
    }
    
    identityTmp$eqComponentsNames=sort(unique(namesOnEq)) 
    #RAW must be refined
    if (! is.null(ifRaw)) identityTmp$ifRaw=paste0(ifRaw,';') 
    #append to model list of identities.  
    if (! is.null(model$identities[[identityName]])) 
    {
      #check lhs function is the same for all identity equations in same vendog
      baseLhsFun=model$identities[[identityName]]$multipleLhsFun[[1]]
      if (baseLhsFun$funName!=lhsFun$funName || any(baseLhsFun$args != lhsFun$args)) 
      {
        stop('LOAD_MODEL(): error in identity "',identityName,'", all EQ equations must have the same LHS function.')
      }
      
      model$identities[[identityName]]$eqRaw=paste0(model$identities[[identityName]]$eqRaw,identityTmp$eqRaw) 
      model$identities[[identityName]]$ifCondition=paste0(model$identities[[identityName]]$ifCondition,identityTmp$ifCondition) 
      model$identities[[identityName]]$multipleLhsFun[length(model$identities[[identityName]]$multipleLhsFun)+1]=
        list(lhsFun) 
      if (! is.null(ifRaw)) 
      {
        model$identities[[identityName]]$ifRaw=paste0(model$identities[[identityName]]$ifRaw,identityTmp$ifRaw) 
        #identity$hasIf == TRUE if any eq has IF condition
        model$identities[[identityName]]$hasIF=model$identities[[identityName]]$hasIF || identityTmp$hasIF 
      }
      model$identities[[identityName]]$eqFull=paste0(model$identities[[identityName]]$eqFull,identityTmp$eqFull) 
      model$identities[[identityName]]$eqComponentsNames=sort(unique(c(model$identities[[identityName]]$eqComponentsNames,identityTmp$eqComponentsNames))) 
    } else
    {
      #if new identity has no if than store only latter
      model$identities[[identityName]]=identityTmp 
      
      #add name to vendog in related position (required for ordering algo)
      #we want the same order in vendog as the one in MDL (?)
      model$vendog[identityIndexes[idxII]]=identityName 
    }
    
  }# end identities loop
  
  #store stuff
  model$cleanModel=cleanModel 
  model$rawData=rawData 
  
  totNumEqs=length(behavioralIndexes) 
  totNumIds=length(model$identities) #can have more indexes than identities due to IF
  model$totNumEqs=totNumEqs 
  model$totNumIds=totNumIds 
  model$eqCoeffNum=totNumCoeff 
  
  #set max lag/leead to default
  model$max_lag=0 
  model$max_lead=0 
  
  if (totNumEqs+totNumIds==0) stop('LOAD_MODEL(): empty model.') 
  
  #remove NAs from vendog (we inserted vars in model$vendog using their line position in model text)
  model$vendog=model$vendog[! is.na(model$vendog)] 
  
  #create sublists for vendog behaviorals and vendog identities
  vendogBehaviorals=base::intersect(model$vendog,names(model$behaviorals)) 
  if (length(vendogBehaviorals)>0)
    if (all(nchar(vendogBehaviorals)>0))
      model$vendogBehaviorals=vendogBehaviorals 
  vendogIdentities=base::intersect(model$vendog,names(model$identities)) 
  if (length(vendogIdentities)>0)
    if (all(nchar(vendogIdentities)>0))
      model$vendogIdentities=vendogIdentities 
  
  #create field for exogenous variables
  vexog=c() 
  
  if (length(model$behaviorals)>0) for (i in 1:length(model$behaviorals))
  {
    vexog=c(vexog,model$behaviorals[[i]]$eqComponentsNames)
  }
  
  if (length(model$identities)>0) for (i in 1:length(model$identities))
  {
    vexog=c(vexog,model$identities[[i]]$eqComponentsNames)
  }
  
  vexog=unique(vexog) 
  
  #remove endogenous
  rmvIdx=which(vexog %in% model$vendog)
  if (length(rmvIdx)>0) vexog=vexog[-rmvIdx] 
  model$vexog=vexog 
  
  #deal with reserved words
  allNames=c(model$vendog,model$vexog) 
  if (any(grepl('__',allNames)))
    stop(paste0('LOAD_MODEL(): variable names cannot contain double underscore "__". Please change the following variable names: ',paste0(allNames[which(grepl('__',allNames))],collapse=', ')))
  
  if (length(model$vexog)==0 && (! quietly) ) .MODEL_outputText(outputText=!quietly,'\nLOAD_MODEL(): warning, model has no exogenous variables.\n')
  
  #probably never occur
  if (length(model$vendog)==0 && (! quietly) ) .MODEL_outputText(outputText=!quietly,'\nLOAD_MODEL(): warning, model has no endogenous variables.\n')
  
  # behavioral expr and max_lag  ----------------------------------
  
  .MODEL_outputText(outputText=!quietly,'Analyzing behaviorals...\n') 
  #build RHS expressions in behaviorals and identities (needed to speedup simulation...)
  
  #cycle in behaviorals
  length_model_behaviorals=length(model$behaviorals) 
  names_model_behaviorals=names(model$behaviorals) 
  if (length_model_behaviorals>0) for(idxB in 1:length_model_behaviorals)
  {#cycle in components
    outRHS='' 
    currentVendog=names_model_behaviorals[idxB] 
    currentBehavioral=model$behaviorals[[idxB]] 
    for (idxC in 1:length(currentBehavioral$eqCoefficientsNames))
    {
      #build "+COEF*REGR"
      outRHS=paste0(outRHS, '+' ,
                    paste0(currentVendog,'__COEFF__',currentBehavioral$eqCoefficientsNames[idxC]),
                    '*',
                    (currentBehavioral$eqRegressorsNames[idxC])) 
    }
    
    #replace fun names
    outRHS=.MODEL_MOD_FUNC_NAMES(outRHS) 
    #add constant adjustment (outRHS already starts with "+")
    outRHS=paste0(currentVendog,'__ADDFACTOR',outRHS) 
    #adapt eqRHS to autocorrelation if required
    if (! is.null(currentBehavioral$errorType) && currentBehavioral$errorType=='AUTO')
    {
      #add lagged errors to eqRHS, e.g. TSDELTAP(y)-EQ
      errorEqRaw=paste0(.MODEL_MOD_FUNC_NAMES(currentBehavioral$lhsFun$raw),'-(',outRHS,')') 
      for (idxED in 1:currentBehavioral$errorDim)
      {
        #final RHS EQ is original one plus lagged errors
        #warning: in DYNAMIC simulation RHO*LAG(y-f(x)) is always 0
        #if simulation period > errorDim
        outRHS=paste0(outRHS,'+',currentVendog,'__RHO__',idxED,'*.MODEL_TSLAG(',errorEqRaw,',',idxED,')') 
      }
    }
    
    localComponentsNames=currentBehavioral$eqComponentsNames 
    #create local sublist for behaviorals and identities components
    localComponentsNamesBehaviorals=base::intersect(model$vendogBehaviorals,localComponentsNames) 
    if (length(localComponentsNamesBehaviorals)>0)
      if (all(nchar(localComponentsNamesBehaviorals)>0))
        currentBehavioral$eqComponentsNamesBehaviorals=localComponentsNamesBehaviorals 
    localComponentsNamesIdentities=base::intersect(model$vendogIdentities,localComponentsNames) 
    if (length(localComponentsNamesIdentities)>0)
      if (all(nchar(localComponentsNamesIdentities)>0))
        currentBehavioral$eqComponentsNamesIdentities=localComponentsNamesIdentities 
    localComponentsNamesExogenous=base::intersect(model$vexog,localComponentsNames) 
    if (length(localComponentsNamesExogenous)>0)
      if (all(nchar(localComponentsNamesExogenous)>0))
        currentBehavioral$eqComponentsNamesExogenous=localComponentsNamesExogenous 
    tryCatch({
      
      outSim='' 
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
          stop('Non-numeric lag in LHS function: TSDELTA()') 
        
        if (currentBehavioral$lhsFun$argsCount>1) tmpS=currentBehavioral$lhsFun$args[2]
        else tmpS='1'
        outSim=paste0(currentVendog,'=',outRHS,'+.MODEL_TSLAG(',currentVendog,',',tmpS,')')
        
      } else if (currentBehavioral$lhsFun$funName=='TSDELTAP' )
      { 
        if (currentBehavioral$lhsFun$argsCount>1 && ! is.finite(as.numeric(currentBehavioral$lhsFun$args[2])))
          stop('Non-numeric lag in LHS function: TSDELTAP()') 
        
        if (currentBehavioral$lhsFun$argsCount>1) tmpS=currentBehavioral$lhsFun$args[2]
        else tmpS='1'
            
        outSim=paste0(currentVendog,'=(',outRHS,
                      ')*.MODEL_TSLAG(',currentVendog,',',tmpS,')/100',
                      '+.MODEL_TSLAG(',currentVendog,',',tmpS,')')
        
      } else if (currentBehavioral$lhsFun$funName=='TSDELTALOG' )
      { 
        if (currentBehavioral$lhsFun$argsCount>1 && ! is.finite(as.numeric(currentBehavioral$lhsFun$args[2])))
          stop('Non-numeric lag in LHS function: TSDELTALOG()') 
        
        if (currentBehavioral$lhsFun$argsCount>1) tmpS=currentBehavioral$lhsFun$args[2]
        else tmpS='1'
        
        outSim=paste0(currentVendog,'=exp(',outRHS,
                      ')*.MODEL_TSLAG(',currentVendog,',',tmpS,')')
      }
      
      #add indexes to vars
      outSim=.appendIndexToVars(outSim,c(localComponentsNames,
                                         paste0(currentVendog,'__ADDFACTOR'))
                                ,'0') 
      #explode model funs
      tempS=.explodeTSLAG(outSim)
      tempS=.explodeTSLEAD(tempS$output)
      tempS=.explodeTSDELTA(tempS$output)
      tempS=.explodeTSDELTAP(tempS$output)
      tempS=.explodeTSDELTALOG(tempS$output)
      tempS=.explodeMTOT(tempS$output)
      tempS=.explodeMAVE(tempS$output)
      
      outSim=tempS$output
      
      #get max lag lead
      tempOut=.getLowerLagAndHigherLead(outSim)
      model$max_lag=max(model$max_lag,-tempOut$outL) 
      model$max_lead=max(model$max_lead,tempOut$outH) 
      
      #save sim expression
      currentBehavioral$eqSimExpText=outSim 
      
    },error=function(e){stop(paste0('LOAD_MODEL(): error in behavioral "',
                                    currentVendog,'" while parsing EQ expression "',currentBehavioral$eq,'". ',e$message))}) 
    
    #store stuff
    model$behaviorals[[idxB]]=currentBehavioral 
  }
  
  # identity expr and max lag  ----------------------------------
  
  .MODEL_outputText(outputText=!quietly,'Analyzing identities...\n') 
  
  #cycle in identities
  length_model_identities=length(model$identities) 
  names_model_identities=names(model$identities) 
  if (length_model_identities>0) for(idxI in 1:length_model_identities)
  {
    currentIdentity=model$identities[[idxI]] 
    currentVendog=names_model_identities[idxI] 
    localComponentsNames=currentIdentity$eqComponentsNames 
    
    #create local sublist for behaviorals and identities components
    localComponentsNamesBehaviorals=base::intersect(model$vendogBehaviorals,localComponentsNames) 
    if (length(localComponentsNamesBehaviorals)>0)
      if (all(nchar(localComponentsNamesBehaviorals)>0))
        currentIdentity$eqComponentsNamesBehaviorals=localComponentsNamesBehaviorals 
    localComponentsNamesIdentities=base::intersect(model$vendogIdentities,localComponentsNames) 
    if (length(localComponentsNamesIdentities)>0)
      if (all(nchar(localComponentsNamesIdentities)>0))
        currentIdentity$eqComponentsNamesIdentities=localComponentsNamesIdentities 
    localComponentsNamesExogenous=base::intersect(model$vexog,localComponentsNames) 
    if (length(localComponentsNamesExogenous)>0)
      if (all(nchar(localComponentsNamesExogenous)>0))
        currentIdentity$eqComponentsNamesExogenous=localComponentsNamesExogenous 
    
    #get identities multiple eqs and conditions
    eqRawSplitted=strsplit(currentIdentity$eqRaw,';')[[1]] 
    ifConditionSplitted=strsplit(currentIdentity$ifCondition,';')[[1]] 
    
    if (length(eqRawSplitted)!=length(ifConditionSplitted))
      stop(paste0('LOAD_MODEL(): equations EQ count differs from logical IF condition count in identity "',currentVendog,'".')) 
    if (length(eqRawSplitted)!=length(currentIdentity$multipleLhsFun))
      stop(paste0('LOAD_MODEL(): equations EQ count differs from LHS functions count in identity "',currentVendog,'".')) 
    
    outSim='' 
    #cycle in eq
    for (eqIdx in 1:length(eqRawSplitted))
    {
      tryCatch({ 
        
        #get single eq and if condition in identity
        eqRaw=eqRawSplitted[eqIdx] 
        ifConditionRaw=ifConditionSplitted[eqIdx] 
        
        RHSRawComponent=strsplit(eqRaw,'=')[[1]][2] 
        
        if (ifConditionRaw=='__TRUE__') 
        {
          ifRawComponent='TRUE' 
        } else 
        {
          ifRawComponent=ifConditionRaw 
        }
        
        #parse expression
        ifModText=.MODEL_MOD_FUNC_NAMES(ifRawComponent) 
        RHSmodeText=.MODEL_MOD_FUNC_NAMES(RHSRawComponent) 
        
        #add add-factor
        RHSmodeText=paste0(currentVendog,'__ADDFACTOR+',RHSmodeText) 
        
        if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'I')
        {
          #nop
        } else if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'LOG')
        {
          RHSmodeText=paste0('exp(',RHSmodeText,')') 
        } else if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'EXP')
        {
          RHSmodeText=paste0('log(',RHSmodeText,')') 
        } else if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'TSDELTA' || currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'DEL')
        {
          if (currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1 && ! is.finite(as.numeric(currentIdentity$multipleLhsFun[[eqIdx]]$args[2])))
            stop('Non-numeric lag in LHS function: TSDELTA()') 
          
          if (currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1) tmpS=currentIdentity$multipleLhsFun[[eqIdx]]$args[2]
          else tmpS='1'
              
          RHSmodeText=paste0(RHSmodeText,'+.MODEL_TSLAG(',currentVendog,',',tmpS,')')
          
        } else if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'TSDELTAP' )
        {
          if (currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1 && ! is.finite(as.numeric(currentIdentity$multipleLhsFun[[eqIdx]]$args[2])))
            stop('Non-numeric lag in LHS function: TSDELTAP()') 
          
          if (currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1) tmpS=currentIdentity$multipleLhsFun[[eqIdx]]$args[2]
          else tmpS='1'
          
          RHSmodeText=paste0('(',RHSmodeText,
                             ')*.MODEL_TSLAG(',currentVendog,',',tmpS,')/100',
                             '+.MODEL_TSLAG(',currentVendog,',', tmpS,')')
          
        } else if (currentIdentity$multipleLhsFun[[eqIdx]]$funName == 'TSDELTALOG' )
        {
          if (currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1 && ! is.finite(as.numeric(currentIdentity$multipleLhsFun[[eqIdx]]$args[2])))
            stop('Non-numeric lag in LHS function: TSDELTALOG()') 
          
          if (currentIdentity$multipleLhsFun[[eqIdx]]$argsCount>1) tmpS=currentIdentity$multipleLhsFun[[eqIdx]]$args[2]
          else tmpS='1'
          
          RHSmodeText=paste0('exp(',RHSmodeText,
                             ')*.MODEL_TSLAG(',currentVendog,',',tmpS,')')
        }
        
        if (ifRawComponent=='TRUE') tmpS=paste0(currentVendog,'=',RHSmodeText,';')
        else  tmpS=paste0(currentVendog,'=.MODEL_VIF(',currentVendog,
                          ',',ifModText,',',RHSmodeText,');') 
          
        #build sim expresion w. constant adjustment
        outSim=paste0(outSim,tmpS)
      
        
      },error=function(e){
        stop(paste0('LOAD_MODEL(): error parsing EQ expression "',eqRaw,'" in identity "',currentVendog, '" - ', e$message)) 
      }) 
    }
    
    tryCatch({
      
      #add indexes to vars
      outSim=.appendIndexToVars(outSim,c(localComponentsNames,
                                         paste0(currentVendog,'__ADDFACTOR'))
                                ,'0') 
      #explode model funs
      tempS=.explodeTSLAG(outSim)
      tempS=.explodeTSLEAD(tempS$output)
      tempS=.explodeTSDELTA(tempS$output)
      tempS=.explodeTSDELTAP(tempS$output)
      tempS=.explodeTSDELTALOG(tempS$output)
      tempS=.explodeMTOT(tempS$output)
      tempS=.explodeMAVE(tempS$output)
      outSim=tempS$output
      
      currentIdentity$eqSimExpText=outSim
      
      #get max lag lead
      tempOut=.getLowerLagAndHigherLead(outSim)
      model$max_lag=max(model$max_lag,-tempOut$outL) 
      model$max_lead=max(model$max_lead,tempOut$outH) 
      
    },error=function(e){stop(paste0('LOAD_MODEL(): error while building simulation expression of identity ',
                                    currentVendog,'. ',e$message))}) 
    
    #update identity
    model$identities[[idxI]]=currentIdentity 
    
  }#end loop identities
  
  
  # reordering incidence ------------------------
  
  #backward looking model
  if (model$max_lead==0)
  {
    .MODEL_outputText(outputText=!quietly,'Optimizing...\n') 
    
    #build incidence matrix
    incidence_matrix=matrix(0,nrow=length(model$vendog), ncol=length(model$vendog)) 
    colnames(incidence_matrix)=model$vendog 
    rownames(incidence_matrix)=model$vendog 
    
    if (length_model_behaviorals>0) for(idxB in 1:length_model_behaviorals)
    {
      currentVendog=names_model_behaviorals[idxB] 
      currentBehavioral=model$behaviorals[[idxB]] 
      
      tryCatch({
        
        #get incidence matrix row
        vendogComponents=c(currentBehavioral$eqComponentsNamesBehaviorals,currentBehavioral$eqComponentsNamesIdentities)
        
        outSim=currentBehavioral$eqSimExpText
        
        incidenceVendogs=.getIncidenceVendogs(currentVendog,outSim,
                                              vendogComponents) 
        #save stuff
        if (length(incidenceVendogs)>0)
          for (idxIV in 1:length(incidenceVendogs))
            incidence_matrix[currentVendog,
                             incidenceVendogs[idxIV]]=1
        
      },error=function(e){stop(paste0('LOAD_MODEL(): error while building incidence row for behavioral ',
                                      currentVendog,'. ',e$message))}) 
    }
    
    if (length_model_identities>0) for(idxI in 1:length_model_identities)
    {
      currentIdentity=model$identities[[idxI]] 
      currentVendog=names_model_identities[idxI] 
      
      tryCatch({
        
        #get incidence matrix
        vendogComponents=c(currentIdentity$eqComponentsNamesBehaviorals,currentIdentity$eqComponentsNamesIdentities)
        
        outSim=currentIdentity$eqSimExpText
        
        incidenceVendogs=.getIncidenceVendogs(currentVendog,outSim,
                                              vendogComponents) 
        
        #save stuff
        if (length(incidenceVendogs)>0)
          for (idxIV in 1:length(incidenceVendogs))
            incidence_matrix[currentVendog,
                             incidenceVendogs[idxIV]]=1
        
      },error=function(e){stop(paste0('LOAD_MODEL(): error while building incidence vector of identity ',
                                      currentVendog,'. ',e$message))}) 
      
    }
    
    #export incidence matrix
    model$incidence_matrix=incidence_matrix
    
    #create vblocks
    modelBlocks=.buildBlocks(incidence_matrix)
    model$vblocks=modelBlocks$vblocks
    model$vpre=modelBlocks$vpre
    
    varsCount=length(model$vpre)
    if (length(model$vblocks)>0)
      for (idxB in 1:length(model$vblocks)) varsCount=varsCount+
                                          length(model$vblocks[[idxB]]$vsim)+
                                          length(model$vblocks[[idxB]]$vpost)
    if (varsCount != length(model$vendog)) 
      stop('LOAD_MODEL(): unknown error while optimizing model. Countings do not match.') 
    
  } else {
    .MODEL_outputText(outputText=!quietly,'This is a forward looking model...\n') 
    }
  
  # build sim expressions ----------------------------------------------
  
  #we need at least 2 rows in sim matrices in case of simType='FORECAST'
  max_lag_sim_1=max(model$max_lag,1)+1 
 
  tryCatch({
    if (length(model$behaviorals)>0) for(idxB in 1:length(model$behaviorals))
    {
      #eqSimExpText must be kept original (i.e. no __LEAD__ tx) because it is needed to
      #create leaded expressions
      
      #base sim equation as text, used only to lead base eq in leaded model
      model$behaviorals[[idxB]]$eqSimExpText=.addToIndexInString(model$behaviorals[[idxB]]$eqSimExpText,
                                                                 max_lag_sim_1)
      #base sim equation with lead tx as text
      #we need eqSimExpLeadedText also in non-leaded models
      #because users can simulate with forceForwardLooking=TRUE
      model$behaviorals[[idxB]]$eqSimExpLeadedText=.addToIndexInString(model$behaviorals[[idxB]]$eqSimExpText,
                                                                         0,
                                                                         TRUE,
                                                                         max_lag_sim_1)
      #parsed base sim eq with lead tx 
      model$behaviorals[[idxB]]$eqSimExp=parse(text=model$behaviorals[[idxB]]$eqSimExpLeadedText)
      
    }
  },error=function(err){stop(paste0('LOAD_MODEL(): error while building sim expression in behavioral "',names(model$behaviorals)[idxB],'". ',err$message))}) 
  
  tryCatch({
    if (length(model$identities)>0) for(idxI in 1:length(model$identities))
    {
      #eqSimExpText must be kept original (i.e. no __LEAD__ tx) because it is needed to
      #create leaded expressions
      
      #base sim equation as text, used only to lead base eq in leaded model
      model$identities[[idxI]]$eqSimExpText=.addToIndexInString(model$identities[[idxI]]$eqSimExpText,
                                                                max_lag_sim_1)
      
      #base sim equation with lead tx as text
      #we need eqSimExpLeadedText also in non-leaded models
      #because users can simulate with forceForwardLooking=TRUE
      model$identities[[idxI]]$eqSimExpLeadedText=.addToIndexInString(model$identities[[idxI]]$eqSimExpText,
                                                                        0,
                                                                        TRUE,
                                                                        max_lag_sim_1)
      
      #parsed base sim eq with lead tx 
      model$identities[[idxI]]$eqSimExp=parse(text=model$identities[[idxI]]$eqSimExpLeadedText)
      
    }
  },error=function(err){stop(paste0('LOAD_MODEL(): error while building sim expression in identity "',names(model$identities)[idxI],'". ',err$message))}) 
  
  .MODEL_outputText(outputText=!quietly,'Loaded model "',modelName,'":\n',
                    sprintf("%5i",model$totNumEqs),' behaviorals\n',
                    sprintf("%5i",model$totNumIds),' identities\n',
                    sprintf("%5i",model$eqCoeffNum),' coefficients\n',sep='') 
  
  model$modelName=modelName 
  
  #set model version
  model$bimets_version=getOption('BIMETS_VERSION')
  
  .MODEL_outputText(outputText=!quietly,'...LOAD MODEL OK\n')
  
  return(model) 
  
}


# LOAD MODEL DATA code ---------------------------------------------------------

LOAD_MODEL_DATA <- function(model=NULL, 
                            modelData=NULL, 
                            quietly=FALSE, 
                            ...)
{
  #check args
  if (is.null(model)) stop('LOAD_MODEL_DATA(): NULL model argument.') 
  if (!(inherits( model ,'BIMETS_MODEL')) ) stop('LOAD_MODEL_DATA(): model must be instance of BIMETS_MODEL class.') 
  
  .checkModelBimetsVersion(model, 'LOAD_MODEL_DATA')
  
  #check arg
  if (!(is.logical(quietly))) stop('LOAD_MODEL_DATA(): "quietly" must be TRUE or FALSE.')
  
  .MODEL_outputText(outputText=!quietly,paste0('Load model data "',substitute(modelData),'" into model "', model$modelName,'"...\n')) 
  
  #copy list to model
  model$modelData=modelData 
  tryCatch({
    .CHECK_MODEL_DATA(model,showWarnings=(! quietly),'LOAD_MODEL_DATA(): ') 
  },error=function(e){stop('LOAD_MODEL_DATA(): ',e$message)}) 
  
  #add frequency info
  model$frequency=frequency(model$modelData[[1]]) 
  
  .MODEL_outputText(outputText=!quietly,'...LOAD MODEL DATA OK\n') 
  
  return(model) 
  
}

# .CHECK MODEL_DATA code -------------------------------------------------------

.CHECK_MODEL_DATA <- function(model=NULL,showWarnings=TRUE,caller='.CHECK_MODEL_DATA(): ',...)
{
  if (is.null(model)) stop(caller,'NULL model argument.') 
  if (!(inherits( model ,'BIMETS_MODEL'))) stop(caller,'model must be instance of BIMETS_MODEL class.') 
  if (is.null(model$vendog))  stop(caller,'list of endogenous variables not found.') 
  if (is.null(model$vexog))  stop(caller,'list of exogenous variables not found.') 
  if (is.null(model$modelData)) stop(caller,'model has no data. Please use LOAD_MODEL_DATA().') 
  
  modelData=model$modelData 
  if (! is.list(modelData)) stop(caller,'modelData must be a list of BIMETS time series.') 
  if (length(modelData)==0) stop(caller,'empty modelData list.') 
  if (is.null(names(modelData))) stop(caller,'names of modelData list are NULL.') 
  if (any(names(modelData)=='')) stop(caller,'there are NULL names in modelData list.') 
  
  #check time series
  for (idx in 1:length(model$modelData))
  {
    if (! is.bimets(model$modelData[[idx]])) 
      stop(caller,'modelData must be a list of BIMETS time series: "',names(model$modelData)[idx],'" time series is not compliant.') 
    if (frequency(model$modelData[[idx]])!=frequency(model$modelData[[1]])) 
      stop(caller,'time series must have the same frequency. Check time series "',names(model$modelData)[idx],'"') 
    if (showWarnings && any(! is.finite(coredata(model$modelData[[idx]])))) 
      .MODEL_outputText(outputText=showWarnings,paste0(caller,'warning, there are non-finite values in time series "',names(model$modelData)[idx],'".\n',sep=''))
    
  }
  
  return(TRUE) 
}

# ESTIMATE code ----------------------------------------------------------------

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
  if (is.null(model) ) stop('ESTIMATE(): NULL model.') 
  if (!(inherits( model ,'BIMETS_MODEL'))) stop('ESTIMATE(): model must be instance of BIMETS_MODEL class.') 
  .checkModelBimetsVersion(model, 'ESTIMATE')
  
  if (is.null(estTech) || ((estTech!='OLS') && (estTech!='IV'))) stop('ESTIMATE(): estimation technique not supported.') 
  
  if (! is.finite(tol) || tol<=0) stop('ESTIMATE(): please provide a valid tolerance value.')
  
  if ((! is.finite(digits) )|| (digits %% 1 !=0) || digits<=0 || digits > 22) 
    stop('ESTIMATE(): digits must be an integer between 1 and 16.') 
  if (!(is.logical(centerCOV)) || is.na(centerCOV)) stop('ESTIMATE(): "centerCOV" must be TRUE or FALSE.') 
  if (!(is.logical(quietly)) || is.na(quietly)) stop('ESTIMATE(): "quietly" must be TRUE or FALSE.')
  if (!(is.logical(forceTSRANGE)) || is.na(forceTSRANGE)) stop('ESTIMATE(): "forceTSRANGE" must be TRUE or FALSE.')
  if (!(is.logical(forceIV)) || is.na(forceTSRANGE)) stop('ESTIMATE(): "forceIV" must be TRUE or FALSE.')
  
  if ((! is.logical(CHOWTEST))  || is.na(CHOWTEST)) stop('ESTIMATE(): "CHOWTEST" must be TRUE or FALSE.') 
  if (is.null(model$modelData) ) stop('ESTIMATE(): model has no data. Please reload model data.') 
  if (is.null(model$frequency) ) stop('ESTIMATE(): model has no frequency. Please reload model data.') 
  if (CHOWTEST==TRUE)
  {
    if (! is.null(CHOWPAR) )
      tryCatch({
        
        CHOWPAR=normalizeYP(CHOWPAR,f=model$frequency) 
      },error=function(e){
        stop('ESTIMATE(): please provide "CHOWPAR" as a c(year,period) array.') 
      })
  }
  
  #flag for messages vertbosity
  outputText=!quietly 
  
  #get regex definitions
  regExDefs=.RegExGlobalDefinition() 
  reservedKeyw=regExDefs$reservedKeyw 
  symOnEqCleaner=regExDefs$symOnEqCleaner 
  allowedCharOnName=regExDefs$allowedCharOnName 
  allowedCharOnNameToken=regExDefs$allowedCharOnNameToken
  charsOnNumbs=regExDefs$charsOnNumbs 
  charsOnNumWithSign=regExDefs$charsOnNumWithSign 
  strongCharOnNumbs=regExDefs$strongCharOnNumbs 
  strongCharOnNumbsWithSign=regExDefs$strongCharOnNumbsWithSign 
  
  #a new local environment is created in order to improve performance
  #it will contains model time series, coefficients, parameters, etc.
  #model equations are faster if executed in a isolated environment
  #in localE parameters, coefficient, etc. names must contains
  #double underscore in order to avoid collisions with model variable names
  localE=new.env() 
  
  if (! avoidCompliance)
  {
    tryCatch({
      
      .CHECK_MODEL_DATA(model,showWarnings=(! quietly),'ESTIMATE(): ') 
    },error=function(e){stop('ESTIMATE(): ',e$message)}) 
  }
  
  #check requested eq names are in model behavioral names
  if (! is.null(eqList)) 
  {
    for ( eqReqName in eqList) 
    {
      if (! (eqReqName %in% names(model$behaviorals))) 
        stop('ESTIMATE(): requested equation "',eqReqName,'" is not in model behaviorals list.')
    }
  }	else 
  {
    eqList=names(model$behaviorals) 
  }
  
  if (length(eqList)==0) 
  {
    .MODEL_outputText(outputText=!quietly,('ESTIMATE(): no behavioral equations to be estimated. Estimation will exit.\n')) 
    return(model) 
  }
  
  .MODEL_outputText(outputText=!quietly,'\nEstimate the Model ',model$modelName,':\n',sep='') 
  .MODEL_outputText(outputText=!quietly,'the number of behavioral equations to be estimated is ',length(eqList),'.\n',sep='') 
  # num of total coeffs
  totCoeff=0 
  for (eqIdx in 1:length(eqList))
  {
    totCoeff=totCoeff+length(model$behaviorals[[eqList[eqIdx]]]$eqCoefficientsNames) 
  }
  
  .MODEL_outputText(outputText=!quietly,'The total number of coefficients is ',totCoeff,'.\n',sep='') 
  
  #model data frequecy
  frequency=model$frequency 
  
  # main loop -----------------------------------------------
  
  for (eqIdx in 1:length(eqList))
  { 
    #proxy current behavioral
    currentBehavioral=model$behaviorals[[eqList[eqIdx]]] 
    
    #deal with IV
    IVlist=list() 
    
    #check args
    if (estTech=='IV' && is.null(IV) && is.null(currentBehavioral$iv)) stop('ESTIMATE(): please provide instrumental variables "IV".')
    if (estTech=='IV' && is.null(IV) && forceIV==TRUE) stop('ESTIMATE(): please provide instrumental variables "IV". "forceIV" is TRUE.')
  
    if (! is.null(IV)) 
    {
      if (length(IV)==0) stop('ESTIMATE(): syntax error in "IV".')
      
      for (idxIV in 1:length(IV))
        if (! .checkExpression(IV[idxIV])) 
          stop('ESTIMATE(): syntax error in "IV".')
    }
    
    if (!forceIV && !is.null(currentBehavioral$iv))
    {
      #split iv from MDL
      IV=strsplit(currentBehavioral$iv,';')[[1]] 
    }
 
    if (! is.null(IV)) 
    {
      namesOnIV=c()
      
      #trim and check size
      for (idxIV in 1:length(IV))
      {
        #trim leading/trailing spaces
        IV[idxIV]=gsub("\\s*", "", IV[idxIV])
        
        #check unknown funs names
        unknownFuns=.unknownFunsNames(IV[idxIV],reservedKeyw)
        if (length(unknownFuns)>0) stop('ESTIAMTE(): unsupported functions in IV definition: "',IV[idxIV],'" in behavioral "',eqList[eqIdx],'": ',
                                        paste0(paste0(unknownFuns,'()'),collapse=', '))
        
        if (min(nchar(IV[idxIV]))==0) stop(paste0('ESTIMATE(): misspecified IV element #'),idxIV,' in behavioral "',eqList[eqIdx],'"') 
        namesOnIV=c(namesOnIV,strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnEqCleaner,' ',IV[idxIV],ignore.case=TRUE)),'\\s+')[[1]]) 
      }
      
      #remove numbers from names
      rmvNumOnIVIdx=c() 
      for (idxNamesOnEq in 1:length(namesOnIV)) {
        if (length(grep(strongCharOnNumbs,namesOnIV[idxNamesOnEq]))>0) 
        {
          rmvNumOnIVIdx=c(rmvNumOnIVIdx,idxNamesOnEq) 
        }
      }
      if (length(rmvNumOnIVIdx)>0) namesOnIV=namesOnIV[-rmvNumOnIVIdx] 
      namesOnIV=sort(unique(namesOnIV)) 
      
      #create local proxies for all references (components names used in eq) in IV
      for (idxName in 1:length(namesOnIV))
      {
        tryCatch(
          {
            tempName=namesOnIV[idxName] 
            if (is.null(model$modelData[[tempName]])) stop() 
            
            localE[[tempName]]=model$modelData[[tempName]]
            
          },error=function(e){
            stop('ESTIMATE(): modelData must contain instrumental variable time series "',tempName,'".',' (behavioral "',eqList[eqIdx],'")') 
          })
      }
      
      #eval expression IV[idx]
      for (idxIV in 1:length(IV))
      {
        tryCatch(
          {
            #fix funs names
            tempName=.MODEL_MOD_FUNC_NAMES(IV[idxIV]) 
            
            #eval IV expr
            tmpIV=eval(parse(text=tempName)
                       ,envir=localE
            )
            
            #assign to output list
            IVlist[[idxIV]]=tmpIV 
            
          },error=function(e){
            stop(paste0('ESTIMATE(): cannot evaluate instrumental variable expression "',IV[idxIV],'": ',e$message)) 
          })
      }
      
    }# IV exists
    
    statsOut=NULL 
    #P matrix and options in Cochrane Orcutt (must be defined even if no Cochrane nedeed)
    bm_resP=NULL 
    bm_COmaxIters=1 #CO max iterations
    bm_COconvergence=0.005 #CO convergence 
    bm_COcurrentIter=0 #CO current iteration
    bm_COprevRho=NULL #CO previous rho values
    
    .MODEL_outputText(outputText=outputText,'\n_________________________________________\n',sep='') 
    .MODEL_outputText(outputText=outputText,'\nBEHAVIORAL EQUATION: ',eqList[eqIdx],'\n',sep='') 
    .MODEL_outputText(outputText=outputText,'Estimation Technique: ',estTech,'\n',sep='') 
    #number of current behavioral coefficients
    bm_coeffNum=length(currentBehavioral$eqCoefficientsNames) 
    #deal with PDL
    thereArePDL=FALSE 
    bm_pdlMatrix=NULL 
    if (!(is.null(currentBehavioral$pdlMatrix) || 
          is.null(currentBehavioral$pdlRaw) ))
    {
      thereArePDL=TRUE 
      bm_pdlMatrix=currentBehavioral$pdlMatrix 
      bm_pdlRestrictionMatrix=currentBehavioral$pdlRestrictionMatrix 
      bm_pdlRestrictionVector=currentBehavioral$pdlRestrictionVector 
    }
    
    #deal with error autocorrelation
    
    thereAreErrorCorr=FALSE 
    bm_errorType=NULL 
    bm_errorDim=0 
    if (!(is.null(currentBehavioral$errorRaw) || 
          is.null(currentBehavioral$errorType) ||
          is.null(currentBehavioral$errorDim) ))
    {
      thereAreErrorCorr=TRUE 
      bm_errorType=currentBehavioral$errorType 
      bm_errorDim=currentBehavioral$errorDim 
      bm_errorRaw=currentBehavioral$errorRaw 
      bm_COmaxIters=20 
    }
    
    if (thereAreErrorCorr==TRUE)
    {
      .MODEL_outputText(outputText=outputText,'Autoregression of Order ',bm_errorDim,' (Cochrane-Orcutt procedure)\n') 
    }
    
    #current behavioral TSRANGE
    localTSRANGE=NULL 
    localTSRANGE_bk=NULL #this is needed if error autocorrelation is required
    
    bm_vectorY_bk=NULL 
    bm_matrixX_bk=NULL 
    #behavioral has TSRANGE
    if (!forceTSRANGE && all(is.finite(currentBehavioral$tsrange)))
    {
      localTSRANGE=currentBehavioral$tsrange 
    } else 
    {
      #check if TSRANGE function argument is compliant
      if (! is.null(TSRANGE)) {
        tryCatch({
          if (! ( is.numeric(TSRANGE) && length(TSRANGE)==4 && 
                  .isCompliantYP(c(TSRANGE[1],TSRANGE[2]),frequency) && 
                  .isCompliantYP(c(TSRANGE[3],TSRANGE[4]),frequency) &&
                  NUMPERIOD(c(TSRANGE[1],TSRANGE[2]),c(TSRANGE[3],TSRANGE[4]),frequency)>=0)
          ) stop() 
        },error=function(e) {stop('ESTIMATE(): syntax error in TSRANGE: ',e$message)}) 
        localTSRANGE=TSRANGE 
      } else
      { 
        stop('ESTIMATE(): neither a local TSRANGE in the behavioral "',eqList[eqIdx],'" MDL nor a global one defined.') 
      }
    }		
    
    #build main X matrix: y = X * b + u
    bm_matrixX=c() 
    localTSRANGE_bk=localTSRANGE 
    if (thereAreErrorCorr==TRUE)
    {
      #extend tsrange errorDim obs in the past. Cochrane-Orcutt procedure iteration 1
      tmpTSR=normalizeYP(c(localTSRANGE[1],localTSRANGE[2]-bm_errorDim),frequency) 
      localTSRANGE[1]=tmpTSR[1] 
      localTSRANGE[2]=tmpTSR[2] 
    }
    
    #check tsrange consistence
    tryCatch({
      if (NUMPERIOD(c(localTSRANGE[1],localTSRANGE[2]),c(localTSRANGE[3],localTSRANGE[4]),frequency)<0) stop() 
    },error=function(e){stop('ESTIMATE(): uncompliant TSRANGE in behavioral "',eqList[eqIdx],'"')}) 
    #create local proxies for all references (components names used in eq) in current behavior
    for (idxName in 1:length(currentBehavioral$eqComponentsNames))
    {
      tryCatch(
        {
          tempName=currentBehavioral$eqComponentsNames[idxName]
          
          if (is.null(model$modelData[[tempName]])) stop() 
          
          localE[[tempName]]=model$modelData[[tempName]]
          
        },error=function(e){
          stop('ESTIMATE(): modelData must contain time series "',currentBehavioral$eqComponentsNames[idxName],'"') 
        })  
    }
    
    #do we have a constant term in regression?
    bm_ConstTermExist=FALSE 
    #used to check regressors have the same length
    regressorLengthTest=NULL
    
    #evaluate regressors: min 2
    if (length(currentBehavioral$eqRegressorsNames)<1) 
      stop('ESTIMATE(): behavioral "',eqList[eqIdx],'" has no regressors.')
    
    # regressor eval -----------------------------
    
    #cycle in regressors (#1 is eq name)
    regressorLengthTest=1+NUMPERIOD(c(localTSRANGE[1],localTSRANGE[2]),c(localTSRANGE[3],localTSRANGE[4]),frequency) 
    
    #build matrix Z if IV
    bm_matrixZ=matrix(ncol=length(IVlist),nrow=regressorLengthTest) 
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
          if ((! is.ts(IVlist[[idxIV]])) && (! is.xts(IVlist[[idxIV]]))  )
          {
            tempRegressor=rep(IVlist[[idxIV]],regressorLengthTest) 
          } else  tempRegressor=(TSPROJECT(IVlist[[idxIV]],ARRAY=TRUE,TSRANGE=localTSRANGE,avoidCompliance=TRUE)) 
          #check regressor size 
          if (length(tempRegressor)!=regressorLengthTest) stop() 
          if (any(! is.finite(tempRegressor))) stop() 
          #assign to matrix
          bm_matrixZ[,idxIV]=tempRegressor 
        }
        
      },error=function(e)
      {
        stop('ESTIMATE(): instrumental variable expression "',IV[idxIV],
             '" in behavioral "',
             eqList[eqIdx],'"\n is not defined over the entire TSRANGE ',paste0(localTSRANGE,collapse=','),': ',e$message) 
      }) 
    }  #now we have Z matrix
    
    for (idxRegrs in 1:length(currentBehavioral$eqRegressorsNames))
    {
      #replace numerical component with columns of numbers (constant term)
      if(grepl(strongCharOnNumbs,currentBehavioral$eqRegressorsNames[idxRegrs]))
      {
        tempRegressorExpr=currentBehavioral$eqRegressorsNames[idxRegrs] 
        
        #evaluate expression (regressors definition e.g. 1)
        tryCatch({
          
          tempRegressor=eval(parse(text=tempRegressorExpr)
                             ,envir=localE) 
        },error=function(e)
        {
          stop('ESTIMATE(): cannot evaluate expression "',currentBehavioral$eqRegressorsNames[idxRegrs],
               '"\n(regressor #',idxRegrs,ifelse(thereArePDL,' with PDL expansion',''),') in behavioral "',
               eqList[eqIdx],'": ',e$message)}) 
        tempRegressor=rep(tempRegressor,regressorLengthTest)
        bm_ConstTermExist=TRUE 
       
      } else {
        
        #fix functions names (e.g. LAG -> TSLAG)
        tempRegressorExpr=.MODEL_MOD_FUNC_NAMES(currentBehavioral$eqRegressorsNames[idxRegrs]) 
        
        #evaluate expression (regressors definition e.g. LAG(GDP,3))
        tryCatch({
          
          tempRegressor=eval(parse(text=tempRegressorExpr)
                             ,envir=localE) 
        },error=function(e)
        {
          stop('ESTIMATE(): cannot evaluate expression "',currentBehavioral$eqRegressorsNames[idxRegrs],
               '"\n(regressor #',idxRegrs,ifelse(thereArePDL,' with PDL expansion',''),') in behavioral "',
               eqList[eqIdx],'": ',e$message)})
        
        #project regressor on tsrange
        tryCatch({
          
          tempRegressor=(TSPROJECT(tempRegressor,TSRANGE=localTSRANGE,ARRAY=TRUE,avoidCompliance=TRUE)) 
          
          #check regressor size 
          if (length(tempRegressor)!=regressorLengthTest) stop() 
          
        },error=function(e)
        {
          stop('ESTIMATE(): expression "',currentBehavioral$eqRegressorsNames[idxRegrs],
               '"\n(regressor #',idxRegrs,
               ifelse(thereArePDL,' with PDL expansion',''),
               ifelse(thereAreErrorCorr,' with ERROR autocorrelation TSRANGE extension',''),
               ') in behavioral "',
               eqList[eqIdx],'"\ndoes not overlap with TSRANGE ',paste0(localTSRANGE,collapse=','),': ',e$message) 
        }) 
      }	
      
      #check regressor has NA
      if (any(! is.finite(tempRegressor))) 
        stop('\n ESTIMATE(): there are undefined values in expression "',
             currentBehavioral$eqRegressorsNames[idxRegrs],
             '" (regressor #',idxRegrs,ifelse(thereArePDL,' with PDL expansion',''),
             ')\n inside the TSRANGE of behavioral "',eqList[eqIdx],'"\n',sep='')
      
      #adde evaluated regressor to X matrix
      bm_matrixX=cbind(bm_matrixX,tempRegressor) 
      
    }#end regressor generating cycle
    #now we have X matrix
    
    #project y
    bm_vectorY=localE[[eqList[eqIdx]]]
    
    #deal with LHS funs
    if (currentBehavioral$lhsFun$funName!='I')
    {
      tryCatch({
        if (currentBehavioral$lhsFun$funName=='LOG')
        {
          bm_vectorY=log(bm_vectorY) 
        } else if (currentBehavioral$lhsFun$funName=='EXP')
        {
          bm_vectorY=exp(bm_vectorY) 
        } else if (currentBehavioral$lhsFun$funName=='TSDELTA' || currentBehavioral$lhsFun$funName=='DEL')
        {
          if (currentBehavioral$lhsFun$argsCount>1 && ! is.finite(as.numeric(currentBehavioral$lhsFun$args[2])))
            stop('Non-numeric lag in TSDELTA()') 
          
          if (currentBehavioral$lhsFun$argsCount>1) tmpV=as.numeric(currentBehavioral$lhsFun$args[2])
          else tmpV=1
          
          bm_vectorY=TSDELTA(bm_vectorY,tmpV) 
          
        } else if (currentBehavioral$lhsFun$funName=='TSDELTAP' )
        {
          if (currentBehavioral$lhsFun$argsCount>1 && ! is.finite(as.numeric(currentBehavioral$lhsFun$args[2])))
            stop('Non-numeric lag in TSDELTAP()') 
          
          if (currentBehavioral$lhsFun$argsCount>1) tmpV=as.numeric(currentBehavioral$lhsFun$args[2])
          else tmpV=1
          
          bm_vectorY=TSDELTAP(bm_vectorY,tmpV) 
          
        } else if (currentBehavioral$lhsFun$funName=='TSDELTALOG' )
        {
          if (currentBehavioral$lhsFun$argsCount>1 && ! is.finite(as.numeric(currentBehavioral$lhsFun$args[2])))
            stop('Non-numeric lag in TSDELTALOG()') 
          
          if (currentBehavioral$lhsFun$argsCount>1) tmpV=as.numeric(currentBehavioral$lhsFun$args[2])
          else tmpV=1
          
          bm_vectorY=TSDELTALOG(bm_vectorY,tmpV) 
        }
        
      },error=function(e)
      {
        stop('ESTIMATE(): error in LHS caclulation of "',currentBehavioral$lhsFun$raw,'" in behavioral "',eqList[eqIdx],'"',': ',e$message) 
      }) 
    }
    
    tryCatch({
      
      bm_vectorY=(TSPROJECT(bm_vectorY,TSRANGE=localTSRANGE,ARRAY=TRUE,avoidCompliance=TRUE)) 
      
      #check vector Y size
      if (length(bm_vectorY)!=regressorLengthTest) stop()
      
    },error=function(e)
    {
      stop('ESTIMATE(): LHS expression "',
           currentBehavioral$lhsFun$raw, '" in behavioral "',eqList[eqIdx],'" does not overlap with TSRANGE ',
           paste0(localTSRANGE,collapse=','),': ',e$message) 
    })
    
    #check Y has NA
    if (any(! is.finite(bm_vectorY))) 
      stop('\n ESTIMATE(): there are undefined values in LHS expression "',
           currentBehavioral$lhsFun$raw, '" inside the TSRANGE of behavioral "',eqList[eqIdx],'"\n',sep='')
    
    #now we have vector Y
    
    thereAreRestrictions=FALSE 
    bm_restrictionsNum=0 
    
    #deal with restrictions
    if (!(is.null(currentBehavioral$restrictRaw) || 
          is.null(currentBehavioral$vectorR) ||
          is.null(currentBehavioral$matrixR) ))
    {
      thereAreRestrictions=TRUE 
    }  
    
    if (thereAreRestrictions == TRUE || 
        (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
    {
      bm_matrixR=c() 
      bm_vectorR=c() 
      if (thereAreRestrictions == TRUE && thereArePDL == TRUE)
      {
        bm_matrixR=rbind(currentBehavioral$matrixR,currentBehavioral$pdlRestrictionMatrix) 
        bm_vectorR=c(currentBehavioral$vectorR,currentBehavioral$pdlRestrictionVector) 
      }
      else if (thereArePDL == TRUE )  {
        
        bm_matrixR=currentBehavioral$pdlRestrictionMatrix 
        bm_vectorR=currentBehavioral$pdlRestrictionVector 
        thereAreRestrictions=TRUE 
      } else {
        #extract restriction from model definition
        bm_matrixR=currentBehavioral$matrixR 
        bm_vectorR=currentBehavioral$vectorR 
      }
      
      bm_restrictionsNum=length(bm_vectorR)
      
    }
    
    #obs count
    bm_numObs=1+NUMPERIOD(c(localTSRANGE_bk[1],localTSRANGE_bk[2]),c(localTSRANGE_bk[3],localTSRANGE_bk[4]),frequency)
    
    #degree of freedom
    bm_DoF=bm_numObs-bm_coeffNum+bm_restrictionsNum-bm_errorDim 
    statsOut$DegreesOfFreedom=bm_DoF 
    
    #check consistence of regression
    if (bm_DoF<0) stop('ESTIMATE(): negative count of degrees of freedom in behavioral "',eqList[eqIdx],'".') 
    if (bm_DoF==0) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, zero degrees of freedom in behavioral "',eqList[eqIdx],'". Other warnings and errors may arise.\n')) 
    
    # CO start --------------------------------------------------------
    
    #used in CO error autocorrelation
    bm_COlastIter=FALSE 
    
    #cycle multiple time if error correlation required, else just 1 time
    for (bm_COcurrentIter in 1:bm_COmaxIters)
    {     
      #this is skipped anyway on first iteration
      if ((thereAreErrorCorr==TRUE) && (bm_COcurrentIter>1))
      {
        #CO transformation
        
        #backup original vector and matrix
        bm_vectorY_bk=bm_vectorY 
        bm_matrixX_bk=bm_matrixX 
        if (estTech=='IV') bm_matrixZ_bk=bm_matrixZ 
        
        #CO transform
        bm_vectorY=bm_resP %*% bm_vectorY 
        bm_matrixX=bm_resP %*% bm_matrixX 
        
        #in CO betaHat is calculated on original tsrange
        bm_vectorY=bm_vectorY[(1+bm_errorDim):length(bm_vectorY)] 
        bm_matrixX=bm_matrixX[(1+bm_errorDim):nrow(bm_matrixX),] 
        if (estTech=='IV') bm_matrixZ=bm_matrixZ[(1+bm_errorDim):nrow(bm_matrixZ),] 
      }
      
      bm_betaHat=c()   
     
      #if restriction or pdl with length>degree+1 else OLS
      if (thereAreRestrictions == TRUE || 
          (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
      {
        if (estTech=='OLS') 
        { 
          bm_WpX=t(bm_matrixX) %*% bm_matrixX 
        }
        
        if (estTech=='IV') 
        {
          tryCatch({
            
            bm_matrixXZ=bm_matrixZ %*% solve( crossprod( bm_matrixZ ),
                                              crossprod( bm_matrixZ, bm_matrixX ),
                                              tol=tol)
          },error=function(e)
          {
            stop('ESTIMATE(): given Z = the matrix of the instrumental variables as columns, Z\'*Z is not invertible. Behavioral: "',eqList[eqIdx],'". Check regressors and "IV" or try to reduce tolerance "tol" value. ',e$message) 
          }) 
          bm_WpX=t(bm_matrixXZ) %*% bm_matrixXZ 
        }
        
        bm_ave_WpX=base::mean(bm_WpX) 
        bm_Rscale=c() 
        for (idxRscale in 1:dim(bm_matrixR)[1])
        {
          bm_Rscale=c(bm_Rscale,bm_ave_WpX/max(abs(bm_matrixR[idxRscale,]))) 
        }
        
        bm_matrixR_tx=bm_matrixR*bm_Rscale 
        if (dim(bm_WpX)[1] != dim(bm_matrixR_tx)[2]) stop('ESTIMATE(): elements size mismatch in restriction augmented-cross product calculation. Behavioral: "',eqList[eqIdx],'".')
        
        bm_AWpX=rbind(bm_WpX,bm_matrixR_tx)
        
        bm_AmatrixR=cbind(bm_matrixR_tx,matrix(rep(0,dim(bm_matrixR_tx)[1]^2),nrow=dim(bm_matrixR_tx)[1]))
        
        bm_AAWpX=cbind(bm_AWpX,t(bm_AmatrixR))
        
        if (estTech=='OLS') bm_AvectorY=c(t(bm_matrixX) %*% bm_vectorY,bm_vectorR * bm_Rscale)
        if (estTech=='IV') bm_AvectorY=c(t(bm_matrixXZ) %*% bm_vectorY,bm_vectorR * bm_Rscale)
        
        tryCatch({
          
          bm_AAWpXi =solve(bm_AAWpX,
                           tol=tol) 
          bm_betaHat=bm_AAWpXi %*% bm_AvectorY
          
          bm_betaHat=bm_betaHat[1:dim(bm_matrixX)[2],,drop=F] 
          
        },error=function(e)
        {
          stop('ESTIMATE(): in restriction ',currentBehavioral$restrictRaw,
               ' given betaHat = (W\'X)^(-1) * (W\'Y), W\'X is not invertible. Behavioral: "',eqList[eqIdx],'". Check regressors or try to reduce tolerance "tol" value. ',e$message) 
        }) 
        
        #get f-statistics for restrictions
        #unrestricted beta OLS
        if (estTech=='OLS') {
          #check inverse exist
          bm_matrixXpXinv=NULL 
          tryCatch({
            
            bm_matrixXpXinv=solve(t(bm_matrixX) %*% bm_matrixX,
                                  tol=tol) 
          },error=function(e)
          {
            stop('ESTIMATE(): given y = X * b + u, X\'*X is not invertible (un-restricted case in F-test). Behavioral: "',eqList[eqIdx],'". Check regressors or try to reduce tolerance "tol" value. ',e$message) 
          }) 
          
          #OLS unrestricted :)
          bm_betaHat_unrestricted=bm_matrixXpXinv %*% t(bm_matrixX) %*% bm_vectorY 
        }
        
        #get f-statistics for restrictions
        
        #unrestricted beta IV
        if (estTech=='IV') {
          
          #check inverse exist
          bm_matrixXpXinv=NULL 
          tryCatch({
            bm_matrixXpXinv=solve( crossprod( bm_matrixXZ ),
                                   tol=tol) 
            bm_betaHat_unrestricted=bm_matrixXpXinv %*% crossprod( bm_matrixXZ, bm_vectorY )  
          },error=function(e)
          {
            stop('ESTIMATE(): given y = X * b + u and given Z = the matrix of the instrumental variables as columns, and X_hat = Z * ( Z\' * Z )^(-1) * Z\' * X, X_hat\'*X_hat is not invertible (un-restricted case in F-test). Behavioral: "',eqList[eqIdx],'". Check regressors or try to reduce tolerance "tol" value. ',e$message) 
          }) 
        }
        
      } else 
      {  
        #no restriction           
        #default OLS
         
        if (estTech=='OLS') {
          #check inverse exist
          bm_matrixXpXinv=NULL 
          tryCatch({
            bm_matrixXpXinv=solve(t(bm_matrixX) %*% bm_matrixX,
                                  tol=tol) 
          },error=function(e)
          {
            stop('ESTIMATE(): given y = X * b + u, X\'*X is not invertible. Behavioral: "',eqList[eqIdx],'". Check regressors or try to reduce tolerance "tol" value. ',e$message) 
          }) 
          
          #OLS :)
          bm_betaHat=bm_matrixXpXinv %*% t(bm_matrixX) %*% bm_vectorY 
        }
        
        if (estTech=='IV') {
          
          #check inverse exist
          bm_matrixXpXinv=NULL 
          tryCatch({
            
            bm_matrixXZ=bm_matrixZ %*% solve( crossprod( bm_matrixZ ),
                                              crossprod( bm_matrixZ, bm_matrixX ),
                                              tol=tol )
            
          },error=function(e)
          {
            stop('ESTIMATE(): given Z = the matrix of the instrumental variables as columns, Z\'*Z is not invertible. Behavioral: "',eqList[eqIdx],'". Check regressors and "IV" or try to reduce tolerance "tol" value. ',e$message) 
          })
          
          tryCatch({
            bm_matrixXpXinv=solve( crossprod( bm_matrixXZ ),
                                   tol=tol) 
            bm_betaHat=bm_matrixXpXinv %*% crossprod( bm_matrixXZ, bm_vectorY )  
          },error=function(e)
          {
            stop('ESTIMATE(): given y = X * b + u and given Z = the matrix of the instrumental variables as columns, and X_hat = Z * ( Z\' * Z )^(-1) * Z\' * X, X_hat\'*X_hat is not invertible. Behavioral: "',eqList[eqIdx],'". Check regressors and "IV" or try to reduce tolerance "tol" value. ',e$message) 
          }) 
        }
        
      } # end if no restriction
      
      if (thereAreRestrictions == TRUE || 
          (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
      {
        statsOut$matrixXaug=bm_AAWpX 
        statsOut$vectorYaug=bm_AvectorY 
      }
      
      #deal with error correlations
      if ((thereAreErrorCorr==TRUE) && (bm_COcurrentIter>1))
      {         
        #restore original Y and X (the longer ones...): we need extended regressor in order to have coherent errors lags         
        bm_vectorY=bm_vectorY_bk 
        bm_matrixX=bm_matrixX_bk 
        if (estTech=='IV') bm_matrixZ=bm_matrixZ_bk 
      }       
      
      #exit from loop if convergence reached (variable set later...)
      if (bm_COlastIter==TRUE) break 
      
      #these are residuals
      bm_unSquare=bm_vectorY - bm_matrixX %*% bm_betaHat 
      
      #f-statistics for restrictions
      if (thereAreRestrictions == TRUE || 
          (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
        bm_unSquare_unrestricted=bm_vectorY - bm_matrixX %*% bm_betaHat_unrestricted 
      
      #perform OLS on errors: u=LAG(u)*rho + z
      if (thereAreErrorCorr==TRUE)
      {
        #cochrane orcutt evaluation
        
        #OLS on AR(bm_errorDim) on residuals
        bm_resX=NULL
        bm_resY=NULL
        
        #project residuals on original tsrange
        if (length(bm_unSquare)<(bm_errorDim+1)) 
          stop('ESTIMATE(): order of error autocorrelation greater than either available observations or TSRANGE specifications (Cochrane-Orcutt). Behavioral: "',eqList[eqIdx],'".') 
        bm_resY=bm_unSquare[(1+bm_errorDim):length(bm_unSquare)] 
        
        #create lagged residuals
        for(idxRS in 1:bm_errorDim)
        {
          bm_resX=cbind(bm_resX,bm_unSquare[(1+bm_errorDim-idxRS):(length(bm_unSquare)-idxRS)]) 
        }
        
        #check inverse exist
        bm_matrixresXpresXinv=NULL 
        
        tryCatch({
          bm_matrixresXpresXinv=solve(t(bm_resX) %*% bm_resX,
                                      tol=tol) 
        },error=function(e)
        {
          stop('ESTIMATE(): given u = LAG(u) * rho + z, LAG(u)\'*LAG(u) is not invertible (Cochrane-Orcutt). Behavioral: "',
               eqList[eqIdx],'". Check residuals or try to reduce tolerance "tol" value. ',e$message) 
        }) 
        
        #there are error autocorrelation coefficients
        bm_rhoHat=bm_matrixresXpresXinv %*% t(bm_resX) %*% bm_resY 
        
        #deal with regression standard errors...
        bm_COunSquare=bm_resY-bm_resX %*% bm_rhoHat 
        bm_COuSquare=bm_COunSquare * bm_COunSquare 
        bm_COssr=sum(bm_COuSquare) 
        bm_COcoeffNum=bm_errorDim 
        bm_COnumObs=1+NUMPERIOD(c(localTSRANGE_bk[1],localTSRANGE_bk[2]),c(localTSRANGE_bk[3],localTSRANGE_bk[4]),frequency) 
        
        #bm_COser=sqrt(bm_COssr/(bm_COnumObs-bm_COcoeffNum))
        if (bm_COnumObs - bm_COcoeffNum - bm_coeffNum +bm_restrictionsNum==0) tmpV=0
        else tmpV=sqrt((bm_COssr-(sum(bm_COunSquare)^2)/bm_COnumObs)/
                         (bm_COnumObs - bm_COcoeffNum - bm_coeffNum + bm_restrictionsNum)) #...dont know why bm_COcoeffNum and bm_restrictionsNum
          
        bm_COser=tmpV 
        
        bm_COserAdj=bm_COser 
        
        if (bm_COnumObs - bm_COcoeffNum - bm_coeffNum +bm_restrictionsNum==0) tmpV=0
        else tmpV=sqrt((bm_COssr)/
                         (bm_COnumObs - bm_COcoeffNum - bm_coeffNum + bm_restrictionsNum)) #...dont know why bm_COcoeffNum and bm_restrictionsNum
        
        if (!centerCOV) bm_COserAdj=tmpV 
        
        bm_COvcov=(bm_COserAdj*bm_COserAdj) * bm_matrixresXpresXinv 
        bm_COstderr=sqrt(diag(bm_COvcov)) 
        statsOut$RhosCovariance=bm_COvcov 
        rownames(statsOut$RhosCovariance)=paste0("RHO_",1:(bm_COcoeffNum)) 
        colnames(statsOut$RhosCovariance)=paste0("RHO_",1:(bm_COcoeffNum)) 
        
        #create CO transf matrix (P matrix in user guide)
        bm_realTSRANGEobs=1+NUMPERIOD(c(localTSRANGE[1],localTSRANGE[2]),c(localTSRANGE[3],localTSRANGE[4]),frequency) 
        bm_resP=matrix(rep(0,bm_realTSRANGEobs^2),nrow=bm_realTSRANGEobs) 
        
        #bm_repP is modifier for original Y and X
        for (idxP in 1:bm_realTSRANGEobs)
        {
          bm_resP[idxP,idxP]=1 
          for (idxRS in 1:bm_errorDim)
          {
            if (idxP>idxRS) bm_resP[idxP,idxP-idxRS]=-bm_rhoHat[idxRS] 
          }
        }
      
      #set flag if convergence (we need at least 1 iteration in order to get betahat with current rho)
      if (bm_COcurrentIter>1)
        {
          #convergence check
          if (all(abs(bm_rhoHat-bm_COprevRho)<bm_COconvergence)) bm_COlastIter=TRUE 
          #if (sqrt(sum((bm_rhoHat-bm_COprevRho)^2))/sqrt(sum(bm_COprevRho^2)) < bm_COconvergence) bm_COlastIter=TRUE 
        }
        
        #save results for next iteration
        bm_COprevRho=bm_rhoHat 
        
      }#end thereAreErrorCorr
      
      #take stop between iterations
      
    }#end CO iteration
    
    # CO end --------------------------------------------------
    
    if (thereAreErrorCorr==TRUE)
    { 
      #restore localRANGE
      localTSRANGE=localTSRANGE_bk 
      .MODEL_outputText(outputText=outputText,'\nConvergence reached in ',bm_COcurrentIter,' iterations.\n\n')
    }
    
    bm_unSquare_no_error_correction=NULL 
    #in error correlation cases, u must be calculated by using transformed y and x
    if ((thereAreErrorCorr==TRUE))
    {
      #transom with matrix P
      bm_vectorY=bm_resP %*% bm_vectorY 
      bm_matrixX=bm_resP %*% bm_matrixX 
      
      #project on localTSRANGE
      bm_vectorY=bm_vectorY[(1+bm_errorDim):length(bm_vectorY)] 
      bm_matrixX=bm_matrixX[(1+bm_errorDim):nrow(bm_matrixX),] 
      
      #save regressors and y (with ERROR> we save the modified ones)
      statsOut$matrixX_error_corrected=as.matrix(bm_matrixX) 
      statsOut$vectorY_error_corrected=as.matrix(bm_vectorY) 
      colnames(statsOut$matrixX_error_corrected)=currentBehavioral$eqRegressorsNames 
      colnames(statsOut$vectorY_error_corrected)=currentBehavioral$lhsFun$raw 
      
      #calc error
      bm_unSquare= bm_vectorY - bm_matrixX %*% bm_betaHat 
      
      #f-statistics for restrictions
      if (thereAreRestrictions == TRUE || 
          (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
        bm_unSquare_unrestricted=bm_vectorY - bm_matrixX %*% bm_betaHat_unrestricted 
      
      #restore vectors
      bm_vectorY=bm_vectorY_bk 
      bm_matrixX=bm_matrixX_bk 
      bm_vectorY=bm_vectorY[(1+bm_errorDim):length(bm_vectorY)]
      bm_matrixX=bm_matrixX[(1+bm_errorDim):nrow(bm_matrixX),]   
      
      bm_unSquare_no_error_correction=bm_vectorY - bm_matrixX %*% bm_betaHat 
    }
    
    bm_uSquare=bm_unSquare * bm_unSquare 
    
    #save regressors and y
    statsOut$matrixX=as.matrix(bm_matrixX) 
    statsOut$vectorY=as.matrix(bm_vectorY) 
    colnames(statsOut$matrixX)=currentBehavioral$eqRegressorsNames 
    colnames(statsOut$vectorY)=currentBehavioral$lhsFun$raw 
    
    #sum squared residuals
    bm_ssr=sum(bm_uSquare)
    
    #check residuals length
    if (NUMPERIOD(localTSRANGE[1:2],localTSRANGE[3:4],frequency)  +1 != length(bm_unSquare))
    {
      stop(paste0('ESTIMATE(): unknown error on residuals length in behavioral ',eqList[eqIdx])) 
    }
    
    #time series for residuals
    bm_unSquareTS=TSERIES(bm_unSquare,START=localTSRANGE[1:2],FREQ=frequency,avoidCompliance=TRUE) 
    if ((thereAreErrorCorr==TRUE) ) bm_unSquare_no_error_correctionTS=TSERIES(bm_unSquare_no_error_correction,START=localTSRANGE[1:2],FREQ=frequency,avoidCompliance=TRUE) 
    statsOut$estimationTechnique=estTech 
    statsOut$TSRANGE=localTSRANGE 
    statsOut$InstrumentalVariablesRaw=IV 
    statsOut$tol=tol 
    statsOut$digits=digits 
    statsOut$centerCOV=centerCOV 
    
    #standard error regression    
    #bm_ser=sqrt(bm_ssr/(bm_numObs-bm_coeffNum+bm_restrictionsNum)) 
    #here below is centered
    if (bm_DoF==0) tmpV=0
    else tmpV=sqrt((bm_ssr-(sum(bm_unSquare)^2)/bm_numObs)/(bm_DoF))
    bm_ser=tmpV
    
    bm_serAdj=bm_ser 
    if (bm_DoF==0) tmpV=0
    else tmpV=sqrt((bm_ssr)/(bm_DoF))
    if (!centerCOV) bm_serAdj=tmpV
    
    statsOut$StandardErrorRegression=bm_ser 
    
    if (bm_DoF==0) tmpV=0
    else tmpV=sqrt((bm_ssr)/(bm_DoF))
    statsOut$StandardErrorRegressionNotCentered=tmpV
    
    #var-covar coeff
    if ( ! thereAreRestrictions)
    {
      bm_vcov=(bm_serAdj*bm_serAdj) * bm_matrixXpXinv 
      #bm_vcov=vcov(linear_model)
    } else {
      bm_vcov=(bm_serAdj*bm_serAdj) * bm_AAWpXi 
    }
    
    #in model with restrictions, diagonal cov elements can be very small but negatives... so abs(diag()) here below
    #coeff of variation betaHat
    #if restrictions size vcov != size betaHat
    bm_vcov=bm_vcov[1:length(bm_betaHat),1:length(bm_betaHat),drop=F] 
    suppressWarnings({bm_betaHatStdErr=sqrt(abs(diag(bm_vcov)))}) #we have warnings if restrictions due to NAs
    #bm_betaHatStdErr=bm_betaHatStdErr[1:length(bm_betaHat)] 
    
    #remove 0/0 cases
    bm_betaHatCoV=bm_betaHat/bm_betaHatStdErr
    if (any(is.nan(bm_betaHatCoV))) bm_betaHatCoV[which(is.nan(bm_betaHatCoV))]=Inf
    
    names(bm_betaHatCoV)=currentBehavioral$eqCoefficientsNames 
    #bm_vcovOut=bm_vcov[1:length(bm_betaHat),1:length(bm_betaHat),drop=FALSE] 
    rownames(bm_vcov)=currentBehavioral$eqCoefficientsNames 
    colnames(bm_vcov)=currentBehavioral$eqCoefficientsNames 
    statsOut$CoeffCovariance=bm_vcov 
    statsOut$CoeffTstatistic=bm_betaHatCoV 
    
    # R & SPK differs in Rsquared, we choosed R 
    if (bm_ConstTermExist) tmpV=ave(bm_vectorY)
    else tmpV=0
    bm_rSquared=1-(bm_ssr)/(sum((bm_vectorY-tmpV)^2))
    
    #we keep old code just as reminder
    # #r-squared
    # if (thereAreErrorCorr==TRUE  )
    # {
    #   #this is ok even without error correlation - DEFAULT R lm formula
    #   bm_rSquared=1-(bm_ssr)/(sum((bm_vectorY - ave(bm_vectorY)  )^2)) #eq by wiki works fine with error autocorrelation
    #   
    # }   
    # 
    # if ((thereAreRestrictions == TRUE || 
    #      (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix)))))
    # { 
    #     # DEFAULT SPK 
    # 
    #     #eq by user guide
    #     bm_rSquared_tempY=bm_matrixX %*% bm_betaHat 
    #    
    #     bm_rSquared=(sum((bm_vectorY-base::mean(bm_vectorY))*(((bm_rSquared_tempY)-base::mean(bm_rSquared_tempY))))^2)/
    #          ((sum((((bm_rSquared_tempY)-base::mean(bm_rSquared_tempY))^2)))*(sum((bm_vectorY-base::mean(bm_vectorY))^2))) 
    #   
    # }
    
    statsOut$RSquared=bm_rSquared 
    #adj r-squared
    #R lm default formula
    
    if (bm_ConstTermExist) tmpV2=1
    else tmpV2=0
    
    if (bm_DoF==0) tmpV=1
    else tmpV=1-(1-bm_rSquared)*(bm_numObs-tmpV2)/(bm_DoF)
      
    bm_adjrSquared=tmpV  #+1-1
    
    if ((thereAreErrorCorr==TRUE  ) || ((thereAreRestrictions == TRUE || 
        (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))))
    {
      if (bm_DoF==0) tmpV=1
      else tmpV=1-(1-bm_rSquared)*(bm_numObs- 1)/(bm_DoF)
            
      bm_adjrSquared=tmpV  #+1-1
    }
    
    statsOut$AdjustedRSquared=bm_adjrSquared 
    
    #assign beta hat to coefficients
    currentBehavioral$coefficients=bm_betaHat 
    if (thereAreErrorCorr==TRUE) rownames(bm_rhoHat)=paste0("RHO_",1:(bm_COcoeffNum)) 
    if (thereAreErrorCorr==TRUE) currentBehavioral$errorCoefficients=bm_rhoHat 
    if (length(currentBehavioral$coefficients) !=
        length(currentBehavioral$eqCoefficientsNames))
    {
      stop('ESTIMATE(): coefficients count differs from coefficients names length.') 
    }
    
    rownames(currentBehavioral$coefficients)=currentBehavioral$eqCoefficientsNames 
    
    #save residuals
    currentBehavioral$residuals=bm_unSquareTS 
    if (thereAreErrorCorr==TRUE) currentBehavioral$residuals_no_error_correction=bm_unSquare_no_error_correctionTS 
    
    #f-statistics for restrictions
    if (thereAreRestrictions == TRUE || 
        (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
    { 
      bm_uSquare_unrestricted=bm_unSquare_unrestricted * bm_unSquare_unrestricted 
      #sum squared residuals not restricted
      bm_ssr_unrestricted=sum(bm_uSquare_unrestricted) 
      #f-test for restrictions
      if( thereAreRestrictions==TRUE ) 
      {
        if (bm_numObs-bm_coeffNum-bm_errorDim<=0) tmpV=NA
        else tmpV=((bm_ssr-bm_ssr_unrestricted )/bm_restrictionsNum)/((bm_ssr_unrestricted)/(bm_numObs-bm_coeffNum-bm_errorDim))
        bm_Ftest_restriction=tmpV
        
        if (bm_numObs-bm_coeffNum-bm_errorDim<=0) tmpV=NA
        else tmpV=((1-pf(bm_Ftest_restriction,bm_restrictionsNum,bm_numObs-bm_coeffNum)))
        bm_Ftest_probability=tmpV
      }
    }
    
    #calc p-values
    pValues=bm_betaHat*0 
    if (bm_DoF>0) pValues=2*pt(-abs(bm_betaHatCoV),df=bm_DoF) 
    statsOut$CoeffPvalues=pValues 
    
    #print stuff in screen
    #print estimated eq data
    stdFormat=paste0("%-",digits+5,".",digits,'g') 
    stdFormatS=paste0("%-",digits+5,'s') 
    for (idxCoeff in 1:length(bm_betaHat))
    {
      tmpSign='+' 
      if (bm_betaHat[idxCoeff]<0) tmpSign='-' 
      tmpPrefix=paste0(sprintf("%-20s",''),tmpSign,'   ') 
      if (idxCoeff==1) {
        
        if (bm_betaHat[idxCoeff]<0) {
          tmpPrefix=paste0(sprintf("%-18s",currentBehavioral$lhsFun$raw),'= -   ') 
        } else {
          tmpPrefix=paste0(sprintf("%-20s",currentBehavioral$lhsFun$raw),'=   ') 
        }
      }      
      
      suffix=''
      if (grepl(strongCharOnNumbs,currentBehavioral$eqRegressorsNames[idxCoeff])) 
      {
        if(currentBehavioral$eqRegressorsNames[idxCoeff]!='1')
          suffix=paste0('CONST*',currentBehavioral$eqRegressorsNames[idxCoeff]) 
      } else {
        suffix=paste0(currentBehavioral$eqRegressorsNames[idxCoeff]) 
      }
      
      if (thereArePDL==FALSE)
      { 
        #if modify these lines also modify below
        #print tmpPrefix (+ or -) then coeff value then regressor eq
        .MODEL_outputText(outputText=outputText,'\n',tmpPrefix,sprintf(stdFormat,abs(bm_betaHat[idxCoeff])),suffix,'\n',sep='') 
        
        #if restricted print string else CoV
        tmpCoV=bm_betaHatCoV[idxCoeff] 
        if (! is.finite(tmpCoV) || abs(tmpCoV)>1e9) 
        {
          tmpCoV='RESTRICT' 
        } else
        {
          localPvalueS='' 
          #if (pValues[idxCoeff]<0.1) localPvalueS='.' 
          if (pValues[idxCoeff]<0.05) localPvalueS='*' 
          if (pValues[idxCoeff]<0.01) localPvalueS='**' 
          if (pValues[idxCoeff]<0.001) localPvalueS='***' 
          tmpCoV=sprintf(stdFormat,tmpCoV) 
          tmpCoV=paste0('T-stat. ',tmpCoV,localPvalueS) 
        }
        .MODEL_outputText(outputText=outputText,sprintf("%-24s",''),tmpCoV,'\n',sep='') 
      } else {                
        
        if (! currentBehavioral$eqCoefficientsNames[idxCoeff] %in% currentBehavioral$eqCoefficientsNamesOriginal ) next 
        tmpMatrix=currentBehavioral$pdlMatrix
        tmpCoeff=currentBehavioral$eqCoefficientsNames[idxCoeff]
        tmpOrigCoeff=currentBehavioral$eqCoefficientsNamesOriginal
        tmpIdxinOriginal=which(tmpOrigCoeff==tmpCoeff)
        
        #if coefficient has pdl
        if (tmpMatrix[1,tmpIdxinOriginal]==1)
        {
          #print coeff name if pdl instead of value
          .MODEL_outputText(outputText=outputText,'\n',gsub('-','+',tmpPrefix),sprintf(stdFormatS,currentBehavioral$eqCoefficientsNames[idxCoeff]),suffix,'\n',sep='') 
          .MODEL_outputText(outputText=outputText,sprintf("%-24s",''),'PDL\n',sep='') 
        } else {
          #print tmpPrefix (+ or -) then coeff value then regressor eq
          .MODEL_outputText(outputText=outputText,'\n',tmpPrefix,sprintf(stdFormat,abs(bm_betaHat[idxCoeff])),suffix,'\n',sep='') 
          #if restricted print string else CoV
          tmpCoV=bm_betaHatCoV[idxCoeff] 
          if (! is.finite(tmpCoV) || abs(tmpCoV)>1e9) 
          {
            tmpCoV='RESTRICT'          
          } else
          {
            localPvalueS='' 
            #if (pValues[idxCoeff]<0.1) localPvalueS='.' 
            if (pValues[idxCoeff]<0.05) localPvalueS='*' 
            if (pValues[idxCoeff]<0.01) localPvalueS='**' 
            if (pValues[idxCoeff]<0.001) localPvalueS='***' 
            tmpCoV=sprintf(stdFormat,tmpCoV) 
            tmpCoV=paste0('T-stat. ',tmpCoV,localPvalueS) 
          }
          .MODEL_outputText(outputText=outputText,sprintf("%-24s",''),tmpCoV,'\n',sep='') 
        }
      }
    }
    
    #durbin watson
    bm_dw=NA 
    if (bm_numObs>1) {
      
      if (thereAreErrorCorr==TRUE)
      {
        bm_vectorY=bm_vectorY_bk
        bm_matrixX=bm_matrixX_bk
        
        bm_vectorY=bm_resP %*% bm_vectorY 
        bm_matrixX=bm_resP %*% bm_matrixX 
        bm_vectorY=bm_vectorY[(1+bm_errorDim):length(bm_vectorY)] 
        bm_matrixX=bm_matrixX[(1+bm_errorDim):nrow(bm_matrixX),] 
      }      
      
      bm_tmp_dw=bm_vectorY - bm_matrixX %*% bm_betaHat 
      for (idxdw in 2:length(bm_tmp_dw)) bm_dw[idxdw-1]=bm_tmp_dw[idxdw]-bm_tmp_dw[idxdw-1] 
      bm_dw=bm_dw*bm_dw 
      bm_dw=sum(bm_dw)
      bm_dw=bm_dw/bm_ssr
      
      #restore originals
      if (thereAreErrorCorr==TRUE)
      {
        bm_vectorY=bm_vectorY_bk
        bm_matrixX=bm_matrixX_bk
        
        bm_vectorY=bm_vectorY[(1+bm_errorDim):length(bm_vectorY)]
        bm_matrixX=bm_matrixX[(1+bm_errorDim):nrow(bm_matrixX),]  
      }
    }
    
    #log likelihood
    bm_loglike=-(bm_numObs/2)*(1+log(2*pi)+log(bm_ssr/bm_numObs))
    
    #Akaike
    bm_AIC=2*(1+bm_coeffNum-bm_restrictionsNum+bm_errorDim)-2*bm_loglike 
    #Schwarz
    bm_BIC=log(bm_numObs)*(1+bm_coeffNum-bm_restrictionsNum+bm_errorDim)-2*bm_loglike 
    if (thereAreErrorCorr==TRUE)
    {
      .MODEL_outputText(outputText=outputText,'\nERROR STRUCTURE: ',bm_errorRaw,'\n') 
    }
    
    if (bm_ConstTermExist) tmpV2=1
    else tmpV2=0
      
    if (bm_DoF==0) tmpV=+Inf
    else tmpV=(bm_rSquared/(bm_coeffNum-tmpV2+bm_errorDim-bm_restrictionsNum))*
      ((bm_DoF)/(1-bm_rSquared))
      
    bm_fStat=tmpV
    
    if (is.finite(bm_fStat) && bm_DoF>0) tmpV=1-pf(bm_fStat,bm_coeffNum-tmpV2+bm_errorDim-bm_restrictionsNum,bm_DoF)
    else tmpV=1
    
    bm_fProb=tmpV
    
    if (thereAreRestrictions && (! is.null(currentBehavioral$restrictRaw)))
    {
      .MODEL_outputText(outputText=outputText,'\nRESTRICTIONS:') 
      .MODEL_outputText(outputText=outputText,'\n')
      .MODEL_outputText(outputText=outputText,paste(strsplit(currentBehavioral$restrictRaw,';')[[1]],collapse='\n'))
      .MODEL_outputText(outputText=outputText,'\n')
    }
    
    if (thereArePDL == TRUE)
    {
      .MODEL_outputText(outputText=outputText,'\nPDL:\n') 
      .MODEL_outputText(outputText=outputText,paste(strsplit(currentBehavioral$pdlRaw,';')[[1]],collapse='\n')) 
      tmpCoeff=currentBehavioral$eqCoefficientsNames
      .MODEL_outputText(outputText=outputText,'\n') 
      for(idxPrintPDL in (which(bm_pdlMatrix[1,]==1)))
      {
        .MODEL_outputText(outputText=outputText,'\n') 
        tmpLaggedCoeff=currentBehavioral$eqCoefficientsNamesOriginal[idxPrintPDL]
        .MODEL_outputText(outputText=outputText,'Distributed Lag Coefficient:',tmpLaggedCoeff) 
        .MODEL_outputText(outputText=outputText,'\n') 
        .MODEL_outputText(outputText=outputText,
                          sprintf(paste0("%-",8,"s"),'Lag'),
                          sprintf(paste0("%-",digits+8,"s"),'Coeff.'),
                          sprintf(paste0("%-",digits+8,"s"),'Std. Error'),
                          sprintf(paste0("%-",digits+8,"s"),'T-stat.'),
                          '\n',sep='') 
        posCoeffInCoeffArray=which(tmpCoeff==tmpLaggedCoeff) 
        tmpSum=0 
        for(idxPrintSinglePDL in 0:(bm_pdlMatrix[3,idxPrintPDL]-1))
        { 
          tmpValue=bm_betaHat[posCoeffInCoeffArray+idxPrintSinglePDL] 
          tmpSum=tmpSum+tmpValue 
          if(idxPrintSinglePDL==0 && bm_pdlMatrix[4,idxPrintPDL]==1)
          {#PDL N so this is 0
            .MODEL_outputText(outputText=outputText,
                              sprintf(paste0("%-",8,"d"),idxPrintSinglePDL),' ',
                              sprintf(paste0("%-",digits+8,"f"),0),
                              sprintf(paste0("%-",digits+8,"s"),'RESTRICT'),
                              sprintf(paste0("%-",digits+8,"s"),'RESTRICT'),'\n',sep='') 
          } else if(idxPrintSinglePDL==(bm_pdlMatrix[3,idxPrintPDL]-1) && bm_pdlMatrix[5,idxPrintPDL]==1)
          {#PDL F so this is 0
            .MODEL_outputText(outputText=outputText,
                              sprintf(paste0("%-",8,"d"),idxPrintSinglePDL),' ',
                              sprintf(paste0("%-",digits+8,"f"),0),
                              sprintf(paste0("%-",digits+8,"s"),'RESTRICT'),
                              sprintf(paste0("%-",digits+8,"s"),'RESTRICT'),'\n',sep='') 
          } else {
            localErr=bm_betaHatStdErr[posCoeffInCoeffArray+idxPrintSinglePDL] 
            
            if (localErr==0) tmpV=Inf
            else tmpV=(tmpValue)/localErr
            tStat=tmpV
            
            if (bm_DoF==0) tmpV=0
            else tmpV=2*pt(-abs(tStat),bm_DoF)
            pValue=tmpV
            
            localPvalueS='' 
            if (pValue<0.05) localPvalueS='*' 
            if (pValue<0.01) localPvalueS='**' 
            if (pValue<0.001) localPvalueS='***' 
            tmpSD=sprintf(paste0("% -",digits+8,".",digits,"g"),bm_betaHatStdErr[posCoeffInCoeffArray+idxPrintSinglePDL])
            tmpTstat=sprintf(paste0("% -",digits+8,".",digits,"g"),(tStat))
            
            if (! is.finite(tStat) || abs(tStat)>1e9)
            {
              tmpSD=sprintf(paste0("%-",digits+8,"s"),' RESTRICT') 
              tmpTstat=sprintf(paste0("%-",digits+8,"s"),' RESTRICT') 
            }
            
            .MODEL_outputText(outputText=outputText,
                              sprintf(paste0("%-",8,"d"),idxPrintSinglePDL),
                              sprintf(paste0("% -",digits+8,".",digits,"g"),(tmpValue)),
                              tmpSD,
                              tmpTstat,
                              localPvalueS,
                              '\n',sep='') 
          }
        }
        
        #get pdl covariance matrix (subset of whole cov matrix)
        
        tmpCovPdlMatrix=bm_vcov[posCoeffInCoeffArray:(posCoeffInCoeffArray+bm_pdlMatrix[3,idxPrintPDL]-1),
                                posCoeffInCoeffArray:(posCoeffInCoeffArray+bm_pdlMatrix[3,idxPrintPDL]-1)] 
        .MODEL_outputText(outputText=outputText,
                          sprintf(paste0("%-",8,"s"),'SUM'),
                          sprintf(paste0("% -",digits+8,".",digits,"g"),(tmpSum)),
                          sprintf(paste0("% -",digits+8,".",digits,"g"),sqrt(sum(tmpCovPdlMatrix))),'\n',sep='') 
      }
    }
    
    if (thereAreRestrictions == TRUE || 
        (thereArePDL == TRUE && (! is.null(currentBehavioral$pdlRestrictionMatrix))))
    {
      .MODEL_outputText(outputText=outputText,'\nRESTRICTIONS F-TEST:\n') 
      .MODEL_outputText(outputText=outputText,
                        sprintf(paste0("%-",16+2,"s"),'F-value'),
                        sprintf(paste0(":% -",digits+8,".",digits,"g"),(bm_Ftest_restriction)),'\n') 
      .MODEL_outputText(outputText=outputText,
                        sprintf(paste0("%-",16+2,"s"),paste0('F-prob(',bm_restrictionsNum,',',bm_numObs-bm_coeffNum,')')),
                        sprintf(paste0(":% -",digits+8,".",digits,"g"),(bm_Ftest_probability)),'\n') 
      statsOut$FtestRestrValue=bm_Ftest_restriction 
      statsOut$FtestRestrProbability=bm_Ftest_probability 
      statsOut$FtestRestrDoFs=c(bm_restrictionsNum,bm_numObs-bm_coeffNum) 
    }
    
    if (thereAreErrorCorr==TRUE)
    {
      .MODEL_outputText(outputText=outputText,'\nAUTOREGRESSIVE PARAMETERS:\n') 
      .MODEL_outputText(outputText=outputText,
                        sprintf(paste0("%-",digits+8,"s"),'Rho'),
                        sprintf(paste0("%-",digits+8,"s"),'Std. Error'),
                        sprintf(paste0("%-",digits+8,"s"),'T-stat.'),'\n') 
      tStat=(bm_rhoHat)/bm_COstderr 
      pValue=2*pt(-abs(tStat),bm_DoF) 
      statsOut$RhosTstatistics=tStat 
      statsOut$RhosPvalues=pValue 
      rownames(statsOut$RhosTstatistics)=paste0("RHO_",1:bm_COcoeffNum) 
      rownames(statsOut$RhosPvalues)=paste0("RHO_",1:bm_COcoeffNum) 
      for (idxRho in 1:length(bm_rhoHat))
      {
        localPvalueS='' 
        if (pValue[idxRho]<0.05) localPvalueS='*' 
        if (pValue[idxRho]<0.01) localPvalueS='**' 
        if (pValue[idxRho]<0.001) localPvalueS='***' 
        .MODEL_outputText(outputText=outputText,
                          sprintf(paste0("% -",digits+8,".",digits,"g"),(bm_rhoHat[idxRho])),
                          sprintf(paste0("% -",digits+8,".",digits,"g"),bm_COstderr[idxRho]),
                          sprintf(paste0("% -",digits+8,".",digits,"g"),(tStat[idxRho])),
                          localPvalueS,'\n') 
      }
    }
    
    .MODEL_outputText(outputText=outputText,'\n\nSTATs:') 
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
    .MODEL_outputText(outputText=outputText,'\nMean of Dependent Variable     : ',sprintf(stdFormat,base::mean(bm_vectorY)),sep='') 
    .MODEL_outputText(outputText=outputText,'\nNumber of Observations         : ',bm_numObs,sep='') 
    .MODEL_outputText(outputText=outputText,'\nNumber of Degrees of Freedom   : ',bm_DoF,sep='') 
    .MODEL_outputText(outputText=outputText,'\nCurrent Sample (year-period)   : ',paste0(localTSRANGE[1],'-',localTSRANGE[2],' / ',localTSRANGE[3],'-',localTSRANGE[4]),sep='') 
    .MODEL_outputText(outputText=outputText,'\n\n\n') 
    .MODEL_outputText(outputText=outputText,'Signif. codes:   *** 0.001  ** 0.01  * 0.05  \n') 
    .MODEL_outputText(outputText=outputText,'\n') 
    .MODEL_outputText(outputText=outputText,'\n') 
    statsOut$DurbinWatson=bm_dw 
    statsOut$SumSquaresResiduals=bm_ssr 
    statsOut$LogLikelihood=bm_loglike 
    statsOut$AIC=bm_AIC 
    statsOut$BIC=bm_BIC 
    statsOut$Fstatistics=bm_fStat 
    statsOut$Fprobability=bm_fProb 
    statsOut$MeanDependentVariable=base::mean(bm_vectorY) 
    statsOut$ObservationsCount=bm_numObs 
    #export stats
    currentBehavioral$statistics=statsOut 
    #save stsuff
    model$behaviorals[[eqList[eqIdx]]]=currentBehavioral 
    CP_provided=FALSE 
    
    # CHOW test code ---------------------------------------
    if (CHOWTEST==TRUE)
    {
      #get estimation tsrange
      first_estimation_tsrange=currentBehavioral$statistics$TSRANGE
      
      if (is.null(first_estimation_tsrange)) stop('ESTIMATE(): cannot get base estimation TSRANGE of "',eqList[eqIdx],'" during the CHOW test.') 
      
      #check if we have a CHOWPAR
      if (! is.null(CHOWPAR))
      {
        CP_provided=TRUE 
        
        #check chowpar is beyond end of estimation range
        if (NUMPERIOD(c(first_estimation_tsrange[3],first_estimation_tsrange[4]),CHOWPAR,model$frequency)<1) 
          stop('ESTIMATE(): CHOWPAR ',paste(CHOWPAR,collapse='-'),
               ' must be beyond the estimation TSRANGE of "',eqList[eqIdx],'" ',
               paste(first_estimation_tsrange[1:2],collapse='-'),
               ' / ',paste(first_estimation_tsrange[3:4],collapse='-'),'.') 
        
        # perform second estimation on extended tsrange
        tryCatch({
          
          second_model=ESTIMATE(model=model
                                ,TSRANGE=c(first_estimation_tsrange[1],first_estimation_tsrange[2],CHOWPAR[1],CHOWPAR[2])
                                ,forceTSRANGE=TRUE
                                ,eqList=eqList[eqIdx]
                                ,quietly=TRUE
                                ,digits=digits
                                ,CHOWTEST=FALSE #this is mandatory
                                ,tol=tol
                                ,IV=IV
                                ,forceIV=forceIV
                                ,centerCOV=centerCOV
                                ,avoidCompliance=TRUE
                                ,...) 
        },error=function(e){
          stop('ESTIMATE(): error in extended estimation of "',eqList[eqIdx],'", during the CHOW test: ',e$message) 
        })
      } else
      {
        #CHOWPAR must be auto calculated
        
        #set CHOWPAR to first year-period outsite TSRANGE
        CHOWPAR=normalizeYP(c(first_estimation_tsrange[3],first_estimation_tsrange[4]+1),f=frequency) 
        
        #perform second estimation on extended tsrange
        tryCatch({
          
          second_model=ESTIMATE(model=model
                                ,TSRANGE=c(first_estimation_tsrange[1],first_estimation_tsrange[2],CHOWPAR[1],CHOWPAR[2])
                                ,forceTSRANGE=TRUE
                                ,eqList=eqList[eqIdx]
                                ,quietly=TRUE
                                ,digits=digits
                                ,CHOWTEST=FALSE #this is mandatory
                                ,tol=tol
                                ,IV=IV
                                ,forceIV=forceIV
                                ,centerCOV=centerCOV
                                ,avoidCompliance=TRUE
                                ,...) 
        },error=function(e){
          stop('ESTIMATE(): cannot extend TSRANGE during CHOW test in estimation of "',eqList[eqIdx],'": ',e$message) 
        })
        
        tryCatch({
          
          #cycle until error in extended range
          while(TRUE) {
            
            #try incremental chow pars.
            CHOWPAR_tmp=normalizeYP(c(CHOWPAR[1],CHOWPAR[2]+1),f=frequency) 
            second_model=ESTIMATE(model=model
                                  ,TSRANGE=c(first_estimation_tsrange[1],first_estimation_tsrange[2],CHOWPAR_tmp[1],CHOWPAR_tmp[2])
                                  ,forceTSRANGE=TRUE
                                  ,eqList=eqList[eqIdx]
                                  ,quietly=TRUE
                                  ,digits=digits
                                  ,CHOWTEST=FALSE #this is mandatory
                                  ,avoidCompliance=TRUE
                                  ,tol=tol
                                  ,IV=IV
                                  ,forceIV=forceIV
                                  ,centerCOV=centerCOV
                                  ,...) 
            
            #if estimation completes, update the CHOWPAR
            CHOWPAR=CHOWPAR_tmp 
          }
        },error=function(e){}) 
      }
      
      baseBehavioral=currentBehavioral 
      extendedBehavioral=second_model$behaviorals[[eqList[eqIdx]]] 
      
      #get first SSR and df
      SSR=baseBehavioral$statistics$SumSquaresResiduals 
      df=baseBehavioral$statistics$DegreesOfFreedom 
      if (!is.finite(SSR)) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, SSR in first estimation of ',eqList[eqIdx],' is not finite.\n')) 
      if (!quietly && !is.finite(df)) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, degrees of freedom in first estimation of ',eqList[eqIdx],' are not finite.\n')) 
      
      #get second SSR and df
      chow_SSR=extendedBehavioral$statistics$SumSquaresResiduals 
      chow_df=extendedBehavioral$statistics$DegreesOfFreedom 
      if ( !is.finite(SSR)) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, SSR in second estimation of ',eqList[eqIdx],' is not finite.\n')) 
      if ( !is.finite(df)) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, degrees of freedom in second estimation of ',eqList[eqIdx],' are not finite.\n')) 
      if (chow_df==df) .MODEL_outputText(outputText=!quietly,paste0('ESTIMATE(): warning, base and extended estimations of ',eqList[eqIdx],' have the same degrees of freedom\n')) 
      
      #out stats
      chowOut=list() 
      
      #calc stats.
      Fchow = (chow_SSR/SSR-1)*chow_df/(chow_df-df) 
      chowOut$Fvalue=Fchow
      
      DoFchow=c() 
      DoFchow[1]=chow_df-df 
      DoFchow[2]=chow_df 
      chowOut$FtestDegreesOfFreedom=DoFchow 
      ProbFchow=pf(Fchow,DoFchow[1],DoFchow[2],lower.tail=FALSE) 
      chowOut$Fprob=ProbFchow 
      
      #out of sample simulation RESCHECK
      outOfSampleTSRANGE=c(normalizeYP(c(first_estimation_tsrange[3],first_estimation_tsrange[4]+1),f=model$frequency),CHOWPAR) 
      predOutSample=NULL 
      
      tryCatch({
        predOutSample=SIMULATE( model,
                                TSRANGE=outOfSampleTSRANGE,
                                simType = 'RESCHECK',
                                simIterLimit=100,
                                quietly=TRUE,
                                RESCHECKeqList=eqList[eqIdx],
                                avoidCompliance=TRUE) 
      },error=function(e){
        stop('ESTIMATE(): error in out-of-sample prediction (extended RESCHECK simulation) of "',eqList[eqIdx],'" during CHOW test: ',e$message) 
      }) 
      
      PREDICT =predOutSample$simulation[[eqList[eqIdx]]] 
      ACTUAL  =predOutSample$modelData[[eqList[eqIdx]]] 
      ERROR   =ACTUAL-PREDICT 
      
      #fitted standard error calc
      fit_se=vector(mode="numeric", length=length(PREDICT))
      
      #get matrices and sers
      base_ser       =baseBehavioral$statistics$StandardErrorRegression 
      ext_ser        =extendedBehavioral$statistics$StandardErrorRegression 
      base_matrixX   =baseBehavioral$statistics$matrixX 
      ext_matrixX    =extendedBehavioral$statistics$matrixX 
      base_nobs      =baseBehavioral$statistics$ObservationsCount
      base_matrixXaug=baseBehavioral$statistics$matrixXaug 
      ext_matrixXaug =extendedBehavioral$statistics$matrixXaug 
      
      tryCatch({
        #no restrictions on model
        if (is.null(currentBehavioral$matrixR) && is.null(currentBehavioral$pdlRestrictionMatrix))
        {
          #THIS IS CODE DERIVED FROM STANDARD FORMULA
          for (k in 1:(length(PREDICT))) 
            fit_se[k] = base_ser*sqrt(1+ext_matrixX[base_nobs+k,]%*%solve(t(base_matrixX)%*%base_matrixX)%*%ext_matrixX[base_nobs+k,])
          
          #THIS IS CODE DERIVED FROM SPK RESULTS (probably wrong)
          #for (k in 1:(length(PREDICT))) 
          #  fit_se[k] = ext_ser*sqrt(1+ext_matrixX[base_nobs+k,]%*%solve(t(ext_matrixX)%*%ext_matrixX)%*%ext_matrixX[base_nobs+k,])
          
        } else
        {
          coeffCount=baseBehavioral$eqCoefficientsCount 
          
          #THIS IS CODE DERIVED FROM FORMULA
          for (k in 1:(length(PREDICT))) 
            fit_se[k] = base_ser*sqrt(1+ext_matrixX[base_nobs+k,]%*%(solve(base_matrixXaug)[1:coeffCount,1:coeffCount])%*%ext_matrixX[base_nobs+k,])
          
          #THIS IS CODE DERIVED FROM SPK RESULTS
          #for (k in 1:(length(PREDICT))) 
          #  fit_se[k] = ext_ser*sqrt(1+ext_matrixX[base_nobs+k,]%*%(solve(ext_matrixXaug)[1:coeffCount,1:coeffCount])%*%ext_matrixX[base_nobs+k,])
          
        }  
      },error=function(e){
        stop('ESTIMATE(): cannot calculate the standard error of "',eqList[eqIdx],'" in year-period ',
             paste(normalizeYP(c(outOfSampleTSRANGE[1],outOfSampleTSRANGE[2]+k-1),f=model$frequency),collapse = '-'),' during CHOW test: ',e$message) 
      }) 
      
      STDERR=TSERIES(fit_se,START=outOfSampleTSRANGE[1:2],FREQ=model$frequency,avoidCompliance=TRUE) 
      TSTAT=ERROR/STDERR 
      
      #project on tsrange
      ACTUAL=TSPROJECT(ACTUAL,TSRANGE=outOfSampleTSRANGE,avoidCompliance=TRUE)
      PREDICT=TSPROJECT(PREDICT,TSRANGE=outOfSampleTSRANGE,avoidCompliance=TRUE)
      ERROR=TSPROJECT(ERROR,TSRANGE=outOfSampleTSRANGE,avoidCompliance=TRUE)
      STDERR=TSPROJECT(STDERR,TSRANGE=outOfSampleTSRANGE,avoidCompliance=TRUE)
      TSTAT=TSPROJECT(TSTAT,TSRANGE=outOfSampleTSRANGE,avoidCompliance=TRUE)
      
      chowOut$PredictivePower=list()
      chowOut$PredictivePower$ACTUAL=ACTUAL 
      chowOut$PredictivePower$PREDICT=PREDICT 
      chowOut$PredictivePower$ERROR=ERROR 
      chowOut$PredictivePower$STDERR=STDERR 
      chowOut$PredictivePower$TSTAT=TSTAT 
      chowOut$TSRANGE=first_estimation_tsrange 
      chowOut$CHOWPAR=CHOWPAR 
      #print output
      
      .MODEL_outputText(outputText = !quietly,"\nSTABILITY ANALYSIS:\nBehavioral equation:",eqList[eqIdx],"\n\n") 
      .MODEL_outputText(outputText = !quietly,"Chow test:\n") 
      .MODEL_outputText(outputText = !quietly,sprintf(paste0("%-",16+2,"s"),ifelse(!CP_provided,'Sample (auto)','Sample')),
                        ":",paste0(outOfSampleTSRANGE[1:2],collapse='-'),'/',paste0(outOfSampleTSRANGE[3:4],collapse='-'),'\n') 
      .MODEL_outputText(outputText = !quietly,sprintf(paste0("%-",16+2,"s"),'F-value'),
                        sprintf(paste0(":% -",digits+8,".",digits,"g"),(Fchow)),'\n') 
      .MODEL_outputText(outputText = !quietly,
                        sprintf(paste0("%-",16+2,"s"),paste0('F-prob(',DoFchow[1],',',DoFchow[2],')')),
                        sprintf(paste0(":% -",digits+8,".",digits,"g"),(ProbFchow)),'\n') 
      
      .MODEL_outputText(outputText = !quietly,"\nPredictive Power:\n") 
      if (!quietly) 
      {
        #change TABIT column names
        Actual=ACTUAL 
        Predict=PREDICT 
        Error=ERROR 
        `Std. Error`=STDERR 
        `T-stat`=TSTAT 
        TABIT(Actual, Predict, Error, `Std. Error`, `T-stat`, digits=digits)
      }
      
      #store results
      model$behaviorals[[eqList[eqIdx]]]$ChowTest=chowOut 
      
    }#end CHOW test
    
  }#end requested eqs cycle
  
  .MODEL_outputText(outputText=!quietly,'...ESTIMATE OK\n') 
  
  return(model) 
  
}

# RENORM code ------------------------------------------------------------------

RENORM <- function(model=NULL,
                   simAlgo='GAUSS-SEIDEL',
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
                   renormIterLimit=10,
                   renormConvergence=1e-4,
                   TARGET=NULL,
                   INSTRUMENT=NULL,
                   MM_SHOCK=0.00001,
                   quietly=FALSE,
                   quietlyMULTMATRIX=FALSE,
                   tol=1e-28,
                   JACOBIAN_SHOCK=1e-4,
                   JacobianDrop=NULL,
                   forceForwardLooking=FALSE,
                   avoidCompliance=FALSE,
                   ...)
{
  #checks...
  if (is.null(model) ) stop('RENORM(): NULL model.') 
  if (is.null(class( model )) || !(inherits( model ,'BIMETS_MODEL')) )
    stop('RENORM(): "model" must be instance of BIMETS_MODEL class.') 
  if (! (
    is.numeric(renormIterLimit) &&  (renormIterLimit > 0) 
  ) ) stop('RENORM(): "renormIterLimit" must be a positive number.') 
  if (is.null(TSRANGE) ) stop('RENORM(): "TSRANGE" must be defined.') 
  if (! is.finite(tol) || tol<=0) stop('RENORM(): please provide a valid tolerance value.')
  
  if (!(is.logical(quietly)) || is.na(quietly)) stop('RENORM(): "quietly" must be TRUE or FALSE.')
  if (!(is.logical(quietlyMULTMATRIX)) || is.na(quietlyMULTMATRIX)) stop('RENORM(): "quietlyMULTMATRIX" must be TRUE or FALSE.')
  
  if (quietly) quietlyMULTMATRIX=quietly
  
  if (! (
    is.numeric(BackFill) &&  (BackFill >= 0) &&  (BackFill %% 1 == 0)
  ) ) stop('RENORM(): "BackFill" must be zero or a positive integer number.') 
  
  if (!(is.logical(verbose))) stop('RENORM(): "verbose" must be TRUE or FALSE.')
  
  if (! avoidCompliance)
  {
    #check model data
    tryCatch({
      
      .CHECK_MODEL_DATA(model,showWarnings=verbose,'RENORM(): ') 
    },error=function(e){stop('RENORM(): ',e$message)}) 
  }
  
  #reset status
  model$renorm=NULL 
  model$simulation=NULL 
  
  #get data frequecy
  frequency=frequency(model$modelData[[1]]) 
  
  #check TSRANGE is consistent
  if (! is.null(TSRANGE)) {
    tryCatch({
      if (! ( is.numeric(TSRANGE) && length(TSRANGE)==4 && 
              .isCompliantYP(c(TSRANGE[1],TSRANGE[2]),frequency) && 
              .isCompliantYP(c(TSRANGE[3],TSRANGE[4]),frequency) &&
              NUMPERIOD(c(TSRANGE[1],TSRANGE[2]),c(TSRANGE[3],TSRANGE[4]),frequency)>=0)
      ) stop() 
    },error=function(e) {stop('RENORM(): syntax error in TSRANGE: ',e$message)}) 
  }
  
  #periods in TSRANGE
  TSRANGEextension=NUMPERIOD(c(TSRANGE[1],TSRANGE[2]),c(TSRANGE[3],TSRANGE[4]),frequency)+1 
  
  #check TARGET 
  if (is.null(TARGET)) stop('RENORM(): "TARGET" must be defined.') 
  if (! is.list(TARGET) || length(TARGET)==0) 
    stop(paste0('RENORM(): "TARGET" must be a list built of endogenous variables as names and related time series as items.'))
  
  for (idxT in names(TARGET))
    if (! idxT %in% model$vendog) 
      stop(paste0('RENORM(): TARGET "',idxT,'" is not a model endogenous variable.'))
  
  #check TARGET is consistent in TSRANGE
  for (idxT in 1:length(TARGET))
  {
    if (! is.bimets(TARGET[[idxT]])) 
      stop('RENORM(): "TARGET" must be a list of BIMETS time series: "',names(TARGET)[idxT],'" time series is not compliant.') 
    if (frequency(TARGET[[idxT]])!=frequency) 
      stop('RENORM(): time series have not the same frequency. Check TARGET time series "',names(TARGET)[idxT],'".') 
    #project TARGET on TSRANGE
    TARGET[[idxT]]=TSPROJECT(TARGET[[idxT]],TSRANGE=TSRANGE,EXTEND=TRUE,avoidCompliance=TRUE) 
    #deal with missings
    if (any(! is.finite(coredata(TARGET[[idxT]])))) 
      stop('RENORM(): there are undefined values in TSRANGE ',paste0(TSRANGE,collapse='-'),' in TARGET time series "',names(TARGET)[idxT],'"') 
  }
  
  #check INSTRUMENT 
  if (is.null(INSTRUMENT) || length(INSTRUMENT)==0) 
    stop(paste0('RENORM(): "INSTRUMENT" must be defined.'))
  for (idxT in 1:length(INSTRUMENT))
  {
    #check tragets in vendog list
    if (! INSTRUMENT[idxT] %in% c(model$vexog,model$vendog)) 
      stop(paste0('RENORM(): INSTRUMENT variable "',INSTRUMENT[idxT] ,
                  '" is not a model variable.')) 
  }
  
  if (length(INSTRUMENT) != length(TARGET))
    stop(paste0('RENORM(): the targets count must be equal to the instruments count.')) 
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
          stop('RENORM(): constant adjustment must be a list of BIMETS time series: "',idxCA,'" time series is not compliant.') 
        #CA needs same frequency as modelData
        if (frequency(ConstantAdjustment[[idxCA]])!=frequency(model$modelData[[1]])) 
          stop('RENORM(): time series do not have the same frequency. Check constant adjustment time series "',idxCA,'".') 
        #warning about missings
        if (any(! is.finite(coredata(ConstantAdjustment[[idxCA]])))) 
          .MODEL_outputText(outputText=!quietly,'RENORM(): warning, there are undefined values in constant adjustment of the time series "',idxCA,'"\n',sep='') 
      }
  }
  
  #store original INSTRUMENT, in case we need to restore after error
  INSTRUMENT_ORIGINAL=list() 
  INSTRUMENT_CURRENT=list() 
  for (idxI in 1:length(INSTRUMENT))
  {
    tmpI=INSTRUMENT[idxI] 
    if (tmpI %in% model$vexog)
    {
      #instrument is vexog so get data from model$data
      INSTRUMENT_ORIGINAL[[tmpI]]=model$modelData[[tmpI]] 
    } else if (tmpI %in% model$vendog)
    {
      #instrument is vendog so get data from CA
      if (is.null(ConstantAdjustment[[tmpI]])) 
      {
        #CA does not exist so it is zero... 
        ConstantAdjustment[[tmpI]]=TSERIES(rep(0,TSRANGEextension),START=TSRANGE[1:2],FREQ=frequency) 
        INSTRUMENT_ORIGINAL[[tmpI]]=ConstantAdjustment[[tmpI]] 
        
      } else 
      {
        INSTRUMENT_ORIGINAL[[tmpI]]=ConstantAdjustment[[tmpI]] 
      }
    }
    else stop(paste0('RENORM(): INSTRUMENT variable "', tmpI,
                     '" is not a model exogenous variable.')) 
  }
  
  #export some stuff to model
  model$renorm[['__RENORM_PARAMETERS__']]$Exogenize=Exogenize 
  model$renorm[['__RENORM_PARAMETERS__']]$ConstantAdjustment=ConstantAdjustment 
  model$renorm[['__RENORM_PARAMETERS__']]$renormIterLimit=renormIterLimit
  model$renorm[['__RENORM_PARAMETERS__']]$renormConvergence=renormConvergence
  
  if (! is.list(model$simulation)) model$simulation=list()
  
  #init simulated TARGET with historical values
  for (idxN in names(TARGET))
  {
    #get data from historical
    model$simulation[[idxN]]=TSPROJECT(model$modelData[[idxN]],TSRANGE=TSRANGE,EXTEND=TRUE,avoidCompliance=TRUE) 
   
  }
  
  #main cycle index
  renormNIter=0 
  
  #get actual TARGETs in TARG_
  xxx_=matrix(nrow=length(TARGET),ncol=TSRANGEextension) 
  for (idxR in 1:length(TARGET))
  {
    #already projected
    xxx_[idxR,]=coredata(TARGET[[idxR]]) 
  }
  TARG_=vector('double',length(xxx_)) 
  TARG_=c(xxx_) 
  
  #used in convergence
  DIFF_=list() 
  
  #main cycle
  for (renormNIter in 0:renormIterLimit)
  {
    #names on unconverged TARGETs
    unConvergedTARGET=c() 
    convergenceRenorm=TRUE 
    
    #check differences in TARGETs
    if (renormNIter>0) for (tmpN in names(TARGET))
    {
      tmpTs=TSPROJECT(model$simulation[[tmpN]],TSRANGE=TSRANGE,avoidCompliance=TRUE)
      
      DIFF_[[tmpN]]=(TARGET[[tmpN]]-tmpTs)/TARGET[[tmpN]]
      
      if (any(TARGET[[tmpN]]==0)) 
      {
        zeroLoc=which(TARGET[[tmpN]]==0)
        DIFF_[[tmpN]][zeroLoc]=(TARGET[[tmpN]][zeroLoc]-tmpTs[zeroLoc])
      }
      
      #verify convergence
      if (sqrt(sum(DIFF_[[tmpN]]^2))>=renormConvergence) 
      {
        unConvergedTARGET=c(unConvergedTARGET,tmpN) 
        convergenceRenorm=FALSE 
      }
    }
    
    #no convergence
    if (convergenceRenorm==FALSE && renormNIter==renormIterLimit)
    {
      .MODEL_outputText(outputText=!quietly,paste0('\nRENORM(): warning, no convergence in ',renormNIter,' iterations.\n')) 
      
      #export unconverged symbols
      model$renorm$unConvergedTARGET=unConvergedTARGET 
      break 
    }
    
    # enable this code to enable initial TARGET check
    # if (convergenceRenorm==TRUE && renormNIter==0)
    # {
    #   .MODEL_outputText(outputText=!quietly,paste0('\nRENORM(): all TARGET are equal to model data. This procedure will exit.\n')) 
    #   break 
    # }			
    
    #convergence
    if (convergenceRenorm==TRUE && renormNIter>0)
    {
      .MODEL_outputText(outputText=!quietly,paste0('\nConvergence reached in ',renormNIter,' iterations.\n...RENORM OK\n')) 
      
      #export results
      tmpL=list() 
      for (idxI in 1:length(INSTRUMENT))
      {
        tmpL[[INSTRUMENT[idxI]]]=TSERIES(SINSTR_[idxI,],START=c(TSRANGE[1],TSRANGE[2]),FREQ=frequency,avoidCompliance=TRUE) 
      }
      model$renorm$INSTRUMENT=tmpL 
      
      #export data used in convergence (before restore)
      model$renorm$modelData=model$modelData 
      model$renorm$ConstantAdjustment=ConstantAdjustment 
      break 
    }
    
    .MODEL_outputText(outputText=!quietly,paste0('\nRENORM(): iteration #',renormNIter+1,'\n')) 
    
    #get current INSTRUMENT
    for (idxI in 1:length(INSTRUMENT))
    {
      tmpI=INSTRUMENT[idxI] 
      if (tmpI %in% model$vexog)
      {
        #instrument is vexog so get data from model$data
        INSTRUMENT_CURRENT[[tmpI]]=model$modelData[[tmpI]] 
      } else if (tmpI %in% model$vendog)
      {
        INSTRUMENT_CURRENT[[tmpI]]=ConstantAdjustment[[tmpI]] 
      }
      
      #project in TSRANGE
      INSTRUMENT_CURRENT[[tmpI]]=TSPROJECT(INSTRUMENT_CURRENT[[tmpI]],TSRANGE=TSRANGE,EXTEND=TRUE,avoidCompliance=TRUE) 
      xxx_[idxI,]=coredata(INSTRUMENT_CURRENT[[tmpI]]) 
    }
    INSTR_=c(xxx_) 
    
    tryCatch({
      
      # multmatrix -------------------------------------------------------------------------------
     
      #calc MM
      model=MULTMATRIX(model=model,
                       simAlgo=simAlgo,
                       RENORM=TRUE,
                       TSRANGE=TSRANGE,
                       simType=simType,
                       simConvergence=simConvergence,
                       simIterLimit=simIterLimit,
                       ZeroErrorAC=ZeroErrorAC,
                       BackFill=BackFill,
                       Exogenize=Exogenize,
                       ConstantAdjustment=ConstantAdjustment,
                       verbose=verbose,
                       verboseSincePeriod=verboseSincePeriod,
                       verboseVars=verboseVars,
                       TARGET=names(TARGET),
                       INSTRUMENT=INSTRUMENT,
                       MM_SHOCK=MM_SHOCK,
                       quietly=quietlyMULTMATRIX,
                       JACOBIAN_SHOCK=JACOBIAN_SHOCK,
                       JacobianDrop=JacobianDrop,
                       forceForwardLooking=forceForwardLooking,
                       avoidCompliance=TRUE,
                       ...) 
    }, error=function(err){
      
      stop(paste0('\nRENORM(): error in iteration #',renormNIter+1,': ',err$message)) 
    })
   
    #get simulated TARGETs
    for (idxT in 1:length(TARGET))
    {
      #in MULTMAT we have multiple simulation column results, so [,1]
      xxx_[idxT,]=coredata(TSPROJECT(model$simulation[[names(TARGET)[idxT]]],TSRANGE=TSRANGE,avoidCompliance=TRUE))
    }
    TARG0_=c((xxx_)) 
    
    #calc delta in TARGET
    NTARG_ =TARG_-TARG0_ 
    
    #get adjusted INSTRUMENTs
    tryCatch({
      
      INSTR_=as.vector(INSTR_+solve(model$MultiplierMatrix,
                                    tol=tol) %*% NTARG_) 
    },error=function(err){
      
      stop(paste0('RENORM(): cannot invert multipliers matrix in iteration #',renormNIter+1,': ',err$message)) 
    })  
    
    SINSTR_=matrix(INSTR_,nrow=length(INSTRUMENT),ncol=TSRANGEextension)
    
    #update adjusted INSTRUMENTs
    for (idxI in 1:length(INSTRUMENT))
    {
      tmpI=INSTRUMENT[idxI] 
      if (tmpI %in% model$vexog)
      {
        #instrument is vexog so append new data to model$data
        model$modelData[[tmpI]][[TSRANGE[1],TSRANGE[2]]]=SINSTR_[idxI,] 
      } else if (tmpI %in% model$vendog)
      {
        ConstantAdjustment[[tmpI]][[TSRANGE[1],TSRANGE[2]]]=SINSTR_[idxI,] 
      }
    }
  }#end main cycle
  
  #restore model data
  for (idxI in 1:length(INSTRUMENT))
  {
    tmpI=INSTRUMENT[idxI] 
    if (tmpI %in% model$vexog)
    {
      #instrument is vexog so restore data to model$data
      model$modelData[[tmpI]]=INSTRUMENT_ORIGINAL[[tmpI]] 
    } else if (tmpI %in% model$vendog)
    {
      #instrument is vendog so restore data to CA
      ConstantAdjustment[[tmpI]]=INSTRUMENT_ORIGINAL[[tmpI]] 
    }
  }
  
  #export achieved TARGET
  tmpL=list() 
  for (idxN in names(TARGET))
  {
    #...probably TSPROJECT not required								
    tmpL[[idxN]]=TSPROJECT(model$simulation[[idxN]],TSRANGE=TSRANGE,EXTEND=TRUE,avoidCompliance=TRUE) 
  }
  model$renorm$TARGET=tmpL 
  
  #export renorm parameters 
  model$renorm[['__RENORM_PARAMETERS__']]$TSRANGE=TSRANGE 
  model$renorm[['__RENORM_PARAMETERS__']]$TARGET=TARGET 
  model$renorm[['__RENORM_PARAMETERS__']]$INSTRUMENT=INSTRUMENT 
  
  return(model)
  
}

# MULTMATRIX code --------------------------------------------------------------

MULTMATRIX <- function(model=NULL,
                       simAlgo='GAUSS-SEIDEL',
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
                       TARGET=NULL,
                       INSTRUMENT=NULL,
                       MM_SHOCK=0.00001,
                       quietly=FALSE,
                       JACOBIAN_SHOCK=1e-4,
                       JacobianDrop=NULL,
                       forceForwardLooking=FALSE,
                       avoidCompliance=FALSE,
                       ...)
{
  tryCatch({
    
    model=SIMULATE(model=model,
                   simAlgo=simAlgo,
                   TSRANGE=TSRANGE,
                   simType=simType,
                   simConvergence=simConvergence,
                   simIterLimit=simIterLimit,
                   ZeroErrorAC=ZeroErrorAC,
                   BackFill=BackFill,
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
                   JACOBIAN_SHOCK=JACOBIAN_SHOCK,
                   JacobianDrop=JacobianDrop,
                   forceForwardLooking=forceForwardLooking,
                   avoidCompliance=avoidCompliance,
                   ...) 
  }, error=function(err){
    
    stop('\n',err$message)
  })
  
  return(model)
}

# OPTIMIZE code ----------------------------------------------------------------

OPTIMIZE <- function(model=NULL,
                     simAlgo='GAUSS-SEIDEL',
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
                     StochReplica=100,
                     StochSeed=NULL,
                     OptimizeBounds=NULL,
                     OptimizeRestrictions=NULL,
                     OptimizeFunctions=NULL,
                     quietly=FALSE,
                     RESCHECKeqList=NULL,
                     JACOBIAN_SHOCK=1e-4,
                     JacobianDrop=NULL,
                     forceForwardLooking=FALSE,
                     avoidCompliance=FALSE,
                     ...)
{
  tryCatch({
 
    model=SIMULATE(model=model,
                   simAlgo=simAlgo,
                   OPTIMIZE=TRUE,
                   TSRANGE=TSRANGE,
                   simType=simType,
                   simConvergence=simConvergence,
                   simIterLimit=simIterLimit,
                   ZeroErrorAC=ZeroErrorAC,
                   BackFill=BackFill,
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
                   JACOBIAN_SHOCK=JACOBIAN_SHOCK,
                   JacobianDrop=JacobianDrop,
                   forceForwardLooking=forceForwardLooking,
                   avoidCompliance=avoidCompliance,
                   ...) 
  }, error=function(err){
    
    stop('\n',err$message)
  })
  
  return(model)
}

# STOCHSIMULATE code -----------------------------------------------------------

STOCHSIMULATE <- function(model=NULL,
                          simAlgo='GAUSS-SEIDEL',
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
                          StochStructure=NULL,
                          StochReplica=100,
                          StochSeed=NULL,
                          quietly=FALSE,
                          RESCHECKeqList=NULL,
                          JACOBIAN_SHOCK=1e-4,
                          JacobianDrop=NULL,
                          forceForwardLooking=FALSE,
                          avoidCompliance=FALSE,
                          ...)
{
  tryCatch({
    
    model=SIMULATE(model=model,
                   simAlgo=simAlgo,
                   STOCHSIMULATE=TRUE,
                   TSRANGE=TSRANGE,
                   simType=simType,
                   simConvergence=simConvergence,
                   simIterLimit=simIterLimit,
                   ZeroErrorAC=ZeroErrorAC,
                   BackFill=BackFill,
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
                   JACOBIAN_SHOCK=JACOBIAN_SHOCK,
                   JacobianDrop=JacobianDrop,
                   forceForwardLooking=forceForwardLooking,
                   avoidCompliance=avoidCompliance,
                   ...) 
  }, error=function(err){
    
    stop('\n',err$message)
  })
  
  return(model)
}

# SIMULATE code ----------------------------------------------------------------

SIMULATE <- function(model=NULL,
                     simAlgo='GAUSS-SEIDEL',
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
                     JACOBIAN_SHOCK=1e-4,
                     JacobianDrop=NULL,
                     forceForwardLooking=FALSE,
                     avoidCompliance=FALSE,
                     ...)
{ 
  if (is.null(model) ) stop(callerName,'NULL model.') 
  if (is.null(class( model )) || !(inherits( model ,'BIMETS_MODEL')) )
    stop(callerName,'model must be instance of "BIMETS_MODEL" class.') 
  
  #parent function call name
  callerName='SIMULATE'
  
  if (! is.logical(MULTMATRIX) || is.na(MULTMATRIX)) stop('SIMULATE(): "MULTMATRIX" must be logical.') 
  if (! is.logical(RENORM) || is.na(RENORM)) stop('SIMULATE(): "RENORM" must be logical.') 
  if (! is.logical(STOCHSIMULATE) || is.na(STOCHSIMULATE)) stop('SIMULATE(): "STOCHSIMULATE" must be logical.') 
  if (! is.logical(OPTIMIZE) || is.na(OPTIMIZE)) stop('SIMULATE(): "OPTIMIZE" must be logical.') 
  
  if (MULTMATRIX)
  {
    if (RENORM)
    { 
      callerName='RENORM'
    } else 
    {
      callerName='MULTMATRIX'
    }
  }
  
  if (STOCHSIMULATE) callerName='STOCHSIMULATE'
  if (OPTIMIZE) callerName='OPTIMIZE'
  
  .checkModelBimetsVersion(model, callerName)
  
  callerName=paste0(callerName,'(): ')
  
  if (is.null(TSRANGE) ) stop(callerName,'"TSRANGE" must be defined.') 
  
  if (! ( is.numeric(simIterLimit) &&  (simIterLimit > 0) )) 
    stop(callerName,'"simIterLimit" must be a positive number.') 
  if (! ( is.numeric(BackFill) &&  (BackFill >= 0) &&  (BackFill %% 1 == 0) )) 
    stop(callerName,'"BackFill" must be zero or a positive integer number.') 
  if (! ( is.numeric(simConvergence) && (simConvergence > 0) )) 
    stop(callerName,'"simConvergence" must be a positive number.') 
  if (! is.character(simType) || ! toupper(simType) %in% c('DYNAMIC','STATIC','RESCHECK','FORECAST')) 
    stop(callerName,'"simType" must be "DYNAMIC", "STATIC", "FORECAST" or "RESCHECK"') 
  simType=toupper(simType) 
  
  if (! is.character(simAlgo) || ! toupper(simAlgo) %in% c('GAUSS-SEIDEL', 'NEWTON','FULLNEWTON')) 
    stop(callerName,'"simAlgo" must be "GAUSS-SEIDEL", "NEWTON" or "FULLNEWTON".') 
  if (simAlgo=='FULLNEWTON' && MULTMATRIX==FALSE && RENORM==FALSE && OPTIMIZE==FALSE && STOCHSIMULATE==FALSE)  
    stop(callerName,'"FULLNEWTON" algorithm cannot be used in SIMULATE procedure. Please switch to "NETWON" or "GAUSS-SEIDEL".')
  simType=toupper(simType) 
  if (! (is.numeric(MM_SHOCK) && is.finite(MM_SHOCK) && MM_SHOCK>0)) 
    stop(callerName,'"MM_SHOCK" must be numeric and positive.')
  if (! (is.numeric(JACOBIAN_SHOCK) && is.finite(JACOBIAN_SHOCK) && JACOBIAN_SHOCK>0)) 
    stop(callerName,'"JACOBIAN_SHOCK" must be numeric and positive.')
  
  if (! is.logical(verbose) || is.na(verbose)) stop(callerName,'"verbose" must be logical.') 
  if (! is.logical(forceForwardLooking) || is.na(forceForwardLooking)) stop(callerName,'"forceForwardLooking" must be logical.') 
  
  if (! is.logical(quietly) || is.na(quietly)) stop(callerName,'"quietly" must be logical.') 
  if (MULTMATRIX && simType=='RESCHECK')																			   
    stop(callerName,'a "RESCHECK" simulation is not allowed in a MULTMATRIX() operation.')
  
  if (! (
    is.numeric(verboseSincePeriod) &&  (verboseSincePeriod >= 0) 
  ) ) stop(callerName,'"verboseSincePeriod" must be a positive number.') 
  if (MULTMATRIX && STOCHSIMULATE) stop(callerName,'if "STOCHSIMULATE" is TRUE then "MULTMATRIX" must be FALSE.') 
  if (OPTIMIZE && STOCHSIMULATE) stop(callerName,'if "STOCHSIMULATE" is TRUE then "OPTIMIZE" must be FALSE.') 
  if (MULTMATRIX && OPTIMIZE) stop(callerName,'if "OPTIMIZE" is TRUE then "MULTMATRIX" must be FALSE.') 
  if (verbose) quietly=FALSE 
  
  if (! avoidCompliance)
  {
    #check model data
    tryCatch({
      .CHECK_MODEL_DATA(model,showWarnings=verbose,callerName) 
    },error=function(e){stop(callerName,'',e$message)}) 
  }
  
  #get regex definitions
  regExDefs=.RegExGlobalDefinition() 
  reservedKeyw=regExDefs$reservedKeyw 
  symOnEqCleaner=regExDefs$symOnEqCleaner 
  symOnIfCleaner=regExDefs$symOnIfCleaner 
  allowedCharOnName=regExDefs$allowedCharOnName 
  allowedCharOnNameToken=regExDefs$allowedCharOnNameToken 
  charsOnNumbs=regExDefs$charsOnNumbs 
  charsOnNumWithSign=regExDefs$charsOnNumWithSign 
  strongCharOnNumbs=regExDefs$strongCharOnNumbs 
  strongCharOnNumbsWithSign=regExDefs$strongCharOnNumbsWithSign 
  allowedLhsEQfuns=regExDefs$allowedLhsEQfuns 
  allowedLhsEQfunsPub=regExDefs$allowedLhsEQfunsPub 
  
  #main tryCatch for compliance speedup
  tryCatch({
  
  #get compliance status
  complianceStatus=getBIMETSconf('BIMETS_CONF_NOC')
  setBIMETSconf('BIMETS_CONF_NOC',TRUE, suppressOutput=TRUE)
  
  #get data frequency
  frequency=frequency(model$modelData[[1]]) 
  
  #check TSRANGE is consistent
  if (! is.null(TSRANGE)) {
    tryCatch({
      if (! ( is.numeric(TSRANGE) && length(TSRANGE)==4 && 
              .isCompliantYP(c(TSRANGE[1],TSRANGE[2]),frequency) && 
              .isCompliantYP(c(TSRANGE[3],TSRANGE[4]),frequency) &&
              NUMPERIOD(c(TSRANGE[1],TSRANGE[2]),c(TSRANGE[3],TSRANGE[4]),frequency)>=0
      )
      ) stop() 
    },error=function(e) {stop(callerName,'syntax error in TSRANGE: ',e$message)}) 
  }
  
  #check exoginanda vendog exists in model
  if (! is.null(Exogenize))
  {
    if (! is.list(Exogenize) ) 
      stop(callerName,'"Exogenize" must be a list built of endogenous variables as names and related TSRANGE as items.')
    
    if (length(names(Exogenize)) != length(unique(names(Exogenize)))) stop(callerName,'there are duplicated names in "Exogenize".') 
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
    if (OPTIMIZE)
      stop(callerName,'"RESCHECKeqList" in not available in a OPTIMIZE operation.')
    
    if (simType!='RESCHECK')
    {
      .MODEL_outputText(outputText=!quietly,paste0(callerName,'simulation of type "',simType,'" requested. "RESCHECKeqList" will be set to NULL.\n')) 
      RESCHECKeqList=NULL 
    } else
    {
      if (! is.character(RESCHECKeqList) ) 
        stop(callerName,'"RESCHECKeqList" must be a character array built of endogenous variables names.')
      
      if (length(base::setdiff((RESCHECKeqList),model$vendog)) >0) 
        stop(callerName,'request to RESCHECK the following "',
             paste0(base::setdiff((RESCHECKeqList),model$vendog),collapse=', '),
             '", not endogenous variable(s) of the model.')
      
      if (length(Exogenize)>0) 
        for (idxEL in names(Exogenize))
        {
          if (! idxEL %in% RESCHECKeqList)
          {
            .MODEL_outputText(outputText=!quietly,paste0(callerName,'warning, request to exogenize "',idxEL,'", but it is not in "RESCHECKeqList". It will be removed from "Exogenize" list.\n'))
            Exogenize[[idxEL]]=NULL
          }
        }
      
      if (length(ConstantAdjustment)>0) 
        for (idxCA in names(ConstantAdjustment))
        {
          if (! idxCA %in% RESCHECKeqList)
          {
            .MODEL_outputText(outputText=!quietly,paste0(callerName,'warning, constant adjustment "',idxCA,'" is not in "RESCHECKeqList". It will be removed from "ConstantAdjustment" list.\n'))
            ConstantAdjustment[[idxCA]]=NULL
          }
        }
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
        Exogenize[[idxEL]]=TSRANGE 
        next 
      }
      
      #check provided TSRANGE
      tryCatch({
        tmpTSRANGE=Exogenize[[idxEL]]
        if (! ( is.numeric(tmpTSRANGE) && length(tmpTSRANGE)==4 && 
                .isCompliantYP(c(tmpTSRANGE[1],tmpTSRANGE[2]),frequency) && 
                .isCompliantYP(c(tmpTSRANGE[3],tmpTSRANGE[4]),frequency) &&
                NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                          c(tmpTSRANGE[3],tmpTSRANGE[4]),frequency)>=0
        )
        ) stop() 
      },error=function(e) {stop(callerName,'syntax error in Exogenize TSRANGE of "',
                                names(Exogenize)[idxEL],'". ',e$message)
      }) 
    } 

  #sim steps count
  simSteps=NUMPERIOD(TSRANGE[1:2],TSRANGE[3:4],frequency)+1 
  
  #check (ConstantAdjustment) is consistent
  if (length(ConstantAdjustment)>0) 
    for (idxCA in 1:length(ConstantAdjustment))
    {
      if (! is.bimets(ConstantAdjustment[[idxCA]])) 
        stop(callerName,'"ConstantAdjustment" must be a list of BIMETS time series: "',names(ConstantAdjustment)[idxCA],'" time series is not compliant.') 
      if (frequency(ConstantAdjustment[[idxCA]])!=frequency(model$modelData[[1]])) 
        stop(callerName,'time series must have the same frequency. Check constant adjustment time series "',names(ConstantAdjustment)[idxCA],'".') 
      if (verbose && any(! is.finite(coredata(ConstantAdjustment[[idxCA]])))) .MODEL_outputText(outputText = !quietly,callerName,'warning, there are undefined values in constant adjustment time series "',names(ConstantAdjustment)[idxCA],'".\n',sep='') 
    }
  
  #check intersection between Simulation TSRANGE and Exogenize TSRANGE
  #remove items whose exogenation TSRANGE is outside Simulation TSRANGE
  idxToBeRemoved=c() 
  if (length(Exogenize)>0) 
    for (idxEL in 1:length(Exogenize))
    {
      tmpTSRANGE=Exogenize[[idxEL]]
      
      if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                    c(TSRANGE[1],TSRANGE[2]),frequency)>0)
      {
        if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxEL) 
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, exogenize TSRANGE of "',names(Exogenize)[idxEL],'" does not intersect with simulation TSRANGE.\n')) 
        } 
      }
      
      if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                    c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
      {
        if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxEL) 
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, exogenize TSRANGE of "',names(Exogenize)[idxEL],'" does not intersect with simulation TSRANGE.\n')) 
        }
      }
    }
  
  #remove out of range exogenizations
  if (length(idxToBeRemoved)>0) Exogenize=Exogenize[-idxToBeRemoved]
  
  if (length(Exogenize)>0) 
    for (idxEL in 1:length(Exogenize))
    {
      #cut exogenization range up to sim start
      if (NUMPERIOD(c(Exogenize[[idxEL]][1],Exogenize[[idxEL]][2]),
                    c(TSRANGE[1],TSRANGE[2]),frequency)>0)
      {
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, exogenize TSRANGE of "',names(Exogenize)[idxEL],'" starts before the simulation TSRANGE.\n')) 
        Exogenize[[idxEL]][1]=TSRANGE[1] 
        Exogenize[[idxEL]][2]=TSRANGE[2] 
      }
      
      #cut exogenization range back to sim end
      if (NUMPERIOD(c(Exogenize[[idxEL]][3],Exogenize[[idxEL]][4]),
                    c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
      {
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, exogenize TSRANGE of "',names(Exogenize)[idxEL],'" ends after the simulation TSRANGE.\n')) 
        Exogenize[[idxEL]][3]=TSRANGE[3] 
        Exogenize[[idxEL]][4]=TSRANGE[4] 
      }
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
  instrumentVendogs=NULL 
  if (anyDuplicated(INSTRUMENT)>0) stop(callerName,'there are duplicated entries on INSTRUMENT namelist: "',INSTRUMENT[anyDuplicated(INSTRUMENT)],'".')
  if (anyDuplicated(TARGET)>0) stop(callerName,'there are duplicated entries on TARGET namelist: "',TARGET[anyDuplicated(TARGET)],'".')
  
  if (MULTMATRIX==TRUE && length(INSTRUMENT)>0) 
  { 
    instrumentVendogs=base::intersect(INSTRUMENT,model$vendog) 
    for (idxT in 1:length(INSTRUMENT))
    {
      #if instrument is endogenous then we use related add-factor
      if (INSTRUMENT[idxT] %in% model$vendog) 
      {
        INSTRUMENT[idxT]=paste0(INSTRUMENT[idxT],'__ADDFACTOR') 
        next 
      }
      
      #check tragets in vendog list
      if (! INSTRUMENT[idxT] %in% model$vexog) stop(callerName,'INSTRUMENT variable "',INSTRUMENT[idxT] ,
                                                    '" is not a model variable.')
    }
  }
  
  #set replica if MULTMATRIX selected
  if (MULTMATRIX==TRUE) 
  {
    #in lead case replica count is the same
    replica=length(INSTRUMENT)*simSteps+1 
  } else replica=1 
 
  # local var localE speedup -----------------------------------------------
  
  #a local env will contains model time series in assign(), get() and eval()
  #it will speed up calculus
  localE=new.env() 
  
  #we need at least 1 lag in order to start simulation in case simType=FORECAST 
  model_max_lag=max(model$max_lag,1) 
  model_max_lag_1=model_max_lag+1
  model_max_lead=model$max_lead
   
  #use local var for speedup
  model_vendog=model$vendog 
  model_vexog=model$vexog 
  length_model_vexog=length(model_vexog)
  model_names_behavioral=names(model$behaviorals) 
  model_names_identity=names(model$identities) 
  model_fullComponentList=c(model$vendog,model$vexog) 
  model_vpre=model$vpre
  length_model_vpre=length(model_vpre)
  length_model_vblocks=length(model$vblocks)
  model_vblocks=model$vblocks
  model_vsim=list()
  model_vpost=list()
  model_vfeed=list()
  length_model_vsim=c()
  length_model_vpost=c()
  length_model_vfeed=c()
  
  if (length(ConstantAdjustment)>0) 
    CA_names=paste0(names(ConstantAdjustment),'__ADDFACTOR')
  if (length(instrumentVendogs)>0) 
    instrum_names=paste0(instrumentVendogs,'__ADDFACTOR')
  
  # check STOCHSIMULATE ------------------------------------------------------------
 
  if (STOCHSIMULATE)
  {
    #check arguments
    if (!is.list(StochStructure) || is.null(names(StochStructure))) 
      stop(callerName,'"StochStructure" must be a named list.') 
    if (length(names(StochStructure)) != length(unique(names(StochStructure)))) 
      stop(callerName,'there are duplicated names in "StochStructure"') 
    
    for (idxI in 1:length(StochStructure))
    {
      idxN=names(StochStructure)[idxI]
      
      if (! idxN %in% c(model$vendog,model$vexog))
        stop(callerName,'"StochStructure" has a named element "',idxN,'" that is not a model variable.')
      
      if (! is.list(StochStructure[[idxN]]) 
          || is.null(names(StochStructure[[idxN]])) 
          || length(StochStructure[[idxN]])!=3
          || all(sort(names(StochStructure[[idxN]])) != c('PARS','TSRANGE','TYPE')))
        stop(callerName,'"StochStructure$',idxN,'" must be a named list built of the following 3 components: TSRANGE, TYPE, PARS.')
      
      if (length(StochStructure[[idxN]]$TSRANGE)==1 && is.logical(StochStructure[[idxN]]$TSRANGE) && ! is.na(StochStructure[[idxN]]$TSRANGE)) 
      {
        if (StochStructure[[idxN]]$TSRANGE!=TRUE) 
          stop(callerName,'"StochStructure$',idxN,'$TSRANGE" must be TRUE or a 4 element integer array.')
        
        #set full TSRANGE if StochStructure$idxN$TSRANGE is TRUE
        StochStructure[[idxN]]$TSRANGE=TSRANGE 
        
      } else
      { 
        #check TSRANGE
        tryCatch({
          tmpTSRANGE=StochStructure[[idxN]]$TSRANGE
          if (! ( is.numeric(tmpTSRANGE) && length(tmpTSRANGE)==4 && 
                  .isCompliantYP(c(tmpTSRANGE[1],tmpTSRANGE[2]),frequency) && 
                  .isCompliantYP(c(tmpTSRANGE[3],tmpTSRANGE[4]),frequency) &&
                  NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                            c(tmpTSRANGE[3],tmpTSRANGE[4]),frequency)>=0
          )
          ) stop() 
        },error=function(e) {stop(callerName,'syntax error in "StochStructure$',idxN,'$TSRANGE". ',e$message)
        }) 
      }
    }
    
    idxToBeRemoved=c() 
    for (idxN in 1:length(StochStructure))
    {
      elemName=names(StochStructure)[idxN] 
      tmpTSRANGE=StochStructure[[idxN]]$TSRANGE
      
      #check intersection between StochStructure TSRANGE and Simulation TSRANGE
      #remove items whose TSRANGE is outside Simulation TSRANGE
      if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                    c(TSRANGE[1],TSRANGE[2]),frequency)>0)
      {
        if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxN) 
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "StochStructure$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n')) 
        }
      }
      
      if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                    c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
      {
        if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxN) 
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "StochStructure$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n')) 
        }
      }
    }
    
    #remove unused stoch perturbation
    if (length(idxToBeRemoved)>0) StochStructure=StochStructure[-idxToBeRemoved]
    
    #project stoch TSRANGE in sim TSRANGE
    if (length(StochStructure)>0) 
    {  for (idxN in 1:length(StochStructure))
      {
        tmpTSRANGE=StochStructure[[idxN]]$TSRANGE
        if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "StochStructure$',names(StochStructure)[idxN],'$TSRANGE" starts before the simulation TSRANGE\n')) 
          tmpTSRANGE[1]=TSRANGE[1] 
          tmpTSRANGE[2]=TSRANGE[2] 
        }
        
        if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "StochStructure$',names(StochStructure)[idxN],'$TSRANGE" ends after the simulation TSRANGE\n')) 
          tmpTSRANGE[3]=TSRANGE[3] 
          tmpTSRANGE[4]=TSRANGE[4] 
        }
        StochStructure[[idxN]]$TSRANGE=tmpTSRANGE
      }
      
      for (idxI in 1:length(StochStructure))
      {
        idxN=names(StochStructure)[idxI]
        
        if (! is.character(StochStructure[[idxN]]$TYPE) 
            || ! StochStructure[[idxN]]$TYPE %in% c('NORM','UNIF','MATRIX')
        ) stop(callerName,'"StochStructure$',idxN,'$TYPE" must be NORM or UNIF or MATRIX.')
        
        if (StochStructure[[idxN]]$TYPE=='UNIF'
            || StochStructure[[idxN]]$TYPE=='NORM')
        {
          if (any(is.null(StochStructure[[idxN]]$PARS)) 
              || any(! is.finite(StochStructure[[idxN]]$PARS))
              || length(StochStructure[[idxN]]$PARS) !=2) 
            stop(callerName,'"StochStructure$',idxN,'$PARS" must be a 2 elements finite numerical array.')
        }
        
        if (StochStructure[[idxN]]$TYPE=='MATRIX')
        {
          if (! is.matrix(StochStructure[[idxN]]$PARS) || !(all(is.finite(StochStructure[[idxN]]$PARS)))) 
            stop(callerName,'"StochStructure$',idxN,'$PARS" must be a finite matrix.')
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
      set.seed(StochSeed) 
    }
    
    #set replica if STOCHSIMULATE selected
    #(first column is unperturbed model so we add one)
    replica=StochReplica+1 
    
  }#end if STOCHSIMULATE
  
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
      set.seed(StochSeed) 
    }
    
    #set replica if OPTIMIZE selected
    #(first column id unperturbed model so we add plus one)
    replica=StochReplica+1 
    
    #check argument OptimizeBounds
    if (!is.list(OptimizeBounds) || is.null(names(OptimizeBounds))) 
      stop(callerName,'"OptimizeBounds" must be a named list.') 
    if (length(names(OptimizeBounds)) != length(unique(names(OptimizeBounds)))) 
      stop(callerName,'there are duplicated names in "OptimizeBounds".') 
    
    for (idxI in 1:length(OptimizeBounds))
    {
      idxN=names(OptimizeBounds)[idxI]
      
      if (! idxN %in% model_fullComponentList)
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
        OptimizeBounds[[idxN]]$TSRANGE=TSRANGE 
        
      } else
      { 
        #check TSRANGE
        tryCatch({
          tmpTSRANGE=OptimizeBounds[[idxN]]$TSRANGE
          if (! ( is.numeric(tmpTSRANGE) && length(tmpTSRANGE)==4 && 
                  .isCompliantYP(c(tmpTSRANGE[1],tmpTSRANGE[2]),frequency) && 
                  .isCompliantYP(c(tmpTSRANGE[3],tmpTSRANGE[4]),frequency) &&
                  NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                            c(tmpTSRANGE[3],tmpTSRANGE[4]),frequency)>=0
          )
          ) stop() 
        },error=function(e) {stop(callerName,'syntax error in "OptimizeBounds$',idxN,'$TSRANGE". ',e$message)
        }) 
      }
    }
    
    idxToBeRemoved=c() 
    for (idxN in 1:length(OptimizeBounds))
    {
      elemName=names(OptimizeBounds)[idxN] 
      tmpTSRANGE=OptimizeBounds[[idxN]]$TSRANGE
      
      #check intersection between OptimizeBounds TSRANGE and Simulation TSRANGE
      #remove items whose TSRANGE is outside Simulation TSRANGE
      if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                    c(TSRANGE[1],TSRANGE[2]),frequency)>0)
      {
        if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxN) 
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeBounds$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n')) 
        } 
      }
      
      if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                    c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
      {
        if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxN) 
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeBounds$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n')) 
        }
      }
    }
    
    if (length(idxToBeRemoved)>0) OptimizeBounds=OptimizeBounds[-idxToBeRemoved]
    
    if (length(OptimizeBounds)>0) 
    {  
      for (idxN in 1:length(OptimizeBounds))
      {
        tmpTSRANGE=OptimizeBounds[[idxN]]$TSRANGE
        
        if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeBounds$',names(OptimizeBounds)[idxN],'$TSRANGE" starts before the simulation TSRANGE.\n')) 
          tmpTSRANGE[1]=TSRANGE[1] 
          tmpTSRANGE[2]=TSRANGE[2] 
        }
        
        if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeBounds$',names(OptimizeBounds)[idxN],'$TSRANGE" ends after the simulation TSRANGE.\n')) 
          tmpTSRANGE[3]=TSRANGE[3] 
          tmpTSRANGE[4]=TSRANGE[4] 
        }
        
        OptimizeBounds[[idxN]]$TSRANGE=tmpTSRANGE
      }
      
      for (idxI in 1:length(OptimizeBounds))
      {
        idxN=names(OptimizeBounds)[idxI]
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
      if (!is.list(OptimizeRestrictions) || is.null(names(OptimizeRestrictions))) stop(callerName,'"OptimizeRestrictions" must be a named list.') 
      if (length(names(OptimizeRestrictions)) != length(unique(names(OptimizeRestrictions)))) stop(callerName,'there are duplicated names in "OptimizeRestrictions".') 
      
      for (idxI in 1:length(OptimizeRestrictions))
      {
        idxN=names(OptimizeRestrictions)[idxI]
        
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
          OptimizeRestrictions[[idxN]]$TSRANGE=TSRANGE 
        } else
        { 
          #check TSRANGE
          tryCatch({
            tmpTSRANGE=OptimizeRestrictions[[idxN]]$TSRANGE
            if (! ( is.numeric(tmpTSRANGE) && length(tmpTSRANGE)==4 && 
                    .isCompliantYP(c(tmpTSRANGE[1],tmpTSRANGE[2]),frequency) && 
                    .isCompliantYP(c(tmpTSRANGE[3],tmpTSRANGE[4]),frequency) &&
                    NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                              c(tmpTSRANGE[3],tmpTSRANGE[4]),frequency)>=0
            )
            ) stop() 
          },error=function(e) {stop(callerName,'syntax error in "OptimizeRestrictions$',idxN,'$TSRANGE". ',e$message)
          }) 
        }
      }
      
      idxToBeRemoved=c() 
      for (idxN in 1:length(OptimizeRestrictions))
      {
        elemName=names(OptimizeRestrictions)[idxN] 
        tmpTSRANGE=OptimizeRestrictions[[idxN]]$TSRANGE
        
        #check intersection between OptimizeRestrictions TSRANGE and Simulation TSRANGE
        #remove items whose TSRANGE is outside Simulation TSRANGE
        if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                        c(TSRANGE[1],TSRANGE[2]),frequency)>0)
          {
            idxToBeRemoved=c(idxToBeRemoved,idxN) 
            .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeRestrictions$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n')) 
          } 
        }
        
        if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
        {
          if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                        c(TSRANGE[3],TSRANGE[4]),frequency)<0)
          {
            idxToBeRemoved=c(idxToBeRemoved,idxN) 
            .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeRestrictions$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n')) 
          }
        }
      }
      
      if (length(idxToBeRemoved)>0) OptimizeRestrictions=OptimizeRestrictions[-idxToBeRemoved]
      
      if (length(OptimizeRestrictions)>0) 
      {  
        for (idxN in 1:length(OptimizeRestrictions))
        {
          tmpTSRANGE=OptimizeRestrictions[[idxN]]$TSRANGE
          
          if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                        c(TSRANGE[1],TSRANGE[2]),frequency)>0)
          {
            .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeRestrictions$',names(OptimizeRestrictions)[idxN],'$TSRANGE" starts before the simulation TSRANGE.\n')) 
            tmpTSRANGE[1]=TSRANGE[1] 
            tmpTSRANGE[2]=TSRANGE[2] 
          }
          
          if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                        c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
          {
            .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeRestrictions$',names(OptimizeRestrictions)[idxN],'$TSRANGE" ends after the simulation TSRANGE.\n')) 
            tmpTSRANGE[3]=TSRANGE[3] 
            tmpTSRANGE[4]=TSRANGE[4] 
          }
          
          OptimizeRestrictions[[idxN]]$TSRANGE=tmpTSRANGE
        }
        
        #check there's no overlapping in Restrictions TSRANGEs
        if (length(OptimizeRestrictions)>1)
        {
          for (idxN in 1:(length(OptimizeRestrictions)-1))
          {
            for (idxM in (idxN+1):length(OptimizeRestrictions))
            {
              tmpTSRANGE=OptimizeRestrictions[[idxN]]$TSRANGE
              tmp2TSRANGE=OptimizeRestrictions[[idxM]]$TSRANGE
              
              if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                            c(tmp2TSRANGE[1],tmp2TSRANGE[2])
                            ,frequency)<1
                  &&
                  NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                            c(tmp2TSRANGE[3],tmp2TSRANGE[4])
                            ,frequency)>-1)
                stop(callerName,'"OptimizeRestrictions$',names(OptimizeRestrictions)[idxN],'$TSRANGE" overlaps with "OptimizeRestrictions$',names(OptimizeRestrictions)[idxM],'$TSRANGE".')
              
              if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                            c(tmp2TSRANGE[1],tmp2TSRANGE[2])
                            ,frequency)<1
                  &&
                  NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                            c(tmp2TSRANGE[3],tmp2TSRANGE[4])
                            ,frequency)>-1)
                stop(callerName,'"OptimizeRestrictions$',names(OptimizeRestrictions)[idxN],'$TSRANGE" overlaps with "OptimizeRestrictions$',names(OptimizeRestrictions)[idxM],'$TSRANGE".')
            }
          }
        }
        
        for (idxI in 1:length(OptimizeRestrictions))
        {
          idxN=names(OptimizeRestrictions)[idxI]
          
          if (! .checkExpression(OptimizeRestrictions[[idxN]]$INEQUALITY))
            stop(callerName,'syntax error in OptimizeRestrictions$',idxN,'$INEQUALITY".')
          
          #transform inequality raw text into suitable expression
          outIE=OptimizeRestrictions[[idxN]]$INEQUALITY
          
          #check double logical operator
          if (grepl('&&',outIE)) stop(callerName,'INEQUALITY definition "',outIE,'" in "OptimizeRestrictions$',idxN,'" cannot contain "&&". Please consider using the single operator "&".') 
          if (grepl('\\|\\|',outIE)) stop(callerName,'INEQUALITY definition "',outIE,'" in "OptimizeRestrictions$',idxN,'" cannot contain "||". Please consider using the single operator "|".') 
          
          #check unknown funs names
          unknownFuns=.unknownFunsNames(outIE,reservedKeyw)
          
          if (length(unknownFuns)>0) stop(callerName,'unsupported functions in "OptimizeRestrictions$',idxN,'": ',
                                          paste0(paste0(unknownFuns,'()'),collapse=', ')) 
          
          #extract components names from restriction
          namesOnRestr=strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnIfCleaner,' ',outIE,ignore.case=TRUE)),'\\s+')[[1]] 
          
          #remove numbers
          rmvNumOnRSIdx=c() 
          for (idxNamesOnR in 1:length(namesOnRestr)) {
            if (length(grep(strongCharOnNumbs,namesOnRestr[idxNamesOnR]))>0) 
            {
              rmvNumOnRSIdx=c(rmvNumOnRSIdx,idxNamesOnR) 
            }
          }
          
          if (length(rmvNumOnRSIdx)>0) namesOnRestr=namesOnRestr[-rmvNumOnRSIdx] 
          
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
          
          outIE=gsub('<-','< -',outIE) 
          if (! grepl('(==|<|>|>=|<=|!=|&|\\|)',outIE))
            stop(callerName,'syntax error in INEQUALITY definition "',outIE,'" in "OptimizeRestrictions$',idxN,'". No logical operators found.')
          
          outIE=.MODEL_MOD_FUNC_NAMES(outIE)
          outIE=.appendAFtoEndVars(outIE,model_vendog)
          outIE=.appendIndexToVars(outIE,c(model_vexog,
                                           paste0(model_vendog,'__ADDFACTOR'))
                                   ,'0') 
          #explode model funs
          tempS=.explodeTSLAG(outIE)
          tempS=.explodeTSLEAD(tempS$output)
          tempS=.explodeTSDELTA(tempS$output)
          tempS=.explodeTSDELTAP(tempS$output)
          tempS=.explodeTSDELTALOG(tempS$output)
          tempS=.explodeMTOT(tempS$output)
          tempS=.explodeMAVE(tempS$output)
          
          outIE=tempS$output
          
          #stop if the inequality requires lagging time series more than model equations
          tempOut=.getLowerLagAndHigherLead(outIE)
          if (-tempOut$outL>model_max_lag) 
            stop(callerName,'"OptimizeRestrictions$',idxN,'$INEQUALITY" expression "',OptimizeRestrictions[[idxN]]$INEQUALITY,'" requires to lag time series more than model equations do.')
          if (tempOut$outH>model_max_lead) 
            stop(callerName,'"OptimizeRestrictions$',idxN,'$INEQUALITY" expression "',OptimizeRestrictions[[idxN]]$INEQUALITY,'" requires to lead time series more than model equations do.')
          
          #add model lag to inequality expression
          outIE=.addToIndexInString(outIE,  model_max_lag_1)
          
          #append expression to list
          tryCatch({
            OptimizeRestrictions[[idxN]]$inExpr=parse(text=outIE)
          },error=function(err) stop(callerName,'syntax error in INEQUALITY definition "',OptimizeRestrictions[[idxN]]$INEQUALITY,'" in "OptimizeRestrictions$',idxN,'".')
          )
        }
        
      } else {
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeRestrictions" are null in the simulation TSRANGE.\n')) 
      }
    }
    
    # end check OptimizeRestrictions
    
    #check argument OptimizeFunctions
    if (!is.list(OptimizeFunctions) || is.null(names(OptimizeFunctions))) stop(callerName,'"OptimizeFunctions" must be a named list.') 
    if (length(names(OptimizeFunctions)) != length(unique(names(OptimizeFunctions)))) stop(callerName,'there are duplicated names in "OptimizeFunctions".') 
    
    for (idxI in 1:length(OptimizeFunctions))
    {
      idxN=names(OptimizeFunctions)[idxI]
      
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
        OptimizeFunctions[[idxN]]$TSRANGE=TSRANGE 
      } else
      { 
        #check TSRANGE
        tryCatch({
          tmpTSRANGE=OptimizeFunctions[[idxN]]$TSRANGE
          
          if (! ( is.numeric(tmpTSRANGE) && length(tmpTSRANGE)==4 && 
                  .isCompliantYP(c(tmpTSRANGE[1],tmpTSRANGE[2]),frequency) && 
                  .isCompliantYP(c(tmpTSRANGE[3],tmpTSRANGE[4]),frequency) &&
                  NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                            c(tmpTSRANGE[3],tmpTSRANGE[4]),frequency)>=0)) stop() 
        },error=function(e) {stop(callerName,'syntax error in "OptimizeFunctions$',idxN,'$TSRANGE". ',e$message)
        }) 
      }
    }
    
    idxToBeRemoved=c() 
    for (idxN in 1:length(OptimizeFunctions))
    {
      elemName=names(OptimizeFunctions)[idxN] 
      tmpTSRANGE=OptimizeFunctions[[idxN]]$TSRANGE
      
      #check intersection between OptimizeFunctions TSRANGE and Simulation TSRANGE
      #remove items whose TSRANGE is outside Simulation TSRANGE
      if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                    c(TSRANGE[1],TSRANGE[2]),frequency)>0)
      {
        if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxN) 
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeFunctions$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n')) 
        } 
      }
      
      if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                    c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
      {
        if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0)
        {
          idxToBeRemoved=c(idxToBeRemoved,idxN) 
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeFunctions$',elemName,'$TSRANGE" does not intersect with simulation TSRANGE.\n')) 
        }
      }
    }
    
    if (length(idxToBeRemoved)>0) OptimizeFunctions=OptimizeFunctions[-idxToBeRemoved]
    
    if (length(OptimizeFunctions)>0) 
    {  
      for (idxN in 1:length(OptimizeFunctions))
      {
        tmpTSRANGE=OptimizeFunctions[[idxN]]$TSRANGE
        
        if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                      c(TSRANGE[1],TSRANGE[2]),frequency)>0)
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeFunctions$',names(OptimizeFunctions)[idxN],'$TSRANGE" starts before the simulation TSRANGE.\n')) 
          tmpTSRANGE[1]=TSRANGE[1] 
          tmpTSRANGE[2]=TSRANGE[2] 
        }
        
        if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                      c(TSRANGE[3],TSRANGE[4]),frequency)<0) 
        {
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "OptimizeFunctions$',names(OptimizeFunctions)[idxN],'$TSRANGE" ends after the simulation TSRANGE.\n')) 
          tmpTSRANGE[3]=TSRANGE[3] 
          tmpTSRANGE[4]=TSRANGE[4] 
        }
        
        OptimizeFunctions[[idxN]]$TSRANGE=tmpTSRANGE
      }
      
      #check there's no overlapping in Restrictions TSRANGEs
      if (length(OptimizeFunctions)>1)
      {
        for (idxN in 1:(length(OptimizeFunctions)-1))
        {
          for (idxM in (idxN+1):length(OptimizeFunctions))
          {
            tmpTSRANGE=OptimizeFunctions[[idxN]]$TSRANGE
            tmp2TSRANGE=OptimizeFunctions[[idxM]]$TSRANGE
            
            if (NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                          c(tmp2TSRANGE[1],tmp2TSRANGE[2])
                          ,frequency)<1
                &&
                NUMPERIOD(c(tmpTSRANGE[3],tmpTSRANGE[4]),
                          c(tmp2TSRANGE[3],tmp2TSRANGE[4])
                          ,frequency)>-1)
              stop(callerName,'"OptimizeFunctions$',names(OptimizeFunctions)[idxN],'$TSRANGE" overlaps with "OptimizeFunctions$',names(OptimizeFunctions)[idxM],'$TSRANGE".')
            
            if (NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                          c(tmp2TSRANGE[1],tmp2TSRANGE[2])
                          ,frequency)<1
                &&
                NUMPERIOD(c(tmpTSRANGE[1],tmpTSRANGE[2]),
                          c(tmp2TSRANGE[3],tmp2TSRANGE[4])
                          ,frequency)>-1)
              stop(callerName,'"OptimizeFunctions$',names(OptimizeFunctions)[idxN],'$TSRANGE" overlaps with "OptimizeFunctions$',names(OptimizeFunctions)[idxM],'$TSRANGE".')
          }
        }
      }
      
      for (idxI in 1:length(OptimizeFunctions))
      {
        idxN=names(OptimizeFunctions)[idxI]
        
        if (! .checkExpression(OptimizeFunctions[[idxN]]$FUNCTION))
          stop(callerName,'syntax error in "OptimizeFunctions$',idxN,'$FUNCTION".')
        
        #transform fun raw text into suitable expression
        outFUN=OptimizeFunctions[[idxN]]$FUNCTION
        
        #check unknown funs names
        unknownFuns=.unknownFunsNames(outFUN,reservedKeyw)
        if (length(unknownFuns)>0) stop(callerName,'unsupported functions in "OptimizeFunctions$',idxN,'": ',
                                        paste0(paste0(unknownFuns,'()'),collapse=', ')) 
        
        #extract components names from restriction
        namesOnFun=strsplit(gsub("^\\s+|\\s+$", "",gsub(symOnEqCleaner,' ',outFUN,ignore.case=TRUE)),'\\s+')[[1]] 
        
        #remove numbers
        rmvNumOnFunIdx=c() 
        for (idxNamesOnF in 1:length(namesOnFun)) {
          if (length(grep(strongCharOnNumbs,namesOnFun[idxNamesOnF]))>0) 
          {
            rmvNumOnFunIdx=c(rmvNumOnFunIdx,idxNamesOnF) 
          }
        }
        if (length(rmvNumOnFunIdx)>0) namesOnFun=namesOnFun[-rmvNumOnFunIdx] 
        
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
        outFUN=.appendIndexToVars(outFUN,c(model_vexog,
                                           model_vendog)
                                  ,'0') 
        #explode model funs
        tempS=.explodeTSLAG(outFUN)
        tempS=.explodeTSLEAD(tempS$output)
        tempS=.explodeTSDELTA(tempS$output)
        tempS=.explodeTSDELTAP(tempS$output)
        tempS=.explodeTSDELTALOG(tempS$output)
        tempS=.explodeMTOT(tempS$output)
        tempS=.explodeMAVE(tempS$output)
        
        outFUN=tempS$output
        
        #stop if the function requires lagging time series more than model equations
        tempOut=.getLowerLagAndHigherLead(outFUN)
       
        if (-tempOut$outL>model_max_lag) 
          stop(callerName,'"OptimizeFunctions$',idxN,'$FUNCTION" expression "',OptimizeFunctions[[idxN]]$FUNCTION,'" requires to lag time series more than model equations do.')
        if (tempOut$outH>model_max_lead) 
          stop(callerName,'"OptimizeFunctions$',idxN,'$FUNCTION" expression "',OptimizeFunctions[[idxN]]$FUNCTION,'" requires to lead time series more than model equations do.')
        
        #add model lag to function expression 
        outFUN=.addToIndexInString(outFUN,  model_max_lag_1)
        
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
  
  hasLeads=FALSE
  if (model_max_lead>0 || forceForwardLooking) 
  { 
    hasLeads=TRUE
  } 
  
  if (hasLeads && (! (simType %in% (c('RESCHECK','DYNAMIC'))))) stop(callerName,'in forward looking models "simType" must be "DYNAMIC", or "RESCHECK"')
  if (hasLeads && verboseSincePeriod > 0) stop(callerName,'"verboseSincePeriod" must be zero in forward looking models.')   
  
  # extended tsrange --------------------------------------
  
  #TSRANGE is the simulation range
  #EXTENDED_TSRANGE is TSRANGE extended back by model_max_lag periods 
  #and is used in simulation
  
  #extend TSRANGE by max lag required by model
  newStart=normalizeYP(c(TSRANGE[1],TSRANGE[2]-model_max_lag),frequency) 
  EXTENDED_TSRANGE=TSRANGE 
  EXTENDED_TSRANGE[1]=newStart[1] 
  EXTENDED_TSRANGE[2]=newStart[2] 
  extSimSteps=NUMPERIOD(EXTENDED_TSRANGE[1:2],EXTENDED_TSRANGE[3:4],frequency)+1 
  
  # check coefficients exist -----------------------------------------
  
  #create sublists of requested RESCHECK
  RESCHECK_behaviorals=NULL 
  RESCHECK_identities=NULL 
  if (!is.null(RESCHECKeqList)) 
  {
    RESCHECK_behaviorals=base::intersect(RESCHECKeqList,names(model$behaviorals)) 
    RESCHECK_identities =base::intersect(RESCHECKeqList,names(model$identities)) 
    #empty intersect returns character(0) but we need a NULL
    if (length(RESCHECK_behaviorals)==0) RESCHECK_behaviorals=NULL 
    if (length(RESCHECK_identities)==0)  RESCHECK_identities=NULL 
  }
  
  #check behavioral coefficients
  if (length(model$behaviorals)>0) 
  {  if (is.null(RESCHECKeqList)) 
  { 
    #user requested a full simulation
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
   
  #user requested a selection of endogenous rescheck
  if (!is.null(RESCHECKeqList))
  {
    model_fullComponentList=c()
    
    if (length(RESCHECK_behaviorals)>0)
      for (idx in 1:length(RESCHECK_behaviorals))
      {
        #add local components to list of variables to be proxied
        model_fullComponentList=c(model_fullComponentList,
                                  model$behaviorals[[RESCHECK_behaviorals[idx]]]$eqComponentsNames)
      }
    
    if (length(RESCHECK_identities)>0)
      for (idx in 1:length(RESCHECK_identities))
      {
        #add local components to list of variables to be proxied
        model_fullComponentList=c(model_fullComponentList,model$identities[[RESCHECK_identities[idx]]]$eqComponentsNames)
      }
    
    model_vendog=base::intersect(model$vendog,model_fullComponentList) 
    model_vexog=base::intersect(model$vexog,model_fullComponentList) 
    model_names_behavioral=base::intersect(names(model$behaviorals),RESCHECK_behaviorals) 
    model_names_identity=base::intersect(names(model$identities),RESCHECK_identities) 
    if (! is.null(verboseVars))
    {
      verboseVars=base::intersect(verboseVars,model_fullComponentList) 
      if (length(verboseVars)==0) 
      {
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, "verboseVars" is not coherent and will be set equal to "RESCHECKeqList".\n')) 
      }
    }
  }
  
  #forecast does not propagate vendog with IF condition... 
  #model_vendog_no_if contains all vendog but those with IF>
  #model_vendog_exogenized == vendog - model_vendog_no_if
  model_vendog_no_if=c() 
  for (idx in 1:length(model_vendog))
  {
    if (model_vendog[idx] %in% model_names_identity)
    {
      if (model$identities[[model_vendog[idx]]]$hasIF==FALSE)
      {
        #no IF so add...
        model_vendog_no_if=c(model_vendog_no_if,model_vendog[idx]) 
      }
    }
    else
    {
      #we are in behaviorals so no IF allowed
      model_vendog_no_if=c(model_vendog_no_if,model_vendog[idx]) 
    }
  }
  
  #get exogenized vendog with IF condition (vendog_forecast UNION vendog_exogenized == vendog)
  model_vendog_exogenized=base::setdiff(model_vendog,model_vendog_no_if) 
  
  if (length(ConstantAdjustment) >0) 
    for (idxI in 1:length(ConstantAdjustment))
    {
      idxCA=names(ConstantAdjustment)[idxI]
      tryCatch(
        {
          projectTS=TSPROJECT(ConstantAdjustment[[idxCA]],TSRANGE=EXTENDED_TSRANGE)
          outTSL=TSLOOK(projectTS)
          
          .MODEL_outputText(outputText = !quietly,paste0(callerName,'endogenous variable "',
                                                        idxCA,
                                                        '" has a constant adjustment from year-period ',
                                                        paste0(c(outTSL$STARTY,outTSL$STARTP),collapse='-'),
                                                        ' to ',
                                                        paste0(c(outTSL$ENDY,outTSL$ENDP),collapse='-'),'.\n')) 
        },error=function(e){
          
          .MODEL_outputText(outputText = !quietly,
                            paste0(callerName,'warning, constant adjustment "',idxCA,'" does not intersect with simulation TSRANGE.\n')) 
        }
      ) 
    }
  
  if (length(Exogenize)>0) 
    if (MULTMATRIX && length(base::intersect(names(Exogenize),TARGET))>0)
    {
      .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, the following TARGET have been exogenized, therefore related multiplier will be zero: ',paste(base::intersect(names(Exogenize),TARGET),collapse =', '),'.\n')) 
    }
  
  # deal with leads ----------------------------------
   
  if (hasLeads) {
    
    #SUPER_EXTENDED_TSRANGE is EXTENDED_TSRANGE extended forward by model_max_lead periods
    #and is used in matrices dataset building
    SUPER_EXTENDED_TSRANGE=EXTENDED_TSRANGE
    SUPER_EXTENDED_TSRANGE[3:4]=normalizeYP(c(EXTENDED_TSRANGE[3],EXTENDED_TSRANGE[4]+model_max_lead),frequency)
    superExtSimSteps=NUMPERIOD(SUPER_EXTENDED_TSRANGE[1:2],SUPER_EXTENDED_TSRANGE[3:4],frequency)+1 
    
    #in case of leads, simulation is performed only on first TSRANGE period
    #but simultaneously for all endogenous variable leaded in the whole TSRANGE)
    EXTENDED_TSRANGE[3]=TSRANGE[1] 
    EXTENDED_TSRANGE[4]=TSRANGE[2] 
    extSimSteps=NUMPERIOD(EXTENDED_TSRANGE[1:2],EXTENDED_TSRANGE[3:4],frequency)+1 
    
  }
  
  # create proxies -----------------------------------
   
  #create local proxies (i.e. model variables stored in matrices)
  #for all references (i.e. component names used in eq) in behaviorals/identities
  #and stores each var as "name"__ORIGINAL__EXTENDED 
  
  #deal also with forceForwardLooking, so we need model_max_lead>0
  #all components must be project into simSteps+model_max_lead
  if (hasLeads && (model_max_lead>0 || simSteps>1)) { 
    
     tryCatch(
     {
      for (idxI in 1:length(model_fullComponentList))
      {
        tempName=model_fullComponentList[idxI]
        
        #project ts in ext TSRANGE
        tsValues=TSPROJECT(model$modelData[[tempName]],
                           TSRANGE=SUPER_EXTENDED_TSRANGE,
                           ARRAY=TRUE,
                           EXTEND=TRUE) 
        #build matrices (in case of parallel sims)
        tempValues=matrix(tsValues,ncol=replica,nrow=superExtSimSteps) 
        
        #create proxy for full original time series (on EXTENDED TSRANGE)
        #__ORIGINAL__EXTENDED series wont be modified during sim 
        localE[[paste0(tempName,'__ORIGINAL__EXTENDED')]]=tempValues[1:extSimSteps,,drop=F]
        
        for (idxSS in 1:(model_max_lead+simSteps-1)) 
        {
          localE[[paste0(tempName,'__LEAD__',idxSS)]]=tempValues[idxSS+(1:extSimSteps),,drop=F]
        }
      }
        
      },error=function(e){
        stop(callerName,'model data must contain time series "',tempName,'".') 
      }) 
     } else 
     {
       #no leads
       tryCatch(
         {
           for ( idxI in 1:length(model_fullComponentList))
           {
             tempName=model_fullComponentList[idxI]
             
             #project ts in ext TSRANGE
             tsValues=TSPROJECT(model$modelData[[tempName]],
                                TSRANGE=EXTENDED_TSRANGE,
                                ARRAY=TRUE,
                                EXTEND=TRUE) 
             #build matrices (in case of parallel sims)
             tempValues=matrix(tsValues,ncol=replica,nrow=extSimSteps) 
             
             #create proxy for full original time series (on EXTENDED_TSRANGE)
             #__ORIGINAL__EXTENDED series wont be modified during sim
             localE[[paste0(tempName,'__ORIGINAL__EXTENDED')]]=tempValues
             
           }
           
         },error=function(e){
           stop(callerName,'model data must contain time series "',tempName,'".') 
         })
     }
  
    #we need ADDFACTOR only in simulation TSRANGE
    tryCatch(
      {
        #add factor all set to 0
        tsValues=rep(0,extSimSteps) 
        tempValues=matrix(tsValues,ncol=replica,nrow=extSimSteps) 
        
        tempS=paste0(model_vendog,'__ADDFACTOR')
        for (idxI in 1:length(tempS))
        {
          tempName=tempS[idxI]
          
          #create proxy for full original time series (on EXTENDED_TSRANGE)
          localE[[paste0(tempName,'__ORIGINAL__EXTENDED')]]=tempValues
          
          #this is needed if tmpName time series is not in CA, INSTRUMENT or StochStructure
          localE[[paste0(tempName)]]=tempValues
          
          if (hasLeads && simSteps>1) {
            
            #we need ADDFACTOR only in simulation TSRANGE
            for (idxSS in 1:(simSteps-1)) 
            {
              #create proxy for original time series (on EXTENDED_TSRANGE)
              localE[[paste0(tempName,'__LEAD__',idxSS)]]=tempValues
            } 
          }
        } 
        
      },error=function(e){
        stop(callerName,'cannot initialize the constant adjustment of "',tempName,'".') 
      }) 
  
    #we need ADDFACTOR only in simulation TSRANGE
    if (hasLeads && simSteps>1) {
 
      tryCatch(
      {
        #... create proxies for explicit Constant Adjustment
        if (length(ConstantAdjustment)>0) 
          for (idxAF in 1:length(ConstantAdjustment))
          {
            tempName=paste0(names(ConstantAdjustment)[idxAF],'__ADDFACTOR') 
            
            #assign from input list to local environment
            #project to ext TSRANGE
            #we use SUPER_EXTENDED_TSRANGE because original EXTENDED_TSRANGE has been modified
            #original EXTENDED_TSRANGE would have been be sufficient
            tsValues=TSPROJECT(ConstantAdjustment[[idxAF]],
                               TSRANGE=SUPER_EXTENDED_TSRANGE,
                               ARRAY=TRUE,
                               EXTEND=TRUE) 
            
            #replace NA with 0 (EXTEND in TSPROJECT has just inserted NA)
            tsValues[which(is.na(tsValues))]=0 
            tempValues=matrix(tsValues,ncol=replica,nrow=superExtSimSteps) 
            
            #create proxy for full original time series (on SUPER_EXTENDED_TSRANGE) 
            localE[[paste0(tempName,'__ORIGINAL__EXTENDED')]]=tempValues[1:extSimSteps,,drop=F]
            
            #we need ADDFACTOR only in simulation TSRANGE
            for (idxSS in 1:(simSteps-1)) 
            {
              #create proxy for original time series (on EXTENDED_TSRANGE)
              localE[[paste0(tempName,'__LEAD__',idxSS)]]=tempValues[idxSS+(1:extSimSteps),,drop=F]
            }
          } 
        
      },error=function(e){
        stop(callerName,'unknown error in "ConstantAdjustment" projection of time series "',names(ConstantAdjustment)[idxAF],'".') 
      }) 

    } else 
    { #no (lead && simSteps>1)
      tryCatch(
        {
          #... create proxies for explicit Constant Adjustment
          if (length(ConstantAdjustment)>0) 
            for (idxAF in 1:length(ConstantAdjustment))
            {
              tempName=paste0(names(ConstantAdjustment)[idxAF],'__ADDFACTOR') 
              #assign from input list to local environment
              #project to ext TSRANGE
              tsValues=TSPROJECT(ConstantAdjustment[[idxAF]],
                                 TSRANGE=EXTENDED_TSRANGE,
                                 ARRAY=TRUE,
                                 EXTEND=TRUE) 
              #replace NA with 0 (EXTEND in TSPROJECT has just inserted NA)
              tsValues[which(is.na(tsValues))]=0 
              tempValues=matrix(tsValues,ncol=replica,nrow=extSimSteps) 
              
              #create proxy for full original time series (on EXTENDED_TSRANGE)
              localE[[paste0(tempName,'__ORIGINAL__EXTENDED')]]=tempValues
            } 
          
        },error=function(e){
          stop(callerName,'unknown error in "ConstantAdjustment" projection of time series "',names(ConstantAdjustment)[idxAF],'".') 
        }) 
    }
  
  #if multipliers requested then modify INSTRUMENTS columns with SHOCKs...
  if (MULTMATRIX) 
  { 
    tmpI=0 
    for (idx in 1:length(INSTRUMENT))
    { 
      idxI=INSTRUMENT[idx]
      
      #we need INSTRUMENT column index in matrix MM
      tmpI=tmpI+1 
      
      #get instrument
      tmpM=localE[[paste0(idxI, '__ORIGINAL__EXTENDED')]]
      
      #we need shocked INSTRUMENT only in simulation TSRANGE
      for (idxR in 1:simSteps)
      {
        #shock related column
        #get base value
        tmpV=tmpM[model_max_lag+idxR,1] 
        #choice delta
        if (tmpV<1) tmpD=MM_SHOCK
        else tmpD=tmpV*MM_SHOCK
        
        #apply shock to related column
        #each matrix has simsteps x length(INSTRUMENT) columns count
        #as well as replica==simsteps x length(INSTRUMENT)+1
        tmpM[model_max_lag+idxR,1+(tmpI-1)*simSteps+idxR]=tmpV+tmpD 
        #save back
        localE[[paste0(idxI, '__ORIGINAL__EXTENDED')]]=tmpM
        
        #deal with lead case
        if (hasLeads) break
      }
      
      if (hasLeads && simSteps>1)
        for (idxR in 1:(simSteps-1))
        {
          #we need shocked INSTRUMENT only in simulation TSRANGE
          
          #get leaded instrument
          tmpM=localE[[paste0(idxI,'__LEAD__',idxR)]]
          
          #shock related column
          #get base value
          tmpV=tmpM[model_max_lag_1,1] 
          
          #choice delta
          if (tmpV<1) tmpD=MM_SHOCK
          else tmpD=tmpV*MM_SHOCK
          
          #apply shock to related column
          #each matrix has simsteps x length(INSTRUMENT) columns count
          #as well as replica==simsteps x length(INSTRUMENT)+1
          tmpM[model_max_lag_1,1+(tmpI-1)*simSteps+(idxR+1)]=tmpV+tmpD 
          
          #save back
          localE[[paste0(idxI,'__LEAD__',idxR)]]=tmpM
        }
    }#end INSTRUMENT 
  }
  
  # check missings and lengths --------------------------------
  
  #trailingTSRANGE  = EXTENDED_TSRANGE - TSRANGE
  trailingTSRANGE=EXTENDED_TSRANGE 
  trailingTSRANGE[3:4]=normalizeYP(c(TSRANGE[1],TSRANGE[2]-1),f=frequency) 
  trailingSteps=extSimSteps-simSteps 
  
  #if leads fix trailingSteps
  if (hasLeads) trailingSteps=extSimSteps-1
  
  #missing check in first max_lag obs: NA allowed so in case print a warning
  
  #FOLLOWING CODE IS OK ALSO WITH LEADS: 
  #we dont need to check missing of leaded vars 
  #in TSRANGE+(1:model_max_lag) because same missings will issue an error 
  #in check of related non-leaded var in TSRANGE
  
  if (length(model_fullComponentList)>0) 
    for (idxI in 1:length(model_fullComponentList))    
    {
      idxName=model_fullComponentList[idxI]
      
      tmpProxy=localE[[paste0(idxName,'__ORIGINAL__EXTENDED')]]
      
      #verify there are no missing values in first max_lag values
      localMissingIndexes=which(! is.finite(tmpProxy[(1:model_max_lag),])) 
      
      #index in GETDATE accounts for replicas
      if (length(localMissingIndexes)>0 )
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, ',
                                                       ifelse(idxName %in% model_vendog,'endogenous','exogenous'),' variable "',idxName,'" is not fully defined in extended TSRANGE. There are undefined values in year-period ',
                                                       paste0(date2yp(as.Date(
                                                         GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=trailingTSRANGE,EXTEND=TRUE),
                                                                 (localMissingIndexes[length(localMissingIndexes)]-1) %% trailingSteps+1)
                                                       ),f=frequency),collapse = '-'),'.\n')) 
    }
  
  if (length(ConstantAdjustment)>0) 
    for (idxI in 1:length(ConstantAdjustment))
    {
      idxCA=names(ConstantAdjustment)[idxI]
      
      tmpProxy=localE[[paste0(idxCA,'__ADDFACTOR__ORIGINAL__EXTENDED')]]
      
      #verify there are no missing values in first max_lag values
      localMissingIndexes=which(! is.finite(tmpProxy[(1:model_max_lag),])) 
      if (length(localMissingIndexes)>0 )
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, constant adjustment of endogenous variable "',idxCA,'" is not fully defined in extended TSRANGE. There are undefined values in year-period ',
                                                       paste0(date2yp(as.Date(
                                                         GETDATE(TSPROJECT(ConstantAdjustment[[idxCA]],TSRANGE=trailingTSRANGE,EXTEND=TRUE),
                                                                 (localMissingIndexes[length(localMissingIndexes)]-1) %% trailingSteps+1)
                                                       ),f=frequency),collapse = '-'),'.\n')) 
      }
  
  #check missing and length in vexog time series in TSRANGE: if NA then error
  if (length(model_vexog)>0) 
    for (idxI in 1:length(model_vexog))    
    {
      idxName = (model_vexog)[idxI]
      
      tmpProxy=localE[[paste0(idxName,'__ORIGINAL__EXTENDED')]]
     
      #verify there are no missing values
      #we cut first max_lag values that could be unused
      localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),])) 
      #index in GETDATE accounts for replicas
      if (length(localMissingIndexes)>0 )
      {
        if (hasLeads)
        {
          outDate=TSRANGE[1:2]
        } else { 
          outDate=date2yp(as.Date(
            GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=TSRANGE,EXTEND=TRUE),
                    (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1)
          ),f=frequency)
        }
        
        stop(callerName,'exogenous variable "',idxName,'" is not fully defined in TSRANGE. There are undefined values in year-period ',
             paste0(outDate,collapse = '-'),'\n')
      }
    }
  
  #deal also with forceForwardLooking
  #vexog can be leaded too so we need to check up to model_max_lead
  if (hasLeads && (simSteps>1 || model_max_lead>0))
  {
    #vexog in TSRANGE must be defined, after TSRANGE we issue a warning
    if (length(model_vexog)>0) 
      for (idxSS in 1:(model_max_lead+simSteps-1)) for (idxI in 1:length(model_vexog))    
      {
        idxName = (model_vexog)[idxI]
        
        tmpProxy=localE[[paste0(idxName,'__LEAD__',idxSS)]]
        
        #verify there are no missing values
        #we cut first max_lag values that could be unused
        #in replicas we could have not only just 1 but also up to 1:(replicas+1)
        localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),])) 
       
        #index in GETDATE accounts for replicas
        if (length(localMissingIndexes)>0 )
        {
          realYP=normalizeYP(c(TSRANGE[1],TSRANGE[2]+idxSS),frequency)
          
          if (idxSS <= simSteps-1)
          {
            #we are in TSRANGE so missing -> stop
            stop(callerName,'exogenous variable "',idxName,'" is not fully defined in TSRANGE. There are undefined values in year-period ',
               paste0(realYP,collapse = '-'),'\n')
          } else {
            .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, exogenous variable "',idxName,'" is not fully defined in extended TSRANGE. There are undefined values in year-period ',
                                                           paste0(realYP,collapse = '-'),'\n'))
          }
        }
      }
  }
  
  #check missing and length in endo exogenized time series (i.e. with IF) inside TSRANGE: if NA then error
  #lead case is included in model_vendog check (later in code)
  if (length(model_vendog_exogenized)>0  && hasLeads==FALSE) 
    for (idxI in 1:length(model_vendog_exogenized))
    {
      idxName=model_vendog_exogenized[idxI]
      
      tmpProxy=localE[[paste0(idxName,'__ORIGINAL__EXTENDED')]]
      
      #verify there are no missing values
      localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),])) 
      if (length(localMissingIndexes)>0  ) 
        stop(callerName,'exogenized variable "',idxName,'" is not fully defined in TSRANGE. There are undefined values in year-period ',
             paste0(date2yp(as.Date(
               GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=TSRANGE,EXTEND=TRUE),
                       (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1)
             ),f=frequency),collapse = '-'),'.\n')
    }
  
  #check missing and length in vendog time series inside TSRANGE: if NA then ...depends on simType
  for ( idxI in 1:length(model_vendog))
    { 
      idxName=model_vendog[idxI]
      
      tmpProxy=localE[[paste0(idxName, '__ORIGINAL__EXTENDED')]]
      
      #verify there are no missing values 
      localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),])) 
      
      #in DYNAMIC and FORECAST sim we can allow missings in trailing obs, but not in leads model
      if (length(localMissingIndexes)>0  ) 
      { 
        if ((simType=='DYNAMIC' || simType == 'FORECAST') && hasLeads==FALSE)
        {
          if (simType != 'FORECAST') .MODEL_outputText(outputText = !quietly,
                                                       paste0('\n',callerName,'warning, endogenous variable "',idxName,'" is not fully defined in TSRANGE. There are undefined values in year-period ',
                                                              paste0(date2yp(as.Date(
                                                                GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=TSRANGE,EXTEND=TRUE),
                                                                        (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1)
                                                              ),f=frequency),collapse = '-'),'.\n')) 
          if (simType=='DYNAMIC')
          {
            .MODEL_outputText(outputText = !quietly,paste0('Simulation will continue replicating last observation in time series "', idxName,'" over the full TSRANGE. Use the "FORECAST" option in "simType" argument to avoid this message.\n')) 
          }
          
          #should never happen
          if (all(! is.finite(tmpProxy))) 
            stop(callerName,'all endogenous time series "', idxName,'" values are undefined in the extended TSRANGE. Cannot initialize the simulation.')
          
          #here we propagate last known finite observation if any trailing missing
          localMissingIndexes=which(! is.finite(tmpProxy)) 
          
          numRows=dim(tmpProxy)[1]
          
          #replicate last known finite observation 
          if (length(localMissingIndexes)>0 && numRows>1)
            for (idxR in 2:numRows)
              if (any(! is.finite(tmpProxy[idxR,]))) tmpProxy[idxR,]=tmpProxy[idxR-1,]
          
          #update original and current time series
          localE[[paste0(idxName,'__ORIGINAL__EXTENDED')]]=tmpProxy
          
          localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),])) 
          
          #check all missings are gone (may be we have NA inside TSRANGE but latest obs are ok)
          if (length(localMissingIndexes)>0  ) 
            stop(paste0('\n',callerName,'endogenous variable "',idxName,'" is not fully defined in TSRANGE. There are undefined values in year-period ',
                        paste0(date2yp(as.Date(
                          GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=TSRANGE,EXTEND=TRUE),
                                  (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1)
                        ),f=frequency),collapse = '-'),'.\n')) 
        }
        else
        {
          #STATIC or RESCHECK or leads
          
          if (hasLeads)
          {
            #in leaded model TSRANGE[1:2] is the single and last simulation period
            outDate=TSRANGE[1:2]
          } else { 
            outDate=date2yp(as.Date(
              GETDATE(TSPROJECT(model$modelData[[idxName]],TSRANGE=TSRANGE,EXTEND=TRUE),
                      (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1)
            ),f=frequency)
          }
          
          #missings are not allowed on STATIC or RESCHECK or leads
          stop(callerName,'endogenous variable "',idxName,'" is not fully defined in TSRANGE. There are undefined values in year-period ',
               paste0(outDate,collapse = '-'),'.')
        }
      }
    }

  #deal also with forceForwardLooking
  #check leaded vendog
  if (hasLeads && (simSteps>1 || model_max_lead>0))
  {
    #if we have leads, vendog in TSRANGE must be defined.
    #we do not allow missings neither in DYNAMIC simulation 
    #(i.e. no replication of last known finite value allowed:
    #probably we need finite values also after TSRANGE
    #so has no meaning to allow missing values inside TSRANGE).
    #FORECAST simulation is not feasible with leads
    if (length(model_vendog)>0) 
      for (idxSS in 1:(model_max_lead+simSteps-1)) for (idxI in 1:length(model_vendog))
      { 
        idxName=model_vendog[idxI]
        
        tmpProxy=localE[[paste0(idxName,'__LEAD__',idxSS)]]
        
        #verify there are no missing values 
        #in replicas we could have not only just 1 but up to 1:(replicas+1)
        #note: all missings/NaNs are converted in zeros before in code 
        #when dealing with add-factor projection in TSRANGE
        #so probably this check is redundant
        localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),])) 
        
        if (length(localMissingIndexes)>0  ) 
        { 
          #missings are not allowed 
          realYP=normalizeYP(c(TSRANGE[1],TSRANGE[2]+idxSS),frequency)
          
          if (idxSS <= simSteps-1)
          {
            #we are in TSRANGE so missing -> stop
            stop(callerName,'endogenous variable "',idxName,'" is not fully defined in TSRANGE. There are undefined values in year-period ',
                 paste0(realYP,collapse = '-'),'\n')
          } else {
            .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, endogenous variable "',idxName,'" is not fully defined in extended TSRANGE. There are undefined values in year-period ',
                                                           paste0(realYP,collapse = '-'),'\n'))
          }
        }
      }
  }
  
  #check missing and length in CA time series inside TSRANGE: if NA then stop
  if (length(ConstantAdjustment)>0) 
    for ( idxI in 1:length(ConstantAdjustment))
    {
      idxCA=names(ConstantAdjustment)[idxI]
      
      tmpProxy=localE[[paste0(idxCA,'__ADDFACTOR__ORIGINAL__EXTENDED')]]
      
      #verify there are no missing values
      localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),])) 
      if (length(localMissingIndexes)>0 )
      {
        if (hasLeads)
        {
          #this is only the base case; leads are analyzed later
          outDate=TSRANGE[1:2]
        } else { 
          outDate=date2yp(as.Date(
            GETDATE(TSPROJECT(ConstantAdjustment[[idxCA]],TSRANGE=TSRANGE,EXTEND=TRUE),
                    (localMissingIndexes[length(localMissingIndexes)]-1) %% simSteps+1)
          ),f=frequency)
        }
        
        stop(callerName,'constant adjustment of endogenous variable "',idxCA,'" is not fully defined in TSRANGE. There are undefined values in year-period ',
             paste0(outDate,collapse = '-'),'.\n')
      }
    }
  
  if (hasLeads && simSteps>1)
  {
    #check missing and length in CA time series inside TSRANGE: if NA then stop
    if (length(ConstantAdjustment)>0) 
      for (idxSS in 1:(model_max_lead+simSteps-1)) for (idxI in 1:length(ConstantAdjustment))
      {
        idxCA=names(ConstantAdjustment)[idxI]
        
        tmpProxy=localE[[paste0(idxCA,'__ADDFACTOR__LEAD__',idxSS)]]
        
        #verify there are no missing values
        #note: all missings/NaNs are converted in zeros before in code 
        #when dealing with add-factor projection in TSRANGE
        #so probably this check is redundant
        localMissingIndexes=which(! is.finite(tmpProxy[-(1:model_max_lag),])) 
       
        if (length(localMissingIndexes)>0 )
        {
          realYP=normalizeYP(c(TSRANGE[1],TSRANGE[2]+idxSS),frequency)
          
          if (idxSS <= simSteps-1)
          {
            #we are in TSRANGE so missing -> stop
            stop(callerName,'constant adjustment of endogenous variable "',idxCA,'" is not fully defined in TSRANGE. There are undefined values in year-period ',
                 paste0(realYP,collapse = '-'),'\n')
          } else {
            .MODEL_outputText(outputText = !quietly,paste0(callerName,'warning, constant adjustment of endogenous variable "',idxCA,'" is not fully defined in extended TSRANGE. There are undefined values in year-period ',
                                                           paste0(realYP,collapse = '-'),'\n'))
          }
        }
      }
  }
  
  #backupped and later saved in __SIM_PARAMETERS__
  Exogenize_original=Exogenize 
  
  #print messages....
  if (length(Exogenize_original) > 0) 
    for (idxI in 1:length(Exogenize_original))
    {
      idxEL=names(Exogenize_original)[idxI]
      
      .MODEL_outputText(outputText = !quietly,paste0(callerName,'endogenous variable "',
                                                     idxEL,
                                                     '" has been exogenized from year-period ',
                                                     paste0(Exogenize[[idxEL]][1:2],collapse='-'),
                                                     ' to ',
                                                     paste0(Exogenize[[idxEL]][3:4],collapse='-'),'.\n')) 
      #convert to indexes
      Exogenize[[idxEL]]  = c(
        (NUMPERIOD(TSRANGE[1:2],Exogenize[[idxEL]][1:2],frequency)+1):
          (NUMPERIOD(TSRANGE[1:2],Exogenize[[idxEL]][3:4],frequency)+1)
            )
   
  
      #deal with exogenization in leads
      #e.g. exogenize y=2,3 then
      #y_lead_1=1, y_lead_2=1
      if (hasLeads && simSteps>1 )
      { 
        preLeadValules=Exogenize[[idxEL]] #these are integers
        Exogenize[[idxEL]]=NULL
        
          for (idxEP in preLeadValules) 
          {
            if (idxEP==1)
            {
              Exogenize[[idxEL]]=1
            } else {
              Exogenize[[paste0(idxEL,'__LEAD__',idxEP-1)]]=1
            }
          } 
        }
  }
 
  #backupped and later saved in simulate_parameters
  StochStructure_original=StochStructure 
  OptimizeRestrictions_original=OptimizeRestrictions
  
  #print messages....
  if (STOCHSIMULATE && length(StochStructure)>0) 
    for (idxI in 1:length(StochStructure))
    {
      idxSS=names(StochStructure)[idxI]
      
      typePertS=''
      if (StochStructure[[idxSS]]$TYPE=='UNIF') typePertS='uniformly'
      if (StochStructure[[idxSS]]$TYPE=='NORM') typePertS='normally'
      if (StochStructure[[idxSS]]$TYPE=='MATRIX') typePertS='manually'
      
      .MODEL_outputText(outputText = !quietly,paste0(callerName,'',ifelse(idxSS %in% model_vendog,'endogenous variable "','exogenous variable "'),
                                                     idxSS,
                                                     '" has been ',typePertS,' perturbed from year-period ',
                                                     paste0(StochStructure[[idxSS]]$TSRANGE[1:2],collapse='-'),
                                                     ' to ',
                                                     paste0(StochStructure[[idxSS]]$TSRANGE[3:4],collapse='-'),'.\n')) 
     
      #convert to indexes
      StochStructure[[idxSS]]$TSRANGE  = c(
        (NUMPERIOD(EXTENDED_TSRANGE[1:2],StochStructure[[idxSS]]$TSRANGE[1:2],frequency)+1):
          (NUMPERIOD(EXTENDED_TSRANGE[1:2],StochStructure[[idxSS]]$TSRANGE[3:4],frequency)+1)
      )
      
      if (StochStructure[[idxSS]]$TYPE == 'MATRIX')
      { 
        if( (nrow(StochStructure[[idxSS]]$PARS) != length(StochStructure[[idxSS]]$TSRANGE)) ||
            (ncol(StochStructure[[idxSS]]$PARS) != (replica-1)) )
          stop(callerName,paste0('"StochStructure$',idxSS,'PARS" must be a ',length(StochStructure[[idxSS]]$TSRANGE),' x ',replica-1,' matrix (i.e. "periods in TSRANGE" x "StochReplica").'))
      }
    }
  
  if (OPTIMIZE)
  {
    if(length(OptimizeBounds)>0) 
    {
      for (idxI in 1:length(OptimizeBounds))
      {
        idxSS=names(OptimizeBounds)[idxI]
        
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'optimization boundaries for the ',ifelse(idxSS %in% model_vendog,'add-factor of endogenous variable "','exogenous variable "'),
                                                       idxSS,
                                                       '" are (',paste0(OptimizeBounds[[idxSS]]$BOUNDS[1:2],collapse=','),') from year-period ',
                                                       paste0(OptimizeBounds[[idxSS]]$TSRANGE[1:2],collapse='-'),
                                                       ' to ',
                                                       paste0(OptimizeBounds[[idxSS]]$TSRANGE[3:4],collapse='-'),'.\n')) 
        #convert to indexes
        OptimizeBounds[[idxSS]]$TSRANGE  = c(
          (NUMPERIOD(EXTENDED_TSRANGE[1:2],OptimizeBounds[[idxSS]]$TSRANGE[1:2],frequency)+1):
            (NUMPERIOD(EXTENDED_TSRANGE[1:2],OptimizeBounds[[idxSS]]$TSRANGE[3:4],frequency)+1)
        )
      }
    }
    
    if(length(OptimizeRestrictions)>0) 
    {
      for (idxI in 1:length(OptimizeRestrictions))
      {
        idxSS=names(OptimizeRestrictions)[idxI]
        
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'optimization restriction "',
                                                       idxSS,'" is active from year-period ',
                                                       paste0(OptimizeRestrictions[[idxSS]]$TSRANGE[1:2],collapse='-'),
                                                       ' to ',
                                                       paste0(OptimizeRestrictions[[idxSS]]$TSRANGE[3:4],collapse='-'),'.\n')) 
        #convert to indexes
        OptimizeRestrictions[[idxSS]]$TSRANGE  = c(
          (NUMPERIOD(EXTENDED_TSRANGE[1:2],OptimizeRestrictions[[idxSS]]$TSRANGE[1:2],frequency)+1):
            (NUMPERIOD(EXTENDED_TSRANGE[1:2],OptimizeRestrictions[[idxSS]]$TSRANGE[3:4],frequency)+1)
        )
      }
    }
    
    if(length(OptimizeFunctions)>0) 
    {
      for (idxI in 1:length(OptimizeFunctions))
      {
        idxSS=names(OptimizeFunctions)[idxI]
        
        .MODEL_outputText(outputText = !quietly,paste0(callerName,'objective function "',
                                                       idxSS,'" is active from year-period ',
                                                       paste0(OptimizeFunctions[[idxSS]]$TSRANGE[1:2],collapse='-'),
                                                       ' to ',
                                                       paste0(OptimizeFunctions[[idxSS]]$TSRANGE[3:4],collapse='-'),'.\n')) 
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
    #store var for perturbed instruments
    INSTRUMENT_MM=list()
    
    if (length(StochStructure)>0) 
    { 
      for (idxI in 1:length(StochStructure))
      {
        idxN=names(StochStructure)[idxI]
        
        if ( (! is.null(RESCHECKeqList)) && length(base::intersect(idxN,model_fullComponentList))==0)
        {
          #with RESCHECK user can select eq to be simulated
          #so idxN could be not in vendog nor in vexog
          .MODEL_outputText(outputText =!quietly,paste0(callerName,'warning, "',idxN,'" appears in "StochStructure" but it is not required in order to perform the simulations requested in "RESCHECKeqList". It will be skipped.\n')) 
          next 
        }
        
        #create uniform perturbation
        if (StochStructure[[idxN]]$TYPE=='UNIF')
          noise=matrix(
            stats::runif(
              length(StochStructure[[idxN]]$TSRANGE)*(replica-1),
              min=StochStructure[[idxN]]$PARS[1],
              max=StochStructure[[idxN]]$PARS[2])
            ,nrow=length(StochStructure[[idxN]]$TSRANGE)
          )
        
        #create normal perturbation
        if (StochStructure[[idxN]]$TYPE=='NORM')
          noise=matrix(
            stats::rnorm(
              length(StochStructure[[idxN]]$TSRANGE)*(replica-1),
              mean=StochStructure[[idxN]]$PARS[1],
              sd=StochStructure[[idxN]]$PARS[2])
            ,nrow=length(StochStructure[[idxN]]$TSRANGE)
          )
        
        #pass perturbation matrix to model
        if (StochStructure[[idxN]]$TYPE=='MATRIX')
        {
          noise=StochStructure[[idxN]]$PARS
        }
        
        if (hasLeads && simSteps>1)
        { 
          
          for (idxSS in 1:(simSteps))
          {
            if (idxN %in% model_vendog)
            {
             
              if (idxSS==1)
              {
                #get un-noised time series
                tmpNoised_ORIG=localE[[paste0(idxN,'__ADDFACTOR__ORIGINAL__EXTENDED')]]
                
              } else {
                
                #get un-noised leaded time series
                tmpNoised_ORIG=localE[[paste0(idxN,'__ADDFACTOR__LEAD__',(idxSS-1))]]
              }
              
            } else if (idxN %in% model_vexog)
            {
              
              if (idxSS==1)
              {
                #get un-noised time series
                tmpNoised_ORIG=localE[[paste0(idxN,'__ORIGINAL__EXTENDED')]]
                
              } else {
                
                #get un-noised leaded time series
                tmpNoised_ORIG=localE[[paste0(idxN,'__LEAD__',(idxSS-1))]]
              }
            } else 
            {
              stop(callerName,'unknown error while perturbing time series "',idxN,'".')
            }
            
            if ((extSimSteps+idxSS-1) %in% StochStructure[[idxN]]$TSRANGE)
            {
              noise_lead=noise[(extSimSteps+idxSS-1)-StochStructure[[idxN]]$TSRANGE[1]+1,,drop=F]
             
              #add noise to matrix data (replace if MATRIX and EXOG)
              if ((StochStructure[[idxN]]$TYPE=='MATRIX') && (idxN %in% model_vexog))
              {
                tmpNoised_ORIG[extSimSteps,2:replica]=noise_lead
              } else
              {
                tmpNoised_ORIG[extSimSteps,2:replica]=tmpNoised_ORIG[extSimSteps,2:replica]+noise_lead
              }
              
              if (idxN %in% model_vendog)
              {
                if (idxSS==1)
                {
                  #save noised ts
                  localE[[paste0(idxN,'__ADDFACTOR','__ORIGINAL__EXTENDED')]]=tmpNoised_ORIG
                  
                } else {
                  #save noised ts
                  localE[[paste0(idxN,'__ADDFACTOR','__LEAD__',(idxSS-1))]]=tmpNoised_ORIG
                }
                
              } else if (idxN %in% model_vexog)
              {
                if (idxSS==1)
                {
                  #save noised ts
                  localE[[paste0(idxN,'__ORIGINAL__EXTENDED')]]=tmpNoised_ORIG
                  
                } else {
                  #save noised ts
                  localE[[paste0(idxN,'__LEAD__',(idxSS-1))]]=tmpNoised_ORIG
                }
                
              }  else stop(callerName,'unknown error while storing perturbed time series "',idxN,'".')
              
            }#simStep is in TSRANGE
            
              if (idxSS==1)
              {
                #store perturbed ts for output
                INSTRUMENT_MM[[idxN]]=tmpNoised_ORIG[extSimSteps,,drop=F]
                
              } else {
                
                #store perturbed ts for output
                INSTRUMENT_MM[[paste0(idxN,'__LEAD__',(idxSS-1))]]=tmpNoised_ORIG[extSimSteps,,drop=F]
              }
            
          }#cycle in simSteps
          
        } else {#hasleads=FALSE
          
          if (idxN %in% model_vendog)
          {
            #get un-noised time series
            tmpNoised_ORIG=localE[[paste0(idxN,'__ADDFACTOR__ORIGINAL__EXTENDED')]]
            
          } else if(idxN %in% model_vexog)
          {
            #get un-noised time series
            tmpNoised_ORIG=localE[[paste0(idxN,'__ORIGINAL__EXTENDED')]]
            
          } else stop(callerName,'unknown error while perturbing time series "',idxN,'".')
          
          #add noise to matrix data (replace if MATRIX and EXOG)
          if ((StochStructure[[idxN]]$TYPE=='MATRIX') && (idxN %in% model_vexog))
          {
            tmpNoised_ORIG[StochStructure[[idxN]]$TSRANGE,2:replica]=noise
          } else
          {
            tmpNoised_ORIG[StochStructure[[idxN]]$TSRANGE,2:replica]=tmpNoised_ORIG[StochStructure[[idxN]]$TSRANGE,2:replica]+noise
          }
          
          if (idxN %in% model_vendog)
          {
            #save noised ts
            localE[[paste0(idxN,'__ADDFACTOR','__ORIGINAL__EXTENDED')]]=tmpNoised_ORIG
            
          } else if(idxN %in% model_vexog)
          {
            #save noised ts
            localE[[paste0(idxN,'__ORIGINAL__EXTENDED')]]=tmpNoised_ORIG
            
          }  else stop(callerName,'unknown error while storing perturbed time series "',idxN,'".')
          
          #store perturbed ts for output
          INSTRUMENT_MM[[idxN]]=tmpNoised_ORIG[-(1:model_max_lag),,drop=F]
       
        } #else has leads
      }
    }
  }
  
 
  # populate optimization search domain --------------------------------------------------------------
  
  #populate search domain
  if (OPTIMIZE)
  {
    #store var for perturbed instruments
    INSTRUMENT_MM=list() 
    #columns to be kept after optimization restrictions
    optColsToBeKept=rep(TRUE,replica-1) 
    #optimize function results
    optFunResults=rep(0,replica-1) 
    #optimize fun time series
    optFunTSMM=c()
    optFunTS=NULL
    
    if (length(OptimizeBounds)>0) 
    { 
      for (idxI in 1:length(OptimizeBounds))
      {
        idxN=names(OptimizeBounds)[idxI]
        
       if ( ! is.null(RESCHECKeqList) && length(base::intersect(idxN,model_fullComponentList))==0)
          {
            #with RESCHECK user can select eq to be simulated
            #so idxN could be not in vendog nor in vexog
            .MODEL_outputText(outputText =!quietly,paste0(callerName,'warning, "',idxN,'" appears in "OptimizeBounds" but it is not required in order to perform the simulations requested in "RESCHECKeqList". It will be skipped.\n')) 
            next 
          } 
        
        #create unif noise in range
        noise=matrix(
          stats::runif(
            length(OptimizeBounds[[idxN]]$TSRANGE)*(replica-1),
            min=OptimizeBounds[[idxN]]$BOUNDS[1],
            max=OptimizeBounds[[idxN]]$BOUNDS[2])
          ,nrow=length(OptimizeBounds[[idxN]]$TSRANGE)
        )
        
        if (hasLeads && simSteps>1)
        { 
          for (idxSS in 1:(simSteps))
          {
            if (idxN %in% model_vendog)
            {
              if (idxSS==1)
              {
                #get un-noised time series
                tmpNoised_ORIG=localE[[paste0(idxN,'__ADDFACTOR__ORIGINAL__EXTENDED')]]
                
              } else {
                
                #get un-noised leaded time series
                tmpNoised_ORIG=localE[[paste0(idxN,'__ADDFACTOR__LEAD__',(idxSS-1))]]
                
              }
              
            } else if (idxN %in% model_vexog)
            {
              if (idxSS==1)
              {
                #get un-noised time series
                tmpNoised_ORIG=localE[[paste0(idxN,'__ORIGINAL__EXTENDED')]]
                
              } else {
                
                #get un-noised leaded time series
                tmpNoised_ORIG=localE[[paste0(idxN,'__LEAD__',(idxSS-1))]]
                
              }
              
            } else stop(callerName,'unknown error while populating time series "',idxN,'".')
            
            if ((extSimSteps+idxSS-1) %in% OptimizeBounds[[idxN]]$TSRANGE)
            {
              noise_lead=noise[(extSimSteps+idxSS-1)-OptimizeBounds[[idxN]]$TSRANGE[1]+1,,drop=F]
              
              #replace matrix data
              tmpNoised_ORIG[extSimSteps,2:replica]=noise_lead 
              
              if (idxN %in% model_vendog)
              {
                if (idxSS==1)
                {
                  #save populated ts
                  localE[[paste0(idxN,'__ADDFACTOR__ORIGINAL__EXTENDED')]]=tmpNoised_ORIG
                  
                } else {
                  
                  #save noised ts
                  localE[[paste0(idxN,'__ADDFACTOR','__LEAD__',(idxSS-1))]]=tmpNoised_ORIG
                  
                }
                
              } else if (idxN %in% model_vexog)
              {
                
                if (idxSS==1)
                {
                  #save populated ts
                  localE[[paste0(idxN,'__ORIGINAL__EXTENDED')]]=tmpNoised_ORIG
                  
                } else {
                  
                  #save noised ts
                  localE[[paste0(idxN,'__LEAD__',(idxSS-1))]]=tmpNoised_ORIG
                  
                }
               
              }  else stop(callerName,'unknown error while storing populated time series "',idxN,'".')
              
            }#simStep is in TSRANGE
            
            if (idxSS==1)
            {
              #store perturbed ts for output
              INSTRUMENT_MM[[idxN]]=tmpNoised_ORIG[extSimSteps,,drop=F]
              
            } else {
              
              #store perturbed ts for output
              INSTRUMENT_MM[[paste0(idxN,'__LEAD__',(idxSS-1))]]=tmpNoised_ORIG[extSimSteps,,drop=F]
              
            }
            
          } #end cycle idxSS simSteps
          
        } else { #hasleads==FALSE
          
          if (idxN %in% model_vendog)
          {
            #get un-noised time series
            tmpNoised_ORIG=localE[[paste0(idxN,'__ADDFACTOR__ORIGINAL__EXTENDED')]]
            
          } else if (idxN %in% model_vexog)
          {
            #get un-noised time series
            tmpNoised_ORIG=localE[[paste0(idxN,'__ORIGINAL__EXTENDED')]]
            
          } else stop(callerName,'unknown error while populating time series "',idxN,'".')
          
          #replace matrix data
          tmpNoised_ORIG[OptimizeBounds[[idxN]]$TSRANGE,2:replica]=noise 
          if (idxN %in% model_vendog)
          {
            #save populated ts
            localE[[paste0(idxN,'__ADDFACTOR__ORIGINAL__EXTENDED')]]=tmpNoised_ORIG
            
          } else if (idxN %in% model_vexog)
          {
            #save populated ts
            localE[[paste0(idxN,'__ORIGINAL__EXTENDED')]]=tmpNoised_ORIG
            
          }  else stop(callerName,'unknown error while storing populated time series "',idxN,'".')
          
          #store perturbed ts for output
          INSTRUMENT_MM[[idxN]]=tmpNoised_ORIG[-(1:model_max_lag),,drop=F]
        }
      }
    }
  }
  
  # populate behaviorals coefficient -------------------------------------------------------
  
  #temp list with sim results
  simulation=list() 
  .MODEL_outputText(outputText=!quietly,"\n") 
 
  
  if (length(model_names_behavioral)>0)
  for (idxI in 1:length(model_names_behavioral))
  { 
    currentVendog=model_names_behavioral[idxI]
    
    #cycle in endogenous
    
    #get eq components names and assign into them the behavioral coefficient values
    currentBehavioral=model$behaviorals[[currentVendog]] 
    
    #get names of coefficients for current behavioral endogenous
    localCoefficientNames=currentBehavioral$eqCoefficientsNames 
    
    #get values of coefficients for current behavioral endogenous
    localCoefficientValues=c(currentBehavioral$coefficients) 
    
    #check there is a coefficient value for every coefficient name
    if (length(localCoefficientNames) != length(localCoefficientValues)) 
      stop(callerName,'coefficients count error in behavioral "',currentVendog,'".')
    
    #assign values to coefficients
    for (idxA in 1:length(localCoefficientValues))
    {
      localE[[paste0(currentVendog,'__COEFF__',localCoefficientNames[idxA])]]=localCoefficientValues[idxA]
    } 
    
    #assign values to error coefficients
    if ( ! is.null(currentBehavioral$errorType) && currentBehavioral$errorType=='AUTO')
    {  
      for (idxEC in 1:currentBehavioral$errorDim)
      {
        #check if rhos have to be zeroed
        if (ZeroErrorAC)
        {
          localE[[paste0(currentVendog,'__RHO__',idxEC)]]=0
        }
        else
        {
          localE[[paste0(currentVendog,'__RHO__',idxEC)]]=currentBehavioral$errorCoefficients[idxEC]
        }
      }
    }# end deal with AUTO
    
  }#for in model_names_behaviorals
  
  # build sim expressions -------------------------------------------------------
 
  if (hasLeads && simSteps>1)
  {
    #cycle behaviorals
    if (simSteps>1 && length(model_names_behavioral)>0) for (idx in 1:length(model_names_behavioral))
    {
      currentVendog=model_names_behavioral[idx]
      
      #lead vendog sim eq up to simSteps, and convert to __LEAD__ when i>model_max_lax+1,
      #given vendog[i,] in sim eq
      model$behaviorals[[currentVendog]]$leadsEqSimExpText=.leadEqSim(currentVendog,
                                                                  model$behaviorals[[currentVendog]]$eqSimExpText,
                                                                  model_max_lag_1,
                                                                  simSteps)
      
      #parse text eq
      model$behaviorals[[currentVendog]]$leadsEqSimExp=lapply(model$behaviorals[[currentVendog]]$leadsEqSimExpText,
                                                              function(x) parse(text=x))
    }
    
    if (simSteps>1 && length(model_names_identity)>0) for (idx in 1:length(model_names_identity))
    {
     
      currentVendog=model_names_identity[idx]
      #lead vendog sim eq up to simSteps, and convert to __LEAD__ when i>model_max_lax+1,
      #given vendog[i,] in sim eq
      model$identities[[currentVendog]]$leadsEqSimExpText=.leadEqSim(currentVendog,
                                                                 model$identities[[currentVendog]]$eqSimExpText, #ok with IF> in identity
                                                                 model_max_lag_1,
                                                                 simSteps)
      
      #parse text eq
      model$identities[[currentVendog]]$leadsEqSimExp=lapply(model$identities[[currentVendog]]$leadsEqSimExpText,
                                                              function(x) parse(text=x))
    }
  }
  
  # build lead incidence and order lists ---------------------------------------

  #build incidence matrix with leads (RESCHECK excluded)
  if (hasLeads && simType=='DYNAMIC')
  {
    .MODEL_outputText(outputText=!quietly,'Optimizing forward looking...\n')  
    
    #create empty incidence matrix
    incidence_matrix_size=length(model_vendog)*simSteps
    incidence_matrix=matrix(0,nrow=incidence_matrix_size, ncol=incidence_matrix_size) 
    
    if (simSteps==1)
    {
      colnames(incidence_matrix)=model_vendog 
      rownames(incidence_matrix)=model_vendog 
      
    } else {
     
      tmpNames=model_vendog 
      tmpS=paste0(model_vendog,'__LEAD__')
      
      for (idxSS in 1:(simSteps-1))
      {
        tmpNames=c(tmpNames,paste0(tmpS,idxSS))
      }
      
      colnames(incidence_matrix)=tmpNames
      rownames(incidence_matrix)=tmpNames
    }
    
    #build ordered lists
    if (length(model_names_behavioral)>0)
    for (idxI in 1:length(model_names_behavioral))
    {
      currentVendog=model_names_behavioral[idxI]
      
      #get non vexogs components
      baseVendogComponents=c(model$behaviorals[[currentVendog]]$eqComponentsNamesBehaviorals,
        model$behaviorals[[currentVendog]]$eqComponentsNamesIdentities)
      
      vendogComponents=c()
      
      if (simSteps>1)
      {
        tmpS=paste0(baseVendogComponents,'__LEAD__')
        
        for (idxSS in 1:(simSteps-1))
          vendogComponents=c(vendogComponents,paste0(tmpS,idxSS))
      }
      
      #extend list with leaded components
      vendogComponents=c(baseVendogComponents,vendogComponents)
      
      #extract current and leaded components that produce incidence
      #current...
      incidenceVendogs=.getIncidenceVendogs(currentVendog,
                                            model$behaviorals[[currentVendog]]$eqSimExpLeadedText,
                                            vendogComponents,
                                            baseIndex=model_max_lag_1) 
      
      if (length(incidenceVendogs)>0)
        for (idxIV in 1:length(incidenceVendogs))
          incidence_matrix[currentVendog,
                            incidenceVendogs[idxIV]]=1
      
       #leaded...
       if (simSteps>1)
       {
         tmpNames=names(model$behaviorals[[currentVendog]]$leadsEqSimExp)
         
         for (idxII in 1:length(model$behaviorals[[currentVendog]]$leadsEqSimExp))
         {
           idxSE=tmpNames[idxII]
           
           incidenceVendogs=.getIncidenceVendogs(idxSE,
                                model$behaviorals[[currentVendog]]$leadsEqSimExpText[[idxSE]],
                                vendogComponents,
                                baseIndex=model_max_lag_1) 
           
           if (length(incidenceVendogs)>0)
             for (idxIV in 1:length(incidenceVendogs))
               incidence_matrix[idxSE,
                                 incidenceVendogs[idxIV]]=1
         }
       }
    }
  
    if (length(model_names_identity)>0)
    for (idxI in 1:length(model_names_identity))
    {
      currentVendog=model_names_identity[idxI]
      
      #get non vegoxs components
      baseVendogComponents=c(model$identities[[currentVendog]]$eqComponentsNamesBehaviorals,
                             model$identities[[currentVendog]]$eqComponentsNamesIdentities)
      
      vendogComponents=c()
    
      if (simSteps>1)
      {
        tmpS=paste0(baseVendogComponents,'__LEAD__')
        
        for (idxSS in 1:(simSteps-1))
          vendogComponents=c(vendogComponents,paste0(tmpS,idxSS))
      }
      
      #extend list with leaded component
      vendogComponents=c(baseVendogComponents,vendogComponents)
      
      #extract current and leaded components that produce incidence
      #current...
      incidenceVendogs=.getIncidenceVendogs(currentVendog,
                                            model$identities[[currentVendog]]$eqSimExpLeadedText,#ok with IF> in identity
                                            vendogComponents,
                                            baseIndex=model_max_lag_1) 
    
      if (length(incidenceVendogs)>0)
        for (idxIV in 1:length(incidenceVendogs))
          incidence_matrix[currentVendog,
                            incidenceVendogs[idxIV]]=1
        
      #leaded...
      if (simSteps>1)
      {
        tmpNames=names(model$identities[[currentVendog]]$leadsEqSimExp)
          
        for (idxII in 1:length(model$identities[[currentVendog]]$leadsEqSimExp))
        {
          idxSE=tmpNames[idxII]
          
          eqSimText=model$identities[[currentVendog]]$leadsEqSimExpText[[idxSE]]
         
          incidenceVendogs=.getIncidenceVendogs(idxSE,
                                                eqSimText,
                                                vendogComponents,
                                                baseIndex=model_max_lag_1) 
          
          if (length(incidenceVendogs)>0)
            for (idxIV in 1:length(incidenceVendogs))
              incidence_matrix[idxSE,
                                incidenceVendogs[idxIV]]=1
        }
      }
    }
    
    #get vblocks from incidence matrix using Tarjan
    modelBlocks=.buildBlocks(incidence_matrix) 
    model_vblocks=modelBlocks$vblocks
    length_model_vblocks=length(model_vblocks)
    model_vpre=modelBlocks$vpre
    length_model_vpre=length(model_vpre)
    
  }#end has leads
  
  # build sim expressions lists -------------------------------------------------------
 
  if (length_model_vblocks>0)
    for (idxB in 1:length_model_vblocks)
    {
      model_vsim[[idxB]]=model_vblocks[[idxB]]$vsim
      model_vfeed[[idxB]]=model_vblocks[[idxB]]$vfeed
      
      length_model_vsim=c(length_model_vsim,length(model_vsim[[idxB]]))
      length_model_vfeed=c(length_model_vfeed,length(model_vfeed[[idxB]]))
      
      model_vpost[[idxB]]=model_vblocks[[idxB]]$vpost
      length_model_vpost=c(length_model_vpost,length(model_vblocks[[idxB]]$vpost))
      
    }
  
  if (! simType =='RESCHECK')
  {#full simulation required
    
  #if leads then search for convergence in first period only
   if (hasLeads && simSteps>1)
    {
      actualSimSteps=1
      
    } else {
      
      actualSimSteps=simSteps
      
    }
    
    #array of expressions used in convergence algos
    vpre_expressions=vector('list',actualSimSteps) 
    vsim_expressions=vector('list',actualSimSteps) 
    vpost_expressions=vector('list',actualSimSteps) 
    
    #e.g. vsim_expression[[2]][[3]] is vendog list in simultaneous vblock 3 in simulation period 2
    for (idxS in 1:actualSimSteps)
    {
      vsim_expressions[[idxS]]=vector('list',length_model_vblocks) 
      vpost_expressions[[idxS]]=vector('list',length_model_vblocks) 
    }
    
    #support variables, needed for convergence check on feedback variables
    localE$SIM__ALGO=simAlgo
    
    #model vfeed proxy
    localE$MODEL__VFEED__LIST=model_vfeed
    
    #model vfeed length proxy
    localE$LENGTH__MODEL__VFEED__LIST=length_model_vfeed
    
    #model_max_lag + 1 proxy
    localE$MODEL__MAX__LAG__1=model_max_lag_1
    
    #model replica proxy
    localE$REPLICA__=replica
    
    #newton methods, we need more info copied into localE
    if (simAlgo!='GAUSS-SEIDEL')
    {
      if (length_model_vblocks>0) {
        
        #vfeed active in jacobian
        localE$JACOBIAN__VFEED__ACTIVE__LIST=list()
        
        for (idxB in 1:length_model_vblocks)
          localE$JACOBIAN__VFEED__ACTIVE__LIST[[idxB]]=matrix(1,ncol=actualSimSteps,nrow=length_model_vfeed[idxB])
        
        #init jacobian matrix
        if (simAlgo=='NEWTON')
        {
          localE$JACOBIAN__SINGLE__MATRIX__ACTIVE__LIST=list()
           
        } else { #FULLNEWTON
          
          localE$JACOBIAN__FULL__MATRIX__ACTIVE__LIST=list()
          
        }
        
        #assign default shock to jacobian (note double underscore)
        localE$JACOBIAN__SHOCK=JACOBIAN_SHOCK
        
        #jacobian scale values
        localE$JACOBIAN__SCALE__VALUE__LIST=list()
        for (idxB in 1:length_model_vblocks)
          localE$JACOBIAN__SCALE__VALUE__LIST[[idxB]]=matrix(1,ncol=replica,nrow=length_model_vfeed[idxB])
        
        #flag of jacobian recalculation
        localE$RECALCULATE__JACOBIAN__ARRAY=vector(mode='logical',length=length_model_vblocks)
        localE$RECALCULATE__JACOBIAN__ARRAY[]=TRUE
      
      }
    }
    
    #LAST__EVAL__EQ is used in debug and error message: it keeps name of last evaluated eq during sim
    localE$LAST__EVAL__EQ='unknown'
  
    #init newton stuff...
    if (simAlgo!='GAUSS-SEIDEL' && length_model_vblocks>0)
    {
      #populate matrix of active vfeed in jacobian
      #Exogenize or Drop can reduce the vfeed set active in Jacobian
      
      #purge vfeed list from lead suffix
      model_vfeed_prefix=c()
        for (idxB in 1:length_model_vblocks) model_vfeed_prefix=c(model_vfeed_prefix,model_vfeed[[idxB]])
          
     if (hasLeads && simSteps>1) model_vfeed_prefix=unique(unlist(lapply(strsplit(model_vfeed_prefix,'__LEAD__'), function(x) return(x[1]))))
      
      #check JacobianDrop
      idxToBeRemoved=c()
      if (length(JacobianDrop)>0)
        for (idx in 1:length(JacobianDrop))
          if (! JacobianDrop[idx] %in% model_vfeed_prefix ) 
          {
            .MODEL_outputText(sep='',outputText=!quietly,callerName,'warning, "',JacobianDrop[idx],'" is not a model feedback variable. It will be removed from the "JacobianDrop" argument\n')
            idxToBeRemoved=c(idxToBeRemoved,idx)
          }
      
     if (length(idxToBeRemoved)>0) JacobianDrop=JacobianDrop[-idxToBeRemoved]
      
      #disable vfeed idxVF in iteration idxSP if exogenized
      for (idxB in 1:length_model_vblocks) 
      for (idxVF in 1:length_model_vfeed[idxB])
      { 
        vfeedTemp=model_vfeed[[idxB]][idxVF]
        #drop from jacobian if exogenized
        if (vfeedTemp %in% names(Exogenize))
          for (idxSP in 1:actualSimSteps)
          {
            if ( idxSP %in% Exogenize[[vfeedTemp]]) 
              localE$JACOBIAN__VFEED__ACTIVE__LIST[[idxB]][idxVF,idxSP]=0
          }
        
        #drop from jacobian if in JacobianDrop
        #purge vfeed from __LEAD__ suffix
        if (hasLeads && simSteps>1)  vfeedTemp=strsplit(vfeedTemp,'__LEAD__')[[1]][1]
        if (vfeedTemp %in% JacobianDrop)
        {
          localE$JACOBIAN__VFEED__ACTIVE__LIST[[idxB]][idxVF,]=0
        }
      } 
    }
    
    #build vpre expression
    if (length_model_vpre>0) 
    { 
      for (tmpV in (model_vpre))
      {
        #init tmp var
        tmp_expressions=c() 
        tmpVfull=tmpV
        
        if (hasLeads && simSteps>1 && grepl('__LEAD__',tmpV))
        {
          tmpV=strsplit(tmpVfull,'__LEAD__')[[1]][1]
          
          #append sim expression
          if (tmpV %in% model_names_behavioral)
          {
            tmp_expressions=model$behaviorals[[tmpV]]$leadsEqSimExp[[tmpVfull]]
          } else
          {
            tmp_expressions=model$identities[[tmpV]]$leadsEqSimExp[[tmpVfull]]
          }
          
        } else {
          
          #append sim expression
          if (tmpV %in% model_names_behavioral)
          {
            tmp_expressions=model$behaviorals[[tmpV]]$eqSimExp
          } else
          {
            tmp_expressions=model$identities[[tmpV]]$eqSimExp
          }
        }
        
        #append check on result
        tmp_expressions=c(parse(text=paste0('LAST__EVAL__EQ="',tmpVfull,'";')),
                          tmp_expressions,
                          parse(text=paste0('if (any(! is.finite(',tmpVfull,'[',model_max_lag_1,',]))) stop(\'Uncomputable solution or numeric overflow while evaluating \"',tmpV,'\".\');')))
        
        #update vpre expressions, and deal with exogenization
        if (! tmpVfull %in% names(Exogenize)) 
        {
          for (idxSP in 1:actualSimSteps)
          {
            vpre_expressions[[idxSP]]=c(vpre_expressions[[idxSP]],tmp_expressions) 
          } 
        } else 
        {
          for (idxSP in 1:actualSimSteps)
          {
            if (! idxSP %in% Exogenize[[tmpVfull]])  vpre_expressions[[idxSP]]=c(vpre_expressions[[idxSP]],tmp_expressions) 
          } 
        }
      }}#end vpre
    
    if (length_model_vblocks>0)
      for (idxB in 1:length_model_vblocks)
      {
        #get index of first vfeed in vsim
        if (length_model_vsim[idxB]>0)
        {
          vfeedIndexInVsim=which(model_vsim[[idxB]] == model_vfeed[[idxB]][1]) 
          if (length(vfeedIndexInVsim)==0) stop(callerName,'unknown error while splitting vsim vs vfeed in block ',idxB,'.')
       
          #init vfeed__current to initial state (VFEED__NEXT in iteration 1 contains historical values)
          tmp_expressions=parse(text='VFEED__CURRENT=VFEED__NEXT')
          
          for (idxSP in 1:actualSimSteps)  vsim_expressions[[idxSP]][[idxB]]=c( vsim_expressions[[idxSP]][[idxB]],tmp_expressions) 
          
          for (idxE in 1:length_model_vsim[idxB])
          {
            #init tmp var
            tmp_expressions=c() 
            tmpV=model_vsim[[idxB]][idxE] 
            tmpVfull=tmpV
            
            if (hasLeads && simSteps>1 && grepl('__LEAD__',tmpV))
            {
              tmpV=strsplit(tmpVfull,'__LEAD__')[[1]][1]
              
              #append sim expression
              if (tmpV %in% model_names_behavioral)
              {
                tmp_expressions=model$behaviorals[[tmpV]]$leadsEqSimExp[[tmpVfull]]
              } else
              {
                tmp_expressions=model$identities[[tmpV]]$leadsEqSimExp[[tmpVfull]]
              }
              
            } else {
              
              #append sim expression
              if (tmpV %in% model_names_behavioral)
              {
                tmp_expressions=model$behaviorals[[tmpV]]$eqSimExp
              } else
              {
                tmp_expressions=model$identities[[tmpV]]$eqSimExp
              }
            }
            
            #store vsim expressions
            tmp_expressions=c(parse(text=paste0('LAST__EVAL__EQ="',tmpVfull,'";')),
                              tmp_expressions,
                              parse(text=paste0('if (any(! is.finite(',tmpVfull,'[',  model_max_lag_1,',]))) stop(\'Uncomputable solution or numeric overflow while evaluating \"',tmpV,'\".\');')))
            
            #deal with exogenization
            if (! tmpVfull %in% names(Exogenize)) 
            {
              for (idxSP in 1:actualSimSteps)
              {
                vsim_expressions[[idxSP]][[idxB]]=c( vsim_expressions[[idxSP]][[idxB]],tmp_expressions) 
              } 
            } else 
            {
              for (idxSP in 1:actualSimSteps)
              {
                if (! idxSP %in% Exogenize[[tmpVfull]])   vsim_expressions[[idxSP]][[idxB]]=c( vsim_expressions[[idxSP]][[idxB]],tmp_expressions) 
              } 
            }
            
            #store next vfeed values (we need for convergence test and jacobian)
            if (idxE>=vfeedIndexInVsim) 
            {
              tmp_expressions=parse(text=paste0('VFEED__NEXT[',1+idxE-vfeedIndexInVsim,',]=',tmpVfull,'[',  model_max_lag_1,',]'))
              
              for (idxSP in 1:actualSimSteps)  vsim_expressions[[idxSP]][[idxB]]=c( vsim_expressions[[idxSP]][[idxB]],tmp_expressions) 
            }
          }#end idxE
        }
        
        #build vpost expression
        if (length_model_vpost[idxB]>0) 
        { 
          for (idxI in 1:length(model_vpost[[idxB]]))
          {
            tmpV=model_vpost[[idxB]][idxI]
            
            #init tmp vars
            tmp_expressions=c() 
            tmpVfull=tmpV
            
            if (hasLeads && simSteps>1 && grepl('__LEAD__',tmpV))
            {
              tmpV=strsplit(tmpVfull,'__LEAD__')[[1]][1]
              
              #append sim expression
              if (tmpV %in% model_names_behavioral)
              {
                tmp_expressions=model$behaviorals[[tmpV]]$leadsEqSimExp[[tmpVfull]]
              } else
              {
                tmp_expressions=model$identities[[tmpV]]$leadsEqSimExp[[tmpVfull]]
              }
              
            } else {
              
              #append sim expression
              if (tmpV %in% model_names_behavioral)
              {
                tmp_expressions=model$behaviorals[[tmpV]]$eqSimExp
              } else
              {
                tmp_expressions=model$identities[[tmpV]]$eqSimExp
              }
            }
            
            tmp_expressions=c(parse(text=paste0('LAST__EVAL__EQ="',tmpVfull,'";')),
                              tmp_expressions,
                              parse(text=paste0('if (any(! is.finite(',tmpVfull,'[',  model_max_lag_1,',]))) stop(\'Uncomputable solution or numeric overflow while evaluating \"',tmpV,'\".\');')))
            
            #deal with exogenization
            if (! tmpVfull %in% names(Exogenize)) 
            {
              for (idxSP in 1:actualSimSteps)
              {
                vpost_expressions[[idxSP]][[idxB]]=c(vpost_expressions[[idxSP]][[idxB]],tmp_expressions) 
              } 
            } else 
            {
              for (idxSP in 1:actualSimSteps)
              {
                if (! idxSP %in% Exogenize[[tmpVfull]])  vpost_expressions[[idxSP]][[idxB]]=c(vpost_expressions[[idxSP]][[idxB]],tmp_expressions) 
              } 
            }
          }
        }
        
      }#end vblocks for
    
    #set convergence value in local environment
    localE$CONVERGENCE__VALUE=simConvergence/100
    
  }#end block simType != RESCHECK
  
  if (simType!='RESCHECK' && length_model_vblocks==0) 
    .MODEL_outputText(sep='',outputText=!quietly,callerName,'warning, model\'s incidence graph is not cyclic. There is no convergence to be achieved.\n')
  
  # main loop --------------------------------------------------------------
  
     #proxies 4 speedup - ok with leads too
     if (STOCHSIMULATE )
       noised_vendog=base::intersect(names(StochStructure),model_vendog) 
     
     if (OPTIMIZE )
       optimized_vendog=base::intersect(names(OptimizeBounds),model_vendog) 
 
  tryCatch({
    
    #simrange loop
    for (simIterLoopIdx in 1:simSteps)
    {
      #in leaded model, only the very first period in TSRANGE is evaluated
      if (hasLeads && simIterLoopIdx>1) break;
      
      if (! hasLeads)
      {
        percentageCompletionPorxy=simIterLoopIdx
        
      } else {
        
          percentageCompletionPorxy=0
      }
      
      if (MULTMATRIX)
        {.MODEL_outputText(outputText=!quietly,paste0('\r','Multiplier Matrix:'),sprintf("%10.2f",percentageCompletionPorxy*100/simSteps),'%') 
        } else if (STOCHSIMULATE)
        {.MODEL_outputText(outputText=!quietly,paste0('\r','Stochastic Simulation:'),sprintf("%10.2f",percentageCompletionPorxy*100/simSteps),'%') 
        } else if (OPTIMIZE)
        {.MODEL_outputText(outputText=!quietly,paste0('\r','Optimize:'),sprintf("%10.2f",percentageCompletionPorxy*100/simSteps),'%') 
        } else
        {.MODEL_outputText(outputText=!quietly,paste0('\r','Simulation:'),sprintf("%10.2f",percentageCompletionPorxy*100/simSteps),'%') }
     
      #ok with leads too
      if (simType != 'RESCHECK' && (verbose)) {cat('\n') }
     
      #create local proxies for all references 
      #(i.e. components names used in eq) in behaviorals/identities
      #in sliding window (with range == maxlag+tsrange) starting from simIterLoopIdx
      
      tempRange=simIterLoopIdx+(0:model_max_lag)
      for (idxI in 1:length(model_fullComponentList))
        {
          tempName=model_fullComponentList[idxI]
          
          localE[[tempName]]=localE[[paste0(tempName,'__ORIGINAL__EXTENDED')]][tempRange,,drop=F]
        }
        
      #proxy for explicit constant adjustments
      if (length(ConstantAdjustment)>0) 
        for (idxI in 1:length(CA_names))
        {
          tempName=CA_names[idxI]
          
          localE[[tempName]]=localE[[paste0(tempName,'__ORIGINAL__EXTENDED')]][tempRange,,drop=F]
        } 
      
      #proxy for vendog constant adjustment as INSTRUMENT 
      #vendog can be in INSTRUMENT (e.g. modified endo with SHOCK in MULTMATRIX) but not in CA list
      if (length(instrumentVendogs)>0) 
        for (idxI in 1:length(instrum_names))
        {
          tempName=instrum_names[idxI]
          
          localE[[tempName]]=localE[[paste0(tempName,'__ORIGINAL__EXTENDED')]][tempRange,,drop=F]
        } 
      
      #proxy for vendog noised in stochastic simulation 
      #vendog can be noised (e.g. in StochStructure) but not in CA list
      if (STOCHSIMULATE )
      {
        # if (length(noised_vendog)>0)
        { 
          tempS=paste0(noised_vendog,'__ADDFACTOR')
          for (idxI in 1:length(tempS))
          {
            tempName=tempS[idxI]
            localE[[tempName]]=localE[[paste0(tempName,'__ORIGINAL__EXTENDED')]][tempRange,,drop=F]
          }
        }
      }
      
      #proxy for vendog noised in optimize simulation 
      #vendog can be noised (e.g. in OptimizeBounds) but not in CA list
      if (OPTIMIZE )
      {
        if (length(optimized_vendog)>0)
        { 
          tempS=paste0(optimized_vendog,'__ADDFACTOR')
          for (idxI in 1:length(tempS))
          {
            tempName=tempS[idxI]
            
            localE[[tempName]]=localE[[paste0(tempName,'__ORIGINAL__EXTENDED')]][tempRange,,drop=F]
          }
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
                
                localE$LAST__EVAL__EQ=paste0('OptimizeRestrictions$',names(OptimizeRestrictions)[idxIE])
                
                #eval current restriction using current INSTRUMENT data
                currentRestrictionResults=eval(OptimizeRestrictions[[idxIE]]$inExpr,envir=localE)
                
                if (length(currentRestrictionResults)==0 || any(is.na(currentRestrictionResults)) || ! all(is.logical(currentRestrictionResults))) 
                  stop('restriction must be a computable inequality.')
                
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
        
        if (hasLeads && simSteps>1)
        {
          for (idxIE in 1:length(OptimizeRestrictions))
          {
            eqInExprText=as.character(OptimizeRestrictions[[idxIE]]$inExpr)
            
            #build leaded inequalities
            leadedInequalitiesTemp=lapply(.leadEqSim(names(OptimizeRestrictions)[idxIE],
                       eqInExprText,
                       model_max_lag_1,
                       simSteps
                       ),
                       function(x) parse(text=x))
           
            for (idxSS in 1:(simSteps-1))
            {
              if ((model_max_lag+idxSS+1) %in% OptimizeRestrictions[[idxIE]]$TSRANGE)
              {
                tryCatch({
                 
                  tempName=paste0(names(OptimizeRestrictions)[idxIE],'__LEAD__',idxSS)
                  localE$LAST__EVAL__EQ=paste0('OptimizeRestrictions$',tempName)
                  
                  #eval current restriction using current INSTRUMENT data
                  currentRestrictionResults=eval(leadedInequalitiesTemp[[tempName]],envir=localE)
                  
                  if (length(currentRestrictionResults)==0 || any(is.na(currentRestrictionResults)) || ! all(is.logical(currentRestrictionResults))) 
                    stop('restriction must be a computable inequality.')
                  
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
        } 
      }#end if optimize
     
      # rescheck loop ---------------------------------------
      
      if (simType=='RESCHECK') 
      {
        #base behaviorals rescheck eval
        tryCatch(
          {    
            #cycle behaviorals
            if (length(model_names_behavioral)>0)
            for (idxI in 1:length(model_names_behavioral))
            {  
              currentVendog=model_names_behavioral[idxI]
               
              #backup historical value
              backupVendog=localE[[currentVendog]]
              
              #if not exoginated eval expression
              if ( ! ((simIterLoopIdx %in% Exogenize[[currentVendog]])))
              {
                localE$LAST__EVAL__EQ=currentVendog
                eval(model$behaviorals[[currentVendog]]$eqSimExp,envir=localE) 
              }
              
              #get evaluated current value
              tmpResCheck=localE[[currentVendog]][model_max_lag_1,,drop=F] 
              
              #restore vendog (could be used in other eqs)
              localE[[currentVendog]]=backupVendog
              
              #check missings     
              localMissingIndexes=any(! is.finite(tmpResCheck)) 
              if (localMissingIndexes) 
                stop(paste0('Uncomputable solution or numeric overflow in residual checking of behavioral EQ "',
                            model$behaviorals[[currentVendog]]$eq,
                            '" in year-period ',paste0(
                              normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),frequency)
                              ,collapse = '-'),
                            '.\nCheck equation computability, or lagged time series span.',
                            ifelse(! is.null(model$behaviorals[[currentVendog]]$errorRaw),paste0('\nBehavioral has "ERROR> ',model$behaviorals[[currentVendog]]$errorRaw,'", that lags the whole equation.'),''),
                            ifelse(! is.null(model$behaviorals[[currentVendog]]$pdlRaw),paste0('\nBehavioral has "PDL> ',model$behaviorals[[currentVendog]]$pdlRaw,'", that lags the whole regressor.'),'')
                )) 
              
              #append results
              simulation[[currentVendog]]=rbind(simulation[[currentVendog]],tmpResCheck) 
              
            } #end cycle behaviorals
            
          },error=function(err){
            stop(paste0(err$message))}
          
        )#base behavioral rescheck
        
        if (hasLeads && simSteps>1  && length(model_names_behavioral)>0)
        {
          #leaded behaviorals rescheck eval
          tryCatch(
            {
              #cycle behaviorals
              for (idxSS in 1:(simSteps-1)) for (idxI in 1:length(model_names_behavioral) )
              { 
                currentVendog=model_names_behavioral[idxI]
                
                #backup historical value
                tempName=paste0(currentVendog,'__LEAD__',idxSS)
                backupVendog=localE[[tempName]]
                
                #if not exogenizated eval expression
                if ( ! ((simIterLoopIdx %in% Exogenize[[tempName]])))
                {
                  localE$LAST__EVAL__EQ=tempName
                  eval(model$behaviorals[[currentVendog]]$leadsEqSimExp[[tempName]],envir=localE) 
                }
                
                #get evaluated current value
                tmpResCheck=localE[[tempName]][model_max_lag_1,,drop=F] 
                
                #restore vendog (could be used in other eqs)
                localE[[tempName]]=backupVendog
                
                #check missings     
                localMissingIndexes=any(! is.finite(tmpResCheck)) 
                if (localMissingIndexes) 
                  stop(paste0('Uncomputable solution or numeric overflow in residual checking of behavioral EQ "',
                              model$behaviorals[[currentVendog]]$eq,
                              '" in year-period ',paste0(
                                normalizeYP(c(TSRANGE[1],TSRANGE[2]+idxSS),frequency)
                                ,collapse = '-'),
                              '.\nCheck equation computability, or lagged time series span.',
                              ifelse(! is.null(model$behaviorals[[currentVendog]]$errorRaw),paste0('\nBehavioral has "ERROR> ',model$behaviorals[[currentVendog]]$errorRaw,'", that lags the whole equation.'),''),
                              ifelse(! is.null(model$behaviorals[[currentVendog]]$pdlRaw),paste0('\nBehavioral has "PDL> ',model$behaviorals[[currentVendog]]$pdlRaw,'", that lags the whole regressor.'),'')
                  )) 
                
                #append results
                simulation[[tempName]]=rbind(simulation[[tempName]],tmpResCheck) 
                
              }#end cycle behaviorals
              
            },error=function(err){
              stop(paste0(err$message))}
            
          )#end trycatch
            
        }#has leads behavioral
    
        #base identity rescheck eval
        tryCatch(
          {   
            #cycle identities
            if (length(model_names_identity)>0)
            for (idxI in 1:length(model_names_identity))
            { 
              currentVendog=model_names_identity[idxI]
              
              #backup historical value  
              backupVendog=localE[[currentVendog]]
              
              #if not exogenated eval expression
              if (!((simIterLoopIdx %in% Exogenize[[currentVendog]]))) 
              {
                localE$LAST__EVAL__EQ=currentVendog
                eval(model$identities[[currentVendog]]$eqSimExp,envir=localE) 
              }
              
              #get evaluated current value
              tmpResCheck=localE[[currentVendog]][model_max_lag_1,,drop=F] 
              
              #restore historical  
              localE[[currentVendog]]=backupVendog
              
              #check missings  
              localMissingIndexes=any(! is.finite(tmpResCheck)) 
              if (localMissingIndexes) 
                stop(paste0('Uncomputable solution or numeric overflow in residual checking of identity EQ "',model$identities[[currentVendog]]$eqFull,
                            '" in year-period ',paste0(
                              normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),frequency)
                              ,collapse = '-'),
                            '.\nCheck equation computability or lagged time series span.'
                )) 
              
              #append results 
              simulation[[currentVendog]]=rbind(simulation[[currentVendog]],tmpResCheck)  
              
            }#for in vendog
            
          },error=function(err){
            stop(paste0(err$message))
          }
          
        )#base identity rescheck
        
        #leaded identity rescheck eval
        if (hasLeads && simSteps>1 && length(model_names_identity)>0)
        {
          tryCatch(
          {   
            #cycle identities
            for (idxSS in 1:(simSteps-1)) for (idxI in 1:length(model_names_identity))
            { 
              currentVendog=model_names_identity[idxI]
              
              #backup historical value  
              tempName=paste0(currentVendog,'__LEAD__',idxSS)
              backupVendog=localE[[tempName]]
              
              #if not exogenated eval expression
              if (!((simIterLoopIdx %in% Exogenize[[tempName]]))) 
              {
                localE$LAST__EVAL__EQ=tempName
                eval(model$identities[[currentVendog]]$leadsEqSimExp[[tempName]],envir=localE) 
              }
              
              #get evaluated current value
              tmpResCheck=localE[[tempName]][model_max_lag_1,,drop=F] 
              
              #restore historical  
              localE[[tempName]]=backupVendog
              
              #check missings  
              localMissingIndexes=any(! is.finite(tmpResCheck)) 
              if (localMissingIndexes) 
                stop(paste0('Uncomputable solution or numeric overflow in residual checking of identity EQ "',model$identities[[currentVendog]]$eqFull,
                            '" in year-period ',paste0(
                              normalizeYP(c(TSRANGE[1],TSRANGE[2]+idxSS),frequency)
                              ,collapse = '-'),
                            '.\nCheck equation computability or lagged time series span.'
                )) 
              
              #append results 
              simulation[[tempName]]=rbind(simulation[[tempName]],tmpResCheck)  
              
            }#for in vendog
            
          },error=function(err){
            stop(paste0(err$message))
          }
        ) 
          
       }#hasleads identities
        
        if (hasLeads)
        {
          if (MULTMATRIX)
          {.MODEL_outputText(outputText=!quietly,paste0('\r','Multiplier Matrix:'),sprintf("%10.2f",100),'%') 
          } else if (STOCHSIMULATE)
          {.MODEL_outputText(outputText=!quietly,paste0('\r','Stochastic Simulation:'),sprintf("%10.2f",100),'%') 
          } else if (OPTIMIZE)
          {.MODEL_outputText(outputText=!quietly,paste0('\r','Optimize:'),sprintf("%10.2f",100),'%') 
          } else
          {.MODEL_outputText(outputText=!quietly,paste0('\r','Simulation:'),sprintf("%10.2f",100),'%') }
          
        }
       
        if (verbose && verboseSincePeriod<=simIterLoopIdx && length(verboseVars)>0)
        {
          .printVerboseSim('RESCHECK',
                           ConstantAdjustment,
                           verboseVars,
                           frequency,
                           TSRANGE,
                           simSteps,
                           simIterLoopIdx,
                           1,
                           0,
                           model_max_lag_1,
                           length_model_vexog,
                           model_vendog,
                           model_vexog,
                           replica,
                           localE,
                           model_vpre,
                           model_vsim,
                           model_vpost,
                           hasLeads)
        }
        
      }#end is RESCHECK
      
      # convergence loop ------------------------------------------
      
      if (simType != 'RESCHECK') 
      {
        #if FORECAST is requested, value in first iter is set equal to the previously simulated value
        #(in first period of TSRANGE we set endo equal to his previous historical value)
        if (simType=='FORECAST' && length(model_vendog_no_if)>0) 
          for (idxI in 1:length(model_vendog_no_if))
          {
            tempName = (model_vendog_no_if)[idxI]
            
            #if any error we save current vendog name to be printed out in messages
            localE$LAST__EVAL__EQ=tempName
            
            #NEEDED...deal with exogenization...
            #model_vendog_no_if are endo with no IF... but still can be exogenized
            if ((simIterLoopIdx %in% Exogenize[[tempName]])) next 
            
            #get vendog values
            tmpValues=localE[[tempName]]
            
            #forecast in first period needs previous historical data that can be missing
            if (simIterLoopIdx==1 && any(! is.finite(tmpValues[model_max_lag,] )))
              stop(callerName,'cannot use FORECAST with endogenous variable "',tempName,'" that has a missing value in its last historical observation prior to TSRANGE.')
            
            #copy previous simulated (historical if first period) to current
            tmpValues[  model_max_lag_1,]=tmpValues[model_max_lag,,drop=F] 
            
            #update time series
            localE[[tempName]]=tmpValues
            
          }
      
        #iterations index
        idxIter=0 
        
        # eval vpre ------------------------------
         
        #eval vpre expression
        eval(vpre_expressions[[simIterLoopIdx]]
             ,envir=localE
        ) 
        
        #print stuff if required
        if (verbose && verboseSincePeriod<=simIterLoopIdx  && length(verboseVars)>0)
        {
            .printVerboseSim('VPRE',
                             ConstantAdjustment,
                             verboseVars,
                             frequency,
                             TSRANGE,
                             simSteps,
                             simIterLoopIdx,
                             idxB,
                             idxIter,
                             model_max_lag_1,
                             length_model_vexog,
                             model_vendog,
                             model_vexog,
                             replica,
                             localE,
                             model_vpre,
                             model_vsim,
                             model_vpost,
                             hasLeads)
          }  
       
        #flag for convergence
        flagConvergence=1
        
        localE$FC__=flagConvergence
         
        if (length_model_vblocks>0) for (idxB in 1:length_model_vblocks)
        {
          if (hasLeads)
          {
            if (MULTMATRIX)
            {.MODEL_outputText(outputText=!quietly,paste0('\r','Multiplier Matrix:'),sprintf("%10.2f",(idxB-1)*100/length_model_vblocks),'%') 
            } else if (STOCHSIMULATE)
            {.MODEL_outputText(outputText=!quietly,paste0('\r','Stochastic Simulation:'),sprintf("%10.2f",(idxB-1)*100/length_model_vblocks),'%') 
            } else if (OPTIMIZE)
            {.MODEL_outputText(outputText=!quietly,paste0('\r','Optimize:'),sprintf("%10.2f",(idxB-1)*100/length_model_vblocks),'%') 
            } else
            {.MODEL_outputText(outputText=!quietly,paste0('\r','Simulation:'),sprintf("%10.2f",(idxB-1)*100/length_model_vblocks),'%') }
          }
          
          localE$IDXB__=idxB
         
          if ( length_model_vsim[idxB]>0 )
          {
            flagConvergence=0
            
            localE$FC__=flagConvergence
           
            with(localE,{
                
              MODEL__VFEED=MODEL__VFEED__LIST[[IDXB__]]
               
              LENGTH__MODEL__VFEED=LENGTH__MODEL__VFEED__LIST[IDXB__]
             
              #new feedback values
              VFEED__NEXT=matrix(ncol=REPLICA__,nrow=LENGTH__MODEL__VFEED)
                
              #current feedback values
              VFEED__CURRENT=VFEED__NEXT
                
              #temp feedback values
              VFEED__TEMP=VFEED__NEXT
            
              for (IDXE__ in 1:LENGTH__MODEL__VFEED)
              {
                #init feedback proxy with historical values (we need it for convergence test)
                #during iterations we assign VFEED_CURRENT=VFEED_NEXT
                VFEED__NEXT[IDXE__,]=localE[[MODEL__VFEED[IDXE__]]][MODEL__MAX__LAG__1,,drop=F]
              }
            })
            
            # init jacobian  ------------------------------
            
            if (simAlgo!='GAUSS-SEIDEL'  )
            {
              #proxy var to localE
              localE$SIM__ITER__LOOP__IDX=simIterLoopIdx
              
              with(localE,{
              
                #reset jacobian
                if (!  RECALCULATE__JACOBIAN__ARRAY[IDXB__])
                {
                  if (SIM__ALGO=='NEWTON')
                  {
                    JACOBIAN__SINGLE__MATRIX__ACTIVE=JACOBIAN__SINGLE__MATRIX__ACTIVE__LIST[[IDXB__]]
                    
                  } else{#FULLNEWTON
                    
                    JACOBIAN__FULL__MATRIX__ACTIVE=JACOBIAN__FULL__MATRIX__ACTIVE__LIST[[IDXB__]]
                  }
                }
                
                JACOBIAN__SCALE__VALUE=JACOBIAN__SCALE__VALUE__LIST[[IDXB__]]
                
                #jacobian base line
                JACOBIAN__VFEED__BASELINE=matrix(ncol=REPLICA__,nrow=LENGTH__MODEL__VFEED)
                
                JACOBIAN__VFEED__ACTIVE=JACOBIAN__VFEED__ACTIVE__LIST[[IDXB__]]
                
                #init performance parameter
                JACOBIAN__RELAX__VALUE=1
                JACOBIAN__DMAX__CURRENT=0
                JACOBIAN__DMAX__PREV=0
                JACOBIAN__RATIO__CURRENT=0.5
                JACOBIAN__RATIO__PREV=0
                JACOBIAN__GMRRP__=0
                
                #check there are active vfeed
                VFEED__ACTIVE__JACOBIAN__IDXS=which(JACOBIAN__VFEED__ACTIVE[,SIM__ITER__LOOP__IDX]==1)
                LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS=length(VFEED__ACTIVE__JACOBIAN__IDXS)
                
                if (LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS==0)
                {
                  .MODEL_outputText(outputText=verbose,paste0('\n',callerName,'warning, a "NEWTON" algorithm is requested but all feedback variables have been exogenized or dropped from the Jacobian matrix in simulation period ',SIM__ITER__LOOP__IDX,', year-period ',paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+SIM__ITER__LOOP__IDX-1),f=frequency),collapse='-'),', block ',IDXB__,'.\n')) 
                  
                } else {
                  
                  #check if a new Jacobian is required due to new vfeed exogenizations in current period
                  if(SIM__ITER__LOOP__IDX>1 && 
                     any(xor(JACOBIAN__VFEED__ACTIVE[,SIM__ITER__LOOP__IDX],JACOBIAN__VFEED__ACTIVE[,SIM__ITER__LOOP__IDX-1]))
                  )
                  {
                    RECALCULATE__JACOBIAN__ARRAY[IDXB__]=TRUE
                  }
                }
                
              })#end with localE
              
            }#end init jacobian
            
            # eval vsim ------------------------------
            
            #main sim stuff... cycle till convergence or max iter limit
            #WARNING: THIS FOR-CYCLE MUST BE SUPER FAST
            
            vsim_expressions_local=vsim_expressions[[simIterLoopIdx]][[idxB]]
            for (idxIter in 1:simIterLimit)
            { 
              #eval vsim expressions
              eval( vsim_expressions_local,envir=localE) 
              
              if (simAlgo!='GAUSS-SEIDEL')
              {
                #proxy vars in localE
                localE$IDX__ITER=idxIter
               
                with(localE,{
                  
                  if (LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS > 0)
                  {
                    # evaluate convergence speed --------------------------
                    
                    #backup current convergence speed indicators
                    JACOBIAN__DMAX__PREV=JACOBIAN__DMAX__CURRENT
                    JACOBIAN__DMAX__CURRENT=max(abs(VFEED__NEXT-VFEED__CURRENT))
                    JACOBIAN__RATIO__PREV=JACOBIAN__RATIO__CURRENT
                    
                    if (SIM__ALGO=='NEWTON')
                    {
                      GMRRP__LIMIT__1=0.9
                      GMRRP__LIMIT__2=0.75
                      
                    } else{#FULLNEWTON
                      
                      GMRRP__LIMIT__1=0.6
                      GMRRP__LIMIT__2=0.45
                    }
                   
                    if (IDX__ITER>1)
                    {
                      #recalculate convergence speed indicators
                      if (JACOBIAN__DMAX__PREV>0) JACOBIAN__RATIO__CURRENT=JACOBIAN__DMAX__CURRENT/JACOBIAN__DMAX__PREV
                      
                      JACOBIAN__GMRRP__=sqrt(JACOBIAN__RATIO__CURRENT*JACOBIAN__RATIO__PREV)
                      
                      if (! is.finite(JACOBIAN__GMRRP__)) stop('No convergence, due to numeric overflow in Newton optimization in block ',IDXB__,'.')
                      
                      #convergence evaluation
                      if (JACOBIAN__GMRRP__ > GMRRP__LIMIT__1)
                      {
                        #convergence is too slow, we need a new Jacobian
                        RECALCULATE__JACOBIAN__ARRAY[IDXB__]=TRUE
                        
                      } else if(JACOBIAN__GMRRP__ > GMRRP__LIMIT__2)
                      {
                        #convergence is slow, we try to relax vfeed updates
                        JACOBIAN__RELAX__VALUE=max(JACOBIAN__RELAX__VALUE*0.8,0.5)
                      }
                    }
                     
                    # build jacobian ---------------------------------------
                    
                    if (RECALCULATE__JACOBIAN__ARRAY[IDXB__]) {
                      
                      #reset jacobian
                      if (SIM__ALGO=='NEWTON')
                      {
                        .MODEL_outputText(outputText=verbose,paste0('\n',callerName,'building a new Jacobian matrix (',LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS,'x',LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS,') in ',ifelse(hasLeads,'',paste0('simulation period ',simIterLoopIdx,', year-period ',paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=frequency),collapse='-'),', ')),'block ',IDXB__,', iteration ',IDX__ITER,'.')) 
                        JACOBIAN__SINGLE__MATRIX=matrix(ncol=LENGTH__MODEL__VFEED,nrow=LENGTH__MODEL__VFEED)
                        
                      } else{#FULLNEWTON
                        
                        .MODEL_outputText(outputText=verbose,paste0('\n',callerName,'building a new Jacobian array (',LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS,'x',LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS,'x',REPLICA__,') in ',ifelse(hasLeads,'',paste0('simulation period ',simIterLoopIdx,', year-period ',paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=frequency),collapse='-'),', ')),'block ',IDXB__,', iteration ',IDX__ITER,'.')) 
                        
                        JACOBIAN__FULL__MATRIX=array(dim=c(LENGTH__MODEL__VFEED,LENGTH__MODEL__VFEED,REPLICA__))
                      }
                      
                      #backup vfeed current 
                      VFEED__CURRENT__BACKUP=VFEED__CURRENT 
                      
                      #log operations in case of error
                      LAST__EVAL__EQ='Jacobian Base Solution'
                      
                      #store baseline vfeed value (atm we already have performed one vsim evaluation)
                      JACOBIAN__VFEED__BASELINE=VFEED__NEXT
                      
                      #restore ALL vfeed value
                      #(ALL vfeed changed during last vsim evaluation so ALL must be restored)
                      for (IDX__ in 1:LENGTH__MODEL__VFEED)
                      {
                        localE[[MODEL__VFEED[IDX__]]][MODEL__MAX__LAG__1,]=VFEED__CURRENT__BACKUP[IDX__,]
                      }
                      
                      #cycle to vfeed, shock them one by one, re-calculate vsim, then store derivative in jacobian column
                      for (IDX__ in 1:LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS)
                      {  
                        #calculate local shock
                        TEMP__DELTA=localE[[MODEL__VFEED[VFEED__ACTIVE__JACOBIAN__IDXS[IDX__]]]][MODEL__MAX__LAG__1,]*JACOBIAN__SHOCK
                        TEMP__DELTA[which(abs(TEMP__DELTA)<JACOBIAN__SHOCK)]=JACOBIAN__SHOCK
                        
                        #shock vfeed IDX__
                        localE[[MODEL__VFEED[VFEED__ACTIVE__JACOBIAN__IDXS[IDX__]]]][MODEL__MAX__LAG__1,]=
                          TEMP__DELTA+localE[[MODEL__VFEED[VFEED__ACTIVE__JACOBIAN__IDXS[IDX__]]]][MODEL__MAX__LAG__1,]
                        
                        #calculate shocked vsim model
                        eval( vsim_expressions_local ) 
                        
                        #log operations in case of error
                        LAST__EVAL__EQ='Jacobian Shocked Solution'
                        
                        #calculate single-jacobian single column (note the 1s on the column indexes)
                        if (SIM__ALGO=='NEWTON')
                        { 
                          JACOBIAN__SINGLE__MATRIX[VFEED__ACTIVE__JACOBIAN__IDXS,VFEED__ACTIVE__JACOBIAN__IDXS[IDX__]] = 
                          - (
                            VFEED__NEXT[VFEED__ACTIVE__JACOBIAN__IDXS,1,drop=F]-
                              JACOBIAN__VFEED__BASELINE[VFEED__ACTIVE__JACOBIAN__IDXS,1,drop=F]
                          )/
                          TEMP__DELTA[1,drop=F]
                          
                          #deal with principal diagonal
                          JACOBIAN__SINGLE__MATRIX[VFEED__ACTIVE__JACOBIAN__IDXS[IDX__],VFEED__ACTIVE__JACOBIAN__IDXS[IDX__]] = 
                            1 + JACOBIAN__SINGLE__MATRIX[VFEED__ACTIVE__JACOBIAN__IDXS[IDX__],VFEED__ACTIVE__JACOBIAN__IDXS[IDX__]]
                          
                        } else {#FULLNEWTON
                          
                          #calculate derivatives for all replicas
                          TEMP__MATRIX =  - (
                              VFEED__NEXT[VFEED__ACTIVE__JACOBIAN__IDXS,,drop=F]-
                                JACOBIAN__VFEED__BASELINE[VFEED__ACTIVE__JACOBIAN__IDXS,,drop=F]
                            )/
                            matrix(TEMP__DELTA[,drop=F],nrow=LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS,ncol=REPLICA__,byrow=T)
                          
                          for (IDX2__ in 1:REPLICA__)
                          {
                            #insert values in 3d array JACOBIAN__FULL__MATRIX
                            JACOBIAN__FULL__MATRIX[VFEED__ACTIVE__JACOBIAN__IDXS,VFEED__ACTIVE__JACOBIAN__IDXS[IDX__],IDX2__]=
                            TEMP__MATRIX[,IDX2__,drop=F]
                          
                            #deal with principal diagonal
                            JACOBIAN__FULL__MATRIX[VFEED__ACTIVE__JACOBIAN__IDXS[IDX__],VFEED__ACTIVE__JACOBIAN__IDXS[IDX__],IDX2__] = 
                              1 + JACOBIAN__FULL__MATRIX[VFEED__ACTIVE__JACOBIAN__IDXS[IDX__],VFEED__ACTIVE__JACOBIAN__IDXS[IDX__],IDX2__]
                          }
                          
                        } #end FULLNEWTON
                       
                        #restore ALL vfeed value
                        for (IDX2__ in 1:LENGTH__MODEL__VFEED)
                        {
                          localE[[MODEL__VFEED[IDX2__]]][ MODEL__MAX__LAG__1,]=VFEED__CURRENT__BACKUP[IDX2__,]
                        }
                      }
                      
                      #restore VFEED__NEXT to old VFEED__CURRENT in order to run again vsim
                      #(in vsim we init VFEED__CURRENT=VFEED__NEXT)
                      VFEED__NEXT=VFEED__CURRENT__BACKUP
                      
                      #recalculate current vsim
                      #(we need to restore also all vsim not in vfeed)
                      eval(vsim_expressions_local) 
                      
                      #log operations in case of error
                      LAST__EVAL__EQ='Jacobian Scaling'
                      
                      if (SIM__ALGO=='NEWTON')
                      {
                        #extract active vfeed
                        JACOBIAN__SINGLE__MATRIX__ACTIVE=JACOBIAN__SINGLE__MATRIX[VFEED__ACTIVE__JACOBIAN__IDXS,VFEED__ACTIVE__JACOBIAN__IDXS,drop=F]
                        
                        #scale jacobian with current scale value
                        for (IDX1__ in 1:LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS)
                          for (IDX2__ in 1:LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS)
                          {
                            JACOBIAN__SINGLE__MATRIX__ACTIVE[IDX1__,IDX2__]=JACOBIAN__SINGLE__MATRIX__ACTIVE[IDX1__,IDX2__]*
                              JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS[IDX2__],1]/
                              JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__],1]
                          }
                        
                        #check singularities and calculate new scale values
                        #we try max 10 times to normalize jacobian
                        for (IDX__ in 1:10)
                        {
                          DONE__=TRUE
                          
                          for (IDX1__ in 1:LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS)
                          {
                            SROW__=sum(JACOBIAN__SINGLE__MATRIX__ACTIVE[IDX1__,]^2)
                            SCOL__=sum(JACOBIAN__SINGLE__MATRIX__ACTIVE[,IDX1__]^2)
                            
                            if (!(is.finite(SROW__) && is.finite(SCOL__))) stop('No convergence, due to numeric overflow in Jacobian matrix scaling.')
                            
                            if (SROW__ < 1e-6 || SCOL__ < 1e-6) 
                              stop(paste0('Jacobian matrix is singular in the variable "',
                                          MODEL__VFEED[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__]],'". Try to drop "',strsplit(MODEL__VFEED[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__]],'__LEAD__')[[1]][1],
                                          '" variable from the Jacobian matrix by using the "JacobianDrop" argument.'))
                            
                            FAC__=sqrt(sqrt(SROW__ / SCOL__))
                            
                            if (FAC__ < 0.5 || FAC__ > 2)
                            {
                              DONE__=FALSE
                              
                              JACOBIAN__SINGLE__MATRIX__ACTIVE[IDX1__,]=JACOBIAN__SINGLE__MATRIX__ACTIVE[IDX1__,]/FAC__
                              JACOBIAN__SINGLE__MATRIX__ACTIVE[,IDX1__]=JACOBIAN__SINGLE__MATRIX__ACTIVE[,IDX1__]*FAC__
                              
                              JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__],]=JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__],1]*FAC__
                              
                              if (any(! is.finite(JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__],])))
                                stop('No convergence, due to numeric overflow in Jacobian matrix scaling.')
                            }
                            
                          }#end cycle in active vfeed
                          
                          if (DONE__) break
                          
                        }#end 10 tries
                        
                        if (IDX__==10) 
                          .MODEL_outputText(outputText=verbose,paste0('\n',callerName,'warning, 10 iterations not sufficent to normalize the Jacobian matrix in',ifelse(hasLeads,'',paste0(' simulation period ',simIterLoopIdx,', year-period ',paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=frequency),collapse='-'),',')),' block ', IDXB__ ,', iteration ',IDX__ITER,'.\n')) 
                      
                      } else {#FULLNEWTON scaling
                        
                        #extract active vfeed
                        JACOBIAN__FULL__MATRIX__ACTIVE=JACOBIAN__FULL__MATRIX[VFEED__ACTIVE__JACOBIAN__IDXS,VFEED__ACTIVE__JACOBIAN__IDXS,,drop=F]
                        
                        #scale jacobian with current scale value
                        for (IDX3__ in 1:REPLICA__)
                          for (IDX1__ in 1:LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS)
                            for (IDX2__ in 1:LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS)
                            {
                                JACOBIAN__FULL__MATRIX__ACTIVE[IDX1__,IDX2__,IDX3__]=JACOBIAN__FULL__MATRIX__ACTIVE[IDX1__,IDX2__,IDX3__]*
                                JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS[IDX2__],IDX3__]/
                                JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__],IDX3__]
                            }
                        
                        #check singularities and calculate new scale values
                        #we try max 10 times to normalize jacobian
                        for (IDX__ in 1:10)
                        {
                          DONE__=TRUE
                          
                          for (IDX3__ in 1:REPLICA__)
                          for (IDX1__ in 1:LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS)
                          {
                            SROW__=sum(JACOBIAN__FULL__MATRIX__ACTIVE[IDX1__,,IDX3__]^2)
                            SCOL__=sum(JACOBIAN__FULL__MATRIX__ACTIVE[,IDX1__,IDX3__]^2)
                            
                            if (!(is.finite(SROW__) && is.finite(SCOL__))) stop('No convergence, due to numeric overflow in Jacobian array scaling.')
                            
                            if (SROW__ < 1e-6 || SCOL__ < 1e-6) 
                              stop(paste0('Jacobian array is singular in the variable "',
                                          MODEL__VFEED[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__]],'". Try to drop "',strsplit(MODEL__VFEED[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__]],'__LEAD__')[[1]][1],
                                          '" variable from the Jacobian array by using the "JacobianDrop" argument.'))
                            
                            FAC__=sqrt(sqrt(SROW__ / SCOL__))
                            
                            if (FAC__ < 0.5 || FAC__ > 2)
                            {
                              DONE__=FALSE
                              
                              JACOBIAN__FULL__MATRIX__ACTIVE[IDX1__,,IDX3__]=JACOBIAN__FULL__MATRIX__ACTIVE[IDX1__,,IDX3__]/FAC__
                              JACOBIAN__FULL__MATRIX__ACTIVE[,IDX1__,IDX3__]=JACOBIAN__FULL__MATRIX__ACTIVE[,IDX1__,IDX3__]*FAC__
                              
                              JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__],IDX3__]=JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__],IDX3__]*FAC__
                              
                              if (any(! is.finite(JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS[IDX1__],IDX3__])))
                                stop('No convergence, due to numeric overflow in Jacobian array scaling.')
                            }
                            
                          }#end cycle in active vfeed
                          
                          if (DONE__) break
                          
                        }#end 10 tries
                        
                      }
                      
                      RECALCULATE__JACOBIAN__ARRAY[IDXB__]=FALSE
                      
                    }
                    
                    # eval Jacobian ----------------------------------------
                    
                    #log operations in case of error
                    LAST__EVAL__EQ='Jacobian Inversion'
                    
                    if (SIM__ALGO=='NEWTON')
                    { 
                      #solve(J,X) works well also with replicas, given J a single Jacobian matrix and X a matrix(vfeed,replica) 
                      VFEED__NEXT[VFEED__ACTIVE__JACOBIAN__IDXS,]=solve(JACOBIAN__SINGLE__MATRIX__ACTIVE,
                                                                            (VFEED__NEXT[VFEED__ACTIVE__JACOBIAN__IDXS,,drop=F]-VFEED__CURRENT[VFEED__ACTIVE__JACOBIAN__IDXS,,drop=F])/
                                                                            JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS,,drop=F]
                                                                        )*JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS,,drop=F]*
                                                                  JACOBIAN__RELAX__VALUE+
                                                                  VFEED__CURRENT[VFEED__ACTIVE__JACOBIAN__IDXS,,drop=F]
                      
                    } else { #FULLNEWTON
                    
                      #must avoid drop=T in jacobian array in solve  
                      for (IDX__ in 1:REPLICA__)
                        VFEED__NEXT[VFEED__ACTIVE__JACOBIAN__IDXS,IDX__]=solve(JACOBIAN__FULL__MATRIX__ACTIVE[,,IDX__],
                                                                                  (VFEED__NEXT[VFEED__ACTIVE__JACOBIAN__IDXS,IDX__,drop=F]-VFEED__CURRENT[VFEED__ACTIVE__JACOBIAN__IDXS,IDX__,drop=F])/
                                                                                  JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS,IDX__,drop=F]
                                                                              )*JACOBIAN__SCALE__VALUE[VFEED__ACTIVE__JACOBIAN__IDXS,IDX__]*
                                                                                JACOBIAN__RELAX__VALUE+
                                                                                VFEED__CURRENT[VFEED__ACTIVE__JACOBIAN__IDXS,IDX__]
                        
                    }  
                    
                    if (any(!is.finite(VFEED__NEXT[VFEED__ACTIVE__JACOBIAN__IDXS,]))) stop('No convergence, due to non-finite solution in Jacobian feedback variables update.')
                    
                    #update active vfeed
                    for (IDX__ in 1:LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS)
                    {
                      localE[[MODEL__VFEED[VFEED__ACTIVE__JACOBIAN__IDXS[IDX__]]]][MODEL__MAX__LAG__1,]=VFEED__NEXT[VFEED__ACTIVE__JACOBIAN__IDXS[IDX__],]
                    }
                    
                  }#end length vfeed active
                  
                })#end with localE
                
              }
              
              if (verbose && verboseSincePeriod<=simIterLoopIdx && length(verboseVars)>0)
              {
                  .printVerboseSim('VSIM',
                                   ConstantAdjustment,
                                   verboseVars,
                                   frequency,
                                   TSRANGE,
                                   simSteps,
                                   simIterLoopIdx,
                                   idxB,
                                   idxIter,
                                   model_max_lag_1,
                                   length_model_vexog,
                                   model_vendog,
                                   model_vexog,
                                   replica,
                                   localE,
                                   model_vpre,
                                   model_vsim,
                                   model_vpost,
                                   hasLeads)
                
              }
              
              #eval convergence expression 
              with(localE,{
                
                TMPV__=abs(VFEED__CURRENT)
                TMPV__[which(TMPV__<1)]=1
                
              if (max(
                         abs((VFEED__NEXT - VFEED__CURRENT))
                         /TMPV__ 
                       ) < CONVERGENCE__VALUE
                  )
                    {
                      #if convergence...exit loop  
                      FC__=1
                    }
              
              })
              
              if (localE$FC__==1)
              { 
                flagConvergence=1
                break
              }
              
            }#main convergence vsim for
            
            #store jacobians proxies
            with(localE,{
              
              if ((SIM__ALGO!='GAUSS-SEIDEL') && (LENGTH__VFEED__ACTIVE__JACOBIAN__IDXS > 0)  )
              {
                if (SIM__ALGO=='NEWTON')
                {
                  JACOBIAN__SINGLE__MATRIX__ACTIVE__LIST[[IDXB__]]=JACOBIAN__SINGLE__MATRIX__ACTIVE
                  
                } else{#FULLNEWTON
                  
                  JACOBIAN__FULL__MATRIX__ACTIVE__LIST[[IDXB__]]=JACOBIAN__FULL__MATRIX__ACTIVE
                }
                
                JACOBIAN__SCALE__VALUE__LIST[[IDXB__]]=JACOBIAN__SCALE__VALUE
              }
              
            })
            
          }# length vsim > 0
          
          # eval vpost ------------------------------
          
          if ( length_model_vpost[idxB]>0 )
          {
            #eval vpost expressions
            eval(vpost_expressions[[simIterLoopIdx]][[idxB]],envir=localE) 
            
            if (verbose && verboseSincePeriod<=simIterLoopIdx && length(verboseVars)>0)
            {
                .printVerboseSim('VPOST',
                                 ConstantAdjustment,
                                 verboseVars,
                                 frequency,
                                 TSRANGE,
                                 simSteps,
                                 simIterLoopIdx,
                                 idxB,
                                 idxIter,
                                 model_max_lag_1,
                                 length_model_vexog,
                                 model_vendog,
                                 model_vexog,
                                 replica,
                                 localE,
                                 model_vpre,
                                 model_vsim,
                                 model_vpost,
                                 hasLeads)
              
            }
          }
          
          #...messages
          if (flagConvergence==1) 
          {
            if (verbose) 
            {
              if (hasLeads) 
              {
                .MODEL_outputText(outputText = !quietly,paste0('\n',callerName,'convergence reached in block ', idxB, ', iteration ',idxIter,'.\n')) 
              }
              else {
                .MODEL_outputText(outputText = !quietly,paste0('\n',callerName,'simulation period ',simIterLoopIdx,', year-period ',
                           paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=frequency),collapse='-'),', convergence reached in block ', idxB, ', iteration ',idxIter,'.\n')) 
              }
            }
              
          } else
          {
            if (hasLeads) 
            {
              .MODEL_outputText(outputText = !quietly,paste0('\n',callerName,'warning, in block ',idxB,' no convergence in ',idxIter,' iterations.\n')) 
             }
            else {
              .MODEL_outputText(outputText = !quietly,paste0('\n',callerName,'warning, in simulation period ',simIterLoopIdx,', year-period ',
                                                             paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=frequency),collapse='-'),', block ',idxB,' no convergence in ',idxIter,' iterations.\n')) 
            } 
          }
          
        }#end vblocks for
        
        if (hasLeads)
        {
          if (MULTMATRIX)
          {.MODEL_outputText(outputText=!quietly,paste0('\r','Multiplier Matrix:'),sprintf("%10.2f",100),'%') 
          } else if (STOCHSIMULATE)
          {.MODEL_outputText(outputText=!quietly,paste0('\r','Stochastic Simulation:'),sprintf("%10.2f",100),'%') 
          } else if (OPTIMIZE)
          {.MODEL_outputText(outputText=!quietly,paste0('\r','Optimize:'),sprintf("%10.2f",100),'%') 
          } else
          {.MODEL_outputText(outputText=!quietly,paste0('\r','Simulation:'),sprintf("%10.2f",100),'%') }
          
        }
       
        #store simulated values
        tmpIdx=simIterLoopIdx+model_max_lag 
        
        #actual == original + leaded vendog
        actual_model_vendog=model_vendog
        
        if (hasLeads && (simSteps>1))
          for (idxSS in 1:(simSteps-1))
            actual_model_vendog=c(actual_model_vendog,paste0(model_vendog,'__LEAD__',idxSS))
        
        #export to simulation list
        for (idxI in 1:length(actual_model_vendog))
        { 
          currentVendog = (actual_model_vendog)[idxI]
          
          #cycle in endogenous
          
          #get simulated values
          tmpValue=localE[[currentVendog]][model_max_lag_1,,drop=F] 
          
          #store in simulation list (each row is a sim period)
          simulation[[currentVendog]]=rbind(simulation[[currentVendog]],tmpValue) 
          
          #if dynamic or forecast sim type is requested
          #then store simulated values also as lagged values in ORIGINAL vendogs
          if (!hasLeads)
          if (simType=='DYNAMIC' || simType=='FORECAST')
          { 
            #get ORIGINAL values
            tmpArr=localE[[paste0(currentVendog,'__ORIGINAL__EXTENDED')]]
            
            #insert simulated value
            tmpArr[tmpIdx,] = tmpValue 
            
            #store in ORIGNAL for next iterations
            localE[[paste0(currentVendog,'__ORIGINAL__EXTENDED')]]=tmpArr
          }
          
        }#end for save simulate observation
      
      }#not RESCHECK
      
      # deal with optimize function -------------------------------------
      
      if (OPTIMIZE)
      {
        #time series of OPTIMIZE FUNs results
        #must be initialized with NA
        #zeros values could provide misleading information
        optFunTSRow=rep(NA,replica-1) 
        
        #deal with no lead case
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
                for (idxI in 1:length(simulation))
                {
                  idxS=names(simulation)[idxI]
                  #get original endo in localE
                  tempM=localE[[idxS]]
                 
                  
                  #check simulated obs are coherent 
                  if ( (simIterLoopIdx != dim(simulation[[idxS]])[1]) ||
                       (dim(tempM)[2] != dim(simulation[[idxS]])[2] ))
                    stop('error in adjusting localE in RESCHECK.')
                  
                  #modify endo obs with simulated
                  tempM[(model_max_lag+2-simIterLoopIdx)  : (  model_max_lag_1),]=simulation[[idxS]]
                  
                  #assign to localE
                  localE[[idxS]]=tempM
                }
              }
             
              #eval OPTIMIZ FUNCTION idxIE in current simIterLoopIdx period
              localE$LAST__EVAL__EQ=paste0('OptimizeFunctions$',names(OptimizeFunctions)[idxIE])
              currentFunctionResults=eval(OptimizeFunctions[[idxIE]]$funExpr,envir=localE)
              
              if (length(currentFunctionResults)==0) stop('function must be computable.')
              
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
          
        }#end cycle opt funs
        
        #deal with lead cases
        if (hasLeads && simSteps>1)
        {
          for (idxSS in 1:(simSteps-1))
          {
            #init leaded optimize function results
            assign(paste0('optFunTSRow__LEAD__',idxSS),rep(NA,replica-1))
          }
          
          for (idxIE in 1:length(OptimizeFunctions))
          {
            eqInExprText=as.character(OptimizeFunctions[[idxIE]]$funExpr)
            
            leadedFunExprsTemp=lapply(.leadEqSim(names(OptimizeFunctions)[idxIE],
                                          eqInExprText,
                                          model_max_lag_1,
                                         simSteps
                                          ),
                                      function(x) parse(text=x))
            
            for (idxSS in 1:(simSteps-1))
            {
              #idxSS period of simulation tsrange is in optimize function tsrange
              if ((model_max_lag+idxSS+1) %in% OptimizeFunctions[[idxIE]]$TSRANGE)
              {
                
                tryCatch({
                  
                #in RESCHECK localE does not contain simulated ts
                if (simType=='RESCHECK')
                {
                  #backup env
                  localE_BK=localE
                  
                  #store simulated in localE
                  for (idxI in 1:length(simulation))
                  {
                    idxS=names(simulation)[idxI]
                    
                    #get original endo in localE
                    tempM=localE[[idxS]]
                    
                    #check simulated obs are coherent 
                    if ( (simIterLoopIdx != dim(simulation[[idxS]])[1]) ||
                         (dim(tempM)[2] != dim(simulation[[idxS]])[2] ))
                      stop('error in adjusting localE in RESCHECK.')
                    
                    #modify endo obs with simulated
                    tempM[(model_max_lag+2-simIterLoopIdx)  : (  model_max_lag_1),]=simulation[[idxS]]
                    
                    #assign to localE
                    localE[[idxS]]=tempM
                  }
                }
                
                #get name of leaded opt fun
                tempName=paste0(names(OptimizeFunctions)[idxIE],'__LEAD__',idxSS)
                
                #eval OPTIMIZ FUNCTION idxIE in current simIterLoopIdx period
                localE$LAST__EVAL__EQ=paste0('OptimizeFunctions$',tempName)
                currentFunctionResults=eval(leadedFunExprsTemp[[tempName]],envir=localE)
                
                if (length(currentFunctionResults)==0 ) stop('function must be computable.')
                
                if (length(optFunResults) != length(currentFunctionResults[-1])) stop('unknown error.')
                
                #first unperturbed column must be removed
                optFunResults=optFunResults + currentFunctionResults[-1]
                assign(paste0('optFunTSRow__LEAD__',idxSS),currentFunctionResults[-1])
                
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
              }
            }
          }
        }#hasLeads
        
        #build OPTIMIZE FUNs ts by rbind
        optFunTSMM=rbind(optFunTSMM,optFunTSRow)
        
        #build TSMM in lead cases
        if (hasLeads && simSteps>1)
        {
          for (idxSS in 1:(simSteps-1))
          {
            optFunTSMM=rbind(optFunTSMM,get(paste0('optFunTSRow__LEAD__',idxSS)))
          }
        }
        
        rownames(optFunTSMM)=NULL
        
      }#end if optimize
       
    }#main cycle
    
    # simulate error message -----------------------------------------------    
    
  },error=function(err)
  {
    #intercept sim algo errors
    
    if (!exists('idxIter')) idxIter=0 
    if (!exists('idxB')) idxB=0 
    if (!exists('simIterLoopIdx')) simIterLoopIdx=1 
    if (verbose && length(verboseVars)>0) 
    {
      .printVerboseSim('ERROR',
                       ConstantAdjustment,
                       verboseVars,
                       frequency,
                       TSRANGE,
                       simSteps,
                       simIterLoopIdx,
                       idxB,
                       idxIter,
                       model_max_lag_1,
                       length_model_vexog,
                       model_vendog,
                       model_vexog,
                       replica,
                       localE,
                       model_vpre,
                       model_vsim,
                       model_vpost,
                       hasLeads)
    }
    
    if (hasLeads && simSteps>1)
    {
      #if error and leads extract varName from varName_LEAD_x
      if (grepl('__LEAD__',localE$LAST__EVAL__EQ))
      {
        tempSplit=strsplit(localE$LAST__EVAL__EQ,'__LEAD__')[[1]]
        lastEqLogOut=tempSplit[1]
        simIterLoopIdx=as.integer(tempSplit[2])+1
      } else {
        lastEqLogOut=localE$LAST__EVAL__EQ
        simIterLoopIdx=1
      }
       
    } else {
      
      simIterLogOut=simIterLoopIdx
      lastEqLogOut=localE$LAST__EVAL__EQ
      
    }
    
    stop(paste0('\n',callerName,'error in simulation of type "',simType,'", ','simulation period ',simIterLoopIdx,', year-period ',
                paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]+simIterLoopIdx-1),f=frequency),collapse='-'),
                ', ','block ', idxB ,', iteration ',idxIter,', while evaluating "',
                lastEqLogOut,'".\n',err$message,
                ifelse(quietly==FALSE,'','\nHint: disable "quietly" in order to get insights on lagged missings that could impact on computation.'),
                ifelse(simType !='RESCHECK','\nHint: try a "RESCHECK" simulation in order to check equations definition and computability.',''),
                ifelse(replica==1 && simType !='RESCHECK' && simAlgo =='GAUSS-SEIDEL','\nHint: try the "NEWTON" simulation algorithm.',''),
                ifelse(verbose==TRUE,'','\nHint: enable "verbose" in order to get more details.'),
                ifelse(STOCHSIMULATE,'\nHint: in STOCHSIMULATE() each realization must be computable in order to avoid errors.',''),
                ifelse(OPTIMIZE,'\nHint: in OPTIMIZE() each realization must be computable in order to avoid errors.','')
    ))
  }) 

  # post SIMULATE -------------------------------------------------------------
 
  #deal with lead rbind
  if (length(simulation)>0)
  {
    #row bind leaded variables back to original one
    if (hasLeads && simSteps>1)
    {
      for ( idxI in 1:length(model_vendog))
      {
        vendogName=model_vendog[idxI]
        
        for (idxSS in 1:(simSteps-1))
        { 
          simulation[[vendogName]]=rbind(simulation[[vendogName]],simulation[[paste0(vendogName,'__LEAD__',idxSS)]])
          simulation[[paste0(vendogName,'__LEAD__',idxSS)]]=NULL
        }
      }
    }
  }
  
  # build MULT MATRIX ------------------------------------------------------------
  
  if (MULTMATRIX==TRUE)
  {
    
    #restore original INSTRUMENT
    if (hasLeads && simSteps>1)
    {
      for (idxI in 1:length(INSTRUMENT))
      {  
        idxN=INSTRUMENT[idxI]
        
        for (idxSS in 1:(simSteps-1))
        { 
          #deal with leads
          #row bind leaded variables back to original one
          localE[[paste0(idxN,'__ORIGINAL__EXTENDED')]]=rbind(localE[[paste0(idxN,'__ORIGINAL__EXTENDED')]],
                                                              localE[[paste0(idxN,'__LEAD__',idxSS)]][model_max_lag_1,,drop=F])
          localE[[paste0(idxN,'__LEAD__',idxSS)]]=NULL
        }
      }
    }
    
    lenT=length(TARGET)
    lenI=length(INSTRUMENT)
    
    MultiplierMatrix=matrix(nrow=lenT*simSteps,
                            ncol=lenI*simSteps) 
    
    #assign names to column 
    tmpColNames=vector('character',length=simSteps*lenI) 
    tmpI=0 
    for (idxIS in 1:simSteps)
    {
      for (idxI in 1:lenI)
      {
        tmpI=tmpI+1 
        tmpColNames[tmpI]=paste0(INSTRUMENT[idxI],'_',idxIS) 
      }
    }
    
    #assign names to rows
    tmpRowNames=vector('character',length=simSteps*lenT) 
    tmpI=0 
    for (idxTS in 1:simSteps)
    {
      for (idxT in 1:lenT)
      {
        tmpI=tmpI+1 
        tmpRowNames[tmpI]=paste0(TARGET[idxT],'_',idxTS) 
      }
    }
    
    colnames(MultiplierMatrix)=tmpColNames 
    rownames(MultiplierMatrix)=tmpRowNames 
    
    tmpIA=list()
    for (idxI in 1:lenI)
    {
      tmpIA[[idxI]]=localE[[paste0(INSTRUMENT[[idxI]],'__ORIGINAL__EXTENDED')]]
    }
    
    #build matrix MM
    for (idxT in 1:lenT)
    {
      tmpT=simulation[[TARGET[idxT]]] 
      
      for (idxTS in 1:simSteps)
      {
        rowMM=idxT+(idxTS-1)*lenT 
        
        for (idxI in 1:lenI)
        {
          tmpI=tmpIA[[idxI]] 
          
          colTbase=1+(idxI-1)*simSteps
          
          for (idxIS in 1:simSteps)
          {
            colT=colTbase+idxIS 
            
            dy=tmpT[idxTS,colT]-tmpT[idxTS,1] 
            
            rowI=model_max_lag+idxIS 
            colI=1+(idxI-1)*simSteps+idxIS 
            
            dx=tmpI[rowI,colT]-tmpI[rowI,1]
            
            colMM=idxI+(idxIS-1)*lenI  
            
            #dx cannot be 0 because of shocks
            if (dy==0)
            {MultiplierMatrix[rowMM,colMM]=0} else {MultiplierMatrix[rowMM,colMM]=dy/dx} 
            
          }
        }
      }
    }
    
    model$MultiplierMatrix=MultiplierMatrix 
  }
  
  .MODEL_outputText(outputText=!quietly,'\n') 
  
  #convert results to time series if replica==1
  if (length(simulation)>0)
  {
    if (replica==1)
    {
      for (idxSL in 1:length(simulation))
      {
        simulation[[idxSL]]=TSERIES(simulation[[idxSL]],
                                    START=TSRANGE[1:2],
                                    FREQ=frequency) 
      }
    } else
    {
      #if MULTMATRIX keep as simulation the first column (the unperturbed one)
      if (MULTMATRIX)
      {
        #store simulation list as matrix in model
        simulation_MM=simulation 
        
        for (idxSL in 1:length(simulation))
        {
          simulation[[idxSL]]=TSERIES(simulation[[idxSL]][,1],
                                      START=TSRANGE[1:2],
                                      FREQ=frequency) 
        }
      }#end multmatrix
      
      if (STOCHSIMULATE)
      {
        if (hasLeads && simSteps>1)
        {
          for (idxI in 1:length(StochStructure))
          { 
            idxN=names(StochStructure)[idxI]
            
            for (idxSS in 1:(simSteps-1))
            { 
              #deal with leads
              #row bind leaded variables back to original one
              INSTRUMENT_MM[[idxN]]=rbind(INSTRUMENT_MM[[idxN]],INSTRUMENT_MM[[paste0(idxN,'__LEAD__',idxSS)]])
              INSTRUMENT_MM[[paste0(idxN,'__LEAD__',idxSS)]]=NULL
            }
          }
        }
        
        #store simulation list as matrix in model
        simulation_MM=simulation 
        stochastic_simulation=list() 
        
        for (idxI in 1:length(simulation))
        {
          idxSL=names(simulation)[idxI]
          
          simulation[[idxSL]]=TSERIES(simulation_MM[[idxSL]][,1],
                                      START=TSRANGE[1:2],
                                      FREQ=frequency) 
          
          #mean all rows but first (not noised)
          stochastic_simulation[[idxSL]]=list() 
          stochastic_simulation[[idxSL]]$mean=TSERIES(rowMeans(simulation_MM[[idxSL]][,-1,drop=F]),
                                                      START=TSRANGE[1:2],
                                                      FREQ=frequency)  
          
          #sd all rows but first
          stochastic_simulation[[idxSL]]$sd=TSERIES(apply(simulation_MM[[idxSL]][,-1,drop=F],1,sd),
                                                    START=TSRANGE[1:2],
                                                    FREQ=frequency)  
        }
      }#end if stochmiulation
      
      if (OPTIMIZE)
      {
        if (hasLeads && simSteps>1 && length(OptimizeBounds)>0)
        {
          for (idxI in 1:length(OptimizeBounds))
          { 
            idxN=names(OptimizeBounds)[idxI]
            
            for (idxSS in 1:(simSteps-1))
            { 
              #deal with leads
              #row bind leaded variables back to original one
              INSTRUMENT_MM[[idxN]]=rbind(INSTRUMENT_MM[[idxN]],INSTRUMENT_MM[[paste0(idxN,'__LEAD__',idxSS)]])
              INSTRUMENT_MM[[paste0(idxN,'__LEAD__',idxSS)]]=NULL
            }
          }
        }
        
        #store simulation list as matrix in model
        simulation_MM=simulation 
        
        optimize=list() 
        
        optimize$modelData=model$modelData
        optimize$ConstantAdjustment=ConstantAdjustment
        
        for (idxI in 1:length(simulation))
        {
          idxSL=names(simulation)[idxI]
          
          simulation[[idxSL]]=TSERIES(simulation_MM[[idxSL]][,1],
                                      START=TSRANGE[1:2],
                                      FREQ=frequency) 
        }
        
        if (length(optColsToBeKept) != length(optFunResults)) stop(callerName,' length mismatch in optimize function results.')
        
        #deal with uncomputable objective function
        #realizations could verify restriction but fail in objective function computability,
        #so remove realizations not computable in objective functions too
        optColsToBeKept=optColsToBeKept & is.finite(optFunResults)
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
          optFunMax=max(optFunResultsFiltered,na.rm=TRUE)
          optFunMaxIdx=which.max(optFunResultsFiltered)
          
          #get mean and sd
          optFunAve=base::mean(optFunResultsFiltered,na.rm=TRUE)
          optFunSd=stats::sd(optFunResultsFiltered,na.rm=TRUE)
          
          #get instruments that allow OPTIMIZE FUNs to be maximized
          if (length(OptimizeBounds)>0) 
          for (idx in 1:length(OptimizeBounds))
          {
            idxI=names(OptimizeBounds)[idx]
            
            #get original instrument
            if (idxI %in% model_vexog)
            {
              tempINST=localE[[paste0(idxI,'__ORIGINAL__EXTENDED')]]
            }
            else if (idxI %in% model_vendog)
            {
              tempINST=localE[[paste0(idxI,'__ADDFACTOR','__ORIGINAL__EXTENDED')]]
            } else {
              stop(callerName,'error in "optimize$INSTRUMENT" definition. "',idxI,'" is not a model variable.')
            }
            
            #get column in optFunMaxIdx position
            tempINSTcol=tempINST[,optFunMaxIdx+1]
            
            #export opt instrument (remove pre-TSRANGE obs)
            OPT_INSTRUMENT[[idxI]]=TSERIES(tempINSTcol[(model_max_lag_1):length(tempINSTcol)],
                                           START=TSRANGE[1:2],FREQ=frequency)
            
            #get leaded maximing instrument
            if (hasLeads && simSteps>1)
            { 
              for (idxSS in 1:(simSteps-1))
              {   
                #get leaded instrument
                if (idxI %in% model_vexog)
                {
                  tempINST=localE[[paste0(idxI,'__LEAD__',idxSS)]]
                }
                else if (idxI %in% model_vendog)
                {
                  tempINST=localE[[paste0(idxI,'__ADDFACTOR','__LEAD__',idxSS)]]
                } else {
                  stop(callerName,'error in "optimize$INSTRUMENT" definition. "',idxI,'" is not a model variable.')
                }
                
                #get column in optFunMaxIdx position
                tempINSTcol=tempINST[,optFunMaxIdx+1]
                
                #export leaded optimize instrument (remove pre-TSRANGE obs) into original var
                tempStart=normalizeYP(c(TSRANGE[1],TSRANGE[2]+idxSS),frequency)
                OPT_INSTRUMENT[[idxI]][[tempStart[1],tempStart[2]]]=tempINSTcol[model_max_lag_1]
                
              }
            }
            
            #build modelData and ConstantAdj that allow fun maximum 
            
            #merge selected instrument obs in TSRANGE with his historical data
            if (idxI %in% model_vexog)
            {
              optimize$modelData[[idxI]]=TSMERGE(OPT_INSTRUMENT[[idxI]],optimize$modelData[[idxI]])
            }
            else if (idxI %in% model_vendog)
            {
              #if instrument is endo we use the add-factor (that can be not in CA)
              if (is.null(optimize$ConstantAdjustment[[idxI]]))
              {
                optimize$ConstantAdjustment[[idxI]]=OPT_INSTRUMENT[[idxI]]
                
              } else {
                
                optimize$ConstantAdjustment[[idxI]]=TSMERGE(OPT_INSTRUMENT[[idxI]],optimize$ConstantAdjustment[[idxI]])
              }
              
            } else {
              stop(callerName,'error in "optimize$modelData" definition. "',idxI,'" is not a model variable.')
            }
          }
          
          #create TS with maximum OPTIMIZE FUNs realizations obs
          optFunTS =TSERIES(optFunTSMM[,optFunMaxIdx],
                            START=TSRANGE[1:2],
                            FREQ=frequency) 
          
        } else {
          if (! quietly) cat(callerName,'warning, none of the computed realizations are finite and verify the provided restrictions. Try to review inequalities and objective functions definition or try to increase the "StochReplica" argument value. OPTIMIZE() will exit with no solution.\n',sep='')
        }
        
        #export stuff
        #(modelData and ConstantAdjustment already exported)
        optimize$optFunResults=optFunResults
        optimize$realizationsToKeep=optColsToBeKept
        optimize$optFunMax=optFunMax
        optimize$INSTRUMENT=OPT_INSTRUMENT
        optimize$optFunTS=optFunTS
        optimize$optFunSd=optFunSd
        optimize$optFunAve=optFunAve
        
      }#end optimize
      
    }#end replica > 1
    
    # build add-factor -------------------------------------
    
    if (simType=='RESCHECK')
    {
      #should never happen
      if (length(setdiff(names(simulation),names(model$modelData)))>0) 
        stop(callerName,'unknown error in building ConstantAdjustmentRESCHECK list.')
      
      ConstantAdjustmentRESCHECK=list()
      
      if (replica==1)
      {
       tryCatch({
       
       for (idxCA in names(simulation))
       {
         if (idxCA %in% names(model$behaviorals))
         { 
           funName=model$behaviorals[[idxCA]]$lhsFun$funName
           args=model$behaviorals[[idxCA]]$lhsFun$args
         } else 
         {
           funName=model$identities[[idxCA]]$multipleLhsFun[[1]]$funName
           args=model$identities[[idxCA]]$multipleLhsFun[[1]]$args
         }
         
         if (funName =='LOG' )
         {
           ConstantAdjustmentRESCHECK[[idxCA]]=log(model$modelData[[idxCA]])-log(simulation[[idxCA]])
           
         } else if ( funName=='EXP' )
         {
           ConstantAdjustmentRESCHECK[[idxCA]]=exp(model$modelData[[idxCA]])-exp(simulation[[idxCA]])
           
         } else if ( funName=='TSDELTAP' )
         { 
           localLag=1
           
           if (length(args)>1) localLag=as.numeric(args[2])
           ConstantAdjustmentRESCHECK[[idxCA]]=100*(model$modelData[[idxCA]]-simulation[[idxCA]])/TSLAG(model$modelData[[idxCA]],localLag)
           
         } else if ( funName=='TSDELTALOG' )
         { 
           localLag=1
           if (length(args)>1) localLag=as.numeric(args[2])
           ConstantAdjustmentRESCHECK[[idxCA]]=log(model$modelData[[idxCA]]/simulation[[idxCA]])
           
         } else {
           ConstantAdjustmentRESCHECK[[idxCA]]=model$modelData[[idxCA]]-simulation[[idxCA]]
         }
       }
         
       },error=function(err){stop(callerName,'error in building ConstantAdjustmentRESCHECK list in behavioral "',idxCA, '". ',err)}) #trycatch
        
      #merge past add-factors if any 
      model$ConstantAdjustmentRESCHECK=.combineList(model$ConstantAdjustmentRESCHECK,ConstantAdjustmentRESCHECK) 
        
      }
    }
    
  }#length(simulation)>0
  
  # backfill -----------------------------------------------
  
  #deal with backfill
  
  if (BackFill>0 )
  {
    .MODEL_outputText(outputText = !quietly,paste0('\n',callerName,'"BackFill" enabled.\nSolutions will include up to ',BackFill,' periods (if available) of historical data starting in year-period ',paste0(normalizeYP(c(TSRANGE[1],TSRANGE[2]-BackFill),frequency),collapse = '-'),'\n')) 
   
    #extend simulation
    if (length(simulation)>0) 
      for (idxI in 1:length(simulation))
      {
        idxRC=names(simulation)[idxI]
        
        #get original time series
        originalTs=model$modelData[[idxRC]] 
        
        #get original start
        outTSL=TSLOOK(originalTs)
        originalStart=c(outTSL$STARTY,outTSL$STARTP)
        
        #get max between requested and available observations
        periodsToAdd=min(NUMPERIOD(originalStart,TSRANGE[1:2],frequency),BackFill) 
        if (periodsToAdd<1) next 
        
        #get new starting point
        newStart=normalizeYP(c(TSRANGE[1],TSRANGE[2]-periodsToAdd),frequency) 
        
        #get extra values
        newStaringIndex=1+NUMPERIOD(originalStart,newStart,frequency) 
        extraValues=coredata(originalTs[newStaringIndex:(newStaringIndex-1+periodsToAdd)])
        
        #save result
        simulation[[idxRC]]=TSERIES(c(extraValues,coredata(simulation[[idxRC]]))
                                    ,START=newStart,FREQ=frequency) 
      }
  }
  
  #merge past simulations if any
  model$simulation=.combineList(model$simulation,simulation) 
  
  if (STOCHSIMULATE)
  {
    model$simulation_MM=.combineList(model$simulation_MM,simulation_MM) 
    model$INSTRUMENT_MM=.combineList(model$INSTRUMENT_MM,INSTRUMENT_MM) 
    model$stochastic_simulation=.combineList(model$stochastic_simulation,stochastic_simulation) 
  }
  
  if (OPTIMIZE)
  {
    model$simulation_MM=.combineList(model$simulation_MM,simulation_MM) 
    model$INSTRUMENT_MM=.combineList(model$INSTRUMENT_MM,INSTRUMENT_MM) 
    model$optimize=optimize
  }
  
  #export simulation parameters
  model$simulation[['__SIM_PARAMETERS__']]$TSRANGE=TSRANGE 
  model$simulation[['__SIM_PARAMETERS__']]$simType=simType 
  model$simulation[['__SIM_PARAMETERS__']]$simConvergence=simConvergence 
  model$simulation[['__SIM_PARAMETERS__']]$simIterLimit=simIterLimit 
  model$simulation[['__SIM_PARAMETERS__']]$ZeroErrorAC=ZeroErrorAC 
  model$simulation[['__SIM_PARAMETERS__']]$BackFillBackFill
  model$simulation[['__SIM_PARAMETERS__']]$Exogenize=Exogenize_original 
  model$simulation[['__SIM_PARAMETERS__']]$ConstantAdjustment=ConstantAdjustment 
  model$simulation[['__SIM_PARAMETERS__']]$MULTMATRIX=MULTMATRIX 
  model$simulation[['__SIM_PARAMETERS__']]$TARGET=TARGET 
  model$simulation[['__SIM_PARAMETERS__']]$INSTRUMENT=INSTRUMENT 
  model$simulation[['__SIM_PARAMETERS__']]$MM_SHOCK=MM_SHOCK 
  model$simulation[['__SIM_PARAMETERS__']]$RESCHECKeqList=RESCHECKeqList 
  
  if (simType=='DYNAMIC' &&  hasLeads)
  {
    model$simulation[['__SIM_PARAMETERS__']]$vblocks=model_vblocks 
    model$simulation[['__SIM_PARAMETERS__']]$vpre=model_vpre 
    model$simulation[['__SIM_PARAMETERS__']]$incidence_matrix=incidence_matrix
  }
  
  if (STOCHSIMULATE)
  {
    model$stochastic_simulation[['__STOCH_SIM_PARAMETERS__']]$STOCHSIMULATE=STOCHSIMULATE 
    model$stochastic_simulation[['__STOCH_SIM_PARAMETERS__']]$StochStructure=StochStructure_original 
    model$stochastic_simulation[['__STOCH_SIM_PARAMETERS__']]$StochReplica=StochReplica 
    model$stochastic_simulation[['__STOCH_SIM_PARAMETERS__']]$StochSeed=StochSeed 
  }
  
  if (OPTIMIZE)
  {
    model$optimize[['__OPT_PARAMETERS__']]$OPTIMIZE=OPTIMIZE 
    model$optimize[['__OPT_PARAMETERS__']]$StochReplica=StochReplica 
    model$optimize[['__OPT_PARAMETERS__']]$StochSeed=StochSeed 
    model$optimize[['__OPT_PARAMETERS__']]$OptimizeBounds=OptimizeBounds 
    model$optimize[['__OPT_PARAMETERS__']]$OptimizeRestrictions=OptimizeRestrictions_original 
    model$optimize[['__OPT_PARAMETERS__']]$OptimizeFunctions=OptimizeFunctions 
  }
  
  if (MULTMATRIX)
  {.MODEL_outputText(outputText=!quietly,'...MULTMATRIX OK\n') 
  } else if (STOCHSIMULATE)
  {.MODEL_outputText(outputText=!quietly,'...STOCHSIMULATE OK\n') 
  } else if (OPTIMIZE)
  {.MODEL_outputText(outputText=!quietly,'...OPTIMIZE OK\n') 
  } else
  {.MODEL_outputText(outputText=!quietly,'...SIMULATE OK\n') }
  
  },error=function(e) {   
    setBIMETSconf('BIMETS_CONF_NOC', complianceStatus, suppressOutput=TRUE);stop(e)
    }) #end main tryCatch for compliance speedup
  
  #reset compliance status
  setBIMETSconf('BIMETS_CONF_NOC', complianceStatus, suppressOutput=TRUE)
  
  return(model) 
  
}
