### R code from vignette source 'frb2bimets.Rnw'

###################################################
### code chunk number 1: frb2bimets.Rnw:60-61
###################################################
options( prompt = "R> ", continue = "   " )


###################################################
### code chunk number 2: frb2bimets.Rnw:182-184
###################################################
#load bimets
library(bimets)


###################################################
### code chunk number 3: frb2bimets.Rnw:186-510
###################################################
sim_plot <- function(model,TSRANGE,plotidx)
{
  #define layout 
  par(mfrow = c(2, 2))
  
  if (plotidx==1)
    TSRANGE=c(normalizeYP(TSRANGE[1:2]-c(0,6),4),TSRANGE[3:4])
  
  if (plotidx==2)
    TSRANGE=c(normalizeYP(TSRANGE[1:2]-c(0,2),4),TSRANGE[3:4])
  
  if (plotidx==3)
    TSRANGE=c(normalizeYP(TSRANGE[1:2]-c(0,6),4),TSRANGE[3:4])
  
  if (plotidx==4)
    TSRANGE=c(normalizeYP(TSRANGE[1:2]-c(0,2),4),TSRANGE[3:4])
  
  xlim=c(TSRANGE[1]+(TSRANGE[2]-1)/4,TSRANGE[3]+(TSRANGE[4]-1)/4)
  
  #plot1
  series1=TSDELTAP(model$simulation$xgdp,4)
  series2=TSPROJECT(TSDELTAP(model$modelData$xgdp,4),
                    TSRANGE=TSRANGE)
  min=min(series1,series2)
  max=max(series1,series2)
  range=max-min
  plot(series2,
       font.main=1,
       col='blue',
       main='Real GDP Growth, Quarterly Annualized',
       ylab='Percent',
       xlab=NULL,
       ylim=c(min-0.05*range,max+0.05*range),
       xlim=xlim,
       yaxt='n',
       xaxt='n')
  axis(side=2,las=2)
  axis(side=1,at=time(series2),labels=as.character(as.yearqtr(time(series2))))
  lines(series1,
        lty='dashed',
        col='red')
   
  #plot2
  series1=TSPROJECT(model$simulation$lur,
                 TSRANGE=TSRANGE)
  series2=TSPROJECT(model$modelData$lur,
                  TSRANGE=TSRANGE)
  min=min(series1,series2)
  max=max(series1,series2)
  range=max-min
  plot(series2,
       font.main=1,
       col='blue',
       main='Unemployment Rate',
       ylab='Percent',
       xlab=NULL,
       ylim=c(min-0.05*range,max+0.05*range),
       xlim=xlim,
       yaxt='n',
       xaxt='n')
  axis(side=2,las=2)
  axis(side=1,at=time(series2),labels=as.character(as.yearqtr(time(series2))))
  lines(series1,
        lty='dashed',
        col='red')
  
  #plot3
  series1=TSDELTAP(model$simulation$pcxfe,4) 
  series2=TSPROJECT(TSDELTAP(model$modelData$pcxfe,4),
                  TSRANGE=TSRANGE) 
  min=min(series1,series2)
  max=max(series1,series2)
  range=max-min
  plot(series2,
       font.main=1,
       col='blue',
       main='Core PCE Inflation, Quarterly Annualized',
       ylab=ifelse(plotidx==2,'','Percent'),
       xlab=NULL,
       ylim=c(min-0.05*range,max+0.05*range),
       xlim=xlim,
       yaxt='n',
       xaxt='n')
  axis(side=2,las=2)
  axis(side=1,at=time(series2),labels=as.character(as.yearqtr(time(series2))))
  lines(series1,
        lty='dashed',
        col='red')
  
  #plot4
  series1=TSPROJECT(model$simulation$rff,
                 TSRANGE=TSRANGE)
  series2=TSPROJECT(model$modelData$rff,
                  TSRANGE=TSRANGE)
  min=min(series1,series2)
  max=max(series1,series2)
  range=max-min
  plot(series2,
       font.main=1,
       col='blue',
       main='Federal Funds Rate',
       ylab='Percent',
       xlab=NULL,
       ylim=c(min-0.05*range,max+0.05*range),
       xlim=xlim,
       yaxt='n',
       xaxt='n')
  axis(side=2,las=2)
  axis(side=1,at=time(series2),labels=as.character(as.yearqtr(time(series2))))
  lines(series1,
        lty='dashed',
        col='red')
}

stochsim_plot <- function(model,TSRANGE)
{
  TSRANGE[1:2]=normalizeYP(TSRANGE[1:2]-c(0,6),4)
    
  #define layout 
  par(mfrow = c(2, 2))
  xlim=c(TSRANGE[1]+(TSRANGE[2]-1)/4,TSRANGE[3]+(TSRANGE[4]-1)/4)
 
  #init stuff
  seriesName='xgdp'
  baseStochMatrix=model$simulation_MM[[seriesName]]
  repl=dim(baseStochMatrix)[2]-1
  simPeriods=dim(baseStochMatrix)[1]
  simStart=start(model$stochastic_simulation[[seriesName]]$mean)
  
  #plot1
  seriesName='xgdp'
  baseStochMatrix=model$simulation_MM[[seriesName]]
  baseStochMatrix=baseStochMatrix[,1+1:repl]
  historicalMatrix=matrix(TSPROJECT(model$modelData[[seriesName]],
                                    TSRANGE=c(normalizeYP(c(simStart[1],simStart[2]-4),4),
                                              normalizeYP(c(simStart[1],simStart[2]-1),4))
                                    ),nrow=4,ncol=repl)
  fullMatrix=rbind(historicalMatrix,baseStochMatrix)
  deltap4StochMatrix=100*((fullMatrix[5:(simPeriods+4),]-fullMatrix[1:(simPeriods),])/
                            fullMatrix[1:(simPeriods),])
  rowMeans=rowMeans(deltap4StochMatrix)
  rowSd=c()
  for (idx in 1:(simPeriods)) rowSd[idx]=sd(deltap4StochMatrix[idx,])
  series1=TSPROJECT(TSDELTAP(model$modelData[[seriesName]],4),
                    TSRANGE=TSRANGE)
  #90% confidence 1.644854
  series2=TSMERGE(TSERIES(rowMeans-1.644854*rowSd,START=simStart,FREQ=4),series1)
  series3=TSMERGE(TSERIES(rowMeans+1.644854*rowSd,START=simStart,FREQ=4),series1)
  
  #70% confidence 1.036433
  series4=TSMERGE(TSERIES(rowMeans-1.036433*rowSd,START=simStart,FREQ=4),series1)
  series5=TSMERGE(TSERIES(rowMeans+1.036433*rowSd,START=simStart,FREQ=4),series1)
  min=min(series1,series2,series3,series4,series5)
  max=max(series1,series2,series3,series4,series5)
  range=max-min
  plot(series1,
       col='blue',
       main='Real GDP Growth, Quarterly Annualized',
       ylab='',
       xlab=NULL,
       ylim=c(min-0.05*range,max+0.05*range),
       xlim=xlim,
       yaxt='n',
       xaxt='n')
  axis(side=2,las=2)
  axis(side=1,at=time(series1),labels=as.character(as.yearqtr(time(series1))))
  polygon(c(time(series1),rev(time(series1))),c(series3,rev(series2)),col='gray',border = F)
  polygon(c(time(series1),rev(time(series1))),c(series5,rev(series4)),col='lightgray',border = F)
  lines(series2,
        lwd=1,
        col='black')
  lines(series3,
        lwd=1,
        col='black')
  lines(series4,
        lwd=2,
        col='darkgray')
  lines(series5,
        lwd=2,
        col='darkgray')
  lines(series1,
        lwd=1,
        col='blue')
  
  #plot2
  seriesName='lur'
  series1=TSPROJECT(model$modelData[[seriesName]],
                    TSRANGE=TSRANGE)
  #90% confidence 1.644854
  series2=TSMERGE(TSERIES(model$stochastic_simulation[[seriesName]]$mean-1.644854*model$stochastic_simulation[[seriesName]]$sd,START=simStart,FREQ=4),series1)
  series3=TSMERGE(TSERIES(model$stochastic_simulation[[seriesName]]$mean+1.644854*model$stochastic_simulation[[seriesName]]$sd,START=simStart,FREQ=4),series1)
  #70% confidence 1.036433
  series4=TSMERGE(TSERIES(model$stochastic_simulation[[seriesName]]$mean-1.036433*model$stochastic_simulation[[seriesName]]$sd,START=simStart,FREQ=4),series1)
  series5=TSMERGE(TSERIES(model$stochastic_simulation[[seriesName]]$mean+1.036433*model$stochastic_simulation[[seriesName]]$sd,START=simStart,FREQ=4),series1)
  min=min(series1,series2,series3,series4,series5)
  max=max(series1,series2,series3,series4,series5)
  range=max-min
  plot(series1,
       col='blue',
       main='Unemployment Rate',
       ylab='',
       xlab=NULL,
       ylim=c(min-0.05*range,max+0.05*range),
       xlim=xlim,
       yaxt='n',
       xaxt='n')
  axis(side=2,las=2)
  axis(side=1,at=time(series1),labels=as.character(as.yearqtr(time(series1))))
  polygon(c(time(series1),rev(time(series1))),c(series3,rev(series2)),col='gray',border = F)
  polygon(c(time(series1),rev(time(series1))),c(series5,rev(series4)),col='lightgray',border = F)
  lines(series2,
        lwd=1,
        col='black')
  lines(series3,
        lwd=1,
        col='black')
  lines(series4,
        lwd=2,
        col='darkgray')
  lines(series5,
        lwd=2,
        col='darkgray')
  lines(series1,
        lwd=1,
        col='blue')
  
  #plot3
  seriesName='pcxfe'
  baseStochMatrix=model$simulation_MM[[seriesName]]
  baseStochMatrix=baseStochMatrix[,1+1:repl]
  historicalMatrix=matrix(TSPROJECT(model$modelData[[seriesName]],
                                    TSRANGE=c(normalizeYP(c(simStart[1],simStart[2]-4),4),
                                              normalizeYP(c(simStart[1],simStart[2]-1),4))
  ),nrow=4,ncol=repl)
  fullMatrix=rbind(historicalMatrix,baseStochMatrix)
  deltap4StochMatrix=100*((fullMatrix[5:(simPeriods+4),]-fullMatrix[1:(simPeriods),])/
                            fullMatrix[1:(simPeriods),])
  rowMeans=rowMeans(deltap4StochMatrix)
  rowSd=c()
  for (idx in 1:(simPeriods)) rowSd[idx]=sd(deltap4StochMatrix[idx,])
  series1=TSPROJECT(TSDELTAP(model$modelData[[seriesName]],4),
                    TSRANGE=TSRANGE)
  #90% confidence 1.644854
  series2=TSMERGE(TSERIES(rowMeans-1.644854*rowSd,START=simStart,FREQ=4),series1)
  series3=TSMERGE(TSERIES(rowMeans+1.644854*rowSd,START=simStart,FREQ=4),series1)
  #70% confidence 1.036433
  series4=TSMERGE(TSERIES(rowMeans-1.036433*rowSd,START=simStart,FREQ=4),series1)
  series5=TSMERGE(TSERIES(rowMeans+1.036433*rowSd,START=simStart,FREQ=4),series1)
  min=min(series1,series2,series3,series4,series5)
  max=max(series1,series2,series3,series4,series5)
  range=max-min
  plot(series1,
       col='blue',
       main='Core PCE Inflation, Quarterly Annualized',
       ylab='',
       xlab=NULL,
       ylim=c(min-0.05*range,max+0.05*range),
       xlim=xlim,
       yaxt='n',
       xaxt='n')
  axis(side=2,las=2)
  axis(side=1,at=time(series1),labels=as.character(as.yearqtr(time(series1))))
  polygon(c(time(series1),rev(time(series1))),c(series3,rev(series2)),col='gray',border = F)
  polygon(c(time(series1),rev(time(series1))),c(series5,rev(series4)),col='lightgray',border = F)
  lines(series2,
        lwd=1,
        col='black')
  lines(series3,
        lwd=1,
        col='black')
  lines(series4,
        lwd=2,
        col='darkgray')
  lines(series5,
        lwd=2,
        col='darkgray')
  lines(series1,
        lwd=1,
        col='blue')
  
  #plot4
  seriesName='rff'
  series1=TSPROJECT(model$modelData[[seriesName]],
                    TSRANGE=TSRANGE)
  #90% confidence 1.644854
  series2=TSMERGE(TSERIES(pmax(0,model$stochastic_simulation[[seriesName]]$mean-1.644854*model$stochastic_simulation[[seriesName]]$sd),START=simStart,FREQ=4),series1)
  series3=TSMERGE(TSERIES(pmax(0,model$stochastic_simulation[[seriesName]]$mean+1.644854*model$stochastic_simulation[[seriesName]]$sd),START=simStart,FREQ=4),series1)
  #70% confidence 1.036433
  series4=TSMERGE(TSERIES(pmax(0,model$stochastic_simulation[[seriesName]]$mean-1.036433*model$stochastic_simulation[[seriesName]]$sd),START=simStart,FREQ=4),series1)
  series5=TSMERGE(TSERIES(pmax(0,model$stochastic_simulation[[seriesName]]$mean+1.036433*model$stochastic_simulation[[seriesName]]$sd),START=simStart,FREQ=4),series1)
  min=min(series1,series2,series3,series4,series5)
  max=max(series1,series2,series3,series4,series5)
  range=max-min
  plot(series1,
       col='blue',
       main='Federal Funds Rate',
       ylab='',
       xlab=NULL,
       ylim=c(min-0.05*range,max+0.05*range),
       xlim=xlim,
       yaxt='n',
       xaxt='n')
  axis(side=2,las=2)
  axis(side=1,at=time(series1),labels=as.character(as.yearqtr(time(series1))))
  polygon(c(time(series1),rev(time(series1))),c(series3,rev(series2)),col='gray',border = F)
  polygon(c(time(series1),rev(time(series1))),c(series5,rev(series4)),col='lightgray',border = F)
  lines(series2,
        lwd=1,
        col='black')
  lines(series3,
        lwd=1,
        col='black')
  lines(series4,
        lwd=2,
        col='darkgray')
  lines(series5,
        lwd=2,
        col='darkgray')
  lines(series1,
        lwd=1,
        col='blue')
  
  
}


###################################################
### code chunk number 4: frb2bimets.Rnw:512-514
###################################################
#load FRB/US MDL definition
data(FRB__MODEL)


###################################################
### code chunk number 5: frb2bimets.Rnw:516-518
###################################################
#print first equations in model definition
cat(substring(FRB__MODEL,1,1615))


###################################################
### code chunk number 6: frb2bimets.Rnw:527-535 (eval = FALSE)
###################################################
## #define file path
## modelDefinitionFile <- file('~/FRB__MODEL.txt')
## 
## #save FRB definition in the text file
## writeLines(FRB__MODEL,modelDefinitionFile)    
## 
## #close connection
## close(modelDefinitionFile)


###################################################
### code chunk number 7: frb2bimets.Rnw:548-550
###################################################
#load FRB/US model data
data(LONGBASE)


###################################################
### code chunk number 8: frb2bimets.Rnw:552-554
###################################################
#print GDP in 2022-2024
TABIT(LONGBASE$xgdp,TSRANGE = c(2022,1,2024,1))


###################################################
### code chunk number 9: frb2bimets.Rnw:559-561
###################################################
#create the bimets model
model <- LOAD_MODEL(modelText = FRB__MODEL)


###################################################
### code chunk number 10: frb2bimets.Rnw:563-565
###################################################
#print a sample of endogenous variables
model$vendog[1:10]


###################################################
### code chunk number 11: frb2bimets.Rnw:567-569
###################################################
#print a sample of exogenous variables
model$vexog[1:10]


###################################################
### code chunk number 12: frb2bimets.Rnw:571-573
###################################################
#print GDP equation
model$identities$xgdp$eqFull


###################################################
### code chunk number 13: frb2bimets.Rnw:623-624
###################################################
library(bimets)


###################################################
### code chunk number 14: frb2bimets.Rnw:626-628
###################################################
# Load data
data(LONGBASE)


###################################################
### code chunk number 15: frb2bimets.Rnw:630-633
###################################################
# Load model
data(FRB__MODEL)
model <- LOAD_MODEL(modelText = FRB__MODEL)


###################################################
### code chunk number 16: frb2bimets.Rnw:635-637
###################################################
# Load data into model
model <- LOAD_MODEL_DATA(model, LONGBASE, quietly=TRUE)


###################################################
### code chunk number 17: frb2bimets.Rnw:639-642
###################################################
# Specify dates
start <- c(2040,1)
end <- normalizeYP(start+c(0,23),4)


###################################################
### code chunk number 18: frb2bimets.Rnw:644-647
###################################################
# Standard configuration, use surplus ratio targeting
model$modelData$dfpdbt[[start,end]] <- 0
model$modelData$dfpsrp[[start,end]] <- 1


###################################################
### code chunk number 19: frb2bimets.Rnw:649-655
###################################################
# Solve to baseline with adds
model <- SIMULATE(model,
                  simType='RESCHECK',
                  TSRANGE=c(start,end),
                  ZeroErrorAC = TRUE,
                  quietly=TRUE)


###################################################
### code chunk number 20: frb2bimets.Rnw:657-660
###################################################
# 100 bp monetary policy shock
trac <- model$ConstantAdjustmentRESCHECK
trac$rffintay[[start]] <- trac$rffintay[[start]]+1


###################################################
### code chunk number 21: frb2bimets.Rnw:662-669
###################################################
# Solve
model <- SIMULATE(model,
                  simAlgo = 'NEWTON',
                  TSRANGE = c(start,end),
                  ConstantAdjustment = trac,
                  BackFill = 12,
                  quietly=TRUE)


###################################################
### code chunk number 22: frb2bimets.Rnw:671-673 (eval = FALSE)
###################################################
## # View results
## sim_plot(model,c(start,end),1)


###################################################
### code chunk number 23: frb2bimets.Rnw:689-691
###################################################
# View results
sim_plot(model,c(start,end),1)


###################################################
### code chunk number 24: frb2bimets.Rnw:748-749
###################################################
library(bimets)


###################################################
### code chunk number 25: frb2bimets.Rnw:751-753
###################################################
# Load data
data(LONGBASE)


###################################################
### code chunk number 26: frb2bimets.Rnw:755-758
###################################################
# Load model
data(FRB__MCAP__WP__MODEL)
model <- LOAD_MODEL(modelText = FRB__MCAP__WP__MODEL)


###################################################
### code chunk number 27: frb2bimets.Rnw:760-762
###################################################
# Load data into model
model <- LOAD_MODEL_DATA(model, LONGBASE, quietly=TRUE)


###################################################
### code chunk number 28: frb2bimets.Rnw:764-767
###################################################
# Specify dates
start <- c(2040,1)
end <- normalizeYP(start+c(0,8),4)


###################################################
### code chunk number 29: frb2bimets.Rnw:769-774
###################################################
# Standard MCE configuration, use surplus ratio targeting, rstar endogenous in long run
model$modelData$dfpdbt[[start,end]] <- 0
model$modelData$dfpsrp[[start,end]] <- 1
model$modelData$drstar[[start,end]] <- 0
model$modelData$drstar[[normalizeYP(start+c(0,4),4),end]] <- 1 


###################################################
### code chunk number 30: frb2bimets.Rnw:776-782
###################################################
# Solve to baseline with adds
model <- SIMULATE(model,
                  simType = 'RESCHECK',
                  TSRANGE = c(start,end),
                  ZeroErrorAC = TRUE,
                  quietly=TRUE)


###################################################
### code chunk number 31: frb2bimets.Rnw:784-787
###################################################
# 100 bp monetary policy shock
shock <- model$ConstantAdjustmentRESCHECK
shock$rffintay[[start]] <- shock$rffintay[[start]]+1


###################################################
### code chunk number 32: frb2bimets.Rnw:789-796
###################################################
# Solve
model <- SIMULATE(model,
                  simAlgo = 'NEWTON',
                  TSRANGE = c(start,end),
                  ConstantAdjustment = shock,
                  BackFill = 12,
                  quietly=TRUE)


###################################################
### code chunk number 33: frb2bimets.Rnw:798-800 (eval = FALSE)
###################################################
## # View results
## sim_plot(model,c(start,end),2)


###################################################
### code chunk number 34: frb2bimets.Rnw:816-818
###################################################
# View results
sim_plot(model,c(start,end),2)


###################################################
### code chunk number 35: frb2bimets.Rnw:898-899
###################################################
library(bimets)


###################################################
### code chunk number 36: frb2bimets.Rnw:901-903
###################################################
# Load data
data(LONGBASE)


###################################################
### code chunk number 37: frb2bimets.Rnw:905-908
###################################################
# Load model
data(FRB__MODEL)
model <- LOAD_MODEL(modelText = FRB__MODEL)


###################################################
### code chunk number 38: frb2bimets.Rnw:910-912
###################################################
# Load data into model
model <- LOAD_MODEL_DATA(model, LONGBASE, quietly=TRUE)


###################################################
### code chunk number 39: frb2bimets.Rnw:914-917
###################################################
# Specify dates
start <- c(2040,1)
end <- normalizeYP(start+c(0,24),4)


###################################################
### code chunk number 40: frb2bimets.Rnw:919-922
###################################################
# Standard configuration, use surplus ratio targeting
model$modelData$dfpdbt[[start,end]] <- 0
model$modelData$dfpsrp[[start,end]] <- 1


###################################################
### code chunk number 41: frb2bimets.Rnw:924-927
###################################################
# Use non-inertial Taylor rule
model$modelData$dmptay[[start,end]] <- 1
model$modelData$dmpintay[[start,end]] <- 0


###################################################
### code chunk number 42: frb2bimets.Rnw:929-931
###################################################
# Enable thresholds
model$modelData$dmptrsh[[start,end]] <- 1


###################################################
### code chunk number 43: frb2bimets.Rnw:933-936
###################################################
# Arbitrary threshold values
model$modelData$lurtrsh[[start,end]] <- 6
model$modelData$pitrsh[[start,end]] <- 3


###################################################
### code chunk number 44: frb2bimets.Rnw:938-944
###################################################
# Solve to baseline with adds
model <- SIMULATE(model,
                  simType = 'RESCHECK',
                  TSRANGE = c(start,end),
                  ZeroErrorAC = TRUE,
                  quietly=TRUE)


###################################################
### code chunk number 45: frb2bimets.Rnw:946-948
###################################################
# Get tracking residuals        
trac <- model$ConstantAdjustmentRESCHECK


###################################################
### code chunk number 46: frb2bimets.Rnw:950-958
###################################################
# Zero tracking residuals for funds rate and thresholds 
trac$rfftay[[start,end]] <- 0
trac$rffrule[[start,end]] <- 0
trac$rff[[start,end]] <- 0
trac$dmptpi[[start,end]] <- 0
trac$dmptlur[[start,end]] <- 0
trac$dmptmax[[start,end]] <- 0
trac$dmptr[[start,end]] <- 0


###################################################
### code chunk number 47: frb2bimets.Rnw:960-968
###################################################
# Shocks vaguely derived from historical residuals
aerr <- list()
aerr$eco <- TSERIES(c(-0.002, -0.0016, -0.0070, -0.0045),START=start,FREQ=4)
aerr$ecd <- TSERIES(c(-0.0319, -0.0154, -0.0412, -0.0838),START=start,FREQ=4)
aerr$eh <- TSERIES(c(-0.0512, -0.0501, -0.0124, -0.0723),START=start,FREQ=4)
aerr$rbbbp <- TSERIES(c(0.3999, 2.7032, 0.3391, -0.7759),START=start,FREQ=4)
aerr$lhp <- TSERIES(c(-0.0029,-0.0048,-0.0119,-0.0085,-0.0074,-0.0061,-0.0077,-0.0033,-0.0042),
            START=start,FREQ=4)


###################################################
### code chunk number 48: frb2bimets.Rnw:970-977
###################################################
# Roll off residuals with 0.5 persistence
rho <- 0.5
aerr$eco <- TSEXTEND(aerr$eco,UPTO=end,EXTMODE='MYRATE',FACTOR=rho)
aerr$ecd <- TSEXTEND(aerr$ecd,UPTO=end,EXTMODE='MYRATE',FACTOR=rho)
aerr$eh <- TSEXTEND(aerr$eh,UPTO=end,EXTMODE='MYRATE',FACTOR=rho)
aerr$rbbbp <- TSEXTEND(aerr$rbbbp,UPTO=end,EXTMODE='MYRATE',FACTOR=rho)
aerr$lhp <- TSEXTEND(aerr$lhp,UPTO=end,EXTMODE='MYRATE',FACTOR=rho)


###################################################
### code chunk number 49: frb2bimets.Rnw:979-982
###################################################
# Adds so that thresholds do not trigger before shocks are felt
aerr$dmptr <- TSERIES(c(-1),START=start,FREQ=4)
aerr$dmptlur <- TSERIES(c(-1,-1,-1),START=start,FREQ=4)


###################################################
### code chunk number 50: frb2bimets.Rnw:984-986
###################################################
# Create Constant Adjustments for SIMULATE op.
for (idx in names(aerr)) trac[[idx]] <- trac[[idx]]+aerr[[idx]]


###################################################
### code chunk number 51: frb2bimets.Rnw:988-995
###################################################
# Solve
model <- SIMULATE(model,
                  simAlgo = 'NEWTON',
                  TSRANGE = c(start,end),
                  ConstantAdjustment = trac,
                  BackFill = 12,
                  quietly=TRUE)  


###################################################
### code chunk number 52: frb2bimets.Rnw:997-999 (eval = FALSE)
###################################################
## # View results, unemployment threshold binds
## sim_plot(model,c(start,end),3)


###################################################
### code chunk number 53: frb2bimets.Rnw:1015-1017
###################################################
# View results
sim_plot(model,c(start,end),3)


###################################################
### code chunk number 54: frb2bimets.Rnw:1078-1079
###################################################
library(bimets)


###################################################
### code chunk number 55: frb2bimets.Rnw:1081-1083
###################################################
# Load data
data(LONGBASE)


###################################################
### code chunk number 56: frb2bimets.Rnw:1085-1088
###################################################
# Load model
data(FRB__MODEL)
model <- LOAD_MODEL(modelText = FRB__MODEL)


###################################################
### code chunk number 57: frb2bimets.Rnw:1090-1092
###################################################
# Load data into model
model <- LOAD_MODEL_DATA(model, LONGBASE, quietly=TRUE)


###################################################
### code chunk number 58: frb2bimets.Rnw:1094-1097
###################################################
# Specify dates
start <- c(2021,3)
end <- c(2022,3)


###################################################
### code chunk number 59: frb2bimets.Rnw:1099-1102
###################################################
# Standard configuration, use surplus ratio targeting
model$modelData$dfpdbt[[start,end]] <- 0
model$modelData$dfpsrp[[start,end]] <- 1


###################################################
### code chunk number 60: frb2bimets.Rnw:1104-1110
###################################################
# Solve to baseline with adds
model <- SIMULATE(model,
                  simType = 'RESCHECK',
                  TSRANGE = c(start,end),
                  ZeroErrorAC = TRUE
                  ,quietly=TRUE)


###################################################
### code chunk number 61: frb2bimets.Rnw:1112-1114
###################################################
# Scenario based on 2021Q3 Survey of Professional Forecasters
model$modelData$lurnat[[start,end]] <- 3.78


###################################################
### code chunk number 62: frb2bimets.Rnw:1116-1122
###################################################
# Set up trajectories for mcontrol
targ <- list()
targ$lur <- TSERIES(c(5.3, 4.9, 4.6, 4.4, 4.2),START=start,FREQ=4)
targ$picxfe <- TSERIES(c(3.7, 2.2, 2.1, 2.1, 2.2),START=start,FREQ=4)
targ$rff <- TSERIES(c(0.1, 0.1, 0.1, 0.1, 0.1),START=start,FREQ=4)
targ$rg10 <- TSERIES(c(1.4, 1.6, 1.6, 1.7, 1.9),START=start,FREQ=4)


###################################################
### code chunk number 63: frb2bimets.Rnw:1124-1127
###################################################
# Get GDP level as accumulated growth from initial period
gdp_growth <- model$modelData$xgdp[[2021,2]]*CUMPROD((c(6.8,5.2,4.5,3.4,2.7) / 100 + 1) ** 0.25)
targ$xgdp <- TSERIES(gdp_growth,START=start,FREQ=4)


###################################################
### code chunk number 64: frb2bimets.Rnw:1129-1131
###################################################
# define INSTRUMENT
inst <- c("eco", "lhp", "picxfe", "rff", "rg10p")


###################################################
### code chunk number 65: frb2bimets.Rnw:1133-1142
###################################################
# Run RENORM
model <- RENORM(model,
                simAlgo = 'NEWTON',
                TSRANGE=c(start,end),
                ConstantAdjustment = model$ConstantAdjustmentRESCHECK,
                TARGET=targ,
                INSTRUMENT=inst,
                BackFill = 8,
                quietly=TRUE)


###################################################
### code chunk number 66: frb2bimets.Rnw:1144-1146 (eval = FALSE)
###################################################
## # View results
## sim_plot(model,c(start,end),4)


###################################################
### code chunk number 67: frb2bimets.Rnw:1160-1162
###################################################
# View results
sim_plot(model,c(start,end),4)


###################################################
### code chunk number 68: frb2bimets.Rnw:1214-1215
###################################################
library(bimets)


###################################################
### code chunk number 69: frb2bimets.Rnw:1217-1219
###################################################
# Load data
data(LONGBASE)


###################################################
### code chunk number 70: frb2bimets.Rnw:1221-1224
###################################################
# Load model
data(FRB__MODEL)
model <- LOAD_MODEL(modelText = FRB__MODEL)


###################################################
### code chunk number 71: frb2bimets.Rnw:1226-1228
###################################################
# Load data into model
model <- LOAD_MODEL_DATA(model, LONGBASE, quietly=TRUE)


###################################################
### code chunk number 72: frb2bimets.Rnw:1230-1235
###################################################
# Specify dates and other params
residstart <- c(1975,1)
residend <- c(2018,4)
simstart <- c(2040,1)
simend <- c(2045,4)


###################################################
### code chunk number 73: frb2bimets.Rnw:1237-1239
###################################################
# Number of replications
nrepl <- 1000


###################################################
### code chunk number 74: frb2bimets.Rnw:1241-1244
###################################################
# Policy settings
model$modelData$dfpdbt[[simstart,simend]] <- 0
model$modelData$dfpsrp[[simstart,simend]] <- 1


###################################################
### code chunk number 75: frb2bimets.Rnw:1246-1253
###################################################
# Compute add factors
# Both for baseline tracking and over history, to be used as shocks
model <- SIMULATE(model,
                  simType = 'RESCHECK',
                  TSRANGE = c(residstart,simend),
                  ZeroErrorAC = TRUE,
                  quietly=TRUE)


###################################################
### code chunk number 76: frb2bimets.Rnw:1255-1257
###################################################
# Get tracking residuals
trac <- model$ConstantAdjustmentRESCHECK


###################################################
### code chunk number 77: frb2bimets.Rnw:1259-1261
###################################################
# Set seed
set.seed(9)


###################################################
### code chunk number 78: frb2bimets.Rnw:1263-1272
###################################################
# 64 Stochastic vars as listed in XML FRB/US model file
stochasticVars <- c("ebfi","ecd","ech","eco","egfe","egfen","egfet","egfl","egse",
                  "egsen","egset","egsl","eh","emo","emp","ex","fpxrr","fxgap",
                  "ugfsrp","gtn","gtr","gtrd","hmfpt","hqlfpr","hqlww","ki","leg",
                  "leo","lfpr","lhp","lurnat","lww","mfpt","pbfir","pcer",
                  "pcfr","pegfr","pegsr","phouse","phr","picxfe","pieci","pmo",
                  "poilr","pxr","rbbbp","rcar","rcgain","reqp","rfynic",
                  "rfynil","rg10p","rg30p","rg5p","rgfint","rme","tcin","tpn",
                  "trci","trp","trpt","uynicpnr","ynidn","ynirn")


###################################################
### code chunk number 79: frb2bimets.Rnw:1274-1278
###################################################
# Pseudo random array that maps residuals range to simulation range for each replica
residualsLength <- NUMPERIOD(residstart,residend,4)+1
stochSimLength <- NUMPERIOD(simstart,simend,4)+1
sampleHistoricalResidual <- sample(1:residualsLength,stochSimLength*nrepl,replace=T)


###################################################
### code chunk number 80: frb2bimets.Rnw:1280-1293
###################################################
# Create BIMETS stochastic structure  
modelStochStructure <- list()
for (tmpStochVar in stochasticVars)
{
    #see BIMETS reference manual for details on STOCHSIMULATE and StochStructure
    modelStochStructure[[tmpStochVar]] <- list()
    modelStochStructure[[tmpStochVar]]$TSRANGE <- TRUE
    modelStochStructure[[tmpStochVar]]$TYPE <- 'MATRIX'
    shockMatrix <- matrix(trac[[tmpStochVar]][sampleHistoricalResidual],
                          nrow=stochSimLength,ncol=nrepl)
    shockMatrix <- shockMatrix - ave(shockMatrix)
    modelStochStructure[[tmpStochVar]]$PARS <- shockMatrix
}


###################################################
### code chunk number 81: frb2bimets.Rnw:1295-1303
###################################################
# Call BIMETS stoch sim procedure
model <- STOCHSIMULATE(model,
                       simAlgo = 'NEWTON',
                       TSRANGE = c(simstart,simend),
                       StochStructure = modelStochStructure,
                       StochReplica = nrepl,
                       ConstantAdjustment = trac,
                       quietly=TRUE)


###################################################
### code chunk number 82: frb2bimets.Rnw:1305-1307 (eval = FALSE)
###################################################
## # View results
## stochsim_plot(model,c(simstart,simend))


###################################################
### code chunk number 83: frb2bimets.Rnw:1323-1325
###################################################
# View results
stochsim_plot(model,c(simstart,simend))


