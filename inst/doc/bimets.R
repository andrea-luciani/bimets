### R code from vignette source 'bimets.Rnw'
### Encoding: ISO8859-15

###################################################
### code chunk number 1: bimets.Rnw:61-62
###################################################
options( prompt = "R> ", continue = "   " )


###################################################
### code chunk number 2: bimets.Rnw:84-85 (eval = FALSE)
###################################################
## install.packages('bimets')


###################################################
### code chunk number 3: bimets.Rnw:87-88
###################################################
library(bimets)


###################################################
### code chunk number 4: bimets.Rnw:97-99
###################################################
#yearly time series
myTS <- TIMESERIES(1:10,START=as.Date('2000-01-01'),FREQ=1)


###################################################
### code chunk number 5: bimets.Rnw:102-104
###################################################
#monthly time series
myTS <- TIMESERIES(1:10,START=c(2002,3),FREQ='M')


###################################################
### code chunk number 6: bimets.Rnw:124-126
###################################################
#create a daily time series
myTS <- TIMESERIES((1:100),START=c(2000,1),FREQ='D')


###################################################
### code chunk number 7: computation
###################################################
myTS[1:3]                      #get first three obs.
myTS['2000-01-12']             #get Jan 12, 2000 data
myTS['2000-02-03/2000-02-14']  #get Feb 3 up to Feb 14
myTS[[2000,14]]                #get year 2000 period 14

myTS['2000-01-15'] <- NA        #assign to Jan 15, 2000
myTS[[2000,42]] <- NA           #assign to Feb 11, 2000
myTS[[2000,100]] <- c(-1,-2,-3) #extend time series starting from period 100


###################################################
### code chunk number 8: bimets.Rnw:145-147
###################################################
#create a monthly time series
myMonthlyTS <- TIMESERIES(1:100,START=c(2000,1),FREQ='M')


###################################################
### code chunk number 9: bimets.Rnw:149-151
###################################################
#convert to annual time series using the average as aggregation fun
myAnnualTS <- ANNUAL(myMonthlyTS,'AVE')


###################################################
### code chunk number 10: bimets.Rnw:153-155
###################################################
#convert to daily using central interpolation as disaggregation fun
myDailyTS <- DAILY(myMonthlyTS,'INTERP_CENTER')


###################################################
### code chunk number 11: bimets.Rnw:175-178
###################################################
#define two time series
myTS1 <- TIMESERIES(1:100,START=c(2000,1),FREQ='M')
myTS2 <- TIMESERIES(-(1:100),START=c(2005,1),FREQ='M')


###################################################
### code chunk number 12: bimets.Rnw:180-182
###################################################
#extend time series up to Apr 2020 with quadratic formula
myExtendedTS <- TSEXTEND(myTS1,UPTO = c(2020,4),EXTMODE = 'QUADRATIC')


###################################################
### code chunk number 13: bimets.Rnw:184-186
###################################################
#merge two time series with sum
myMergedTS <- TSMERGE(myExtendedTS,myTS2,fun = 'SUM')


###################################################
### code chunk number 14: bimets.Rnw:188-190
###################################################
#project time series on arbitrary time range
myProjectedTS <- TSPROJECT(myMergedTS,TSRANGE = c(2004,2,2006,4))


###################################################
### code chunk number 15: bimets.Rnw:192-195
###################################################
#lag and delta% time series
myLagTS <- TSLAG(myProjectedTS,2)
myDeltaPTS <- TSDELTAP(myLagTS,2)


###################################################
### code chunk number 16: bimets.Rnw:197-199
###################################################
#moving average
myMovAveTS <- MOVAVG(myDeltaPTS,5)


###################################################
### code chunk number 17: bimets.Rnw:201-206
###################################################
#print data
TABIT(myMovAveTS,
      myTS1,
      TSRANGE=c(2004,8,2004,12)
      )


###################################################
### code chunk number 18: bimets.Rnw:232-267
###################################################
klein1.txt <- "
MODEL 

COMMENT> Consumption
BEHAVIORAL> cn
TSRANGE 1921 1 1941 1
EQ> cn =  a1 + a2*p + a3*TSLAG(p,1) + a4*(w1+w2) 
COEFF> a1 a2 a3 a4

COMMENT> Investment
BEHAVIORAL> i
TSRANGE 1921 1 1941 1
EQ> i = b1 + b2*p + b3*TSLAG(p,1) + b4*TSLAG(k,1)
COEFF> b1 b2 b3 b4

COMMENT> Demand for Labor
BEHAVIORAL> w1 
TSRANGE 1921 1 1941 1
EQ> w1 = c1 + c2*(y+t-w2) + c3*TSLAG(y+t-w2,1) + c4*time
COEFF> c1 c2 c3 c4

COMMENT> Gross National Product
IDENTITY> y
EQ> y = cn + i + g - t

COMMENT> Profits
IDENTITY> p
EQ> p = y - (w1+w2)

COMMENT> Capital Stock
IDENTITY> k
EQ> k = TSLAG(k,1) + i

END
"


###################################################
### code chunk number 19: bimets.Rnw:470-471
###################################################
kleinModel <- LOAD_MODEL(modelText = klein1.txt)


###################################################
### code chunk number 20: bimets.Rnw:480-481
###################################################
kleinModel$behaviorals$cn


###################################################
### code chunk number 21: bimets.Rnw:487-492
###################################################
kleinModel$incidence_matrix
kleinModel$vpre
kleinModel$vsim
kleinModel$vfeed
kleinModel$vpost


###################################################
### code chunk number 22: bimets.Rnw:500-535
###################################################
kleinModelData <- list(  
    cn  =TIMESERIES(39.8,41.9,45,49.2,50.6,52.6,55.1,56.2,57.3,57.8,
                 55,50.9,45.6,46.5,48.7,51.3,57.7,58.7,57.5,61.6,65,69.7, 	
                 START=c(1920,1),FREQ=1),
    g   =TIMESERIES(4.6,6.6,6.1,5.7,6.6,6.5,6.6,7.6,7.9,8.1,9.4,10.7,
                 10.2,9.3,10,10.5,10.3,11,13,14.4,15.4,22.3,	
                 START=c(1920,1),FREQ=1),
    i   =TIMESERIES(2.7,-.2,1.9,5.2,3,5.1,5.6,4.2,3,5.1,1,-3.4,-6.2,
                 -5.1,-3,-1.3,2.1,2,-1.9,1.3,3.3,4.9,	
                 START=c(1920,1),FREQ=1),
    k   =TIMESERIES(182.8,182.6,184.5,189.7,192.7,197.8,203.4,207.6,
                 210.6,215.7,216.7,213.3,207.1,202,199,197.7,199.8,
                 201.8,199.9,201.2,204.5,209.4,	
                 START=c(1920,1),FREQ=1),
    p   =TIMESERIES(12.7,12.4,16.9,18.4,19.4,20.1,19.6,19.8,21.1,21.7,
                 15.6,11.4,7,11.2,12.3,14,17.6,17.3,15.3,19,21.1,23.5,	
                 START=c(1920,1),FREQ=1),
    w1  =TIMESERIES(28.8,25.5,29.3,34.1,33.9,35.4,37.4,37.9,39.2,41.3,
                 37.9,34.5,29,28.5,30.6,33.2,36.8,41,38.2,41.6,45,53.3,	
                 START=c(1920,1),FREQ=1),
    y   =TIMESERIES(43.7,40.6,49.1,55.4,56.4,58.7,60.3,61.3,64,67,57.7,
                 50.7,41.3,45.3,48.9,53.3,61.8,65,61.2,68.4,74.1,85.3,	
                 START=c(1920,1),FREQ=1),
    t   =TIMESERIES(3.4,7.7,3.9,4.7,3.8,5.5,7,6.7,4.2,4,7.7,7.5,8.3,5.4,
                 6.8,7.2,8.3,6.7,7.4,8.9,9.6,11.6,	
                 START=c(1920,1),FREQ=1),
    time=TIMESERIES(NA,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,
                 1,2,3,4,5,6,7,8,9,10,	
                 START=c(1920,1),FREQ=1),
    w2  =TIMESERIES(2.2,2.7,2.9,2.9,3.1,3.2,3.3,3.6,3.7,4,4.2,4.8,
                 5.3,5.6,6,6.1,7.4,6.7,7.7,7.8,8,8.5,	
                 START=c(1920,1),FREQ=1)
	)

kleinModel <- LOAD_MODEL_DATA(kleinModel,kleinModelData)


###################################################
### code chunk number 23: computation
###################################################

lhsKlein1.txt <- "
MODEL

COMMENT> Modified Klein Model 1 of the U.S. Economy with PDL,
COMMENT> autocorrelation on errors, restrictions and conditional evaluations
COMMENT> LHS functions on EQ

COMMENT> Exp Consumption
BEHAVIORAL> cn
TSRANGE 1925 1 1941 1
EQ> EXP(cn) = a1 + a2*p + a3*LAG(p,1) + a4*(w1+w2)
COEFF> a1 a2 a3 a4
ERROR> AUTO(2)
STORE> coe(1)

COMMENT> Log Investment
BEHAVIORAL> i
TSRANGE 1925 1 1941 1
EQ> LOG(i) = b1 + b2*p + b3*LAG(p,1) + b4*LAG(k,1)
COEFF> b1 b2 b3 b4
RESTRICT> b2 + b3 = 1
STORE> coe(15)

COMMENT> Demand for Labor
BEHAVIORAL> w1
TSRANGE 1925 1 1941 1
EQ> w1 = c1 + c2*(TSDELTA(y)+t-w2) + c3*LAG(TSDELTA(y)+t-w2,1)+c4*time
COEFF> c1 c2 c3 c4
PDL> c3 1 3
STORE> coe(29)

COMMENT> Delta Gross National Product
IDENTITY> y
EQ> TSDELTA(y) = EXP(cn) + LOG(i) + g - t

COMMENT> Profits
IDENTITY> p
EQ> p = TSDELTA(y) - (w1+w2)

COMMENT> Capital Stock with switches
IDENTITY> k
EQ> k = LAG(k,1) + LOG(i)
IF> LOG(i).GT.0
IDENTITY> k
EQ> k = LAG(k,1)
IF> LOG(i).LE.0

END"


###################################################
### code chunk number 24: computation
###################################################

#adjust the original data in order to estimate and to simulate the model
lhsKleinModelData <- within(kleinModelData,{
  i =exp(i);     #we have LOG(i)     in the model MDL definition
  cn=log(cn);    #we have EXP(cn)    in the model MDL definition
  y =CUMSUM(y)   #we have TSDELTA(y) in the model MDL definition
})


###################################################
### code chunk number 25: computation
###################################################

lhsKleinModel <- LOAD_MODEL(modelText = lhsKlein1.txt)
lhsKleinModel <- LOAD_MODEL_DATA(lhsKleinModel,lhsKleinModelData)


###################################################
### code chunk number 26: computation
###################################################

#ESTIMATE and SIMULATE functions are described later
lhsKleinModel <- ESTIMATE(lhsKleinModel)
lhsKleinModel <- SIMULATE(lhsKleinModel, TSRANGE = c(1925,1,1930,1))


###################################################
### code chunk number 27: bimets.Rnw:637-638
###################################################
kleinModel <- ESTIMATE(kleinModel, quietly=TRUE)


###################################################
### code chunk number 28: bimets.Rnw:643-644
###################################################
kleinModel <- ESTIMATE(kleinModel, eqList=c('cn'))


###################################################
### code chunk number 29: bimets.Rnw:649-659
###################################################
#print estimated coefficients
kleinModel$behaviorals$cn$coefficients
#print residuals
kleinModel$behaviorals$cn$residuals
#print a selection of estimate statistics
kleinModel$behaviorals$cn$statistics$DegreesOfFreedom
kleinModel$behaviorals$cn$statistics$StandardErrorRegression
kleinModel$behaviorals$cn$statistics$CoeffCovariance
kleinModel$behaviorals$cn$statistics$AdjustedRSquared
kleinModel$behaviorals$cn$statistics$LogLikelihood


###################################################
### code chunk number 30: bimets.Rnw:667-712
###################################################
#define model
advancedKlein1.txt <- 
"MODEL

COMMENT> Modified Klein Model 1 of the U.S. Economy with PDL, 
COMMENT> autocorrelation on errors, restrictions and conditional equation evaluations

COMMENT> Consumption with autocorrelation on errors
BEHAVIORAL> cn
TSRANGE 1925 1 1941 1
EQ> cn =  a1 + a2*p + a3*TSLAG(p,1) + a4*(w1+w2) 
COEFF> a1 a2 a3 a4
ERROR> AUTO(2)

COMMENT> Investment with restrictions
BEHAVIORAL> i
TSRANGE 1923 1 1941 1
EQ> i = b1 + b2*p + b3*TSLAG(p,1) + b4*TSLAG(k,1)
COEFF> b1 b2 b3 b4
RESTRICT> b2 + b3 = 1

COMMENT> Demand for Labor with PDL
BEHAVIORAL> w1 
TSRANGE 1925 1 1941 1
EQ> w1 = c1 + c2*(y+t-w2) + c3*TSLAG(y+t-w2,1) + c4*time
COEFF> c1 c2 c3 c4
PDL> c3 1 2

COMMENT> Gross National Product
IDENTITY> y
EQ> y = cn + i + g - t

COMMENT> Profits
IDENTITY> p
EQ> p = y - (w1+w2)

COMMENT> Capital Stock with IF switches
IDENTITY> k
EQ> k = TSLAG(k,1) + i
IF> i > 0
IDENTITY> k
EQ> k = TSLAG(k,1) 
IF> i <= 0

END"


###################################################
### code chunk number 31: bimets.Rnw:714-717
###################################################
#load model and data
advancedKleinModel <- LOAD_MODEL(modelText=advancedKlein1.txt)
advancedKleinModel <- LOAD_MODEL_DATA(advancedKleinModel,kleinModelData)


###################################################
### code chunk number 32: bimets.Rnw:719-721
###################################################
#estimate model
advancedKleinModel <- ESTIMATE(advancedKleinModel)


###################################################
### code chunk number 33: bimets.Rnw:756-763
###################################################
#chow test for the consumption equation
#base TSRANGE set to 1921/1935
kleinModelChow <- ESTIMATE(kleinModel
                       ,eqList='cn'
                       ,TSRANGE=c(1921,1,1935,1)
                       ,forceTSRANGE=TRUE
                       ,CHOWTEST=TRUE)


###################################################
### code chunk number 34: bimets.Rnw:834-854
###################################################
#FORECAST GNP in 1942 and 1943 
#we need to extend exogenous variables in 1942 and 1943
kleinModel$modelData <- within(kleinModel$modelData,{
                    w2   = TSEXTEND(w2,  UPTO=c(1943,1))
                    t    = TSEXTEND(t,   UPTO=c(1943,1))
                    g    = TSEXTEND(g,   UPTO=c(1943,1))
                    time = TSEXTEND(time,UPTO=c(1943,1)
                                     ,EXTMODE='LINEAR')
                    })

 
#simulate model
kleinModel <- SIMULATE(kleinModel
                  ,simType='FORECAST'
                  ,TSRANGE=c(1940,1,1943,1)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
                  )
#get forecasted GNP
TABIT(kleinModel$simulation$y)


###################################################
### code chunk number 35: computation
###################################################
#STATIC SIMULATION EXAMPLE WITH EXOGENIZATION AND CONSTANT ADJUSTMENTS
 
#define exogenization list
#'cn' exogenized in 1923-1925
#'i' exogenized in the whole TSRANGE
exogenizeList <- list(
                cn = c(1923,1,1925,1),
                i  = TRUE
              )
 
#define add-factor list
constantAdjList <- list(
               cn = TIMESERIES(1,-1,START=c(1923,1),FREQ='A'),
               y  = TIMESERIES(0.1,-0.1,-0.5,START=c(1926,1),FREQ='A')
              )
 
#simulate model
kleinModel <- SIMULATE(kleinModel
                  ,simType='STATIC'
                  ,TSRANGE=c(1923,1,1941,1)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
                  ,Exogenize=exogenizeList
                  ,ConstantAdjustment=constantAdjList 
                  )


###################################################
### code chunk number 36: bimets.Rnw:946-953
###################################################
kleinModel <- MULTMATRIX(kleinModel,
                        TSRANGE=c(1941,1,1941,1),
                        INSTRUMENT=c('w2','g'),
                        TARGET=c('cn','y')
                      )

kleinModel$MultiplierMatrix


###################################################
### code chunk number 37: bimets.Rnw:959-968
###################################################
#multi-period interim multipliers
kleinModel <- MULTMATRIX(kleinModel,
                   TSRANGE=c(1940,1,1941,1),
                   INSTRUMENT=c('w2','g'),
                   TARGET=c('cn','y'))

#output multipliers matrix (note the zeros when the period
#of the INSTRUMENT is greater than the period of the TARGET)
kleinModel$MultiplierMatrix


###################################################
### code chunk number 38: bimets.Rnw:991-997
###################################################
#we want an arbitrary value on Consumption of 66 in 1940 and 78 in 1941
#we want an arbitrary value on GNP of 77 in 1940 and 98 in 1941
kleinTargets <- list(
              cn = TIMESERIES(66,78,START=c(1940,1),FREQ=1),
              y  = TIMESERIES(77,98,START=c(1940,1),FREQ=1)
              )


###################################################
### code chunk number 39: computation
###################################################
kleinModel <- RENORM(kleinModel
                   ,INSTRUMENT = c('w2','g')
                   ,TARGET = kleinTargets
                   ,TSRANGE = c(1940,1,1941,1)
                   ,simIterLimit = 100
                   ,quietly=TRUE )


###################################################
### code chunk number 40: bimets.Rnw:1013-1020
###################################################
with(kleinModel,TABIT(modelData$w2,
                      renorm$INSTRUMENT$w2,
                      modelData$g,
                      renorm$INSTRUMENT$g,
                      TSRANGE=c(1940,1,1941,1)
                      )
     )


###################################################
### code chunk number 41: bimets.Rnw:1027-1029
###################################################
#create a new model
kleinRenorm <- kleinModel


###################################################
### code chunk number 42: bimets.Rnw:1031-1033
###################################################
#get instruments to be used
newInstruments <- kleinModel$renorm$INSTRUMENT


###################################################
### code chunk number 43: bimets.Rnw:1035-1046
###################################################
#change exogenous by using new instruments data
kleinRenorm$modelData <- within(kleinRenorm$modelData,
                 {
                   w2[[1940,1]]=newInstruments$w2[[1940,1]]
                   w2[[1941,1]]=newInstruments$w2[[1941,1]]
                   g[[1940,1]] =newInstruments$g[[1940,1]]
                   g[[1941,1]] =newInstruments$g[[1941,1]]
                 }
                )
#users can also replace last two commands with:
#kleinRenorm$modelData <- kleinRenorm$renorm$modelData


###################################################
### code chunk number 44: bimets.Rnw:1048-1054
###################################################
#simulate the new model
kleinRenorm <- SIMULATE(kleinRenorm
                      ,TSRANGE=c(1940,1,1941,1)
                      ,simConvergence=0.00001
                      ,simIterLimit=100
                      ,quietly=TRUE)


###################################################
### code chunk number 45: bimets.Rnw:1056-1060
###################################################
#verify targets are achieved
with(kleinRenorm$simulation,
     TABIT(cn,y)
     )


