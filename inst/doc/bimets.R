### R code from vignette source 'bimets.Rnw'

###################################################
### code chunk number 1: bimets.Rnw:61-63
###################################################
options( prompt = "R> ", continue = "   " )
library(bimets)


###################################################
### code chunk number 2: bimets.Rnw:78-80
###################################################
#yearly time series
myTS <- TIMESERIES(1:10, START = as.Date('2000-01-01'), FREQ = 1)


###################################################
### code chunk number 3: bimets.Rnw:83-85
###################################################
#monthly time series
myTS <- TIMESERIES(1:10, START = c(2002,3), FREQ = 'M')


###################################################
### code chunk number 4: bimets.Rnw:87-88
###################################################
print(class(myTS))


###################################################
### code chunk number 5: bimets.Rnw:108-110
###################################################
#create a daily time series
myTS <- TIMESERIES((1:100), START = c(2000,1), FREQ = 'D')


###################################################
### code chunk number 6: computation
###################################################
myTS[1:3]                       #get first three obs.
myTS['2000-01-12']              #get Jan 12, 2000 data
myTS['2000-02-03/2000-02-14']   #get Feb 3 up to Feb 14
myTS[[2000,14]]                 #get year 2000 period 14

myTS['2000-01-15'] <- NA        #assign to Jan 15, 2000
myTS[[2000,42]] <- NA           #assign to Feb 11, 2000
myTS[[2000,100]] <- c(-1,-2,-3) #extend time series starting from period 100


###################################################
### code chunk number 7: bimets.Rnw:129-131
###################################################
#create a monthly time series
myMonthlyTS <- TIMESERIES(1:100, START = c(2000,1), FREQ = 'M')


###################################################
### code chunk number 8: bimets.Rnw:133-135
###################################################
#convert to yearly time series using the average as aggregation fun
myYearlyTS <- YEARLY(myMonthlyTS, 'AVE')


###################################################
### code chunk number 9: bimets.Rnw:137-139
###################################################
#convert to daily using central interpolation as disaggregation fun
myDailyTS <- DAILY(myMonthlyTS, 'INTERP_CENTER')


###################################################
### code chunk number 10: bimets.Rnw:159-162
###################################################
#define two time series
myTS1 <- TIMESERIES(1:100, START = c(2000,1), FREQ = 'M')
myTS2 <- TIMESERIES(-(1:100), START = c(2005,1), FREQ = 'M')


###################################################
### code chunk number 11: bimets.Rnw:164-166
###################################################
#extend time series up to Apr 2020 with quadratic formula
myExtendedTS <- TSEXTEND(myTS1, UPTO = c(2020,4), EXTMODE = 'QUADRATIC')


###################################################
### code chunk number 12: bimets.Rnw:168-170
###################################################
#merge two time series with sum
myMergedTS <- TSMERGE(myExtendedTS, myTS2, fun = 'SUM')


###################################################
### code chunk number 13: bimets.Rnw:172-174
###################################################
#project time series on arbitrary time range
myProjectedTS <- TSPROJECT(myMergedTS, TSRANGE = c(2004,2,2006,4))


###################################################
### code chunk number 14: bimets.Rnw:176-179
###################################################
#lag and delta% time series
myLagTS <- TSLAG(myProjectedTS,2)
myDeltaPTS <- TSDELTAP(myLagTS,2)


###################################################
### code chunk number 15: bimets.Rnw:181-183
###################################################
#moving average
myMovAveTS <- MOVAVG(myDeltaPTS,5)


###################################################
### code chunk number 16: bimets.Rnw:185-190
###################################################
#print data
TABIT(myMovAveTS,
      myTS1,
      TSRANGE = c(2004,8,2004,12)
      )


###################################################
### code chunk number 17: bimets.Rnw:222-257
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
### code chunk number 18: bimets.Rnw:481-482
###################################################
kleinModel <- LOAD_MODEL(modelText = klein1.txt)


###################################################
### code chunk number 19: bimets.Rnw:491-492
###################################################
kleinModel$behaviorals$cn


###################################################
### code chunk number 20: bimets.Rnw:498-503
###################################################
kleinModel$incidence_matrix
kleinModel$vpre
kleinModel$vsim
kleinModel$vfeed
kleinModel$vpost


###################################################
### code chunk number 21: bimets.Rnw:511-546
###################################################
kleinModelData <- list(  
    cn   = TIMESERIES(39.8,41.9,45,49.2,50.6,52.6,55.1,56.2,57.3,57.8,
                 55,50.9,45.6,46.5,48.7,51.3,57.7,58.7,57.5,61.6,65,69.7, 	
                 START = c(1920,1), FREQ = 1),
    g    = TIMESERIES(4.6,6.6,6.1,5.7,6.6,6.5,6.6,7.6,7.9,8.1,9.4,10.7,
                 10.2,9.3,10,10.5,10.3,11,13,14.4,15.4,22.3,	
                 START = c(1920,1), FREQ = 1),
    i    = TIMESERIES(2.7,-.2,1.9,5.2,3,5.1,5.6,4.2,3,5.1,1,-3.4,-6.2,
                 -5.1,-3,-1.3,2.1,2,-1.9,1.3,3.3,4.9,	
                 START = c(1920,1), FREQ = 1),
    k    = TIMESERIES(182.8,182.6,184.5,189.7,192.7,197.8,203.4,207.6,
                 210.6,215.7,216.7,213.3,207.1,202,199,197.7,199.8,
                 201.8,199.9,201.2,204.5,209.4,	
                 START = c(1920,1), FREQ = 1),
    p    = TIMESERIES(12.7,12.4,16.9,18.4,19.4,20.1,19.6,19.8,21.1,21.7,
                 15.6,11.4,7,11.2,12.3,14,17.6,17.3,15.3,19,21.1,23.5,	
                 START = c(1920,1), FREQ = 1),
    w1   = TIMESERIES(28.8,25.5,29.3,34.1,33.9,35.4,37.4,37.9,39.2,41.3,
                 37.9,34.5,29,28.5,30.6,33.2,36.8,41,38.2,41.6,45,53.3,	
                 START = c(1920,1), FREQ = 1),
    y    = TIMESERIES(43.7,40.6,49.1,55.4,56.4,58.7,60.3,61.3,64,67,57.7,
                 50.7,41.3,45.3,48.9,53.3,61.8,65,61.2,68.4,74.1,85.3,	
                 START = c(1920,1), FREQ = 1),
    t    = TIMESERIES(3.4,7.7,3.9,4.7,3.8,5.5,7,6.7,4.2,4,7.7,7.5,8.3,5.4,
                 6.8,7.2,8.3,6.7,7.4,8.9,9.6,11.6,	
                 START = c(1920,1), FREQ = 1),
    time = TIMESERIES(NA,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,
                 1,2,3,4,5,6,7,8,9,10,	
                 START = c(1920,1), FREQ = 1),
    w2   = TIMESERIES(2.2,2.7,2.9,2.9,3.1,3.2,3.3,3.6,3.7,4,4.2,4.8,
                 5.3,5.6,6,6.1,7.4,6.7,7.7,7.8,8,8.5,	
                 START = c(1920,1), FREQ = 1)
	)

kleinModel <- LOAD_MODEL_DATA(kleinModel, kleinModelData)


###################################################
### code chunk number 22: computation
###################################################

lhsKlein1.txt <- "
MODEL

COMMENT> Modified Klein Model 1 of the U.S. Economy with PDL,
COMMENT> autocorrelation on errors, restrictions and conditional evaluations
COMMENT> LHS functions on EQ

COMMENT> Exp Consumption
BEHAVIORAL> cn
TSRANGE 1925 1 1941 1
EQ> EXP(cn) = a1 + a2*p + a3*TSLAG(p,1) + a4*(w1+w2)
COEFF> a1 a2 a3 a4
ERROR> AUTO(2)

COMMENT> Log Investment
BEHAVIORAL> i
TSRANGE 1925 1 1941 1
EQ> LOG(i) = b1 + b2*p + b3*TSLAG(p,1) + b4*TSLAG(k,1)
COEFF> b1 b2 b3 b4
RESTRICT> b2 + b3 = 1

COMMENT> Demand for Labor
BEHAVIORAL> w1
TSRANGE 1925 1 1941 1
EQ> w1 = c1 + c2*(TSDELTA(y)+t-w2) + c3*TSLAG(TSDELTA(y)+t-w2,1)+c4*time
COEFF> c1 c2 c3 c4
PDL> c3 1 3

COMMENT> Delta Gross National Product
IDENTITY> y
EQ> TSDELTA(y) = EXP(cn) + LOG(i) + g - t

COMMENT> Profits
IDENTITY> p
EQ> p = TSDELTA(y) - (w1+w2)

COMMENT> Capital Stock with switches
IDENTITY> k
EQ> k = TSLAG(k,1) + LOG(i)
IF> LOG(i) > 0
IDENTITY> k
EQ> k = TSLAG(k,1)
IF> LOG(i) <= 0

END"


###################################################
### code chunk number 23: computation
###################################################

#adjust the original data in order to estimate and to simulate the model
lhsKleinModelData <- within(kleinModelData,{
  i  = exp(i);     #we have LOG(i)     in the model MDL definition
  cn = log(cn);    #we have EXP(cn)    in the model MDL definition
  y  = CUMSUM(y)   #we have TSDELTA(y) in the model MDL definition
})


###################################################
### code chunk number 24: computation
###################################################

lhsKleinModel <- LOAD_MODEL(modelText = lhsKlein1.txt)
lhsKleinModel <- LOAD_MODEL_DATA(lhsKleinModel, lhsKleinModelData)


###################################################
### code chunk number 25: computation
###################################################

#ESTIMATE and SIMULATE functions are described later
lhsKleinModel <- ESTIMATE(lhsKleinModel)
lhsKleinModel <- SIMULATE(lhsKleinModel, TSRANGE = c(1925,1,1930,1))


###################################################
### code chunk number 26: bimets.Rnw:646-647
###################################################
kleinModel <- ESTIMATE(kleinModel, quietly = TRUE)


###################################################
### code chunk number 27: bimets.Rnw:652-653
###################################################
kleinModel <- ESTIMATE(kleinModel, eqList = c('cn'))


###################################################
### code chunk number 28: bimets.Rnw:658-668
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
### code chunk number 29: bimets.Rnw:676-722
###################################################
#define model
advancedKlein1.txt <- 
"MODEL

COMMENT> Modified Klein Model 1 of the U.S. Economy with PDL, 
COMMENT> autocorrelation on errors, restrictions and 
COMMENT> conditional equation evaluations

COMMENT> Consumption with autocorrelation on errors
BEHAVIORAL> cn
TSRANGE 1923 1 1940 1
EQ> cn =  a1 + a2*p + a3*TSLAG(p,1) + a4*(w1+w2) 
COEFF> a1 a2 a3 a4
ERROR> AUTO(2)

COMMENT> Investment with restrictions
BEHAVIORAL> i
TSRANGE 1923 1 1940 1
EQ> i = b1 + b2*p + b3*TSLAG(p,1) + b4*TSLAG(k,1)
COEFF> b1 b2 b3 b4
RESTRICT> b2 + b3 = 1

COMMENT> Demand for Labor with PDL
BEHAVIORAL> w1 
TSRANGE 1923 1 1940 1
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
### code chunk number 30: bimets.Rnw:724-727
###################################################
#load model and data
advancedKleinModel <- LOAD_MODEL(modelText = advancedKlein1.txt)
advancedKleinModel <- LOAD_MODEL_DATA(advancedKleinModel, kleinModelData)


###################################################
### code chunk number 31: bimets.Rnw:729-731
###################################################
#estimate model
advancedKleinModel <- ESTIMATE(advancedKleinModel)


###################################################
### code chunk number 32: bimets.Rnw:766-773
###################################################
#chow test for the consumption equation
#base TSRANGE set to 1921/1935
kleinModelChow <- ESTIMATE(kleinModel
                       ,eqList = 'cn'
                       ,TSRANGE = c(1921,1,1935,1)
                       ,forceTSRANGE = TRUE
                       ,CHOWTEST = TRUE)


###################################################
### code chunk number 33: bimets.Rnw:824-846
###################################################
#FORECAST GNP in 1942:1944
#we need to extend exogenous variables in 1942 up to 1944
#in this exercise we perform a simple time series extension
kleinModel$modelData <- within(kleinModel$modelData,{
            w2   = TSEXTEND(w2,   UPTO = c(1944,1), EXTMODE = 'CONSTANT')
            t    = TSEXTEND(t,    UPTO = c(1944,1), EXTMODE = 'CONSTANT')
            g    = TSEXTEND(g,    UPTO = c(1944,1), EXTMODE = 'CONSTANT')
            time = TSEXTEND(time, UPTO = c(1944,1), EXTMODE = 'LINEAR')
            })

 
#simulate model
kleinModel <- SIMULATE(kleinModel
            ,simType = 'FORECAST'
            ,TSRANGE = c(1941,1,1944,1)
            ,simConvergence = 0.00001
            ,simIterLimit = 100
            ,quietly = TRUE
            )

#get forecasted GNP
TABIT(kleinModel$simulation$y)


###################################################
### code chunk number 34: computation
###################################################
#DYNAMIC NEWTON SIMULATION EXAMPLE WITH EXOGENIZATION AND CONSTANT ADJUSTMENTS
 
#define exogenization list
#'cn' exogenized in 1923-1925
#'i' exogenized in whole TSRANGE
exogenizeList <- list(
                 cn = c(1923,1,1925,1)
                ,i  = TRUE
              )
 
#define add-factor list
constantAdjList <- list(
                          cn = TIMESERIES(1,-1, START = c(1923,1), FREQ = 'A')
                         ,y  = TIMESERIES(0.1,-0.1,-0.5, START = c(1926,1), FREQ = 'A')
                        )
 
#simulate model
kleinModel <- SIMULATE(kleinModel
                  ,simAlgo='NEWTON'
                  ,simType = 'DYNAMIC'
                  ,TSRANGE = c(1923,1,1941,1)
                  ,simConvergence = 0.00001
                  ,simIterLimit = 100
                  ,Exogenize = exogenizeList
                  ,ConstantAdjustment = constantAdjList 
                  )


###################################################
### code chunk number 35: bimets.Rnw:934-1009
###################################################

#COMPARE FORECAST IN 3 ALTERNATIVE 
#EXOGENOUS SCENARIOS

#create 3 new models for the 3 scenarios
modelScenario1 <- advancedKleinModel
modelScenario2 <- advancedKleinModel
modelScenario3 <- advancedKleinModel

#scenario 1, define exogenous paths
modelScenario1$modelData <- within(modelScenario1$modelData,{
              k    = TSEXTEND(k,   UPTO=c(1943,1))
              w2   = TSEXTEND(w2,  UPTO=c(1943,1))
              t    = TSEXTEND(t,   UPTO=c(1943,1))
              g    = TSEXTEND(g,   UPTO=c(1943,1))
              time = TSEXTEND(time,UPTO=c(1943,1)
                                  ,EXTMODE='LINEAR')
})

#scenario 2, define exogenous paths
modelScenario2$modelData <- within(modelScenario2$modelData,{
              k    = TSEXTEND(k,   UPTO=c(1943,1))
              w2   = TSEXTEND(w2,  UPTO=c(1943,1))
              t    = TSEXTEND(t,   UPTO=c(1943,1))
              g    = TSEXTEND(g,   UPTO=c(1943,1)
                            ,EXTMODE='LINEAR')
              time = TSEXTEND(time,UPTO=c(1943,1)
                            ,EXTMODE='LINEAR')
})

#scenario 3, define exogenous paths
#we also change consumption cn add-factor
modelScenario3$modelData <- within(modelScenario3$modelData,{
              k    = TSEXTEND(k,   UPTO=c(1943,1))
              w2   = TSEXTEND(w2,  UPTO=c(1943,1)
                              ,EXTMODE='MEAN4')
              t    = TSEXTEND(t,   UPTO=c(1943,1))
              g    = TSEXTEND(g,   UPTO=c(1943,1)
                              ,EXTMODE='LINEAR')
              time = TSEXTEND(time,UPTO=c(1943,1)
                              ,EXTMODE='LINEAR')
})
constantAdjListScenario3 <- constantAdjList
constantAdjListScenario3$cn[[1941,1]] <- c(1,2,3)

#simulate the 3 models
modelScenario1 <- SIMULATE(modelScenario1
                            ,simAlgo='NEWTON'
                            ,simType='FORECAST'
                            ,TSRANGE=c(1940,1,1943,1)
                            ,simConvergence=1e-5
                            ,simIterLimit=20
                            ,quietly=TRUE)

modelScenario2 <- SIMULATE(modelScenario2
                            ,simAlgo='NEWTON'
                            ,simType='FORECAST'
                            ,TSRANGE=c(1940,1,1943,1)
                            ,simConvergence=1e-5
                            ,simIterLimit=20
                            ,quietly=TRUE)

modelScenario3 <- SIMULATE(modelScenario3
                            ,simAlgo='NEWTON'
                            ,simType='FORECAST'
                            ,TSRANGE=c(1940,1,1943,1)
                            ,simConvergence=1e-5
                            ,simIterLimit=20
                            ,ConstantAdjustment=constantAdjListScenario3 
                            ,quietly=TRUE)

#compare results on GNP
TABIT(modelScenario1$simulation$y, 
      modelScenario2$simulation$y, 
      modelScenario3$simulation$y)


###################################################
### code chunk number 36: bimets.Rnw:1023-1041
###################################################

#we want to perform a stochastic forecast of the GNP up to 1944
#we will add normal disturbances to endogenous Consumption 'cn' 
#in 1942 by using its regression standard error
#we will add uniform disturbances to exogenous Government Expenditure 'g'
#in whole TSRANGE
myStochStructure <- list(
  cn = list(
         TSRANGE = c(1942,1,1942,1)
        ,TYPE = 'NORM'
        ,PARS = c(0,advancedKleinModel$behaviorals$cn$statistics$StandardErrorRegression)
        ),
  g = list(
         TSRANGE = TRUE
        ,TYPE = 'UNIF'
        ,PARS = c(-1,1)
        )
  )


###################################################
### code chunk number 37: bimets.Rnw:1043-1051
###################################################
#we need to extend exogenous variables up to 1944
advancedKleinModel$modelData <- within(advancedKleinModel$modelData,{
    w2    = TSEXTEND(w2,   UPTO = c(1944,1), EXTMODE = 'CONSTANT')
    t     = TSEXTEND(t,    UPTO = c(1944,1), EXTMODE = 'LINEAR')
    g     = TSEXTEND(g,    UPTO = c(1944,1), EXTMODE = 'CONSTANT')
    k     = TSEXTEND(k,    UPTO = c(1944,1), EXTMODE = 'LINEAR')
    time  = TSEXTEND(time, UPTO = c(1944,1), EXTMODE = 'LINEAR')
  })


###################################################
### code chunk number 38: bimets.Rnw:1053-1060
###################################################
#stochastic model forecast
advancedKleinModel <- STOCHSIMULATE(advancedKleinModel
                      ,simType = 'FORECAST'
                      ,TSRANGE = c(1941,1,1944,1)
                      ,StochStructure = myStochStructure
                      ,StochSeed = 123
                      ,quietly = TRUE)


###################################################
### code chunk number 39: bimets.Rnw:1062-1064
###################################################
#print mean and standard deviation of forecasted GNP
with(advancedKleinModel$stochastic_simulation, TABIT(y$mean, y$sd))


###################################################
### code chunk number 40: bimets.Rnw:1066-1069
###################################################
#print the unperturbed forecasted GNP along with the
#first 5 perturbed realizations
with(advancedKleinModel$simulation_MM, print(y[,1:6]))


###################################################
### code chunk number 41: bimets.Rnw:1077-1079
###################################################
TSRANGE <- c(1935,1,1940,1)
StochReplica <- 100


###################################################
### code chunk number 42: bimets.Rnw:1081-1089
###################################################
#we will perturb simulation by using regression residuals
#get cn and i residuals in TSRANGE
cn_residuals <- TSPROJECT(advancedKleinModel$behaviorals$cn$residuals, 
                          TSRANGE=TSRANGE,
                          ARRAY = TRUE)
i_residuals <- TSPROJECT(advancedKleinModel$behaviorals$i$residuals,
                         TSRANGE=TSRANGE,
                         ARRAY = TRUE)


###################################################
### code chunk number 43: bimets.Rnw:1091-1102
###################################################
#define stochastic matrices
cn_matrix <- c()
i_matrix <- c()

#populate matrices
for (idx in 1:StochReplica)
{
  rand <- rnorm(1,0,1)
  cn_matrix <- cbind(cn_matrix,rand*cn_residuals)
  i_matrix <- cbind(i_matrix,rand*i_residuals)
}


###################################################
### code chunk number 44: bimets.Rnw:1104-1117
###################################################
#define stochastic structure
myStochStructure <- list(
  cn=list(
    TSRANGE=TRUE,
    TYPE='MATRIX',
    PARS=cn_matrix
  ),
  i=list(
    TSRANGE=TRUE,
    TYPE='MATRIX',
    PARS=i_matrix
  )
)


###################################################
### code chunk number 45: bimets.Rnw:1119-1124
###################################################
#stochastic simulation
advancedKleinModel <- STOCHSIMULATE(advancedKleinModel
                                    ,TSRANGE=TSRANGE
                                    ,StochStructure=myStochStructure
                                    ,quietly = TRUE)


###################################################
### code chunk number 46: bimets.Rnw:1126-1128
###################################################
#print GNP mean and sd
with(advancedKleinModel$stochastic_simulation,TABIT(y$mean, y$sd))


###################################################
### code chunk number 47: bimets.Rnw:1154-1161
###################################################
kleinModel <- MULTMATRIX(kleinModel
                        ,TSRANGE = c(1941,1,1941,1)
                        ,INSTRUMENT = c('w2','g')
                        ,TARGET = c('cn','y')
                      )

kleinModel$MultiplierMatrix


###################################################
### code chunk number 48: bimets.Rnw:1167-1176
###################################################
#multi-period interim multipliers
kleinModel <- MULTMATRIX(kleinModel
                   ,TSRANGE = c(1940,1,1941,1)
                   ,INSTRUMENT = c('w2','g')
                   ,TARGET = c('cn','y'))

#output multipliers matrix (note the zeros when the period
#of the INSTRUMENT is greater than the period of the TARGET)
kleinModel$MultiplierMatrix


###################################################
### code chunk number 49: bimets.Rnw:1199-1205
###################################################
#we want an arbitrary value on Consumption of 66 in 1940 and 78 in 1941
#we want an arbitrary value on GNP of 77 in 1940 and 98 in 1941
kleinTargets <- list(
               cn = TIMESERIES(66,78, START = c(1940,1), FREQ = 1)
              ,y  = TIMESERIES(77,98, START = c(1940,1), FREQ = 1)
              )


###################################################
### code chunk number 50: computation
###################################################
kleinModel <- RENORM(kleinModel
                   ,INSTRUMENT = c('w2','g')
                   ,TARGET = kleinTargets
                   ,TSRANGE = c(1940,1,1941,1)
                   ,simIterLimit = 100
                   ,quietly = TRUE )


###################################################
### code chunk number 51: bimets.Rnw:1221-1228
###################################################
with(kleinModel,TABIT(modelData$w2
                      ,renorm$INSTRUMENT$w2
                      ,modelData$g
                      ,renorm$INSTRUMENT$g
                      ,TSRANGE = c(1940,1,1941,1)
                      )
     )


###################################################
### code chunk number 52: bimets.Rnw:1235-1237
###################################################
#create a new model
kleinRenorm <- kleinModel


###################################################
### code chunk number 53: bimets.Rnw:1239-1241
###################################################
#get instruments to be used
newInstruments <- kleinModel$renorm$INSTRUMENT


###################################################
### code chunk number 54: bimets.Rnw:1243-1254
###################################################
#change exogenous by using new instruments data
kleinRenorm$modelData <- within(kleinRenorm$modelData,
                 {
                   w2[[1940,1]] = newInstruments$w2[[1940,1]]
                   w2[[1941,1]] = newInstruments$w2[[1941,1]]
                   g[[1940,1]]  = newInstruments$g[[1940,1]]
                   g[[1941,1]]  = newInstruments$g[[1941,1]]
                 }
                )
#users can also replace last two commands with:
#kleinRenorm$modelData <- kleinRenorm$renorm$modelData


###################################################
### code chunk number 55: bimets.Rnw:1256-1262
###################################################
#simulate the new model
kleinRenorm <- SIMULATE(kleinRenorm
                      ,TSRANGE = c(1940,1,1941,1)
                      ,simConvergence = 0.00001
                      ,simIterLimit = 100
                      ,quietly = TRUE)


###################################################
### code chunk number 56: bimets.Rnw:1264-1268
###################################################
#verify targets are achieved
with(kleinRenorm$simulation,
     TABIT(cn,y)
     )


###################################################
### code chunk number 57: bimets.Rnw:1301-1304
###################################################
#load the advanced model
advancedKleinModel <- LOAD_MODEL(modelText = advancedKlein1.txt
                                 ,quietly = TRUE)


###################################################
### code chunk number 58: bimets.Rnw:1306-1310
###################################################
#load time series into the model object
advancedKleinModel <- LOAD_MODEL_DATA(advancedKleinModel
                                      ,kleinModelData
                                      ,quietly = TRUE)


###################################################
### code chunk number 59: bimets.Rnw:1312-1315
###################################################
#estimate the model
advancedKleinModel <- ESTIMATE(advancedKleinModel 
                               ,quietly = TRUE)


###################################################
### code chunk number 60: bimets.Rnw:1317-1324
###################################################
#we want to maximize the non-linear objective function:
#f()=(y-110)+(cn-90)*ABS(cn-90)-(g-20)^0.5
#in 1942 by using INSTRUMENT cn in range (-5,5) 
#(cn is endogenous so we use the add-factor)
#and g in range (15,25)
#we will also impose the following non-linear restriction:
#g+(cn^2)/2<27 & g+cn>17


###################################################
### code chunk number 61: bimets.Rnw:1326-1334
###################################################
#we need to extend exogenous variables up to 1942
advancedKleinModel$modelData <- within(advancedKleinModel$modelData,{
    w2    = TSEXTEND(w2,   UPTO = c(1942,1), EXTMODE = 'CONSTANT')
    t     = TSEXTEND(t,    UPTO = c(1942,1), EXTMODE = 'LINEAR')
    g     = TSEXTEND(g,    UPTO = c(1942,1), EXTMODE = 'CONSTANT')
    k     = TSEXTEND(k,    UPTO = c(1942,1), EXTMODE = 'LINEAR')
    time  = TSEXTEND(time, UPTO = c(1942,1), EXTMODE = 'LINEAR')
})


###################################################
### code chunk number 62: bimets.Rnw:1336-1343
###################################################
#define INSTRUMENT and boundaries
myOptimizeBounds <- list(
    cn = list( TSRANGE = TRUE
            ,BOUNDS = c(-5,5)),
     g = list( TSRANGE = TRUE
            ,BOUNDS = c(15,25))
)


###################################################
### code chunk number 63: bimets.Rnw:1345-1351
###################################################
#define restrictions
myOptimizeRestrictions <- list(
    myRes1=list(
         TSRANGE = TRUE
        ,INEQUALITY = 'g+(cn^2)/2<27 & g+cn>17')
)


###################################################
### code chunk number 64: bimets.Rnw:1353-1359
###################################################
#define objective function
myOptimizeFunctions <- list(
    myFun1 = list(
         TSRANGE = TRUE
        ,FUNCTION = '(y-110)+(cn-90)*ABS(cn-90)-(g-20)^0.5')
)


###################################################
### code chunk number 65: bimets.Rnw:1361-1374
###################################################
#Monte-Carlo optimization by using 10000 stochastic realizations
#and 1E-4 convergence criterion 
advancedKleinModel <- OPTIMIZE(advancedKleinModel
                          ,simType = 'FORECAST'
                          ,TSRANGE=c(1942,1,1942,1)
                          ,simConvergence= 1E-4
                          ,simIterLimit  = 1000
                          ,StochReplica  = 10000
                          ,StochSeed = 123
                          ,OptimizeBounds = myOptimizeBounds
                          ,OptimizeRestrictions = myOptimizeRestrictions
                          ,OptimizeFunctions = myOptimizeFunctions
                          ,quietly = TRUE)


###################################################
### code chunk number 66: bimets.Rnw:1376-1378
###################################################
#print local maximum
advancedKleinModel$optimize$optFunMax


###################################################
### code chunk number 67: bimets.Rnw:1380-1382
###################################################
#print INSTRUMENT that allow local maximum to be achieved
advancedKleinModel$optimize$INSTRUMENT


###################################################
### code chunk number 68: bimets.Rnw:1384-1388
###################################################
#LET'S VERIFY RESULTS
#copy into modelData the computed INSTRUMENT 
#that allow to maximize the objective function 
advancedKleinModel$modelData <- advancedKleinModel$optimize$modelData


###################################################
### code chunk number 69: bimets.Rnw:1390-1402
###################################################
#simulate the model by using the new INSTRUMENT
#note: we used cn add-factor as OPTIMIZE instrument, so we need 
#to pass the computed cn add-factor to the SIMULATE call
newConstantAdjustment <- advancedKleinModel$optimize$ConstantAdjustment
advancedKleinModel <- SIMULATE(advancedKleinModel
                  ,simType = 'FORECAST'
                  ,TSRANGE = c(1942,1,1942,1)
                  ,simConvergence = 1E-5
                  ,simIterLimit = 1000
                  ,ConstantAdjustment = newConstantAdjustment
                  ,quietly = TRUE
)


###################################################
### code chunk number 70: bimets.Rnw:1404-1410
###################################################
#calculate objective function by using the SIMULATE output time series
#(y-110)+(cn-90)*ABS(cn-90)-(g-20)^0.5
y  <- advancedKleinModel$simulation$y
cn <- advancedKleinModel$simulation$cn
g  <- advancedKleinModel$modelData$g
optFunTest <- (y-110)+(cn-90)*abs(cn-90)-(g-20)^0.5


###################################################
### code chunk number 71: bimets.Rnw:1412-1419
###################################################
#verify computed max is equal to optimization max
#(in the following command TSPROJECT could be omitted because
#myFun1$TSRANGE = TRUE)
abs(sum(TSPROJECT(optFunTest
              ,TSRANGE = c(1942,1,1942,1)
              ,ARRAY   = TRUE)
        ) - advancedKleinModel$optimize$optFunMax) < 1E-4


###################################################
### code chunk number 72: bimets.Rnw:1444-1445 (eval = FALSE)
###################################################
## install.packages('bimets')


###################################################
### code chunk number 73: bimets.Rnw:1447-1448
###################################################
library(bimets)


