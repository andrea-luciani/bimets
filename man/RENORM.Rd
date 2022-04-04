\name{RENORM}
\alias{RENORM}


\title{
Endogenous targeting of a BIMETS model.
}
\description{
\if{html}{(Note: this is the html version of the reference manual. Please consider reading the \href{https://CRAN.R-project.org/package=bimets/bimets.pdf}{pdf version} of this reference manual, or the package \href{https://cran.r-project.org/package=bimets/vignettes/bimets.pdf}{vignette}, wherein there are figures and the mathematical expressions are better formatted than in html.)\cr\cr}

The endogenous targeting of econometric models (a.k.a. "renormalization") consists of solving the model while interchanging the role of one or more endogenous variables with an equal number of exogenous variables.\cr\cr This procedure determines the values for the \code{INSTRUMENT} exogenous variables that allow the objective \code{TARGET} endogenous values to be achieved, with respect to the constraints given by the model equations (see \code{\link[=MDL]{MDL}}).\cr\cr
This is an approach to economic and monetary policy analysis, and is based on two assumptions:\cr\cr
1. there exists a desired level for a set of the \code{n} endogenous variables defined as \code{TARGET};\cr
2. there exists a set of the \code{n} exogenous variables defined as \code{INSTRUMENT};\cr\cr
Given these premises, the endogenous targeting process consists in determining the values of the exogenous variables chosen as \code{INSTRUMENT} allowing us to achieve the desired values for the endogenous variables designated as \code{TARGET}. In other words the procedure allows users to exchange the role of exogenous and endogenous among a set of variables pairs. \cr\cr
Given a list of exogenous \code{INSTRUMENT} variables and a list of \code{TARGET} endogenous time series, the iterative procedure can be split into the following steps:\cr\cr
1. Computation of the multipliers matrix \code{MULTMAT} of the \code{TARGET} endogenous variables with respect to the \code{INSTRUMENT} exogenous variables (this is a square matrix by construction);\cr\cr
2. Solution of the linear system (if any): \cr\cr
\eqn{V_{exog}(i+1) = V_{exog}(i) +}{V_(exog)(i+1) = V_(exog)(i) +} \code{MULTMAT} \eqn{^{-1} * (V_{endog}(i) -}{^(-1) * (V_(endog)(i) -} \code{TARGET} \eqn{)}, where \eqn{V_{exog}(i)}{V_(exog)(i)} are the exogenous variables in the \code{INSTRUMENT} list and \eqn{V_{endog}(i)}{V_(endog)(i)} are the endogenous variables that have a related target in the \code{TARGET} list, given \eqn{i} the current iteration;\cr\cr
3. Simulation of the model with the new set of exogenous variables computed in step 2, then a convergence check by comparing the subset of endogenous variables arising from this simulation and the related time series in \code{TARGET} list. If the convergence condition is satisfied, or the maximum number of iterations is reached, the algorithm will stop, otherwise it will go back to step 1;\cr\cr
Users can also declare an endogenous variable as an \code{INSTRUMENT} variable. In this case, the constant adjustment (see \code{\link[=SIMULATE]{SIMULATE}}) related to the provided endogenous variable will be used as the instrument exogenous variable. This procedure is particularly suited for the automatic computation of the add-factors needed to fine tune the model into a baseline path and to improve the forecasting accuracy.\cr\cr
If the convergence condition is satisfied, the \code{RENORM} procedure will return the \code{INSTRUMENT} time series allowing us to achieve the desired values for the endogenous variables designated as \code{TARGET}.\cr\cr



}
\usage{

RENORM(model=NULL,
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
       ...
       )
}

\arguments{
\item{model}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{TSRANGE}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{simType}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{simConvergence}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{simIterLimit}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{ZeroErrorAC}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{Exogenize}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{ConstantAdjustment}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{verbose}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{verboseSincePeriod}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{verboseVars}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{renormIterLimit}{The value representing the maximum number of iterations to be performed. The iterative renormalization procedure will stop when \code{renormIterLimit} is reached or the \code{TARGET} variables satisfy the \code{renormConvergence} criterion}
\item{renormConvergence}{The convergence value requested for the iterative renormalization process, that stops when the Euclidean distance between each \code{TARGET} time series and the related simulated endogenous variable is less than the \code{renormConvergence} value
}

\item{TARGET}{A named list that specifies the target endogenous variables. List names must be equal to the names of the target endogenous variables involved in the renormalization; each list element must contain the time series of the desired target endogenous values; time series must be compliant with the compliance control check defined in \code{\link[=is.bimets]{is.bimets}} (see example)}
\item{INSTRUMENT}{A \code{character} array built with the names of the instrument exogenous variables involved in the renormalization. User can also declare an endogenous variable as \code{INSTRUMENT} variable: in this case the constant adjustment (see \code{\link[=SIMULATE]{SIMULATE}}) related to the provided endogenous variable will be used as instrument exogenous variable (see example)
 }
 \item{tol}{
  the tolerance for detecting linear dependencies in the columns of a matrix while an inversion is requested. }
\item{MM_SHOCK}{see \code{\link[=MULTMATRIX]{MULTMATRIX}}}
\item{quietly}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{avoidCompliance}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{...}{see \code{\link[=SIMULATE]{SIMULATE}}}
}


\value{
This function will add a new named element \code{renorm} into the output BIMETS model object.\cr\cr
This new \code{renorm} element is a named list that contains the following elements:\cr\cr 
- \code{INSTRUMENT}: a named list that contains the \code{INSTRUMENT} exogenous time series that allow the objective \code{TARGET} endogenous values to be achieved. This element is populated only if the convergence is reached. List names are equal to the names of the related exogenous variables. Users can also declare an endogenous variable as \code{INSTRUMENT} variable: in this case the constant adjustment (see \code{\link[=SIMULATE]{SIMULATE}}) related to the provided endogenous variable will be used as instrument exogenous variable, and this \code{INSTRUMENT} output list will contains the constant adjustment time series that allow the objective \code{TARGET} endogenous values to be achieved (see example);\cr\cr
- \code{TARGET}: a named list built with the achieved \code{TARGET} endogenous time series. List names are equal to the target endogenous variable names; \cr\cr
- \code{unConvergedTARGET}: the names array of the endogenous \code{TARGET} variables that failed the convergence. This element is populated only if the convergence has not been reached;\cr\cr
- \code{modelData}: the whole model input dataset wherein the \code{INSTRUMENT} exogenous variables have been modified accordingly to the \code{RENORM} results. This data can be useful in order to refine results or to verify that the model, fed with the proper \code{INSTRUMENT} exogenous time series, produces the desired \code{TARGET} endogenous values. This element is populated only if the convergence is achieved (see example);\cr\cr
- \code{ConstantAdjustment}: a modified constant adjustment input list (see \code{\link[=SIMULATE]{SIMULATE}}) wherein the constant adjustment time series related to a \code{INSTRUMENT} endogenous variables have been modified accordingly to the \code{RENORM} results. This data can be useful in order to refine results or to verify that the model, fed with the proper \code{INSTRUMENT} exogenous time series (therefore with the proper \code{ConstantAdjustment} time series, if any), produces the desired \code{TARGET} endogenous values (see example). This element is populated only if the convergence is achieved;\cr\cr
- \code{__RENORM_PARAMETERS__}: a named list that contains the arguments passed to the function call during the latest \code{RENORM} run, e.g. \code{TSRANGE}, \code{renormIterLimit}, \code{INSTRUMENT}, \code{TARGET}, etc.: this data can be useful in order to replicate the renorm results. \cr\cr



}

\seealso{
\code{\link[=MDL]{MDL}}\cr
\code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
\code{\link[=ESTIMATE]{ESTIMATE}}\cr
\code{\link[=SIMULATE]{SIMULATE}}\cr
\code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}\cr
\code{\link[=MULTMATRIX]{MULTMATRIX}}\cr
\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
}

\examples{

#define model
myModelDefinition<-
"MODEL 
COMMENT> Klein Model 1 of the U.S. Economy 

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

END"

#define model data
myModelData<-list(
  cn
  =TIMESERIES(39.8,41.9,45,49.2,50.6,52.6,55.1,56.2,57.3,57.8,55,50.9,
              45.6,46.5,48.7,51.3,57.7,58.7,57.5,61.6,65,69.7,
              START=c(1920,1),FREQ=1),
  g
  =TIMESERIES(4.6,6.6,6.1,5.7,6.6,6.5,6.6,7.6,7.9,8.1,9.4,10.7,10.2,9.3,10,
              10.5,10.3,11,13,14.4,15.4,22.3,
              START=c(1920,1),FREQ=1),
  i
  =TIMESERIES(2.7,-.2,1.9,5.2,3,5.1,5.6,4.2,3,5.1,1,-3.4,-6.2,-5.1,-3,-1.3,
              2.1,2,-1.9,1.3,3.3,4.9,
              START=c(1920,1),FREQ=1),
  k
  =TIMESERIES(182.8,182.6,184.5,189.7,192.7,197.8,203.4,207.6,210.6,215.7,
              216.7,213.3,207.1,202,199,197.7,199.8,201.8,199.9,
              201.2,204.5,209.4,
              START=c(1920,1),FREQ=1),
  p
  =TIMESERIES(12.7,12.4,16.9,18.4,19.4,20.1,19.6,19.8,21.1,21.7,15.6,11.4,
              7,11.2,12.3,14,17.6,17.3,15.3,19,21.1,23.5,
              START=c(1920,1),FREQ=1),
  w1
  =TIMESERIES(28.8,25.5,29.3,34.1,33.9,35.4,37.4,37.9,39.2,41.3,37.9,34.5,
              29,28.5,30.6,33.2,36.8,41,38.2,41.6,45,53.3,
              START=c(1920,1),FREQ=1),
  y
  =TIMESERIES(43.7,40.6,49.1,55.4,56.4,58.7,60.3,61.3,64,67,57.7,50.7,41.3,
              45.3,48.9,53.3,61.8,65,61.2,68.4,74.1,85.3,
              START=c(1920,1),FREQ=1),
  t
  =TIMESERIES(3.4,7.7,3.9,4.7,3.8,5.5,7,6.7,4.2,4,7.7,7.5,8.3,5.4,6.8,7.2,
              8.3,6.7,7.4,8.9,9.6,11.6,
              START=c(1920,1),FREQ=1),
  time
  =TIMESERIES(NA,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,
              START=c(1920,1),FREQ=1),
  w2
  =TIMESERIES(2.2,2.7,2.9,2.9,3.1,3.2,3.3,3.6,3.7,4,4.2,4.8,5.3,5.6,6,6.1,
              7.4,6.7,7.7,7.8,8,8.5,
              START=c(1920,1),FREQ=1)
)

#load model and model data
myModel<-LOAD_MODEL(modelText=myModelDefinition)
myModel<-LOAD_MODEL_DATA(myModel, myModelData)

#estimate model
myModel<-ESTIMATE(myModel)

#we want an arbitrary value of 66 on Consumption 'cn' in 1940 and 78 in 1941
#we want an arbitrary value of 77 on GNP 'y' in 1940 and 98 in 1941
kleinTargets<-list(
  cn = TSERIES(66,78,START=c(1940,1),FREQ=1),
  y  = TSERIES(77,98,START=c(1940,1),FREQ=1)
)

#Then, we can perform the model endogenous targeting by using Government Wage Bill 'w2' 
#and Government Expenditure 'g' as 
#INSTRUMENT in the years 1940 and 1941:
myModel<-RENORM(myModel
                    ,INSTRUMENT = c('w2','g')
                    ,TARGET = kleinTargets
                    ,TSRANGE = c(1940,1,1941,1)
                    ,simIterLimit = 100
)


with(myModel,TABIT(modelData$w2,
                   renorm$INSTRUMENT$w2,
                   modelData$g,
                   renorm$INSTRUMENT$g))

#  Date, Prd., modelData$w2, renorm$INSTRUMENT$w2, modelData$g, renorm$INSTRUMENT$g
# etc.
#  1938, 1   ,          7.7,                     ,          13,           
#  1939, 1   ,          7.8,                     ,        14.4,           
#  1940, 1   ,            8,              7.41333,        15.4,             16.1069
#  1941, 1   ,          8.5,               9.3436,        22.3,             22.6599
#So, if we want to achieve on Consumption 'cn' 
#an arbitrary simulated value of 66 
#in 1940 and 78 in 1941, and if we want 
#to achieve on GNP 'y' an arbitrary 
#simulated value of 77 in 1940 and 98 in 1941, 
#we need to change exogenous 'w2' from 8 to 7.41 
#in 1940 and from 8.5 to 9.34 in 1941, 
#and we need to change exogenous 'g' 
#from 15.4 to 16.1 in 1940 and from 22.3 to 22.66 in 1941

#Let's verify:
#create a new model
kleinRenorm<-myModel

#get instruments to be used
newInstruments=myModel$renorm$INSTRUMENT

#change exogenous by using new instruments
kleinRenorm$modelData<-within(kleinRenorm$modelData,
                 {
                   w2[[1940,1]]=newInstruments$w2[[1940,1]]
                   w2[[1941,1]]=newInstruments$w2[[1941,1]]
                   g[[1940,1]] =newInstruments$g[[1940,1]]
                   g[[1941,1]] =newInstruments$g[[1941,1]]
                 }
                )
#users can also replace last two commands with:
#kleinRenorm$modelData<-kleinRenorm$renorm$modelData

#simulate the new model
kleinRenorm<-SIMULATE(kleinRenorm
                      ,TSRANGE=c(1940,1,1941,1)
                      ,simConvergence=0.00001
                      ,simIterLimit=100
                      )
#Simulation: 100.00%
#...SIMULATE OK

#verify targets are achieved
with(kleinRenorm$simulation,
     TABIT(cn,y)
     )
     
#Date, Prd., cn        , y
#1940,  1  , 66.01116  , 77.01772
#1941,  1  , 78.02538  , 98.04121



######################################################################


#now use 'i' endogenous variable as an instrument

#first, define the related exogenous constant adjustment
myCA<-list(i = myModel$modelData$i*0+0.1)

#run renorm with endogenous 'i' as instrument
myModel<-RENORM(myModel
               ,INSTRUMENT = c('w2','i')
               ,TARGET = kleinTargets
               ,TSRANGE = c(1940,1,1941,1)
               ,simIterLimit = 100
               ,ConstantAdjustment = myCA
)

#get the values of the constant adjustment for the endogenous 'i' 
#in 1940-1941 that allow achieving the target values for 'cn' and 'y'
myModel$renorm$ConstantAdjustment
#$i
#Time Series:
#Start = 1920 
#End = 1941 
#Frequency = 1 
# [1] 0.1000000 0.1000000 0.1000000 ...
#[20] 0.1000000 0.7069039 0.4388811

#these values are also reported in the INSTRUMENT output list
myModel$renorm$INSTRUMENT$i
#Time Series:
#Start = 1940 
#End = 1941 
#Frequency = 1 
#[1] 0.7069039 0.4388811


}


 
