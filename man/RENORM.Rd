\name{RENORM}
\alias{RENORM}


\title{
Perform the endogenous targeting (a.k.a. "renormalization"") of a BIMETS model object.
}
\description{
\if{html}{(Note: this is the html version of the reference manual. Please consider reading the pdf version of this reference manual, wherein there are figures and the mathematical expressions are better formatted than in html.)\cr\cr}

The endogenous targeting of econometric models (a.k.a. "renormalization") consists of solving the model while interchanging the role of one or more endogenous variables with an equal number of exogenous variables.\cr\cr This function determines the values for the \code{INSTRUMENT} exogenous variables which allow the objective \code{TARGET} endogenous variables to be achieved, with respect to the constraints given by the model \code{MDL} definition.\cr\cr
This is an approach to economic and monetary policy analysis, and is based on two assumptions:\cr\cr
1. there exists a desired level for a set of \code{n} endogenous variables defined as \code{TARGET};\cr
2. there exists a set of \code{n} exogenous variables defined as \code{INSTRUMENT};\cr\cr
Given these premises, the endogenous targeting process consists in determining the values of the exogenous variables chosen as \code{INSTRUMENT} allowing us to achieve the desired values for the endogenous variables designated as \code{TARGET}. In other words the procedure allows users to exchange the role of exogenous and endogenous among a set of time series pairs. \cr\cr
Given a list of exogenous \code{INSTRUMENT} variables and a list of \code{TARGET} endogenous time series, the iterative procedure can be split into the following steps:\cr\cr
1. Computation of the multipliers matrix \code{MULTMAT} of the \code{TARGET} endogenous variables with respect to the \code{INSTRUMENT} exogenous variables (this is a square matrix by construction);\cr\cr
2. Solution of the linear system: \cr\cr
\eqn{V_{exog}(i+1) = V_{exog}(i) +}{V_(exog)(i+1) = V_(exog)(i) +} \code{MULTMAT} \eqn{^{-1} * (V_{endog}(i) -}{^(-1) * (V_(endog)(i) -} \code{TARGET} \eqn{)}, where \eqn{V_{exog}(i)}{V_(exog)(i)} are the exogenous variables in the \code{INSTRUMENT} list and \eqn{V_{endog}(i)}{V_(endog)(i)} are the endogenous variables that have a related target in the \code{TARGET} list, given \eqn{i} the current iteration;\cr\cr
3. Simulation of the model with the new set of exogenous variables computed in step 2, then a convergence check by comparing the subset of endogenous variables arising from this simulation and the related time series in \code{TARGET} list. If the convergence condition is satisfied, or the maximum number of iterations is reached, the algorithm will stop, otherwise it will go back to step 1;\cr\cr
Users can also declare an endogenous variable as an \code{INSTRUMENT} variable. In this case, the constant adjustment related to the provided endogenous variable will be used as the instrument exogenous variable. This procedure is particularly suited for the automatic computation of the add-factors needed to fine tune the model into a baseline path and to improve the forecasting accuracy.\cr\cr
If the convergence condition is satisfied, the \code{RENORM} procedure will return the requested \code{INSTRUMENT} time series allowing us to achieve the desired values for the endogenous variables designated as \code{TARGET}.\cr\cr



}
\usage{

RENORM(model=NULL,
        renormIterLimit=10,
        renormConvergence=10E-5, 
        TSRANGE=NULL,
        TARGET=NULL,
        INSTRUMENT=NULL,
        ConstantAdjustment=NULL,
        quietly=FALSE,
        tol=.Machine$double.eps,
        avoidCompliance=FALSE,
        ...)
}

\arguments{
\item{model}{The BIMETS model object to be renormalized. The operation requires that all the behaviorals in the model have been previously estimated: all the behavioral coefficients (i.e. the estimation coefficients and the autoregression coefficients for the errors, if any) must be numerically defined in the model object. (see also \code{\link[=ESTIMATE]{ESTIMATE}})}
\item{renormIterLimit}{The value representing the maximum number of iterations to be performed. The iterative renormalization procedure will stop when \code{renormIterLimit} is reached or the \code{TARGET} variables satisfy the \code{renormConvergence} criterion;}
\item{renormConvergence}{The convergence value requested for the iterative renormalization process, that stops when the distance between each \code{TARGET} time series and the related simulated endogenous variable is less than the \code{renormConvergence} value;
}
\item{TSRANGE}{The time range of the \code{RENORM} analysis, as a four dimensional numerical array, \cr i.e. \code{TSRANGE=c(start_year, start_period, end_year, end_period)}.}
\item{TARGET}{A named list that specifies the target endogenous variables. The list names are the names of the endogenous variables involved in the renormalization; each element of this list contains the time series of the target endogenous values; the time series must be compliant with the compliance control check defined in \code{\link[=is.bimets]{is.bimets}} (see examples);}
\item{INSTRUMENT}{A \code{charcater} array with the names of the exogenous variables involved in the renormalization. User can also declare an endogenous variable as \code{INSTRUMENT} variable: in this case the constant adjustment (see \code{\link[=SIMULATE]{SIMULATE}}) related to the provided endogenous variable will be used as the instrument exogenous variable (see example);
 }
\item{ConstantAdjustment}{A named list that specifies the constant adjustments (i.e. add factors) to be added to the selected equations of the model (see also \code{\link[=SIMULATE]{SIMULATE}});}
\item{quietly}{If \code{TRUE}, information messages will be suppressed.
 } 
 \item{tol}{
  the tolerance for detecting linear dependencies in the columns of a matrix while an inversion is requested. The default is \code{.Machine$double.eps}.
  }
    \item{avoidCompliance}{
  If \code{TRUE}, compliance control check of model time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
  }
  
   \item{...}{ Other options to be sent to the underlying \code{\link[=MULTMATRIX]{MULTMATRIX}} operation, e.g.:\cr\cr
   \bold{simType}, the simulation type requested:\cr
   DYNAMIC: the default, for interim multipliers. Whenever lagged endogenous variables are needed in the computations, the simulated values of the endogenous variables evaluated in the previous time periods are used;\cr
   STATIC: for impact multiplier. Rather than the simulated values, the actual historical values are used whenever lagged endogenous variables are needed in the computations; see also \code{\link[=MULTMATRIX]{MULTMATRIX}};\cr
   
\bold{simConvergence}: the percentage convergence value requested for the simulation process (see also \code{\link[=SIMULATE]{SIMULATE}}); \cr
  
\bold{simIterLimit}: the value representing the maximum number of iterations to be performed in the simulation process (see also \code{\link[=SIMULATE]{SIMULATE}}); \cr

\bold{ZeroErrorAC}: if \code{TRUE} it zeroes out all the autoregressive terms, if any, in the behavioral equations; \cr
 
\bold{Exogenize}: a named list that specifies the endogenous variables to be exogenized (see also \code{\link[=SIMULATE]{SIMULATE}});\cr

\bold{MM_SHOCK}: the value of the shocks added to variables in the derivative calculation of the multipliers. The default value is \code{0.00001} times the value of the exogenous variable (see also \code{\link[=MULTMATRIX]{MULTMATRIX}});\cr

}

}


\value{
This function will add a new element named \code{renorm} into the output BIMETS model object.\cr\cr
The new \code{renorm} element is a named R list that contains the following elements:\cr\cr 
- \bold{INSTRUMENT}: a named R list that contains the time series of the \code{INSTRUMENT} exogenous variables which allow the objective \code{TARGET} endogenous variables to be achieved. The names of this list are the names of the related exogenous variables. This element is populated only if the convergence is reached. Users can also declare an endogenous variable as \code{INSTRUMENT} variable: in this case the constant adjustment (see \code{\link[=SIMULATE]{SIMULATE}}) related to the provided endogenous variable will be used as the instrument exogenous variable. In this case, this INSTRUMENT output list will contains also the time series of the constant adjustment related to the provided endogenous variable (see example)\cr
- \bold{TARGET} a named R list built with the time series of the achieved \code{TARGET} endogenous variables. The names of this list are the names of the related endogenous variables the time series belong to; \cr
- \bold{unConvergedTARGET}: the names array of the endogenous \code{TARGET} variables that failed the convergence. This element is populated only if the convergence has not been reached;\cr
- \bold{modelData}: the whole model input dataset wherein the \code{INSTRUMENT} exogenous variables have been modified accordingly to the \code{RENORM} output time series. This can be useful in order to verify that the model fed with the proper \code{INSTRUMENT} exogenous variable produces the desired \code{TARGET} endogenous values. This element is populated only if the convergence is reached;\cr
- \bold{ConstantAdjustment}: the constant adjustment input list provided during the function call, if any, wherein the constant adjustment related to a \code{INSTRUMENT} endogenous variables have been modified accordingly to the \code{RENORM} output time series. This can be usefull in order to verify that the model fed with the proper \code{INSTRUMENT} exogenous variable and with the proper \code{ConstantAdjustment} (if any) produces the desired \code{TARGET} endogenous values. This element is populated only if the convergence is reached;\cr
- \bold{__RENORM_PARAMETERS__}: a named R list that contains the arguments passed to the function call during the latest \code{RENORM()} run, e.g. \code{TSRANGE}, \code{renormIterLimit}, \code{INSTRUMENT}, \code{TARGET}, etc.: that can be usefull in order to replicate the renorm results. \cr



}

\seealso{
\code{\link[=MDL]{MDL}}\cr
\code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
\code{\link[=ESTIMATE]{ESTIMATE}}\cr
\code{\link[=SIMULATE]{SIMULATE}}\cr
\code{\link[=MULTMATRIX]{MULTMATRIX}}\cr
\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
}

\examples{

#define model
myModelDefinition=
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
myModelData=list(
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
myModel=LOAD_MODEL(modelText=myModelDefinition)
myModel=LOAD_MODEL_DATA(myModel,myModelData)

#estimate model
myModel=ESTIMATE(myModel)

#we want an arbitrary value of 66 on Consumption 'cn' in 1940 and 78 in 1941
#we want an arbitrary value of 77 on GNP 'y' in 1940 and 98 in 1941
kleinTargets = list(
  cn = TSERIES(66,78,START=c(1940,1),FREQ=1),
  y  = TSERIES(77,98,START=c(1940,1),FREQ=1)
)

#Then, we can perform the model endogenous targeting by using Government Wage Bill 'w2' 
#and Government non-Wage Spending 'g' as 
#INSTRUMENT in the years 1940 and 1941:
myModel=RENORM(myModel
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
kleinRenorm=myModel

#get instruments to be used
newInstruments=myModel$renorm$INSTRUMENT

#change exogenous by using new instruments
kleinRenorm$modelData=within(kleinRenorm$modelData,
                 {
                   w2[[1940,1]]=newInstruments$w2[[1940,1]]
                   w2[[1941,1]]=newInstruments$w2[[1941,1]]
                   g[[1940,1]] =newInstruments$g[[1940,1]]
                   g[[1941,1]] =newInstruments$g[[1941,1]]
                 }
                )
#users can also replace last two commands with:
#kleinRenorm$modelData=kleinRenorm$renorm$modelData

#simulate the new model
kleinRenorm=SIMULATE(kleinRenorm
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
myCA=list(i = myModel$modelData$i*0+0.1)

#run renorm with endogenous 'i' as instrument
myModel=RENORM(myModel
               ,INSTRUMENT = c('w2','i')
               ,TARGET = kleinTargets
               ,TSRANGE = c(1940,1,1941,1)
               ,simIterLimit = 100
               ,ConstantAdjustment = myCA
)

#get the values of the constant adjustment for the endogenous 'i' 
#in 1940-1941 that allow to achieve the target values for 'cn' and 'y'
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


 
