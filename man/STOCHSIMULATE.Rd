\name{STOCHSIMULATE}
\alias{STOCHSIMULATE}


\title{
Stochastic simulation of a BIMETS model
}
\description{

The \code{STOCHSIMULATE} operation performs a stochastic simulation. The simulation algorithms are the same as those used by the \code{\link[=SIMULATE]{SIMULATE}} operation. 
\cr\cr
Forecasts produced by structural econometric models are subject to several sources of error, such as random disturbance term of each stochastic equation, errors in estimated coefficients, errors in forecasts of exogenous variables, errors in preliminary data and mis-specification of the model.\cr\cr
The forecast error depending on the structural disturbances can be analyzed by using the stochastic simulation procedure.\cr\cr
The deterministic simulation is the simultaneous solution of an econometric model obtained by applying, for each stochastic (behavioral) equation, the expected values of the structural disturbances, which are all zero by assumption. In the BIMETS \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}} stochastic simulation, the structural disturbances are given values that have specified stochastic properties. The error terms of the estimated behavioral equation of the model are appropriately perturbed. Identity equations and exogenous variables can be as well perturbed by disturbances that have specified stochastic properties. The model is then solved for each data set with different values of the disturbances. Finally, mean and standard deviation are computed for each simulated endogenous variable. \cr\cr
In terms of computational efficiency, the procedure takes advantage of the fact that multiple datasets are bound together in matrices. Therefore, to achieve a global convergence, the iterative simulation algorithm is executed once for all perturbed datasets. This solution can be viewed as a sort of a SIMD (i.e. Single Instruction Multiple Data) parallel simulation: the \code{STOCHSIMULATE} function transforms time series into matrices and consequently can easily bind multiple datasets by column. At the same time, a single run ensures a fast code execution. Finally, each column in the output matrices represents a stochastic realization.\cr\cr
By using the \code{StochStructure} argument of this function, users can define a stochastic structure for the disturbances. For each variable of the model, users can provide a distinct probability distribution for the disturbance, and a specific time range of application. Mean and standard deviation for each simulated endogenous time series will be stored in the \code{stochastic_simulation} element of the output model object; all the stochastic realizations will be stored in the \code{simulation_MM} element of the output model object as named matrices.

In the following figure, the advanced Klein model (see code example), has been perturbed during the forecast operation by applying a normal disturbance to the endogenous \emph{Consumption} behavioral \code{cn} add-factor in year 1942, and a uniform disturbance to the exogenous \emph{Government Expenditure} time series \code{g} along all the simulation \code{TSRANGE}. The normal disturbance applied to the \code{cn} behavioral has a zero mean and a standard deviation equal to the behavioral regression standard error,\cr i.e. \code{advancedKleinModel$behaviorals$cn$statistics$StandardErrorRegression}, thus roughly replicating the \code{\link[=ESTIMATE]{ESTIMATE}} regression error during the current perturbation (not accounting for inter-equations cross-covariance).\cr \cr 
\figure{StochKleinGNP.png} \cr \cr

At the moment, all the disturbances are i.i.d. and are not transformed into a congruent autoregressive scheme in the case the related perturbed endogenous behavioral presents an autocorrelation for the errors in its \code{\link[=MDL]{MDL}} definition, e.g. \code{ERROR> AUTO(n)}
} 
\usage{
STOCHSIMULATE(model=NULL,
              simAlgo='GAUSS-SEIDEL',
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
              JACOBIAN_SHOCK=1e-4,
              JacobianDrop=NULL,
              avoidCompliance=FALSE,
              ...)		   

}

\arguments{
\item{model}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{simAlgo}{see \code{\link[=SIMULATE]{SIMULATE}}}
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

\item{StochStructure}{the named \code{list()} that defines the disturbance structure applied to the model. Each list element must have a name equal to an endogenous or an exogenous model variable. List names define the \code{INSTRUMENT}.\cr \cr 
If a list element name is equal to an exogenous variable, the disturbance will be applied to the related exogenous time series values. If a list element name is equal to an endogenous variable, the disturbance will be applied to the constant adjustment time series (see \code{\link[=SIMULATE]{SIMULATE}}) of the related endogenous variable. \cr\cr
Each list element must be a named list built with the following three element: \cr \cr 
- \code{TSRANGE}: the time range  wherein the disturbance is active and additive to the related model variable. The \code{TSRANGE} must be a 4 numerical array, i.e. \code{TSRANGE=c(start_year, start_period, end_year, end_period)} or \code{TSRANGE=TRUE} in order to apply the provided disturbance to whole \code{STOCHSIMULATE} \code{TSRANGE}. \cr \cr  
- \code{TYPE}: the type of disturbance distribution. At the moment, only normal, i.e. \code{TYPE='NORM'}, and uniform, i.e. \code{TYPE='UNIF'}, disturbance distributions are allowed; \cr \cr
- \code{PARS}: the parameters that shape the disturbance. \cr\cr In the case of a \code{TYPE='NORM'} distribution, these parameters must contain the mean and the standard deviation of the normal distribution, i.e. \code{PARS=c(mean,sd)}; \cr\cr  in the case of a \code{TYPE='UNIF'} distribution, these parameters must contain the lower and upper bound of the uniform distribution, i.e. \code{PARS=c(min,max)}; \cr\cr in the case of a \code{TYPE='MATRIX'}, these parameters must contain the user-built matrix of realized disturbances, i.e. \code{PARS=matrix}, with \code{matrix} as a \code{[ TSRANGE x StochReplica ]} matrix.\cr \cr 
See example in order to learn how to build a compliant stochastic structure.
}
  
\item{StochReplica}{an integer value that sets the number of stochastic realizations to be performed}
  
\item{StochSeed}{a number used to initialize the pseudo-random number generator. It can be helpful in order to replicate stochastic results}
  
\item{quietly}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{RESCHECKeqList}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{JACOBIAN_SHOCK}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{JacobianDrop}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{avoidCompliance}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{...}{see \code{\link[=SIMULATE]{SIMULATE}}}
   


}


\value{
This function will add, into the output BIMETS model object, three new named elements, respectively \code{stochastic_simulation}, \code{simulation_MM} and \code{INSTRUMENT_MM}.\cr\cr
The \code{stochastic_simulation} element is a named \code{list()}, having endogenous variables as names. Each element will contain two time series: the mean and the standard deviation of the related stochastic simulated endogenous time series.\cr \cr
The arguments passed to the function call during the latest \code{STOCHSIMULATE} run will be inserted into the \code{'__STOCH_SIM_PARAMETERS__'} element of the \code{stochastic_simulation} list; this data can be helpful in order to replicate the stochastic simulation results.\cr\cr
The \code{simulation_MM} element is a named \code{list()}, having the endogenous variables as names. Each element will contain an \code{R x C} matrix, given \code{R} the number of observations in the simulation \code{TSRANGE} and \code{C=1+StochReplica}. The first column of each matrix contains the related endogenous variable's unperturbed simulated values; the remaining columns will contain all the \code{StochReplica} stochastic realizations for the related endogenous variable (see example). \cr \cr
The \code{INSTRUMENT_MM} element is a named \code{list()}, having \code{INSTRUMENT} variables as names. Each element will contain an \code{R x C} matrix, given \code{R} the number of observations in the simulation \code{TSRANGE} and \code{C=1+StochReplica}. The first column of each matrix contains the related \code{INSTRUMENT} variable's unperturbed values; the remaining columns will contain all the \code{StochReplica} stochastic realizations for the related \code{INSTRUMENT} variable. \cr \cr


}

\seealso{
\code{\link[=MDL]{MDL}}\cr
\code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
\code{\link[=ESTIMATE]{ESTIMATE}}\cr
\code{\link[=SIMULATE]{SIMULATE}}\cr
\code{\link[=RENORM]{RENORM}}\cr
\code{\link[=OPTIMIZE]{OPTIMIZE}}\cr
\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
}

\examples{

#define the advanced Klein model
advancedKleinModelDef <- "
MODEL

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

END
"

#load the model
advancedKleinModel <- LOAD_MODEL(modelText = advancedKleinModelDef)


#define data
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
);

#load time series into the model object
advancedKleinModel <- LOAD_MODEL_DATA(advancedKleinModel, kleinModelData)

#estimate the model
advancedKleinModel <- ESTIMATE(advancedKleinModel, quietly=TRUE)

#we want to perform a stochastic forecast of the GNP up to 1944
#we will add normal disturbances to endogenous Consumption 'cn' 
#in 1942 by using its regression standard error
#we will add uniform disturbances to exogenous Government Expenditure 'g'
#in whole TSRANGE
myStochStructure <- list(
  cn=list(
        TSRANGE=c(1942,1,1942,1),
        TYPE='NORM',
        PARS=c(0,advancedKleinModel$behaviorals$cn$statistics$StandardErrorRegression)
        ),
  g=list(
        TSRANGE=TRUE,
        TYPE='UNIF',
        PARS=c(-1,1)
        )
  )

#we need to extend exogenous variables up to 1944
advancedKleinModel$modelData <- within(advancedKleinModel$modelData,{
    w2    = TSEXTEND(w2,  UPTO=c(1944,1),EXTMODE='CONSTANT')
    t     = TSEXTEND(t,   UPTO=c(1944,1),EXTMODE='LINEAR')
    g     = TSEXTEND(g,   UPTO=c(1944,1),EXTMODE='CONSTANT')
    k     = TSEXTEND(k,   UPTO=c(1944,1),EXTMODE='LINEAR')
    time  = TSEXTEND(time,UPTO=c(1944,1),EXTMODE='LINEAR')
  })

#stochastic model forecast
advancedKleinModel <- STOCHSIMULATE(advancedKleinModel
                      ,simType='FORECAST'
                      ,TSRANGE=c(1941,1,1944,1)
                      ,StochStructure=myStochStructure
                      ,StochSeed=123
                      )
                      
#print mean and standard deviation of forecasted GNP
with(advancedKleinModel$stochastic_simulation,TABIT(y$mean, y$sd))

#      Date, Prd., y$mean         , y$sd           
#
#      1941, 1   ,  125.5045      ,  4.250935      
#      1942, 1   ,  173.2946      ,  9.2632        
#      1943, 1   ,  185.9602      ,  11.87774      
#      1944, 1   ,  141.0807      ,  11.6973  
      
#print the unperturbed forecasted GNP along with the
#first 5 perturbed realizations
with(advancedKleinModel$simulation_MM,print(y[,1:6]))

####################################################
#EXAMPLE WITH TYPE='MATRIX'

TSRANGE <- c(1935,1,1940,1)
StochReplica <- 100

#we will perturb simulation by using regression residuals
#get cn and i residuals in TSRANGE
cn_residuals <- TSPROJECT(advancedKleinModel$behaviorals$cn$residuals, 
                          TSRANGE=TSRANGE,
                          ARRAY = TRUE)
i_residuals <- TSPROJECT(advancedKleinModel$behaviorals$i$residuals,
                         TSRANGE=TSRANGE,
                         ARRAY = TRUE)

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

#stochastic simulation
advancedKleinModel <- STOCHSIMULATE(advancedKleinModel
                                    ,TSRANGE=TSRANGE
                                    ,StochStructure=myStochStructure
)

#print GNP mean and sd
with(advancedKleinModel$stochastic_simulation,TABIT(y$mean, y$sd))

#########################################################
#EXAMPLE OF MODEL THAT REQUIRES THE FULL NEWTON ALGORITHM
#see profit equation

myFullNewtonDefinition<-
"MODEL 

COMMENT> Consumption
BEHAVIORAL> cn
TSRANGE 1922 1 1929 1
EQ> cn =  a1 + a2*p + a3*LAG(p,1) + a4*(w1+w2+w3) 
COEFF> a1 a2 a3 a4

COMMENT> Investment
BEHAVIORAL> i
TSRANGE 1922 1 1929 1
EQ> i = b1 + b2*p + b3*LAG(p,1) + b4*LAG(k,1)
COEFF> b1 b2 b3 b4

COMMENT> Demand for Labor
BEHAVIORAL> w1 
TSRANGE 1922 1 1929 1
EQ> w1 = c1 + c2*(z+y+t-w2) + c3*LAG(z+y+t-w2,1) + c4*time
COEFF> c1 c2 c3 c4

COMMENT> Demand for Labor
BEHAVIORAL> w3 
TSRANGE 1922 1 1929 1
EQ> w3 = c1 + c2*(z+y+t-w2) + c3*LAG(z+y+t-w2,1) + c4*time
COEFF> c1 c2 c3 c4

COMMENT> Gross National Product
IDENTITY> y
EQ> y = cn + i + g - t

IDENTITY> z
EQ> z = cn + i + g - t

COMMENT> Profits with cubic dependence on cn
IDENTITY> p
EQ> p = cn^3/1000 + z + y - (w1+w2+w3)

COMMENT> Capital Stock
IDENTITY> k
EQ> k = LAG(k,1) + i

END
"


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
 

#add data to model
myModelData$z <- myModelData$y
myModelData$w3 <- (myModelData$w1)

myFullNewtonModel <- LOAD_MODEL(modelText=myFullNewtonDefinition)
myFullNewtonModel <- LOAD_MODEL_DATA(myFullNewtonModel,myModelData)

myFullNewtonModel <- ESTIMATE(myFullNewtonModel)
 
#simple Newton will fail, due to 
#large variance in normal disturbances                       
#...while full Newton will converge
myFullNewtonModel <- STOCHSIMULATE(myFullNewtonModel,
                       simAlgo='FULLNEWTON',
                       TSRANGE = c(1921, 1, 1923, 1),
                       simConvergence = 1e-5,
                       simIterLimit = 250,
                       StochReplica = 100,
                       StochSeed=123,
                       StochStructure = list(
                         cn=list(
                           TSRANGE=TRUE,
                           TYPE='NORM',
                           PARS=c(0,10)
                         )
                       )
                       ,verbose=TRUE
                       )
					}
