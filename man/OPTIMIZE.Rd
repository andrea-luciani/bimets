\name{OPTIMIZE}
\alias{OPTIMIZE}


\title{
Optimal control of a BIMETS model
}
\description{

The \code{OPTIMIZE} procedure provides a convenient method for performing optimal control exercises; the procedure maximizes an arbitrary objective-function under the constraints imposed by the econometric model and by user-specified constraints.
\cr\cr
An approach to policy evaluation is via a so-called "social welfare function". This approach relaxes the assumptions of the instruments-targets framework, i.e. the \code{\link[=RENORM]{RENORM}} procedure. Rather than assuming specific desired targets for some endogenous variables, it assumes the existence of a social welfare function determining a scalar measure of performance based on both endogenous and policy (exogenous) variables.
\cr\cr
The social welfare function can incorporate information about tradeoffs in objectives that are not allowed by the \code{\link[=RENORM]{RENORM}} instruments-targets approach.
\cr\cr
BIMETS supplies the \code{OPTIMIZE} procedure in order to perform optimal control exercises on econometric models.
\cr\cr
The optimization consists of maximizing a social welfare function, i.e. the objective-function, depending on exogenous and (simulated) endogenous variables, subject to user constraints plus the constraints imposed by the econometric model equations. Users are allowed to define constraints and objective-functions of any degree, and are allowed to provide different constraints and objective-functions in different optimization time periods.
\cr\cr
The core of the \code{OPTIMIZE} procedure is based on a Monte Carlo method that takes advantage of the \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}} procedure. Policy variables, i.e. \code{INSTRUMENT}, are uniformly perturbed in the range defined by the user-provided boundaries, then the \code{INSTRUMENT} values that i) verify the user-provided constraints and ii) maximize the objective-functions are selected and stored into the \code{optimize} element of the output BIMETS model.
\cr\cr
The following steps can describe the procedure implemented in \code{OPTIMIZE}:
\cr\cr
1) check the correctness of input arguments;\cr\cr
2) perform a \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}} by uniformly perturbing the \code{INSTRUMENT} variables inside the user-boundaries provided in the \code{OptimizeBounds} function argument;\cr\cr
3) during the \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}, for each period in the optimization \code{TSRANGE}: i) discard the stochastic realizations that do not verify the restrictions provided in the \code{OptimizeRestrictions} argument; ii) for all the remaining realizations, compute the current value of the objective-functions time series, as defined in the \code{OptimizeFunctions} argument, by using the exogenous and (simulated) endogenous stochastic time series;\cr\cr
4) once the \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}} completes, select the stochastic realization that presents the higher value in the sum of the corresponding objective-function time series values, and return, among other data, the related optimal \code{INSTRUMENT} time series.\cr\cr

In the following figure, the scatter plot is populated with \code{2916} objective function stochastic realizations, computed by using the example code at the end of this section; the \code{210.58} local maximum  is highlighted\cr(i.e. \code{advancedKleinModel$optimize$optFunMax} in first example).\cr\cr In this example:\cr\cr 
i) The objective function definition is:\cr 
\eqn{f(y,cn,g) = (y-110)+(cn-90)*|cn-90|-\sqrt{g-20}} \cr
given \eqn{y} as the simulated \emph{Gross National Product}, \eqn{cn} as the simulated \emph{Consumption} and \eqn{g} as the exogenous \emph{Government Expenditure}: the basic idea is to maximize \emph{Consumption}, and secondarily the \emph{Gross National Product}, while reducing the \emph{Government Expenditure};\cr\cr 
ii) The \code{INSTRUMENT} variables are the \eqn{cn} \emph{Consumption} "booster" (i.e. the add-factor, not to be confused with the simulated \emph{Consumption} in the objective function) and the \eqn{g} \emph{Government Expenditure}, defined over the following domains: \eqn{ cn \in (-5,5)}, \eqn{g \in (15,25)};\cr\cr
iii) The following restrictions are applied to the \code{INSTRUMENT}: \eqn{g + cn^2/2 < 27  \wedge  g + cn > 17}, given \eqn{cn} as the \emph{Consumption} "booster" (i.e. the add-factor) and \eqn{g} as the \emph{Government Expenditure};

\figure{OptKlein.png}\cr \cr

The figure clearly shows that non-linear restrictions have been applied, and that non-computable objective functions have been discarded, e.g. the stochastic realizations having \eqn{g<20} due to the square root operation in the objective function, given instrument \eqn{g \in (15,25)}.

}


\usage{
OPTIMIZE( model=NULL,
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
\item{StochReplica}{see \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}}
\item{StochSeed}{see \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}}

\item{OptimizeBounds}{the named \code{list()} that defines the search boundaries applied to \code{INSTRUMENT} exogenous variables. Each list element must have a name equal to an endogenous or an exogenous model variable.\cr \cr 
The list names define the \code{INSTRUMENT}.\cr\cr
If a list element name is equal to an exogenous variable, then the boundaries will be applied directly to the related exogenous stochastic time series values. If a list element name is equal to an endogenous variable, then the boundaries will be applied to the stochastic constant adjustment  (see \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}) of the related endogenous variable. \cr\cr
Each list element must be a named list built with the following two named variables: \cr \cr 
- \code{TSRANGE}: the time range  wherein the search boundaries are active. The \code{TSRANGE} must be a 4 numerical array,\cr i.e. \code{TSRANGE=c(start_year, start_period, end_year, end_period)} or \code{TSRANGE=TRUE} in order to apply the provided boundaries to the whole \code{OPTIMIZE} \code{TSRANGE}. \cr \cr  
- \code{BOUNDS}: the boundaries that are applied to the related instrument. These parameters must contain the lower and upper bound of the uniform distribution wherein the search for the objective-functions maximum is performed,\cr i.e. \code{BOUNDS=c(lower_bound,upper_bound)}. \cr \cr 
See example in order to learn how to build a compliant boundaries structure.}

\item{OptimizeRestrictions}{the named \code{list()} that defines the restrictions applied to \code{INSTRUMENT} exogenous variables. This list can be \code{NULL}.\cr \cr 
Each list element must be a named list built with the following two named variables: \cr \cr 
- \code{TSRANGE}: the time range  wherein the restriction is active. The \code{TSRANGE} must be a 4 numerical array,\cr i.e. \code{TSRANGE=c(start_year, start_period, end_year, end_period)} or \code{TSRANGE=TRUE} in order to apply the provided restriction to the whole \code{OPTIMIZE} \code{TSRANGE}. \cr \cr  
- \code{INEQUALITY}: the inequality expression, i.e. a \code{character} variable, that defines the restriction. The \code{INEQUALITY} expression can contain exogenous and endogenous variable names, the standard arithmetic and logical operators, parentheses and the \code{\link[=MDL]{MDL}} functions described in the \code{EQ} section of the \code{\link[=MDL]{MDL}} help page. If in the \code{INEQUALITY} expression a variable name refers to an exogenous variable, then that variable will be evaluated by using the related exogenous time series stochastic values. If in the \code{INEQUALITY} expression a variable name refers to an endogenous variable, then that variable will be evaluated by using to the stochastic constant adjustment (see argument \code{StochStructure} of the \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}} help page) of the related endogenous variable. \cr\cr 
Two different \code{OptimizeRestrictions} list element can not have overlapping \code{TSRANGE}. See example in order to learn how to build a compliant restrictions structure.}

\item{OptimizeFunctions}{the named \code{list()} that defines the objective functions to be maximized.\cr \cr 
Each list element must be a named list built with the following two named variables: \cr \cr 
- \code{TSRANGE}: the time range  wherein the objective function is evaluated. The \code{TSRANGE} must be a 4 numerical array,\cr i.e. \code{TSRANGE=c(start_year, start_period, end_year, end_period)} or \code{TSRANGE=TRUE} in order to evaluate the objective function in each period of the \code{OPTIMIZE} \code{TSRANGE}. \cr \cr  
- \code{FUNCTION}: the expression, i.e. a \code{character} variable, that defines the objective function. The \code{FUNCTION} expression can contain exogenous and endogenous variable names, the standard arithmetic and logical operators, parentheses and the \code{\link[=MDL]{MDL}} functions described in the \code{EQ} section of the \code{\link[=MDL]{MDL}} help page. If in the \code{FUNCTION} expression a variable name refers to an exogenous variable, then that variable will be evaluated by using the related exogenous time series stochastic values. If in the \code{FUNCTION} expression a variable name refers to an endogenous variable, then that variable will be evaluated by using the stochastic simulated time series (see \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}) of the related endogenous variable. \cr\cr 
Two different \code{OptimizeFunctions} list element can not have overlapping \code{TSRANGE}. See example in order to learn how to build a compliant objective functions structure.}

\item{quietly}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{RESCHECKeqList}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{avoidCompliance}{see \code{\link[=SIMULATE]{SIMULATE}}}
\item{...}{see \code{\link[=SIMULATE]{SIMULATE}}}
  }

\value{
This function will add, into the output BIMETS model object, three new named elements, respectively \code{optimize}, \code{simulation_MM} and \code{INSTRUMENT_MM}.\cr\cr
The \code{optimize} element is a named \code{list()} that contains the following elements:\cr\cr

- \code{INSTRUMENT}: a named list that contains the time series of the instrument exogenous variables that verify the \code{OptimizeRestrictions} and that allow the objective \code{OptimizeFunctions} to be maximized. This element is populated only if a finite solution exists. List names are equal to the names of the related exogenous variables. Users can also declare an endogenous variable as \code{INSTRUMENT} variable, by using the \code{OptimizeBounds} argument: in this case the constant adjustment (see \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}) related to the provided endogenous variable will be used as instrument exogenous variable, and this output \code{INSTRUMENT} list will contains the constant adjustment time series that allow the objective \code{OptimizeFunction} to be maximized (see example);\cr

- \code{optFunMax}: the scalar value (local maximum) obtained by evaluating the \code{OptimizeFunctions} while the model is fed by the optimized \code{INSTRUMENT} time series. This element is populated only if a finite solution exists;\cr

- \code{optFunTS}: the time series obtained by evaluating the \code{OptimizeFunctions} during each period in the \code{OPTIMIZE} \code{TSRANGE} while the model is fed by the optimized \code{INSTRUMENT} time series. Thus, \code{optFunMax==sum(optFunTS)}. This element is populated only if a finite solution exists;\cr

- \code{optFunAve}: the scalar value that is the mean of all the stochastic \code{OptimizeFunctions} realizations, filtered by the restrictions imposed by the \code{OptimizeRestrictions} argument. This element is populated only if a finite solution exists;\cr

- \code{optFunSd}: the scalar value that is the standard deviation of all the stochastic \code{OptimizeFunctions} realizations, filtered by the restrictions imposed by the \code{OptimizeRestrictions} argument. This element is populated only if a finite solution exists;\cr

- \code{realizationsToKeep}: a \code{1 x StochReplica} boolean row array. If the \code{i}-th element is \code{TRUE} than the related objective function realization is computable and verifies the restrictions imposed by the \code{OptimizeRestricions} argument. It can be useful along with \code{optFunResults} and \code{INSTRUMENT_MM} in order to verify and to refine results;\cr

- \code{optFunResults}: the numerical array containing the evaluated \code{OptimizeFunctions} for all the (unfiltered) realizations;\cr

- \code{modelData}: the whole model input dataset wherein the \code{INSTRUMENT} exogenous variables have been modified accordingly to the \code{OPTIMIZE} results. This data can be useful in order to verify or to refine results (see example);\cr

- \code{ConstantAdjustment}: a modified constant adjustment input list wherein the constant adjustment time series related to a \code{INSTRUMENT} endogenous variables have been modified accordingly to the \code{OPTIMIZE} results. This data can be useful in order to verify or to refine results (see example);\cr


The arguments passed to the function call during the latest \code{OPTIMIZE} run will be inserted into the \code{'__OPT_PARAMETERS__'} element of the model \code{optimize} list; this data can be helpful in order to replicate the optimization results.\cr

The \code{simulation_MM} element is a named \code{list()}, having the endogenous variables as names. Each element will contain an \code{R x C} matrix, given \code{R} the number of observations in the optimization \code{TSRANGE} and \code{C=1+StochReplica}. The first column of each matrix contains the related endogenous variable's unperturbed simulated values; the remaining columns will contain all the \code{StochReplica} stochastic realizations for the related endogenous variable. \cr 

The \code{INSTRUMENT_MM} element is a named \code{list()}, having \code{INSTRUMENT} variables as names. Each element will contain an \code{R x C} matrix, given \code{R} the number of observations in the optimization \code{TSRANGE} and \code{C=1+StochReplica}. The first column of each matrix contains the related \code{INSTRUMENT} variable's unperturbed values; the remaining columns will contain all the \code{StochReplica} stochastic realizations for the related \code{INSTRUMENT} variable. \cr \cr

}

\seealso{
\code{\link[=MDL]{MDL}}\cr
\code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
\code{\link[=ESTIMATE]{ESTIMATE}}\cr
\code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}\cr
\code{\link[=MULTMATRIX]{MULTMATRIX}}\cr
\code{\link[=RENORM]{RENORM}}\cr
\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
}

\examples{


#define the advanced Klein model
advancedKleinModelDef <- "
MODEL

COMMENT> Modified Klein Model 1 of the U.S. Economy with PDL, 
COMMENT> autocorrelation on errors, restrictions and conditional equation evaluations

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
advancedKleinModel <- LOAD_MODEL_DATA(advancedKleinModel,kleinModelData)

#estimate the model
advancedKleinModel <- ESTIMATE(advancedKleinModel, quietly=TRUE)

#we want to maximize the non-linear objective function:
#f()=(y-110)+(cn-90)*ABS(cn-90)-(g-20)^0.5
#in 1942 by using INSTRUMENT cn in range (-5,5) 
#(cn is endogenous so we use the add-factor)
#and g in range (15,25)
#we will also impose the following non-linear restriction:
#g+(cn^2)/2<27 & g+cn>17

#we need to extend exogenous variables up to 1942
advancedKleinModel$modelData <- within(advancedKleinModel$modelData,{
    w2    = TSEXTEND(w2,  UPTO=c(1942,1),EXTMODE='CONSTANT')
    t     = TSEXTEND(t,   UPTO=c(1942,1),EXTMODE='LINEAR')
    g     = TSEXTEND(g,   UPTO=c(1942,1),EXTMODE='CONSTANT')
    k     = TSEXTEND(k,   UPTO=c(1942,1),EXTMODE='LINEAR')
    time  = TSEXTEND(time,UPTO=c(1942,1),EXTMODE='LINEAR')
})

#define INSTRUMENT and boundaries
myOptimizeBounds <- list(
    cn=list(TSRANGE=TRUE,
            BOUNDS=c(-5,5)),
    g=list(TSRANGE=TRUE,
           BOUNDS=c(15,25))
)

#define restrictions
myOptimizeRestrictions <- list(
    myRes1=list(
        TSRANGE=TRUE,
        INEQUALITY='g+(cn^2)/2<27 & g+cn>17')
)

#define objective function
myOptimizeFunctions <- list(
    myFun1=list(
        TSRANGE=TRUE,
        FUNCTION='(y-110)+(cn-90)*ABS(cn-90)-(g-20)^0.5')
)

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
                          ,OptimizeFunctions = myOptimizeFunctions)
                          
#OPTIMIZE(): optimization boundaries for the add-factor of endogenous 
#            variable "cn" are (-5,5) from year-period 1942-1 to 1942-1.
#OPTIMIZE(): optimization boundaries for the exogenous 
#            variable "g" are (15,25) from year-period 1942-1 to 1942-1.
#OPTIMIZE(): optimization restriction "myRes1" is active 
#            from year-period 1942-1 to 1942-1.
#OPTIMIZE(): optimization objective function "myFun1" is active 
#            from year-period 1942-1 to 1942-1.
#
#Optimize:     100.00 %
#OPTIMIZE(): 2916 out of 10000 objective function realizations (29%) 
#            are finite and verify the provided restrictions.
#...OPTIMIZE OK

#print local maximum
advancedKleinModel$optimize$optFunMax
#[1] 210.5755

#print INSTRUMENT that allow local maximum to be achieved
advancedKleinModel$optimize$INSTRUMENT
#$cn
#Time Series:
#Start = 1942 
#End = 1942 
#Frequency = 1 
#[1] 2.032203
#
#$g
#Time Series:
#Start = 1942 
#End = 1942 
#Frequency = 1 
#[1] 24.89773

#LET'S VERIFY RESULTS
#copy into modelData the computed INSTRUMENT 
#that allow to maximize the objective function 
advancedKleinModel$modelData <- advancedKleinModel$optimize$modelData

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
)

#calculate objective function by using the SIMULATE output time series
#(y-110)+(cn-90)*ABS(cn-90)-(g-20)^0.5
y  <- advancedKleinModel$simulation$y
cn <- advancedKleinModel$simulation$cn
g  <- advancedKleinModel$modelData$g
optFunTest <- (y-110)+(cn-90)*abs(cn-90)-(g-20)^0.5

#verify computed max is equal to optimization max
#(in the following command TSPROJECT could be omitted because
#myFun1$TSRANGE = TRUE)
abs(sum(TSPROJECT(optFunTest
              ,TSRANGE=c(1942,1,1942,1)
              ,ARRAY = TRUE)
        ) - advancedKleinModel$optimize$optFunMax)  < 1E-4
#[1] TRUE

#we can also check that the SIMULATE time series 
#are equal to the OPTIMIZE realizations that allow to maximize
#the objective function

#get realization index that maximizes the objective function
maximizingRealizationIdx=with(advancedKleinModel$optimize,
                              which.max(optFunResults[realizationsToKeep]))

#get stochastic realizations unfiltered
#(simulation_MM and INSTRUMENT_MM are populated during the OPTIMIZE call)
y_opt  <- advancedKleinModel$simulation_MM$y
cn_opt <- advancedKleinModel$simulation_MM$cn
g_opt  <- advancedKleinModel$INSTRUMENT_MM$g

#filter by restrictions and by finite solutions
#(first column in all matrices is related to the un-perturbed model)
y_opt  <- y_opt[ ,c(FALSE,advancedKleinModel$optimize$realizationsToKeep),drop=FALSE]
cn_opt <- cn_opt[,c(FALSE,advancedKleinModel$optimize$realizationsToKeep),drop=FALSE]
g_opt  <- g_opt[ ,c(FALSE,advancedKleinModel$optimize$realizationsToKeep),drop=FALSE]

#get maximizing realizations
y_opt  <- y_opt[ ,maximizingRealizationIdx,drop=FALSE]
cn_opt <- cn_opt[,maximizingRealizationIdx,drop=FALSE]
g_opt  <- g_opt[ ,maximizingRealizationIdx,drop=FALSE]

#verify that these variables are equal to the SIMULATE time series
max(abs(y-y_opt))   < 1E-4  
#[1] TRUE

max(abs(cn-cn_opt)) < 1E-4  
#[1] TRUE

max(abs(g[[1942,1]]-g_opt)) < 1E-4  
#[1] TRUE

 
############################################################
#MULTI RESTRICTIONS, MULTI OBJECTIVE FUNCTIONS EXAMPLE

#load the model (reset stuff)
advancedKleinModel <- LOAD_MODEL(modelText = advancedKleinModelDef)

#load time series into the model object
advancedKleinModel <- LOAD_MODEL_DATA(advancedKleinModel,kleinModelData)

#estimate the model
advancedKleinModel <- ESTIMATE(advancedKleinModel, quietly=TRUE)

#we want to maximize the non-linear objective function:
#f1()=(y-110)+(cn-90)*ABS(cn-90)-(g-20)^0.5
#in 1942 by using INSTRUMENT cn in range (-5,5) 
#(cn is endogenous so we use the add-factor)
#and g in range (15,25)
#we want to maximize the non-linear objective function:
#f2()=(y-120)+(cn-100)*ABS(cn-100)-(g-20)^0.5-(w2-8)^0.5
#in 1943 by using INSTRUMENT cn in range (-5,5),
#g in range (15,25)
#and w2 in range (7.5,12.5)
#we will also impose the following non-linear restrictions:
#in 1942: g+(cn^2)/2<27 & g+cn>17
#in 1943: (g^2)/10+(cn^2)/2+w2^2 < 200

#we need to extend exogenous variables up to 1943
advancedKleinModel$modelData <- within(advancedKleinModel$modelData,{
    w2    = TSEXTEND(w2,  UPTO=c(1943,1),EXTMODE='CONSTANT')
    t     = TSEXTEND(t,   UPTO=c(1943,1),EXTMODE='LINEAR')
    g     = TSEXTEND(g,   UPTO=c(1943,1),EXTMODE='CONSTANT')
    k     = TSEXTEND(k,   UPTO=c(1943,1),EXTMODE='LINEAR')
    time  = TSEXTEND(time,UPTO=c(1943,1),EXTMODE='LINEAR')
})

#define INSTRUMENT and boundaries
myOptimizeBounds <- list(
    cn=list(TSRANGE=TRUE,
            BOUNDS=c(-5,5)),
    g=list(TSRANGE=TRUE,
           BOUNDS=c(15,25)),
    w2=list(TSRANGE=c(1943,1,1943,1),
            BOUNDS=c(7.5,12.5))
)

#define restrictions
myOptimizeRestrictions <- list(
    myRes1=list(
        TSRANGE=c(1942,1,1942,1),
        INEQUALITY='g+(cn^2)/2 < 27 & g+cn > 17'),
    myRes2=list(
        TSRANGE=c(1943,1,1943,1),
        INEQUALITY='(g^2)/10+(cn^2)/2+w2^2 < 200')
)

#define objective functions
myOptimizeFunctions <- list(
    myFun1=list(
        TSRANGE=c(1942,1,1942,1),
        FUNCTION='(y-110)+(cn-90)*ABS(cn-90)-(g-20)^0.5'),
    myFun2=list(
        TSRANGE=c(1943,1,1943,1),
        FUNCTION='(y-120)+(cn-100)*ABS(cn-100)-(g-20)^0.5-(w2-8)^0.5')
)

#Monte-Carlo optimization by using 1000 stochastic realizations
#and 1E-4 convergence
advancedKleinModel <- OPTIMIZE(advancedKleinModel
                          ,simType = 'FORECAST'
                          ,TSRANGE=c(1942,1,1943,1)
                          ,simConvergence=1E-4
                          ,simIterLimit = 500
                          ,StochReplica = 1000
                          ,StochSeed = 123
                          ,OptimizeBounds = myOptimizeBounds
                          ,OptimizeRestrictions = myOptimizeRestrictions
                          ,OptimizeFunctions = myOptimizeFunctions)

#print INSTRUMENT that allow local maximum to be achieved
advancedKleinModel$optimize$INSTRUMENT

#LET'S VERIFY RESULTS
#copy into modelData the computed INSTRUMENT 
#that allow to maximize the objective function 
advancedKleinModel$modelData <- advancedKleinModel$optimize$modelData

#simulate the model by using the new INSTRUMENT
newConstantAdjustment <- advancedKleinModel$optimize$ConstantAdjustment
advancedKleinModel <- SIMULATE(advancedKleinModel
                  ,simType = 'FORECAST'
                  ,TSRANGE = c(1942,1,1943,1)
                  ,simConvergence = 1E-5
                  ,simIterLimit = 100
                  ,ConstantAdjustment = newConstantAdjustment
)

#calculate objective functions by using the SIMULATE output time series
y  <- advancedKleinModel$simulation$y
cn <- advancedKleinModel$simulation$cn
g  <- advancedKleinModel$modelData$g
w2 <- advancedKleinModel$modelData$w2
optFunTest1 <- (y-110)+(cn-90)*abs(cn-90)-(g-20)^0.5
optFunTest2 <- (y-120)+(cn-100)*abs(cn-100)-(g-20)^0.5-(w2-8)^0.5

#verify computed max is equal to optimization max
abs(sum(TSPROJECT(optFunTest1
                  ,TSRANGE=c(1942,1,1942,1)
                  ,ARRAY = TRUE)+
        TSPROJECT(optFunTest2
                  ,TSRANGE=c(1943,1,1943,1)
                  ,ARRAY = TRUE)
        ) - advancedKleinModel$optimize$optFunMax)  < 1E-2
#[1] TRUE
    
}
