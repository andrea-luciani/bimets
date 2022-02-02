\name{MULTMATRIX}
\alias{MULTMATRIX}


\title{
Compute the multiplier matrix of a BIMETS model object
}
\description{
\if{html}{(Note: this is the html version of the reference manual. Please consider reading the pdf version of this reference manual, wherein there are figures and the mathematical expressions are better formatted than in html.)\cr\cr}


This function computes the matrix of both impact and interim multipliers, for a selected set of endogenous variables (i.e. \code{TARGET}) with respect to a selected set of exogenous variables (i.e. \code{INSTRUMENT}), by subtracting the results from different simulations in each period of the provided time range (i.e. \code{TSRANGE}). The simulation algorithms are the same as those used for the \code{\link[=SIMULATE]{SIMULATE}} operation. \cr\cr
The \code{MULTMATRIX} procedure is articulated as follows:\cr\cr
1- simultaneous simulations are done;\cr\cr
2- the first simulation establishes the base line solution (without shocks);\cr\cr
3- the other simulations are done with shocks applied to each of the \code{INSTRUMENT} one at a time for every period in \code{TSRANGE};\cr\cr
4- each simulation follows the defaults described in the \code{\link[=SIMULATE]{SIMULATE}} help page, but has to be \code{STATIC} for the IMPACT multipliers and \code{DYNAMIC} for INTERIM multipliers;\cr\cr
5- given the shock amount \code{MM_SHOCK} as a very small positive number, derivatives are computed by subtracting the base line solution of the \code{TARGET} from the shocked solution, then dividing by the value of the base line \code{INSTRUMENT} time the \code{MM_SHOCK}.\cr\cr\cr
The IMPACT multipliers measure the effects of impulse exogenous changes on the endogenous variables in the same time period. They can be defined as partial derivatives of each current endogenous variable with respect to each current exogenous variable, all other exogenous variable being kept constant.\cr
\if{html}{
Given \code{Y(t)} an endogenous variable at time \code{t} and \code{X(t)} an exogenous variable at time \code{t} the impact multiplier \code{m(Y,X,t)} is defined as \code{m(Y,X,t) = dY(t)/dX(t)} and can be approximated by \code{m(Y,X,t)=(Y_shocked(t)-Y(t))/(X_shocked(t)-X(t))}, with \code{Y_shocked(t)} the values fo the simulated endogenous variable \code{Y} at time \code{t} when \code{X(t)} is shocked to \code{X_shocked(t)=X(t)(1+MM_SHOCK)}
}
\if{latex}{
Given \eqn{Y(t)} an endogenous variable at time \eqn{t} and \eqn{X(t)} an exogenous variable at time \eqn{t} the impact multiplier \eqn{m(Y,X,t)} is defined as \eqn{m(Y,X,t) = \partial Y(t)/\partial X(t)} and can be approximated by \eqn{m(Y,X,t)\approx(Y_{shocked}(t)-Y(t))/(X_{shocked}(t)-X(t))}, with \eqn{Y_{shocked}(t)} the values fo the simulated endogenous variable \eqn{Y} at time \eqn{t} when \eqn{X(t)} is shocked to \eqn{X_{shocked}(t)=X(t)(1+MM\_SHOCK)}
}\cr\cr The INTERIM or delay-\code{r} multipliers measure the delay-\code{r} effects of impulse exogenous changes on the endogenous variables in the same time period. The delay-\code{r} multipliers of the endogenous variable \code{Y} with respect to the exogenous variable \code{X} related to a dynamic simulation from time \code{t} to time \code{t+r} can be defined as the partial derivative of the current endogenous variable \code{Y} at time \code{t+r} with respect to the exogenous variable \code{X} at time \code{t}, all other exogenous variable being kept constant.\cr
\if{html}{
Given \code{Y(t+r)} an endogenous variable at time \code{t+r} and \code{X(t)} an exogenous variable at time \code{t} the interim or delay-\code{r} multiplier \code{m(Y,X,t,r)} is defined as \code{m(Y,X,t,r) = dY(t+r)/dX(t)} and can be approximated by \code{(m(Y,X,t,r)=Y_shocked(t+r)-Y(t+r))/(X_shocked(t)-X(t))}, with \code{Y_shocked(t+r)} the values fo the simulated endogenous variable \code{Y} at time \code{t+r} when \code{X(t)} is shocked to \code{X_shocked(t)=X(t)(1+MM_SHOCK)}
}
\if{latex}{
Given \eqn{Y(t+r)} an endogenous variable at time \eqn{t+r} and \eqn{X(t)} an exogenous variable at time \eqn{t} the impact interim or delay-\code{r} multiplier \eqn{m(Y,X,t,r)} is defined as \eqn{m(Y,X,t,r) = \partial Y(t+r)/\partial X(t)} and can be approximated by \eqn{m(Y,X,t,r)\approx(Y_{shocked}(t+r)-Y(t+r))/(X_{shocked}(t)-X(t))}, with \eqn{Y_{shocked}(t+r)} the values fo the simulated endogenous variable \eqn{Y} at time \eqn{t+r} when \eqn{X(t)} is shocked to \eqn{X_{shocked}(t)=X(t)(1+MM\_SHOCK)}
}\cr \cr
Users can also declare an endogenous variable as the \code{INSTRUMENT} variable. In this case, the constant adjustment (see \code{\link[=SIMULATE]{SIMULATE}}) related to the provided endogenous variable will be used as the \code{INSTRUMENT} exogenous variable (see example);

}
\usage{
MULTMATRIX(model=NULL,
		   ...)		   

}

\arguments{
  \item{model}{
  The BIMETS model object for which the multipliers matrix has to be calculated. The operation requires that all the behaviorals in the model have been previously estimated: all the behavioral coefficients (i.e. the estimation coefficients and the autoregression coefficients for the errors, if any) must be numerically defined in the model object. (see also \code{\link[=ESTIMATE]{ESTIMATE}})
  }

 \item{...}{
Other options to be sent to the underlying \code{\link[=SIMULATE]{SIMULATE}} operation, e.g.:\cr\cr
\bold{TSRANGE}: the time range of the multiplier analysis, as a four dimensional numerical array, \cr i.e. \code{TSRANGE=c(start_year, start_period, end_year, end_period)}. \cr

\bold{simType}, the simulation type requested:\cr
   DYNAMIC: the default, for interim multipliers. Whenever lagged endogenous variables are needed in the computations, the simulated values of the endogenous variables evaluated in the previous time periods are used;\cr
   STATIC: for impact multiplier. Rather than the simulated values, the actual historical values are used whenever lagged endogenous variables are needed in the computations;\cr

\bold{TARGET}: a \code{charcater} array built with the names of the endogenous variables for which the multipliers are requested;\cr

\bold{INSTRUMENT}: a \code{charcater} array with the names of the exogenous variables with respect to which the multipliers are computed. Users can also declare an endogenous variable as the \code{INSTRUMENT} variable: in this case the constant adjustment (see \code{\link[=SIMULATE]{SIMULATE}}) related to the provided endogenous variable will be used as the instrument exogenous variable (see example); \cr

\bold{MM_SHOCK}: the shock value added to variables in the derivative calculation. The default value is \code{0.00001} times the value of the exogenous variable (see also \code{\link[=MULTMATRIX]{MULTMATRIX}});  \cr

\bold{simConvergence}: the percentage convergence value requested for the iterative process, that stops when the percentage difference of all the feedback variables between iterations is less than \code{simConvergence} in absolute value;\cr

\bold{simIterLimit}: the value representing the maximum number of iterations to be performed. The iterative procedure will stop when \code{simIterLimit} is reached or the feedback variables satisfy the \code{simConvergence} criterion; \cr


\bold{ZeroErrorAC}: if \code{TRUE} it zeroes out all the autoregressive terms, if any, in the behavioral equations; \cr

\bold{Exogenize}: a named list that specifies the endogenous variables to be exogenized. During the simulation, in the specified time range the exogenized endogenous variables will be assigned their historical values. The list names are the names of the endogenous variables to be exogenized; each element of this list contains the time range of the exogenization for the related endogenous variable, in the form of a 4-dimensional integer array, i.e. start_year, start_period, end_year, end_period. An element of the list can also be assigned \code{TRUE}: in this case the related endogenous variable will be exogenized in the whole simulation \code{TSRANGE} (see \code{\link[=SIMULATE]{SIMULATE}}); \cr 

\bold{ConstantAdjustment}: a named list that specifies the constant adjustments (i.e. add factors) to be added to the selected equations of the model. Each constant adjustment can be see as a new exogenous variable added to the equation of the specified endogenous variable. The list names are the names of the endogenous variables involved; each element of this is list contains the time series to be added to the equation of the related endogenous variable. Each provided time series must be compliant with the compliance control check defined in \code{\link[=is.bimets]{is.bimets}} (see \code{\link[=SIMULATE]{SIMULATE}}); \cr

\bold{verbose}: if \code{TRUE} some verbose output will be activated. Moreover the values of all endogenous variables will be printed out during each iteration for all time periods in the simulation \code{TSRANGE}; \cr

\bold{quietly}. if \code{TRUE} the \code{\link[=MULTMATRIX]{MULTMATRIX}} operation will be executed quietly.\cr 

}
  

  }

\value{
This function will add a new element named \code{MultiplierMatrix} into the output BIMETS model object.\cr\cr
The new \code{MultiplierMatrix} element is a \cr \code{(NumPeriods * Nendogenous) X (NumPeriods * Nexogenous)} matrix,\cr with \code{NumPeriods} as the number of periods specified in the \code{TSRANGE}, \code{Nendogeous} the count of the endogenous variables in the \code{TARGET} array and \code{Nexogenous} the count of the exogenous variables in the \code{INSTRUMENT} array. \cr\cr The arguments passed to the function call during the latest \code{MULTMATRIX()} run will be inserted into the \code{'__SIM_PARAMETERS__'} element of the model \code{simulation} list (see \code{\link[=SIMULATE]{SIMULATE}}); that can be useful in order to replicate the multiplier matrix results. \cr\cr
Row and column names in the output multiplier matrix identify the variables and the periods involved in the derivative solution, with the syntax \code{VARIABLE_PERIOD} (see example).


}

\seealso{
\code{\link[=MDL]{MDL}}\cr
\code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
\code{\link[=ESTIMATE]{ESTIMATE}}\cr
\code{\link[=SIMULATE]{SIMULATE}}\cr
\code{\link[=RENORM]{RENORM}}\cr
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
EQ> w1 = c1 + c2*(y+t-w2) + c3*TSLAG(y+t-w2,1)+c4*time
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

#calculate impact multipliers of Government non-Wage Spending 'g' and
#Government Wage Bill 'w2' with respect of Consumption 'cn' and
#Gross National Product 'y' in the Klein model on the year 1941:

myModel=MULTMATRIX(myModel,
                  symType='STATIC',
                  TSRANGE=c(1941,1,1941,1),
                  INSTRUMENT=c('w2','g'),
                  TARGET=c('cn','y'))
                  
#Multipliter Matrix:    100.00%
#...MULTMATRIX OK

print(myModel$MultiplierMatrix)
#          w2_1      g_1
#cn_1 0.4540346 1.671956
#y_1  0.2532000 3.653260

#Results show that the impact multiplier of "y"
#with respect to "g" is +3.65
#If we change Government non-Wage Spending 'g' value in 1941 
#from 22.3 (its historical value) to 23.3 (+1) 
#then the simulated Gross National Product "y" 
#in 1941 changes from 95.2 to 99, 
#thusly roughly confirming the +3.65 impact multiplier.
#Note that "g" appears only once in the model definition, and only 
#in the "y" equation, with a coefficient of one. (Keynes would approve)



#multi-period interim multipliers
myModel=MULTMATRIX(myModel,
                   TSRANGE=c(1940,1,1941,1),
                   INSTRUMENT=c('w2','g'),
                   TARGET=c('cn','y'))

#output multipliers matrix (note the zeros when the period
#of the INSTRUMENT is greater than the period of the TARGET)
print(myModel$MultiplierMatrix)
#           w2_1      g_1      w2_2      g_2
#cn_1  0.4478202 1.582292 0.0000000 0.000000
#y_1   0.2433382 3.510971 0.0000000 0.000000
#cn_2 -0.3911001 1.785042 0.4540346 1.671956
#y_2  -0.6251177 2.843960 0.2532000 3.653260


#multiplier matrix with endogenous variable 'w1' as instrument
#note the ADDFACTOR suffix in the column name, referring to the
#constant adjustment of the endogneous 'w1'
myModel=MULTMATRIX(myModel,
                    TSRANGE=c(1940,1,1941,1),
                    INSTRUMENT=c('w2','w1'),
                    TARGET=c('cn','y'))

#Multipliter Matrix:    100.00%
#...MULTMATRIX OK
myModel$MultiplierMatrix
#           w2_1 w1_ADDFACTOR_1      w2_2 w1_ADDFACTOR_2
#cn_1  0.4478202      0.7989328 0.0000000      0.0000000
#y_1   0.2433382      0.4341270 0.0000000      0.0000000
#cn_2 -0.3911001     -0.4866248 0.4540346      0.8100196
#y_2  -0.6251177     -0.9975073 0.2532000      0.4517209


}
