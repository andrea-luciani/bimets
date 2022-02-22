\name{MULTMATRIX}
\alias{MULTMATRIX}


\title{
Compute the multiplier matrix of a BIMETS model
}
\description{
\if{html}{(Note: this is the html version of the reference manual. Please consider reading the \href{https://CRAN.R-project.org/package=bimets/bimets.pdf}{pdf version} of this reference manual, or the package \href{https://cran.r-project.org/package=bimets/vignettes/bimets.pdf}{vignette}, wherein there are figures and the mathematical expressions are better formatted than in html.)\cr\cr}


This function computes the matrix of both impact and interim multipliers, for a selected set of endogenous variables (i.e. \code{TARGET}) with respect to a selected set of exogenous variables (i.e. \code{INSTRUMENT}), by subtracting the results from different simulations in each period of the provided time range (i.e. \code{TSRANGE}). The simulation algorithms are the same as those used for the \code{\link[=SIMULATE]{SIMULATE}} operation. \cr\cr
The \code{MULTMATRIX} procedure is articulated as follows:\cr\cr
1- simultaneous simulations are done;\cr\cr
2- the first simulation establishes the base line solution (without shocks);\cr\cr
3- the other simulations are done with shocks applied to each of the \code{INSTRUMENT} one at a time for every period in \code{TSRANGE};\cr\cr
4- each simulation follows the defaults described in the \code{\link[=SIMULATE]{SIMULATE}} help page, but has to be \code{STATIC} for the IMPACT multipliers and \code{DYNAMIC} for INTERIM multipliers;\cr\cr
5- given \code{MM_SHOCK} shock amount as a very small positive number, derivatives are computed by subtracting the base line solution of the \code{TARGET} from the shocked solution, then dividing by the value of the base line \code{INSTRUMENT} time the \code{MM_SHOCK}.\cr\cr\cr
The IMPACT multipliers measure the effects of impulse exogenous changes on the endogenous variables in the same time period. They can be defined as partial derivatives of each current endogenous variable with respect to each current exogenous variable, all other exogenous variables being kept constant.\cr
\if{html}{
Given \code{Y(t)} an endogenous variable at time \code{t} and \code{X(t)} an exogenous variable at time \code{t} the impact multiplier \code{m(Y,X,t)} is defined as \code{m(Y,X,t) = dY(t)/dX(t)} and can be approximated by \code{m(Y,X,t)=(Y_shocked(t)-Y(t))/(X_shocked(t)-X(t))}, with \code{Y_shocked(t)} the values fo the simulated endogenous variable \code{Y} at time \code{t} when \code{X(t)} is shocked to \code{X_shocked(t)=X(t)(1+MM_SHOCK)}
}
\if{latex}{
Given \eqn{Y(t)} an endogenous variable at time \eqn{t} and \eqn{X(t)} an exogenous variable at time \eqn{t} the impact multiplier \eqn{m(Y,X,t)} is defined as \eqn{m(Y,X,t) = \partial Y(t)/\partial X(t)} and can be approximated by \eqn{m(Y,X,t)\approx(Y_{shocked}(t)-Y(t))/(X_{shocked}(t)-X(t))}, with \eqn{Y_{shocked}(t)} the values fo the simulated endogenous variable \eqn{Y} at time \eqn{t} when \eqn{X(t)} is shocked to \eqn{X_{shocked}(t)=X(t)(1+MM\_SHOCK)}
}\cr\cr The INTERIM or delay-\code{r} multipliers measure the delay-\code{r} effects of impulse exogenous changes on the endogenous variables in the same time period. The delay-\code{r} multipliers of the endogenous variable \code{Y} with respect to the exogenous variable \code{X} related to a dynamic simulation from time \code{t} to time \code{t+r} can be defined as the partial derivative of the current endogenous variable \code{Y} at time \code{t+r} with respect to the exogenous variable \code{X} at time \code{t}, all other exogenous variables being kept constant.\cr
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
   \item{TARGET}{  A \code{character} array built with the names of the endogenous variables for which the multipliers are requested }
   \item{INSTRUMENT}{
  A \code{character} array built with the names of the exogenous variables with respect to which the multipliers are evaluated. Users can also declare an endogenous variable as \code{INSTRUMENT} variable: in this case the constant adjustment (see \code{\link[=SIMULATE]{SIMULATE}}) related to the provided endogenous variable will be used as the instrument exogenous variable
  }
   \item{MM_SHOCK}{The value of the shocks added to variables in the derivative calculation of the multipliers. The default value is \code{0.00001} times the value of the exogenous variable
  } 
   \item{quietly}{see \code{\link[=SIMULATE]{SIMULATE}}}
   \item{avoidCompliance}{see \code{\link[=SIMULATE]{SIMULATE}}}
   \item{...}{see \code{\link[=SIMULATE]{SIMULATE}}}

  }

\value{
This function will add a new element named \code{MultiplierMatrix} into the output BIMETS model object.\cr\cr
The new \code{MultiplierMatrix} element is a \cr \code{(NumPeriods * Nendogenous) X (NumPeriods * Nexogenous)} matrix,\cr with \code{NumPeriods} as the number of periods specified in the \code{TSRANGE}, \code{Nendogeous} the count of the endogenous variables in the \code{TARGET} array and \code{Nexogenous} the count of the exogenous variables in the \code{INSTRUMENT} array. \cr\cr The arguments passed to the function call during the latest \code{MULTMATRIX} run will be inserted into the \code{'__SIM_PARAMETERS__'} element of the model \code{simulation} list (see \code{\link[=SIMULATE]{SIMULATE}}); this data can be helpful in order to replicate the multiplier matrix results. \cr\cr
Row and column names in the output multiplier matrix identify the variables and the periods involved in the derivative solution, with the syntax \code{VARIABLE_PERIOD} (see example).


}

\seealso{
\code{\link[=MDL]{MDL}}\cr
\code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
\code{\link[=ESTIMATE]{ESTIMATE}}\cr
\code{\link[=SIMULATE]{SIMULATE}}\cr
\code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}\cr
\code{\link[=RENORM]{RENORM}}\cr
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
myModel<-LOAD_MODEL_DATA(myModel,myModelData)

#estimate model
myModel<-ESTIMATE(myModel)

#calculate impact multipliers of Government Expenditure 'g' and
#Government Wage Bill 'w2' with respect of Consumption 'cn' and
#Gross National Product 'y' in the Klein model on the year 1941:

myModel<-MULTMATRIX(myModel,
                  symType='STATIC',
                  TSRANGE=c(1941,1,1941,1),
                  INSTRUMENT=c('w2','g'),
                  TARGET=c('cn','y'))
                  
#Multiplier Matrix:    100.00%
#...MULTMATRIX OK

print(myModel$MultiplierMatrix)
#          w2_1      g_1
#cn_1 0.4540346 1.671956
#y_1  0.2532000 3.653260

#Results show that the impact multiplier of "y"
#with respect to "g" is +3.65
#If we change Government Expenditure 'g' value in 1941 
#from 22.3 (its historical value) to 23.3 (+1) 
#then the simulated Gross National Product "y" 
#in 1941 changes from 95.2 to 99, 
#thusly roughly confirming the +3.65 impact multiplier.
#Note that "g" appears only once in the model definition, and only 
#in the "y" equation, with a coefficient equal to one. (Keynes would approve)



#multi-period interim multipliers
myModel<-MULTMATRIX(myModel,
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
myModel<-MULTMATRIX(myModel,
                    TSRANGE=c(1940,1,1941,1),
                    INSTRUMENT=c('w2','w1'),
                    TARGET=c('cn','y'))

#Multiplier Matrix:    100.00%
#...MULTMATRIX OK
myModel$MultiplierMatrix
#           w2_1 w1_ADDFACTOR_1      w2_2 w1_ADDFACTOR_2
#cn_1  0.4478202      0.7989328 0.0000000      0.0000000
#y_1   0.2433382      0.4341270 0.0000000      0.0000000
#cn_2 -0.3911001     -0.4866248 0.4540346      0.8100196
#y_2  -0.6251177     -0.9975073 0.2532000      0.4517209


}
