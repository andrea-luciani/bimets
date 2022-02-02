\name{SIMULATE}
\alias{SIMULATE}


\title{
Simulate a BIMETS model object
}
\description{
\if{html}{(Note: this is the html version of the reference manual. Please consider reading the pdf version of this reference manual, wherein there are figures and the mathematical expressions are better formatted than in html.)\cr\cr}
The simulation of an econometric model basically consists in solving the system of the equations describing the model for each time period in the specified time interval. Since the equations may not be linear in the variables, and since the graph derived from the incidence matrix may be cyclic, the usual methods based on linear algebra are not applicable, and the simulation must be solved by using an iterative algorithm.\cr\cr
BIMETS simulation capabilities support:\cr\cr
- \emph{Static simulations}: a static multiple equation simulation, in which the historical values for the lagged endogenous variables are used in the solutions of subsequent periods; \cr\cr
- \emph{Dynamic simulations}: a dynamic simulation, in which the simulated values for the lagged endogenous variables are used in the solutions of subsequent periods;\cr\cr
- \emph{Residuals check}: a single period, single equation simulation; simulated time series in output are just the computation of the RHS (right-hand-side) of their equation, by using the historical values of the involved time series and by accounting for error autocorrelation and PDLs, if any;\cr\cr
- \emph{Forecast simulations}: similar to dynamic simulation, but during the initialization of the iterative algorithm the starting values of endogenous variables in a period are set equal to the simulated values of the previous period. This allows the simulation of future endogenous observations, i.e. the forecast;  \cr\cr
- \emph{Partial or total exogenization of endogenous variables}: in the provided time interval (i.e. partial exog.) or in the whole simulation time range (i.e. total exog.), the values of the selected endogenous variables can be definitely set equal to their historical values, by excluding their equations from the iterative algorithm of simulation;\cr\cr
- \emph{Constant adjustment of endogenous variables (add-factors)}: adds another exogenous time series - the "constant adjustment" - in the equation of the selected endogenous variables;\cr\cr
In details, the generic model suitable for simulation in BIMETS can be written as: \cr
\if{latex}{
\eqn{y_1=f_1(\bar{x},\bar{y}) + z_1} \cr
\eqn{...} \cr
\eqn{y_n=f_n(\bar{x},\bar{y}) + z_n} \cr
}
\if{html}{
\eqn{y_1=f_1(x,y) + z_1} \cr
\eqn{...} \cr
\eqn{y_n=f_n(x,y) + z_n} \cr\cr
}
being: \cr\cr
\eqn{n} the number of equations in the model; \cr
\eqn{\bar{y}=[y_1, ... , y_n]}{y=[y_1, ... , y_n]} the \code{n}-dimensional vector of the endogenous variables;\cr
\eqn{\bar{x}=[x_1, ... , x_m]}{x=[x_1, ... , x_m]} the \code{m}-dimensional vector of the exogenous variables;\cr
\eqn{f_i(...), i=1..n} any kind of functional expression able to be written by using the \code{MDL} syntax;\cr\cr
As described later on, a modified Gauss-Seidel iterative algorithm can solve the system of equations. The convergence properties may vary depending on the model specifications. In some conditions, the algorithm may not converge for a specific model or a specific set of data.\cr\cr
A convergence criterion and a maximum number of iterations to be performed are provided by default. Users can change these criteria by using the \code{simConvergence} and \code{simIterLimit} arguments of the \code{SIMULATE()} function.\cr\cr
The general conceptual scheme of the simulation process (for each time period) is the following:\cr\cr
1. initialize the solution for the current simulation period;\cr
2. iteratively solve the system of equations;\cr
3. save the solution, if any;\cr\cr
Step 2 means that for each iteration you will need to: \cr\cr
2.1 update the values of the current endogenous variables;\cr
2.2 verify that the convergence criterion is satisfied or that the maximum number of allowed iterations has been reached;\cr\cr
The initial solution for the iterative process (step 1) can be given alternatively by:\cr\cr
- the historical value of the endogenous variables for the current simulation period (the default);\cr
- the simulated value of the endogenous variables from the previous simulation period (this alternative is driven by the \code{simType='FORECAST'} argument of the \code{SIMULATE()} function);\cr\cr
In the "dynamic" simulations (i.e. simulations performed by using either the default \code{simType='DYNAMIC'} or the \code{simType='FORECAST'}), whenever lagged endogenous variables are needed in the computation, the simulated values of the endogenous variables \eqn{\bar{y}}{y} assessed in the previous time periods are used. In this case, the results of the simulation in a given time period depends on the results of the simulation in the previous time periods. This kind of simulation is defined as "multiple equation, multiple period".\cr\cr
As an alternative, the actual historical values can be used in the "static" simulations (i.e. simulations performed by using \code{simType='STATIC'}) rather than simulated values whenever lagged endogenous variables are needed in the computations. In this case, the results of the simulation in a given time period does not depend on the results of the simulation in the previous time periods. This kind of simulation is defined as "multiple equation, single period".\cr\cr
The last simulation type available is the residual check (\code{simType='RESCHECK'}). With this option a "single equation, single period" simulation is performed. In this case no iteration must be carried out. The endogenous variables are assessed for each single time period through the use of historical values for each variable on the right-hand side of the equation, for both lagged and current periods. This kind of simulation is very helpful for debugging and checking the logical coherence of the equations and the data, and can be used as a simple tool to compute the add-factors.\cr\cr The debugging of the logical coherence of the model-equation and the data is carried out by means of a procedure called "Residual Check".\cr\cr
It consists in the following steps:\cr\cr
1. add another exogenous variable - the constant adjustment - to every equation of the model, both behavioral and technical identity (i.e. by using the \code{ConstantAdjustment} argument of the \code{SIMULATE()} function);\cr
2. fill in with the estimated residuals all the constant adjustments for the behavioral equations; \cr
3. fill in with zeroes the constant adjustments for the technical identities;\cr
4. perform a simulation of the model with the \code{simType='RESCHECK'} option;\cr
5. compute the difference between the historical and the simulated values for all the endogenous variables;\cr
6. check whether all the differences assessed in step 5 are zero in the whole time range;\cr\cr
If a perfect tracking of the history is obtained then the equations have been written coherently with the data, otherwise a simulated equation not tracking the historical values is an unambiguous symptom of data inconsistent with the model definition.\cr\cr
Aside from the residual check, the add-factors constitute an important tool to significantly improve the accuracy of forecasts made through an econometric model. Considering the following model: \cr\cr
\eqn{y_1=f_1(\bar{x},\bar{y}) + z_1}{y_1=f_1(x,y) + z_1} \cr
\eqn{...} \cr
\eqn{y_n=f_n(\bar{x},\bar{y}) + z_n}{y_n=f_n(x,y) + z_n} \cr\cr
the add-factors \eqn{\bar{z}=[z_1, ... ,z_n]}{z=[z_1, ... ,z_n]} can be interpreted as estimates of the future values of the disturbance terms or, alternatively, as adjustments of the intercepts in each equation. These add-factors round out the forecasts, by summarizing the effects of all the systematic factors not included in the model. One choice for the computation of the add-factors is given by past estimation residuals and past forecast errors or by an average of these errors. This consideration suggests an easy way of computing the add-factors:\cr\cr
1. add the constant adjustments to every equation of the model, both behavioral and technical identity;\cr
2. fill in with zeroes all the constant adjustments;\cr
3. solve the model, with the \code{simType='RESCHECK'} option, in a time interval including some periods beyond the estimation sample;\cr
4. compute the difference between the historical and the simulated values for each the endogenous variables;\cr
5. average, or process in a suitable way, the difference arising from point 4 in the time periods beyond the estimation sample to compute the constant value to be used as an add-factor in the following forecasting exercises;\cr\cr
\tabular{c}{THE OPTIMAL REORDERING}
In fact, the simulation process takes advantage of an appropriate ordering of the equations to increase the performances by iteratively solving only one subset of equations, while the others are solved straightforwardly. \emph{"...a different ordering of the equations can substantially affect the speed of convergence of the algorithm; indeed some orderings may produce divergence. The less feedback there is, the better the chances for fast convergence..."} - Don, Gallo - Solving large sparse systems of equations in econometric models - Journal of Forecasting 1987.\cr\cr
The \code{\link[=LOAD_MODEL]{LOAD_MODEL}} function builds the incidence matrix of the model, then defines the proper equation reordering. The incidence matrix is built from the equations of the model; it is a square matrix in which each row and each column represents an endogenous variable. If the \code{(i,j)} element is equal to 1 then in the model definition the current value of the endogenous variable referred by the \code{i}-row depends directly from the current value of the endogenous variable referred by the \code{j}-column.\cr\cr
In econometric models, the incidence matrix is usually very sparse. Only a few of the total set of endogenous variables are used in each equation. In this situation, ordering the equation in a certain sequence will lead to a sensible reduction of the number of iterations needed to achieve convergence. Reordering the equations is equivalent to rearranging rows and columns of the incidence matrix. In this way the incidence matrix might be made lower triangular for a subset of the equations.
For this subset, an endogenous variable determined in a specific equation has no \emph{incidence} in any equation above it, although the same variable might have incidence in equations below it. Such a subset of equations is called recursive. Recursive systems are easy to solve. It is only necessary to solve each equation once if this is done in the right order. On the other hand, it is unlikely for the whole model to be recursive. Indeed the incidence graph is often cyclic, as in the Klein's model that presents the following circular dependecies in the incidence matrix: \code{cn <- w1 <- y <- cn}\cr\cr
For a subset of the equations, some 1's will occur in the upper triangle of the incidence matrix for all possible orderings. Such subset of equations is called \emph{simultaneous}. In order to be able to solve the endogenous variables in the simultaneous block of equations, an iterative algorithm has to be used. Nevertheless, the equations in the simultaneous block may be ordered so that the pattern of the 1's in the upper triangle of the incidence matrix forms a spike. The variables corresponding to the 1's in the upper triangle are called \emph{feedback} variables.\cr\cr
\if{html}{\cr\cr(check the pdf version of this reference manual in order to see a graphical example)\cr\cr}
\if{latex}{
A qualitative graphical example of an ordered incidence matrix is given in the following figure. The white areas are all 0's, the gray areas contains 0's and 1's. The 1's in the light gray areas refer to variables already evaluated in previous blocks, therefore they are known terms within the block. The 1's in the dark gray areas refer to variables evaluated within the block.\cr\cr
\figure{Reordering.png}{options: width=14cm}
}
The final pattern of an incidence matrix after the equation reordering generally features three blocks: \cr\cr
1. a recursive block (the pre-recursive block);\cr
2. a simultaneous block;\cr
3. another recursive block (the post-recursive block);\cr\cr
As said, the pre-recursive and the post-recursive blocks are lower triangular. Therefore the corresponding equations are solvable with a cascade substitution with no iteration. Just the simultaneous equations set needs an iterative algorithm to be solved. It is important to say that the convergence criterion may also be applied to these variables only: when the feedback variables converge, the rest of the simultaneous variables also do.\cr\cr BIMETS builds and analyzes the incidence matrix of the model, and then it orders the equations in pre-recursive, post-recursive and simultaneous blocks. The simultaneous block is then analyzed in order to find a minimal set of feedback variables. This last problem is known to be NP-complete (Ref: Garey, Johnson - \emph{Computers and Intractability: a Guide to the Theory of NP-completeness} - San Francisco, Freeman 1979).\cr\cr
The optimal reordering of the model equations is programmatically achieved through the use of an iterative algorithm applied to the incidence matrix that can produce 4 ordered lists of endogenous variables:\cr\cr
1. \code{vpre} is the ordered list containing the names of the endogenous pre-recursive variables to be sequentially computed (by using their \code{EQ>} definition in the \code{MDL}) before the simulation iterative algorithm takes place;\cr
2. \code{vsim} is the ordered list containing the names of the endogenous variables to be sequentially computed during each iteration of the simulation iterative algorithm; \cr
3. \code{vfeed} is the list containing the names of the endogenous feedback variables; \cr
4. \code{vpost} is the ordered list containing the names of the endogenous post-recursive variables to be sequentially computed once the simulation iterative algorithm has converged;\cr\cr
If equations are reordered, the previous conceptual scheme is modified as follow:\cr\cr
- initialize the solution for the current simulation period;\cr
- compute the pre-recursive equations (i.e. the equation of the endogenous variables in the \code{vpre} ordered list);\cr
- iteratively compute the system of simultaneous equations (i.e. the equation of the endogenous variables in the \code{vsim} ordered list); for each iteration update the values of the current endogenous variables and verify that the convergence criterion is satisfied on the feedback variables or that the maximum number of iterations has been reached;\cr
- compute the post-recursive equations (i.e. the equation of the endogenous variables in the \code{vpost} ordered list);\cr
- save the solutions;\cr\cr
Given \eqn{y_{i,k}}{y_(i,k)} the value of the \eqn{i}-endogenous variable in the simultaneous block at the iteration \eqn{k}, with \eqn{i} the position of the equation in a reordered model, the modified Gauss-Seidel method simply takes for the approximation of the endogenous variable \eqn{y_{i,k}}{y_(i,k)} the solution of the following:\cr\cr
\eqn{y_{i,k}=f_i(x_1, ..., x_m, y_{1,k}, ..., y_{i-1,k}, y_{i,k-1},..., y_{n,k-1})}{y_(i,k)=f_i(x_1, ..., x_m, y_(1,k), ..., y_(i-1,k), y_(i,k-1),..., y_(n,k-1)}\cr\cr
As said, the convergence is then tested at the end of each iteration on the feedback variables.\cr\cr
Newton's methods on a reordered model require the calculation of the Jacobian matrix on the feedback endogenous variables, i.e. at least \eqn{f+2} iterations per simulation period, with \eqn{f} as the number of feedback variables. For large models (i.e. more than 30 feedback variables) if the overall required convergence is greater than \eqn{10^{-6} \%}{10^(-6) \%} the speedup over the Gauss-Siebel method is small or negative. Moreover the Gauss-Siebel method does not require a matrix inversion, therefore it is more robust against algebraical and numerical issues. For small models both methods are fast on modern computers.\cr\cr The simulation of a non-trivial model, if computed by using the same data but on different hardware, software or numerical libraries, produces numerical differences. Therefore a convergence criterion smaller than \eqn{10^{-7} \%}{10^(-7)} frequently leads to a local solution.\cr\cr See \emph{Numerical methods for simulation and optimal control of large-scale macroeconomic models - Gabay, Nepomiastchy, Rachidi, Ravelli - 1980} for further information.






}


\usage{
SIMULATE(model=NULL,
		 TSRANGE=NULL,
		 simType='DYNAMIC',
		 simConvergence=0.01,
		 simIterLimit=20,
		 ZeroErrorAC=FALSE,
		 BackFill=0,
		 Exogenize=NULL,
		 ConstantAdjustment=NULL,
		 verbose=FALSE,
		 verboseSincePeriod=0,
		 verboseVars=NULL,
		 MULTMATRIX=FALSE,
		 TARGET=NULL,
		 INSTRUMENT=NULL,
		 MM_SHOCK=0.00001,
		 quietly=FALSE,
		 RESCHECKeqList=NULL,
		 avoidCompliance=FALSE,
		 ...)

}

\arguments{
  \item{model}{
  The BIMETS model object to be simulated. The simulation requires that all the behaviorals in the model have been previously estimated: all the behavioral coefficients (i.e. the estimation coefficients and the autoregression coefficients for the errors, if any) must be numerically defined in the model object. (see also \code{\link[=ESTIMATE]{ESTIMATE}})
  }
 
  \item{TSRANGE}{
  The time range of the simulation, as a four dimensional numerical array, \cr i.e. \code{TSRANGE=c(start_year, start_period, end_year, end_period)}. 
  }
   \item{simType}{
   The simulation type requested:\cr
   \bold{DYNAMIC}: the default, whenever lagged endogenous variables are needed in the computations, the simulated values of the endogenous variables evaluated in the previous time periods are used;\cr
   \bold{STATIC}: rather than the simulated values, the actual historical values are used whenever lagged endogenous variables are needed in the computations;\cr
   \bold{FORECAST}: similar to the \code{'DYNAMIC'} option, but the initial solutions for the iterative process are given by the simulated values of the endogenous variables in the previous period. In this case there is no need for historical values of the endogenous variables in the whole provided \code{TSRANGE};\cr
   \bold{RESCHECK}: in this case there is no iteration to carry out. The endogenous variables are evaluated for each single time period by using the historical values for all the variables on the right-hand side of the equation, both lagged and current period;
   }
\item{simConvergence}{
   The percentage convergence value requested for the iterative process, that stops when the percentage difference of all the feedback variables between iterations is less than \code{simConvergence} in absolute value;
   }
  \item{simIterLimit}{
  The value representing the maximum number of iterations to be performed. The iterative procedure will stop when \code{simIterLimit} is reached or the feedback variables satisfy the \code{simConvergence} criterion;
  }
  \item{ZeroErrorAC}{
  If \code{TRUE} it zeroes out all the autoregressive terms, if any, in the behavioral equations; 
  }
  \item{BackFill}{
  Defined as an \code{integer}, it is the length of historical data prior to the simulation \code{TSRANGE} to be saved along with the solutions.
  }
  \item{Exogenize}{
  A named list that specifies the endogenous variables to be exogenized. During the simulation and inside the provided time range, the exogenized endogenous variables will be assigned to their historical values. List names must be the names of the endogenous variables to be exogenized; each element of this list contains the time range of the exogenization for the related endogenous variable, in the form of a 4-dimensional integer array, i.e. start_year, start_period, end_year, end_period. A list element can also be assigned \code{TRUE}: in this case the related endogenous variable will be exogenized in the whole simulation \code{TSRANGE} (see examples);
  }
  \item{ConstantAdjustment}{
  A named list that specifies the constant adjustments (i.e. add factors) to be added to the selected equations of the model. Each constant adjustment can be see as a new exogenous variable added to the equation of the specified endogenous variable. The list names are the names of the involved endogenous variables; each element of this is list contains the time series to be added to the equation of the related endogenous variable. Each provided time series must verify the compliance control check defined in \code{\link[=is.bimets]{is.bimets}} (see examples);
  }
  \item{verbose}{
  If \code{TRUE} some verbose output will be activated. Moreover the values of all endogenous variables will be printed out during each iteration and for all time periods inside the simulation \code{TSRANGE};
  }
  \item{verboseSincePeriod}{
  An integer that activate the verbose output, during the iterative process, only after the provided number of simulation periods;
  }
  \item{verboseVars}{
  A \code{charcater} array with the names of the endogenous variables for which the verbose output will be activated in the iterative process; 
  }
  \item{MULTMATRIX}{It is \code{TRUE} when the simulation is a vectorial simulation requested by a \code{\link[=MULTMATRIX]{MULTMATRIX}} operation;}
  \item{TARGET}{  A \code{charcater} array built with the names of the endogenous variables for which the multipliers are requested (see \code{\link[=MULTMATRIX]{MULTMATRIX}});  }
   \item{INSTRUMENT}{
  A \code{charcater} array built with the names of the exogenous variables with respect to which the multipliers are evaluated. Users can also declare an endogenous variable as \code{INSTRUMENT} variable: in this case the constant adjustment (see \code{\link[=SIMULATE]{SIMULATE}}) related to the provided endogenous variable will be used as the instrument exogenous variable (see \code{\link[=MULTMATRIX]{MULTMATRIX}});
  }
    \item{MM_SHOCK}{The value of the shocks added to variables in the derivative calculation of the multipliers. The default value is \code{0.00001} times the value of the exogenous variable (see \code{\link[=MULTMATRIX]{MULTMATRIX}});
  }  
 \item{quietly}{ If \code{TRUE}, information messages will be suppressed.
  } 
\item{RESCHECKeqList}{ If \code{simType=RESCHECK}, by using this arguments users can select a subset of target endogenous variables: the simulation will be perfmormed only for the selected variables. It must be provided as an array of endogenous names, e.g. \code{c('endo1','endo2',...)}.
  } 
    \item{avoidCompliance}{
  If \code{TRUE}, compliance control check of model time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
  }
 \item{...}{
Backward compatibility.
}
  }

\value{
This function will add a new element named \code{simulation} into the output BIMETS model object.\cr\cr
The new \code{simulation} element is a named R list; the names of the \code{simulation} list are the names of the endogenous variables of the model; each element of the \code{simulation} list contains the simulated time series of the related endogenous variable (see examples). \cr\cr The \code{simulation} list also contains the \code{'__SIM_PARAMETERS__'} element that contains the arguments passed to the function call during the latest \code{SIMULATE()} run, e.g. \code{TSRANGE}, \code{symType}, \code{simConvergence}, \code{symIterLimit}, \code{Exogenize}, etc.: that can be useful in order to replicate the simulation results. 


}

\seealso{
\code{\link[=MDL]{MDL}}\cr
\code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
\code{\link[=ESTIMATE]{ESTIMATE}}\cr
\code{\link[=MULTMATRIX]{MULTMATRIX}}\cr
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
 
#DYNAMIC SIMULATION
 
#simulate model 
myModel=SIMULATE(myModel
                 ,TSRANGE=c(1923,1,1941,1)
                 ,simConvergence=0.00001
                 ,simIterLimit=100
  )
#
#Simulation:    100.00%
#...SIMULATE OK
 
#get simulated time series "cn" and "y"
TABIT(myModel$simulation$cn)
#
#      DATE, PER, myModel$simulation$cn
#
#      1923, 1  ,     50.338
#      1924, 1  ,    55.6994
#      1925, 1  ,    56.7111
#      ...
#      1940, 1  ,    66.7799
#      1941, 1  ,     75.451
#

TABIT(myModel$simulation$y)
#
#      DATE, PER, myModel$simulation$y
#
#      1923, 1  ,    56.0305
#      1924, 1  ,    65.8526
#      1925, 1  ,     64.265
#      ...
#      1940, 1  ,    76.8049
#      1941, 1  ,    93.4459
#
 
#get latest simulation parameters
print(myModel$simulation$'__SIM_PARAMETERS__')
#$TSRANGE
#[1] 1923    1 1941    1
#
#$simType
#[1] "DYNAMIC"
#
#$simConvergence
#[1] 1e-05
#
#$simIterLimit
#[1] 100
#
#$ZeroErrorAC
#[1] FALSE
#
#...etc etc
 
#RESCHECK SIMULATION
 
#simulate model
myModel=SIMULATE(myModel
                  ,simType='RESCHECK'
                  ,TSRANGE=c(1923,1,1941,1)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
 )
#
#Simulation:    100.00%
#...SIMULATE OK
 
#get consumption simulated vs historical differences
TABIT(myModel$simulation$cn-myModel$modelData$cn)
#
#      DATE, PER, myModel$simulation$cn - myModel$modelData$cn
#
#      1923, 1  ,    1.56574
#      1924, 1  ,   0.493503
#      1925, 1  , -0.0076079
#      ... 
#      1939, 1  ,  -0.989201
#      1940, 1  ,  -0.785077
#      1941, 1  ,    2.17345
#
 
#FORECAST GNP in 1942 and 1943 
 
#we need to extend exogenous variables in 1942 and 1943
myModel$modelData$w2   = TSEXTEND(myModel$modelData$w2,  UPTO=c(1943,1))
myModel$modelData$t    = TSEXTEND(myModel$modelData$t,   UPTO=c(1943,1))
myModel$modelData$g    = TSEXTEND(myModel$modelData$g,   UPTO=c(1943,1))
myModel$modelData$time = TSEXTEND(myModel$modelData$time,UPTO=c(1943,1)
                                     ,EXTMODE='LINEAR')
 
#simulate model
myModel=SIMULATE(myModel
                  ,simType='FORECAST'
                  ,TSRANGE=c(1940,1,1943,1)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
)
#
#Simulation:    100.00%
#...SIMULATE OK
 
#get forecasted GNP
TABIT(myModel$simulation$y)
#
#      DATE, PER, myModel$simulation$y
#
#      1940, 1  ,    74.5781
#      1941, 1  ,    94.0153
#      1942, 1  ,    133.969
#      1943, 1  ,    199.913
#
 
#STATIC SIMULATION WITH EXOGENIZATION AND CONSTANT ADJUSTMENTS
 
#define exogenization list
#'cn' exogenized in 1923-1925
#'i' exogenized in the whole TSRANGE
exogenizeList=list(
                cn = c(1923,1,1925,1),
                i  = TRUE
)
 
#define add factor list
constantAdjList=list(
               cn = TIMESERIES(1,-1,START=c(1923,1),FREQ='A'),
               y  = TIMESERIES(0.1,-0.1,-0.5,START=c(1926,1),FREQ='A')
)
 
#simulate model
myModel=SIMULATE(myModel
                  ,simType='STATIC'
                  ,TSRANGE=c(1923,1,1941,1)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
                  ,Exogenize=exogenizeList
                  ,ConstantAdjustment=constantAdjList 
 )
#Endogenous variable cn has been exogenized from year-period 1923-1 to 1925-1
#Endogenous variable i has been exogenized from year-period 1923-1 to 1941-1
#Endogenous variable i has a constant adjustment from year-period 1923-1 to 1924-1
#Endogenous variable i has a constant adjustment from year-period 1926-1 to 1928-1
#
#Simulation:    100.00%
#...SIMULATE OK
 
#VERBOSE SIMULATION
 
myModel=SIMULATE(myModel
                  ,TSRANGE=c(1923,1,1941,1)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
                  ,verbose=TRUE
                  ,verboseSincePeriod=19
                  ,verboseVars=c('cn')
 )
#CHECK_MODEL_DATA(): warning, there are missing values in series "time".
#
#Simulation:      5.26%
#Prd    1 convergence reached in iter 34
#Simulation:     10.53%
#Prd    2 convergence reached in iter 42
#Simulation:     15.79%
#Prd    3 convergence reached in iter 40
#Simulation:     21.05%
#
#... 
#
#Prd   17 convergence reached in iter 40
#Simulation:     94.74%
#Prd   18 convergence reached in iter 37
#Simulation:    100.00%
#VPRE eval
#Prd   19 Iter    0         cn            69.7
#VSIM eval
#Prd   19 Iter    1         cn     71.7197097805143
#VSIM eval
#Prd   19 Iter    2         cn     72.7386970702054
#... etc etc
#Prd   19 Iter   39         cn     75.4510243396176
#VSIM eval
#Prd   19 Iter   40         cn     75.4510298916399
#VPOST eval
#Prd   19 Iter   40         cn      75.4510299
#Prd   19 convergence reached in iter 40
#
#...SIMULATE OK
 

}
