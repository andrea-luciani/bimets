\name{SIMULATE}
\alias{SIMULATE}


\title{
Simulation of a BIMETS model
}
\description{

The simulation of an econometric model basically consists in solving the system of the equations describing the model for each time period in the specified time interval. Since the equations may not be linear in the variables, and since the graph derived from the "incidence matrix" may be cyclic, the usual methods based on linear algebra are not applicable. The simulation must be solved by using an iterative algorithm (Users can find the "indicence matrix" definition in the following section "The Optimal Reordering").\cr\cr
BIMETS simulation capabilities support:\cr\cr
- \emph{Static simulations}: a static multiple equation simulation, in which the historical values for the lagged endogenous variables are used in the solutions of subsequent periods; \cr\cr
- \emph{Dynamic simulations}: a dynamic simulation, in which the simulated values for the lagged endogenous variables are used in the solutions of subsequent periods;\cr\cr
- \emph{Residuals check}: a single period, single equation simulation; simulated time series in output are just the computation of the RHS (right-hand-side) of their equation, by using the historical values of the involved time series and by accounting for error autocorrelation and PDLs, if any;\cr\cr
- \emph{Forecast simulations}: similar to dynamic simulation, but during the initialization of the iterative algorithm the starting values of endogenous variables in a period are set equal to the simulated values of the previous period. This allows the simulation of future endogenous observations, i.e. the forecast;  \cr\cr
- \emph{Stochastic Simulation}: see \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}};\cr\cr
- \emph{Partial or total exogenization of endogenous variables}: in the provided time interval (i.e. partial exog.) or in whole simulation time range (i.e. total exog.), the values of the selected endogenous variables can be definitely set equal to their historical values, by excluding their equations from the iterative algorithm of simulation;\cr\cr
- \emph{Constant adjustment of endogenous variables (add-factors)}: adds up a new exogenous time series - the "constant adjustment" - in the equation of the selected endogenous variables.\cr\cr
- \emph{Gauss-Seidel} and \emph{Newton-Raphson} simulation algorithms: the Gauss-Seidel algorithm is simple, robust, and works well for many backward-looking macro-econometric models. Equations are evaluated as-is in a proper order until the convergence, if any, is verified on the so called "feedback variables" (Users can find the "feedback variable" definition in the next section "The Optimal Reordering"). It is slower than Newton-Raphson algorithms for a very low convergence criterion, and fails to find a convergence for a small set of econometric models, even when a convergence exists. The Newton-Raphson algorithm allows users to solve a broader set of macro-econometric models than the Gauss-Seidel algorithm. Moreover, it is usually faster than the Gauss-Seidel algorithm (on modern computers, users must simulate an extensive econometric model with a low convergence criterion to appreciate the speedup). This type of algorithm requires the construction and the inversion of the Jacobian matrix for the feedback variables; thus, in some scenarios, numerical issues can arise, and users are required to manually exclude some feedback variables from the Jacobian matrix by using the \code{JacobianDrop} argument of the \code{\link[=SIMULATE]{SIMULATE}} procedure.\cr\cr
In details, the generic model suitable for simulation in BIMETS can be written as: \cr

\eqn{y_1=f_1(\bar{x},\bar{y}) } \cr
\eqn{...} \cr
\eqn{y_n=f_n(\bar{x},\bar{y}) } \cr

being: \cr\cr
\eqn{n} the number of equations in the model; \cr
\eqn{\bar{y}=[y_1, ... , y_n]}{y=[y_1, ... , y_n]} the \code{n}-dimensional vector of the endogenous variables;\cr
\eqn{\bar{x}=[x_1, ... , x_m]}{x=[x_1, ... , x_m]} the \code{m}-dimensional vector of the exogenous variables;\cr
\eqn{f_i(...), i=1..n} any kind of functional expression able to be written by using the \code{MDL} syntax;\cr\cr
As described later on, in BIMETS a modified Gauss-Seidel iterative algorithm, or a Newton-Raphson algorithm, can solve the system of equations. The convergence properties may vary depending on the model specifications. In some conditions, the algorithm may not converge for a specific model or a specific set of data.\cr\cr
A convergence criterion and a maximum number of iterations to be performed are provided by default. Users can change these criteria by using the \code{simConvergence} and \code{simIterLimit} arguments of the \code{SIMULATE} function.\cr\cr
The general conceptual scheme of the simulation process (for each time period) is the following:\cr\cr
1. initialize the solution for the current simulation period;\cr
2. iteratively solve the system of equations;\cr
3. save the solution, if any;\cr\cr
Step 2 means that for each iteration, the operations are: \cr\cr
2.1 update the values of the current endogenous variables;\cr
2.2 verify that the convergence criterion is satisfied or that the maximum number of allowed iterations has been reached;\cr\cr
The initial solution for the iterative process (step 1) can be given alternatively by:\cr\cr
- the historical value of the endogenous variables for the current simulation period (the default);\cr
- the simulated value of the endogenous variables from the previous simulation period (this alternative is driven by the \code{simType='FORECAST'} argument of the \code{SIMULATE} function);\cr\cr
In the "dynamic" simulations (i.e. simulations performed by using either the default \code{simType='DYNAMIC'} or the \code{simType='FORECAST'}), whenever lagged endogenous variables are needed in the computation, the simulated values of the endogenous variables \eqn{\bar{y}}{y} assessed in the previous time periods are used. In this case, the simulation results in a given time period depend on the simulation results in the previous time periods. This kind of simulation is defined as "multiple equation, multiple period".\cr\cr
As an alternative, the actual historical values can be used in the "static" simulations (i.e. simulations performed by using \code{simType='STATIC'}) rather than simulated values whenever lagged endogenous variables are needed in the computations. In this case, the simulation results in a given time period do not depend on the simulation results in the previous time periods. This kind of simulation is defined as "multiple equation, single period".\cr\cr
The last simulation type available is the residual check (\code{simType='RESCHECK'}). With this option, a "single equation, single period" simulation is performed. In this case, no iteration must be carried out. The endogenous variables are assessed for each time period by using historical values for each variable on the right-hand side of the equation, for both lagged and current periods. This kind of simulation helps debug and check of the logical coherence of the equations and the data, and can be used as a simple tool to compute the add-factors.\cr\cr The debugging of the logical coherence of equations and data is carried out through a \emph{Residual Check} procedure.\cr\cr
It consists of the following steps:\cr\cr
1. add another exogenous variable - the constant adjustment - to every equation of the model, both behavioral and technical identity: that can be done in BIMETS by using the \code{ConstantAdjustment} argument of the \code{SIMULATE} function, as in step 3;\cr
2. fill in with the estimated residuals all the constant adjustments for the behavioral equations, and fill in with zeroes the constant adjustments for the technical identities: that can be done in BIMETS by using the \code{SIMULATE} procedure with the option \code{simType='RESCHECK'}, then by analyzing and using the \cr \code{ConstantAdjustmentRESCHECK} attribute of the simulated model, as in the following simulation in step 3.\cr
3. perform a simulation of the model: that can be done in BIMETS by using the \code{SIMULATE} procedure with the option \cr \code{ConstantAdjustment=ConstantAdjustmentRESCHECK};\cr
4. compute the difference between the historical and the simulated values for all the endogenous variables;\cr
5. check whether all the differences assessed in step 4 are zero in whole time range, eventually accounting for the error autocorrelation in behaviorals.\cr\cr
An example on \code{ConstantAdjustmentRESCHECK} usage is available at the end of the \code{SIMULATE} help page;\cr\cr
If a perfect tracking of the history is obtained, then the equations have been written coherently with the data, otherwise a simulated equation not tracking the historical values is an unambiguous symptom of data inconsistent with the model definition.\cr\cr
Aside from the residual check, the add-factors constitute an important tool to significantly improve the accuracy of forecasts made through an econometric model. Considering the following model: \cr\cr
\eqn{y_1=f_1(\bar{x},\bar{y}) + z_1}{y_1=f_1(x,y) + z_1} \cr
\eqn{...} \cr
\eqn{y_n=f_n(\bar{x},\bar{y}) + z_n}{y_n=f_n(x,y) + z_n} \cr\cr
the add-factors \eqn{\bar{z}=[z_1, ... ,z_n]}{z=[z_1, ... ,z_n]} can be interpreted as estimates of the disturbance terms' future values or as adjustments of the intercepts in each equation. These add-factors round out the forecasts, by summarizing the effects of all the systematic factors not included in the model. One choice for the computation of the add-factors is given by past estimation residuals and past forecast errors or by an average of these errors. \cr\cr
Please note that, in the case of equation that presents an LHS function, the add-factor will be added before the application of the inverse function, i.e., during the simulation, the following:\cr\cr
\eqn{g_1(y_1)=f_1(\bar{x},\bar{y}) + z_1}{g_1(y_1)=f_1(x,y) + z_1} \cr
\eqn{...} \cr
\eqn{g_n(y_n)=f_n(\bar{x},\bar{y}) + z_n}{g_n(y_n)=f_n(x,y) + z_n} \cr\cr
will be solved as:\cr\cr
\eqn{y_1=g_{1}^{-1}(f_1(\bar{x},\bar{y}) + z_1)}{y_1=g_1^(-1)( f_1(x,y) + z_1 )} \cr
\eqn{...} \cr
\eqn{y_n=g_{n}^{-1}(f_n(\bar{x},\bar{y}) + z_n)}{y_n=g_n^(-1)( f_n(x,y) + z_n )} \cr\cr
If a linear dependence between the simulated endogenous and the add-factor is preferred, users can manually insert an auxiliary equation \eqn{w_i} into the model definition, e.g. the following:\cr\cr
\eqn{g_i(y_i)=f_i(\bar{x},\bar{y})}{g_i(y_i)=f_i(x,y)} \cr\cr
can be replaced by:\cr\cr
\eqn{w_i=f_i(\bar{x},\bar{y})}{w_i=f_i(x,y)} \cr
\eqn{y_i=g_{i}^{-1}(w_i)}{y_i=g_i^(-1)(w_i)} \cr\cr
During the simulation, the add-factors (if requested by the user) will be applied as in the following:\cr\cr
\eqn{w_i=f_i(\bar{x},\bar{y}) + v_i}{w_i=f_i(x,y) + v_i} \cr
\eqn{y_i=g_{i}^{-1}(w_i) + z_i}{y_i=g_i^(-1)(w_i) + z_i} \cr\cr
given \eqn{v_i, z_i} as add-factors and the linear dependence from \eqn{z_i} and \eqn{y_i}. 
\tabular{c}{RATIONAL EXPECTATIONS MODELS}
BIMETS classifies a model as a forward-looking model if any model equation contains the \code{TSLEAD} time series function. Forward-looking models assume that economic agents have complete knowledge of an economic system and calculate the future value of economic variables correctly according to that knowledge. Thus, forward-looking models are called also rational expectations models and, in macro-econometric models, model-consistent expectations.\cr\cr
In forward-looking models, simulation complexity arises, and all simulation periods must be solved simultaneously because equations can contain references to past and future values. Thus, given \code{N} simulation periods requested by the user, each model equation must be replicated \code{N-1} times and modified before the simulation takes place, accounting for lead transformations. Finally, the extended model must be simulated as a single block of equations. \cr\cr
Internal data structures too, such as the incidence and the Jacobian matrix, and the reordering arrays \code{vpre} and \code{vblocks} (described later in the "The Optimal Reordering" section), grow with the number of simulation periods requested by the user. Therefore, they can only be calculated when a new simulation is requested rather than when the model \code{MDL} definition is parsed, further extending computational time in simulation.\cr\cr
A simulation that spans several decades in a forward-looking models with hundreds of equations is not feasible in BIMETS. For a real scenario in a rational expectations model, see "Computational details and capabilities" section in the \href{https://cran.r-project.org/package=bimets/vignettes/frb2bimets.pdf}{"US Federal Reserve quarterly model (FRB/US) in R with bimets"} vignette.\cr \cr
To understand BIMETS internals when dealing with forward-looking models, please consider the following simple example of a forward-looking model having a single identity:
\preformatted{
IDENTITY> Y
EQ> Y = TSLEAD(Y) - TSLAG(Y) + X
}

Given \code{X} as an exogenous variable, if the requested simulation has a \code{TSRANGE} that spans two periods, then the model will be internally transformed into something like:
\preformatted{
IDENTITY> Y
EQ> Y = Y__LEAD__1 - TSLAG(Y) + X

IDENTITY> Y__LEAD__1
EQ> Y__LEAD__1 = TSLEAD(Y,2) - Y + TSLEAD(X)
}
Accordingly, the model will be simulated only on the first period of the \code{TSRANGE}. Please note that \code{TSLAG(Y)} in the first equation, and \code{TSLEAD(Y,2)} in the second equation, are a kind of exogenous variables and must be defined in order for the simulation to be completed. Moreover, \code{Y} and \code{Y__LEAD__1} are simultaneously involved in the iterative simulation algorithm, and both depend on each other, as also stated in the incidence matrix for the extended model:
\preformatted{
$incidence_matrix

           Y Y__LEAD__1
Y          0          1
Y__LEAD__1 1          0
}
Due to the mechanism described above, only \code{DYNAMIC} simulations are allowed in forward-looking models. See examples below, for a Klein-like forward-looking model simulation.
\tabular{c}{THE OPTIMAL REORDERING}
In fact, the simulation process takes advantage of an appropriate equations reordering to increase the performances by iteratively solving only one subset of equations, while the others are solved straightforwardly. \emph{"...a different ordering of the equations can substantially affect the speed of convergence of the algorithm; indeed some orderings may produce divergence. The less feedback there is, the better the chances for fast convergence..."} - Don, Gallo - Solving large sparse systems of equations in econometric models - Journal of Forecasting 1987.\cr\cr
For backward-looking models, the \code{\link[=LOAD_MODEL]{LOAD_MODEL}} function builds the model's incidence matrix, then defines the proper equations reordering. The incidence matrix is built from the equations of the model; it is a square matrix in which each row and each column represent an endogenous variable. If the \code{(i,j)} element is equal to 1 then in the model definition the current value of the endogenous variable referred by the \code{i}-row depends directly from the current value of the endogenous variable referred by the \code{j}-column. The reader can see an incidence matrix example in the section \code{\link[=bimets]{"BIMETS package"}} of this manual wherein the content of the \code{kleinModel$incidence_matrix} variable is printed out.\cr\cr
In econometric models, the incidence matrix is usually very sparse. Only a few of the total set of endogenous variables are used in each equation. In this situation, ordering the equation in a particular sequence will lead to a sensible reduction of the number of iterations needed to achieve convergence. Reordering the equations is equivalent to rearranging rows and columns of the incidence matrix. In this way, the incidence matrix might be made lower triangular for a subset of the equations.
For this subset, an endogenous variable determined in a specific equation has no \emph{incidence} in any equation above it, although the same variable might have incidence in equations below it. Such a subset of equations is called recursive. Recursive systems are easy to solve. It is only necessary to solve each equation once if this is done in the proper order. On the other hand, it is unlikely for whole model to be recursive. Indeed the incidence graph is often cyclic, as in the Klein's model that presents the following circular dependecies in the incidence matrix: \code{p <- w1 <- y <- i <- p} as shown in the \code{\link[=bimets]{"BIMETS package"}} section figure.\cr\cr
For some subsets of the equations, some 1's will occur in the upper triangle of the incidence matrix for all possible orderings. Such subsets of equations are called \emph{simultaneous}. To solve the endogenous variables in the simultaneous subset of equations, an iterative algorithm has to be used. Nevertheless, the equations in a simultaneous subset may be ordered so that the pattern of the 1's in the upper triangle forms a spike. The variables corresponding to the 1's in the upper triangle are called \emph{feedback} variables.\cr\cr
A qualitative graphical example of an ordered incidence matrix is given in the following figure. The white areas are all 0's, the gray areas contain 0's and 1's. The 1's in the light gray areas refer to variables already evaluated in previous subset of equations, therefore they are known terms within the current subset. The 1's in the dark gray areas refer to variables evaluated within the subset.\cr\cr

\figure{Reordering.png}

In BIMETS, the final pattern of an incidence matrix after the equations reordering generally features \code{N+1} blocks: \cr\cr
- One recursive subset of equation, i.e. the pre-recursive \code{VPRE} in image;\cr
- \code{N} blocks of equations, \code{VBLOCK} in image, each built with a simultaneous \code{VSIM} and a post-recursive \code{VPOST} subset of equations;\cr\cr
As said, the pre-recursive and the post-recursive subsets are lower triangular. Therefore the corresponding equations are solvable with a cascade substitution with no iteration. Only the simultaneous subsets need an iterative algorithm to be solved. It is important to say that the convergence criterion may also be applied to feedback variables only: when the feedback variables converge, the rest of the simultaneous variables also do.\cr\cr BIMETS builds and analyzes the model's incidence matrix, and then it i) computes the strongly connected component of the related incidence graph by using the Tarjan algorithm (Ref: Tarjan, Robert - \emph{Depth-first search and linear graph algorithms} - SIAM Journal on Computing - June 1972), and ii) orders the equations in pre-recursive and, for each block of equations, in simultaneous and post-recursive subsets. The simultaneous subsets are then analyzed in order to find a minimal set of feedback variables. This last problem is known to be NP-complete (Ref: Garey, Johnson - \emph{Computers and Intractability: a Guide to the Theory of NP-completeness} - San Francisco, Freeman 1979).\cr\cr
The optimal reordering of the model equations is programmatically achieved through the use of an iterative algorithm applied to the incidence matrix that can produce \code{1+3*N} ordered lists of endogenous variables, respectively:\cr\cr
1. One  list \code{vpre} that is the ordered list containing the names of the endogenous pre-recursive variables to be sequentially computed (once per simulation period) before the simulation iterative algorithm takes place;\cr
2. For each of the \code{N} elements in the \code{vblocks} list: \cr
2.1 One list \code{vsim} (the simultaneous subset) that is the ordered list containing the names of the endogenous variables to be sequentially computed during each iteration of the simulation iterative algorithm;\cr
2.2 One list \code{vfeed} that is the list containing the names of the endogenous feedback variables; generally \code{vfeed} are the last variables in the ordered \code{vsim} list in the sambe block; \cr
2.3. One list \code{vpost} that is the ordered list containing the names of the endogenous post-recursive variables to be sequentially computed (once per simulation period) after the simulation iterative algorithm has found a solution in the previous simultaneous subset in the same block;\cr\cr
Once equations are reordered, the previous conceptual scheme is modified as follows:\cr\cr
1. initialize the solution for the current simulation period;\cr
2. compute the pre-recursive equations (i.e. the equation of the endogenous variables in the \code{vpre} ordered list);\cr
For each block in \code{vblocks}:\cr
3.1 iteratively compute the system of simultaneous equations (i.e. the equation of the endogenous variables in the \code{vsim} ordered list): for each iteration: i) update the values of the current endogenous variables, ii) update the feedback variables accordingly to the simulation algorithm in use (see next section for details on simulation algorithms) and iii) verify that the convergence criterion is satisfied on the feedback variables \code{vfeed} or that the maximum number of iterations has been reached;\cr
3.2 compute the post-recursive equations (i.e. the equation of the endogenous variables in the \code{vpost} ordered list);\cr
Finally:\cr
4. save the solutions;\cr\cr
Clearly, each endogenous variable is updated accordingly to its related equation \code{EQ>} in the \code{MDL} model definition.\cr \cr
In forward-looking models, the incidence matrix and the equations reordering depend on the simulation periods count, therefore the model attributes \code{incidence_matrix}, \code{vblocks}, and \code{vpre} are calculated only after a simulation has been initialized, and will be available to users in the \code{model$simulation[['__SIM_PARAMETERS__']]} lists.\cr\cr
The reader can see an equations reordering example in the section \code{\link[=bimets]{"BIMETS package"}} of this manual wherein the content of the \code{kleinModel$vpre} and \code{kleinModel$vblocks} variables are printed out.\cr\cr
\tabular{c}{THE SIMULATION ALGORITHMS}
Given \eqn{x_{j}}{x_(j)} the \eqn{j}-exogenous variable, \eqn{j=1..m}, and \eqn{y_{i,k}}{y_(i,k)}  the \eqn{i}-endogenous variable in a simultaneous subset, at the iteration \eqn{k}, with \eqn{i=1..n} the position of the equation in a reordered model, the modified Gauss-Seidel method takes for the approximation of the endogenous variable \eqn{y_{i,k}}{y_(i,k)} the solution of the following:\cr\cr
\eqn{y_{i,k}=f_i(x_1, ..., x_m, y_{1,k}, ..., y_{i-1,k}, y_{i,k-1},..., y_{n,k-1})}{y_(i,k)=f_i(x_1, ..., x_m, y_(1,k), ..., y_(i-1,k), y_(i,k-1),..., y_(n,k-1)}\cr\cr
Newton-Raphson's methods can be seen as an extension of the modified Gauss-Seidel algorithm, and a further step is required: in Newton-Raphson, feedback variables are updated not by using their model equations, but by using the inverse of the Jacobian matrix and the following assignment:\cr\cr
\eqn{\bar{y}^F_{k} \leftarrow \bar{y}^F_{k-1}+(I-J)^{-1}[\bar{y}^F_{k}-\bar{y}^F_{k-1}]}\cr\cr
given the vector of feedback variables values  \eqn{\bar{y}^F_{k}=[y_{n-F+1,k},...,y_{n,k}]} at iteration \eqn{k}, the identity matrix \eqn{I}, and the Jacobian matrix \eqn{J}, with \eqn{I,J \in R^{F,F}} and \eqn{F} equal to the number of feedback variables for the given block of equations. Please note that the modified Gauss-Seidel algorithm can be seen as a reduced form of a Netwotn algorithm, given \eqn{J=0}.\cr\cr In Newton-Raphson methods, the Jacobian matrix \eqn{J} is calculated as follows:\cr
1 - shock the feedback variables one at a time by a small amount;\cr
2 - for each shocked feedback variable, evaluate the shocked solution of the simultaneous subset in the current block;\cr
3 - calculate the derivatives (i.e. the column in the Jacobian matrix related to the shocked feedback variable) using the difference quotients between the shocked and the base solution of the simultaneous subset.
\cr\cr
As said, the convergence is always tested at each iteration's end on the feedback variables.\cr\cr
Newton-Raphson methods on a reordered model require the calculation of the Jacobian matrix on the feedback endogenous variables, i.e. at least \eqn{F+2} iterations per simulation period, with \eqn{F} as the number of feedback variables. For large models (i.e. more than 30 feedback variables) if the overall required convergence is greater than \eqn{10^{-6} \%}{10^(-6) \%} the speedup over the Gauss-Seidel method is small or negative, if the Jacobian matrix is recalculated at each iteration. Moreover, the Gauss-Seidel method does not require a matrix inversion; therefore, it is more robust against algebraical and numerical issues. For small models, both methods are fast on modern computers. On the other hand, Gausse-Seidel fails to find a convergence for a small set of econometric models, even when a convergence exists. In general, given a system of equations \eqn{Ax=b}, with \eqn{x,b \in R^n, n>0} and \eqn{A \in R^{n, n}}, the Gauss-Seidel algorithm is known to converge if either:\cr
- \eqn{A} is symmetric positive-definite;\cr
- \eqn{A} is strictly or irreducibly diagonally dominant. \cr\cr
To improve simulation speed, BIMETS evaluates the Newton-Raphson algorithm's performance during simulation, and, at each iteration, a new Jacobian matrix is calculated \emph{only if} the convergence speed is slower than a predefined threshold. In a vectorized simulation (e.g., \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}},  \code{\link[=OPTIMIZE]{OPTIMIZE}},  \code{\link[=RENORM]{RENORM}}), if \code{simAlgo="NEWTON"} the Jacobian matrix is calculated only on the unperturbed model, then applied to all realizations; if \code{simAlgo="FULLNEWTON"} a new Jacobian matrix is calculated for each realization.\cr\cr The simulation of a non-trivial model, if computed by using the same data but on different hardware, software or numerical libraries, produces numerical differences. Therefore a convergence criterion smaller than \eqn{10^{-7} \%}{10^(-7)} frequently leads to a local solution.\cr\cr See \emph{Numerical methods for simulation and optimal control of large-scale macroeconomic models - Gabay, Nepomiastchy, Rachidi, Ravelli - 1980} for further information. \cr \cr

For more realistic scenarios, several advanced econometric exercises on the US Federal Reserve FRB/US econometric model (e.g., dynamic simulation in a monetary policy shock, rational expectations, endogenous targeting, stochastic simulation, etc.) are available in the \href{https://cran.r-project.org/package=bimets/vignettes/frb2bimets.pdf}{"US Federal Reserve quarterly model (FRB/US) in R with bimets"} vignette. \cr \cr






}


\usage{
SIMULATE( model=NULL,
          simAlgo='GAUSS-SEIDEL',
          TSRANGE=NULL,
          simType='DYNAMIC',
          simConvergence=0.01,
          simIterLimit=100,
          ZeroErrorAC=FALSE,
          BackFill=0,
          Exogenize=NULL,
          ConstantAdjustment=NULL,
          verbose=FALSE,
          verboseSincePeriod=0,
          verboseVars=NULL,
          MULTMATRIX=FALSE,
          RENORM=FALSE,
          TARGET=NULL,
          INSTRUMENT=NULL,
          MM_SHOCK=0.00001,
          STOCHSIMULATE=FALSE,
          StochStructure=NULL,
          StochReplica=100,
          StochSeed=NULL,
          OPTIMIZE=FALSE,
          OptimizeBounds=NULL,
          OptimizeRestrictions=NULL,
          OptimizeFunctions=NULL,
          quietly=FALSE,
          RESCHECKeqList=NULL,
          JACOBIAN_SHOCK=1e-4,
          JacobianDrop=NULL,
          forceForwardLooking=FALSE,
          avoidCompliance=FALSE,
          ...)

}

\arguments{
  \item{model}{
  The BIMETS model object to be simulated. The simulation requires that all the model behaviorals, if any, have been previously estimated: all the behavioral coefficients (i.e. the regression coefficients and the autoregression coefficients for the errors, if any) must be numerically defined in the model object. (see also \code{\link[=ESTIMATE]{ESTIMATE}})
  }
  \item{simAlgo}{
  The simulation algorithm to be used to solve the system of model equations for each time period in the simulation \code{TSRANGE}.\cr
The options are:\cr
\bold{GAUSS-SEIDEL}: (default) the Gauss-Seidel algorithm is simple, robust, and works well for many backward-looking macro-econometric models. Equations are evaluated as-is in a proper order until the convergence, if any, is verified on feedback variables (see "The Simulation Algorithms") section up here). It is slower than Newton-Raphson algorithms for a very low convergence criterion, and fails to find a convergence for a small set of econometric models, even when a convergence exists. In general, given a system of equations \eqn{Ax=b}, with \eqn{x,b \in R^n, n>0} and \eqn{A \in R^{n, n}}, the Gauss-Seidel algorithm is known to converge if either:\cr
- \eqn{A} is symmetric positive-definite;\cr
- \eqn{A} is strictly or irreducibly diagonally dominant. \cr
\bold{NEWTON}: the Newton-Raphson algorithm allows users to solve a broader set of macro-econometric models than the Gauss-Seidel algorithm. Moreover, it is faster than the Gauss-Seidel algorithm in most cases (on modern computers, users must simulate an extensive econometric model with a low convergence criterion to appreciate the speedup). This type of algorithm requires the construction and the inversion of the Jacobian matrix for the feedback variables; thus, in some scenarios (e.g., equations with unverified \code{IF>} condition, implicit exogenizations, etc.), numerical issues can arise, and users are required to manually exclude some feedback variables from the Jacobian matrix by using the \code{JacobianDrop} argument.  To improve simulation speed, BIMETS evaluates the Newton-Raphson algorithm performance during simulation, and, at each iteration, a new Jacobian matrix is calculated \emph{only if} the convergence speed is slower than a predefined threshold. In a vectorized simulation (e.g., \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}},  \code{\link[=OPTIMIZE]{OPTIMIZE}},  \code{\link[=RENORM]{RENORM}}), if \code{simAlgo="NEWTON"} the Jacobian matrix is calculated only on the unperturbed model, then applied to all realizations; if \code{simAlgo="FULLNEWTON"} a new Jacobian matrix is calculated for each realization. As in the Gauss-Seidel algorithm, the convergence is evaluated on feedback variables.\cr
\bold{FULLNEWTON}: similar to the "base" Newton case, but when using this option, a different Jacobian matrix is calculated for each stochastic realization in a vectorized simulation (e.g., \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}},  \code{\link[=OPTIMIZE]{OPTIMIZE}},  \code{\link[=RENORM]{RENORM}}). The collection of all the Jacobian matrices is referred to as the 3-dimensional "Jacobian array". Generally, a "full" Newton simulation requires more computational time, mainly due to the increased number of Jacobian matrices to be calculated. However, the "base" Newton algorithm fails to converge when the model is highly non-linear and perturbances are significant (see examples in \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}} help page). Therefore, a "full" Newton algorithm is required in these cases. \cr

}
  \item{TSRANGE}{
  The time range of the simulation, as a four dimensional numerical array, \cr i.e. \code{TSRANGE=c(start_year, start_period, end_year, end_period)} 
  }
   \item{simType}{
   The simulation type requested:\cr
   \bold{DYNAMIC}: the default, whenever lagged endogenous variables are needed in the computations, the simulated values of the endogenous variables evaluated in the previous time periods are used;\cr
   \bold{STATIC}: rather than the simulated values, the actual historical values are used whenever lagged endogenous variables are needed in the computations;\cr
   \bold{FORECAST}: similar to the \code{'DYNAMIC'} option, but the initial solutions for the iterative process are given by the simulated values of the endogenous variables in the previous period. In this case there is no need for historical values of the endogenous variables in whole provided \code{TSRANGE};\cr
   \bold{RESCHECK}: in this case there is no iteration to carry out. The endogenous variables are evaluated for each single time period by using the historical values for all the variables on the right-hand side of the equation, both lagged and current period;
   }
\item{simConvergence}{
   The percentage convergence value requested for the iterative process, which stops when the percentage difference of all the feedback variables between iterations is less than \code{simConvergence} in absolute value
   }
\item{simIterLimit}{
The value representing the maximum number of iterations to be performed. The iterative procedure will stop when \code{simIterLimit} is reached or the feedback variables satisfy the \code{simConvergence} criterion
}
\item{ZeroErrorAC}{
If \code{TRUE} it zeroes out all the autoregressive terms, if any, in the behavioral equations
}
\item{BackFill}{
Defined as an \code{integer}, it is the length of historical data prior to the simulation \code{TSRANGE} to be saved along with the solutions
}
\item{Exogenize}{
A named list that specifies the endogenous variables to be exogenized. During the simulation and inside the provided time range, the exogenized endogenous variables will be assigned to their historical values. List names must be the names of the endogenous variables to be exogenized; each element of this list contains the time range of the exogenization for the related endogenous variable, in the form of a 4-dimensional integer array, i.e. start_year, start_period, end_year, end_period. A list element can also be assigned \code{TRUE}: in this case the related endogenous variable will be exogenized in whole simulation \code{TSRANGE} (see example)
}
\item{ConstantAdjustment}{
A named list that specifies the constant adjustments (i.e. add-factors) to be added to the selected equations of the model. Each constant adjustment can be see as a new exogenous variable added to the equation of the specified endogenous variable. The list names are the names of the involved endogenous variables; each element of this is list contains the time series to be added to the equation of the related endogenous variable. Each provided time series must verify the compliance control check defined in \code{\link[=is.bimets]{is.bimets}} (see example)
}
\item{verbose}{
If \code{TRUE} some verbose output will be activated. Moreover the values of all endogenous variables will be printed out during each iteration of the convergence algorithm for all time periods in the simulation \code{TSRANGE}
}
\item{verboseSincePeriod}{
An integer that activates the verbose output, during the iterative process, only after the provided number of simulation periods
}
\item{verboseVars}{
A \code{character} array with the names of the endogenous variables for which the verbose output will be activated in the iterative process
}
\item{MULTMATRIX}{It is \code{TRUE} when the parent call is a multiplier matrix operation requested by a \code{\link[=MULTMATRIX]{MULTMATRIX}} procedure}
\item{RENORM}{It is \code{TRUE} when the parent call is an endogenous targeting operation requested by a \code{\link[=RENORM]{RENORM}} procedure}
\item{TARGET}{see \code{\link[=MULTMATRIX]{MULTMATRIX}}}
\item{INSTRUMENT}{see \code{\link[=MULTMATRIX]{MULTMATRIX}}}
\item{MM_SHOCK}{see \code{\link[=MULTMATRIX]{MULTMATRIX}}}
\item{STOCHSIMULATE}{It is \code{TRUE} when the parent call is a stochastic simulation requested by a \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}} operation}
\item{StochStructure}{The \code{list()} that defines the disturbance structure applied to the model. See \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}}
\item{StochReplica}{An integer value that sets the number of stochastic simulation replications to be performed. See \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}}
\item{StochSeed}{A number used to initialize the pseudo-random number generator. It can be useful in order to replicate stochastic results. See \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}}
\item{OPTIMIZE}{It is \code{TRUE} when the parent call is an optimize operation requested by a  procedure}
\item{OptimizeBounds}{see \code{\link[=OPTIMIZE]{OPTIMIZE}}}
\item{OptimizeRestrictions}{see \code{\link[=OPTIMIZE]{OPTIMIZE}}}
\item{OptimizeFunctions}{see \code{\link[=OPTIMIZE]{OPTIMIZE}}}
\item{quietly}{ If \code{TRUE}, information messages will be suppressed, e.g. an information message will be printed out if any time series has a missing value in the extended \code{TSRANGE}, that is the simulation \code{TSRANGE} backward extended by \code{model$max_lag} periods (See \code{\link[=LOAD_MODEL]{LOAD_MODEL}} for info on model max lag)
} 
\item{RESCHECKeqList}{ If \code{simType=RESCHECK}, by using this argument users can select a subset of target endogenous variables: the simulation will be performed only for the selected variables. It must be provided as an array of endogenous names, e.g. \code{c('endo1','endo2',...)}
} 
\item{JACOBIAN_SHOCK}{
The value of the shock added to feedback variables in the derivative calculation of the Jacobian matrix. The default value is \code{1e-4} times the value of the feedback variable
}
\item{JacobianDrop}{
The array built with feedback variables names to be excluded from the Jacobian matrix calulation
}
\item{forceForwardLooking}{If \code{TRUE}, a backward-looking model will be simulated considering it such as a forward-looking model. This argument is for internal testing purposes, and there is no advantage for users to use it. See the "Rational expectations models" section here above in the \code{\link[=SIMULATE]{SIMULATE}} help page.}
\item{avoidCompliance}{
If \code{TRUE}, compliance control check of model time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
}
\item{...}{
Backward compatibility
}
  }

\value{
This function will add a new named element \code{simulation} into the output BIMETS model object.\cr\cr
The new \code{simulation} element is a named list; the names of the \code{simulation} list are the names of the endogenous variables of the model; each element of the \code{simulation} list contains the simulated time series of the related endogenous variable (see example). \cr\cr The \code{simulation} list also contains the \code{'__SIM_PARAMETERS__'} element that contains the arguments passed to the function call during the latest \code{SIMULATE} run, e.g. \code{TSRANGE}, \code{symType}, \code{simConvergence}, \code{symIterLimit}, \code{Exogenize}, etc.: this data can be helpful in order to replicate the simulation results. \cr\cr
In case of a \code{simType='RESCHECK'} simulation, a new named element \code{ConstantAdjustmentRESCHECK} will be added to the output model. This new element is populated with a list of time series that contains, for each endogenous variable, the tracking residuals time series such that, when using this tracking residuals as add-factors in simulation, the related equation will solve to the trajectory given, for that variable, by its historical data (see example).


}

\seealso{
\code{\link[=MDL]{MDL}}\cr
\code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
\code{\link[=ESTIMATE]{ESTIMATE}}\cr
\code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}\cr
\code{\link[=MULTMATRIX]{MULTMATRIX}}\cr
\code{\link[=RENORM]{RENORM}}\cr
\code{\link[=OPTIMIZE]{OPTIMIZE}}\cr
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
myModel<-LOAD_MODEL_DATA(myModel,myModelData)

#estimate model
myModel<-ESTIMATE(myModel, quietly = TRUE)
 
#DYNAMIC SIMULATION
 
#simulate model 
myModel<-SIMULATE(myModel
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
#      Date, Prd., myModel$simulation$cn
#
#      1923, 1   ,     50.338
#      1924, 1   ,    55.6994
#      1925, 1   ,    56.7111
#      ...
#      1940, 1   ,    66.7799
#      1941, 1   ,     75.451
#

TABIT(myModel$simulation$y)
#
#      Date, Prd., myModel$simulation$y
#
#      1923, 1   ,    56.0305
#      1924, 1   ,    65.8526
#      1925, 1   ,     64.265
#      ...
#      1940, 1   ,    76.8049
#      1941, 1   ,    93.4459
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
 
 
########################################################
#RESCHECK SIMULATION
 
#simulate model
myModel<-SIMULATE(myModel
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
#      Date, Prd., myModel$simulation$cn - myModel$modelData$cn
#
#      1923, 1   ,    1.56574
#      1924, 1   ,   0.493503
#      1925, 1   , -0.0076079
#      ... 
#      1939, 1   ,  -0.989201
#      1940, 1   ,  -0.785077
#      1941, 1   ,    2.17345
#
 
########################################################
#FORECAST GNP in 1942 and 1943 
 
#we need to extend exogenous variables in 1942 and 1943
myModel$modelData$w2   <- TSEXTEND(myModel$modelData$w2,  UPTO=c(1943,1))
myModel$modelData$t    <- TSEXTEND(myModel$modelData$t,   UPTO=c(1943,1))
myModel$modelData$g    <- TSEXTEND(myModel$modelData$g,   UPTO=c(1943,1))
myModel$modelData$time <- TSEXTEND(myModel$modelData$time,UPTO=c(1943,1)
                                     ,EXTMODE='LINEAR')
 
#simulate model
myModel<-SIMULATE(myModel
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
#      Date, Prd., myModel$simulation$y
#
#      1940, 1   ,    74.5781
#      1941, 1   ,    94.0153
#      1942, 1   ,    133.969
#      1943, 1   ,    199.913
#

########################################################
#VERBOSE SIMULATION
 
myModel<-SIMULATE(myModel
                  ,TSRANGE=c(1923,1,1941,1)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
                  ,verbose=TRUE
                  ,verboseSincePeriod=19
                  ,verboseVars=c('cn')
 )


########################################################
#DYNAMIC NEWTON SIMULATION 
#WITH EXOGENIZATION AND CONSTANT ADJUSTMENTS
 
#define exogenization list
#'cn' exogenized in 1923-1925
#'i' exogenized in whole TSRANGE
exogenizeList<-list(
                cn = c(1923,1,1925,1),
                i  = TRUE
)
 
#define add-factors list
constantAdjList<-list(
               cn = TIMESERIES(1,-1,START=c(1923,1),FREQ='A'),
               y  = TIMESERIES(0.1,-0.1,-0.5,START=c(1926,1),FREQ='A')
)
 
#simulate model
myModel<-SIMULATE(myModel
                  ,simAlgo='NEWTON'
                  ,simType='DYNAMIC'
                  ,TSRANGE=c(1923,1,1941,1)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
                  ,Exogenize=exogenizeList
                  ,ConstantAdjustment=constantAdjList 
 )
#SIMULATE(): endogenous variable "cn" has been exogenized from (trunc) ...
#SIMULATE(): endogenous variable "i" has been exogenized from (trunc) ...
#SIMULATE(): endogenous variable "cn" has a constant adjustment from (trunc) ...
#SIMULATE(): endogenous variable "y" has a constant adjustment from (trunc) ...
#
#Simulation:    100.00%
#...SIMULATE OK

########################################################
#EXAMPLE OF MODEL THAT FAILS GAUSS CONVERGENCE

#define model
myNewtonModelDefinition<-
"
MODEL 
 
COMMENT> Consumption
BEHAVIORAL> cn
TSRANGE 1922 1 1931 1
EQ> cn =  a1 + a2*p + a3*LAG(p,1) + a4*(w1+w2) 
COEFF> a1 a2 a3 a4 

COMMENT> Investment
BEHAVIORAL> i
TSRANGE 1922 1 1931 1
EQ> i = b1 + b2*p + b3*LAG(p,1) + b4*LAG(k,1)
COEFF> b1 b2 b3 b4 

COMMENT> Demand for Labor
BEHAVIORAL> w1 
TSRANGE 1922 1 1931 1
EQ> w1 = c1 + c2*(z+y+t-w2) + c3*LAG(z+y+t-w2,1) + c4*time
COEFF> c1 c2 c3 c4 

COMMENT> Gross National Product
IDENTITY> y
EQ> y = cn + i + g - t 

COMMENT> Simple copy of y in z
IDENTITY> z
EQ> z = cn + i + g - t 

COMMENT> Profits
IDENTITY> p
EQ> p = z + y - (w1+w2)

COMMENT> Capital Stock
IDENTITY> k
EQ> k = LAG(k,1) + i

END
"

#add data to model
myModelData$z <- myModelData$y
myNewtonModel<-LOAD_MODEL(modelText=myNewtonModelDefinition)
myNewtonModel<-LOAD_MODEL_DATA(myNewtonModel,myModelData)

#estimate model
myNewtonModel<-ESTIMATE(myNewtonModel, quietly = TRUE)

#GAUSS simulation fails to converge...
myNewtonModel <- SIMULATE(myNewtonModel,
                       TSRANGE = c(1921, 1, 1930, 1),
                       simConvergence = 1e-7)
                       
#...while NEWTON converges
myNewtonModel <- SIMULATE(myNewtonModel,
                       simAlgo='NEWTON',
                       TSRANGE = c(1921, 1, 1930, 1),
                       simConvergence = 1e-7)
                       
########################################################
#EXAMPLE OF MODEL THAT REQUIRES 
#A VARIABLE EXCLUSION FROM JACOBIAN MATRIX

#define model
myNewtonWithDropModelDefinition<-
"
MODEL 
 
COMMENT> Consumption
BEHAVIORAL> cn
TSRANGE 1922 1 1931 1
EQ> cn =  a1 + a2*p + a3*LAG(p,1) + a4*(w1+w2) 
COEFF> a1 a2 a3 a4 

COMMENT> Investment
BEHAVIORAL> i
TSRANGE 1922 1 1931 1
EQ> i = b1 + b2*p + b3*LAG(p,1) + b4*LAG(k,1)
COEFF> b1 b2 b3 b4 

COMMENT> Demand for Labor
BEHAVIORAL> w1 
TSRANGE 1922 1 1931 1
EQ> w1 = c1 + c2*(z+y+t-w2) + c3*LAG(z+y+t-w2,1) + c4*time
COEFF> c1 c2 c3 c4 

COMMENT> Gross National Product
IDENTITY> y
EQ> y = cn + i + g - t 

COMMENT> Simple copy of y in z
IDENTITY> z
EQ> z = cn + i + g - t 

COMMENT> Profits
IDENTITY> p
EQ> p = z + y - (w1+w2)
IF> y < 0

COMMENT> Capital Stock
IDENTITY> k
EQ> k = LAG(k,1) + i

END
"

#add data to model
myModelData$z <- myModelData$y
myNewtonModel <- LOAD_MODEL(modelText=myNewtonWithDropModelDefinition)
myNewtonModel <- LOAD_MODEL_DATA(myNewtonModel,myModelData)

#estimate model
myNewtonModel <- ESTIMATE(myNewtonModel, quietly = TRUE)

#"p" variable must be removed from Jacobian because of unverified IF>
myNewtonModel <- SIMULATE(myNewtonModel,
                       simAlgo='NEWTON',
                       JacobianDrop='p',
                       TSRANGE = c(1921, 1, 1930, 1),
                       simConvergence = 1e-7)
    

########################################################
#COMPARE FORECAST IN 3 ALTERNATIVE 
#EXOGENOUS SCENARIOS

#define model
myModelDefinition <-
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
myModel <- LOAD_MODEL(modelText=myModelDefinition)
myModel <- LOAD_MODEL_DATA(myModel,myModelData)

#estimate model
myModel <- ESTIMATE(myModel, quietly = TRUE)

#create 3 new models for the 3 scenarios
modelScenario1 <- myModel
modelScenario2 <- myModel
modelScenario3 <- myModel

#scenario 1, define exogenous paths
modelScenario1$modelData <- within(modelScenario1$modelData,{
              w2   = TSEXTEND(w2,  UPTO=c(1943,1))
              t    = TSEXTEND(t,   UPTO=c(1943,1))
              g    = TSEXTEND(g,   UPTO=c(1943,1))
              time = TSEXTEND(time,UPTO=c(1943,1)
                                  ,EXTMODE='LINEAR')
})

#scenario 2, define exogenous paths
modelScenario2$modelData <- within(modelScenario2$modelData,{
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
                            ,simIterLimit=20)

modelScenario2 <- SIMULATE(modelScenario2
                            ,simAlgo='NEWTON'
                            ,simType='FORECAST'
                            ,TSRANGE=c(1940,1,1943,1)
                            ,simConvergence=1e-5
                            ,simIterLimit=20)

modelScenario3 <- SIMULATE(modelScenario3
                            ,simAlgo='NEWTON'
                            ,simType='FORECAST'
                            ,TSRANGE=c(1940,1,1943,1)
                            ,simConvergence=1e-5
                            ,simIterLimit=20
                            ,ConstantAdjustment = constantAdjListScenario3
                            )

#compare results on GNP
TABIT(modelScenario1$simulation$y, 
      modelScenario2$simulation$y, 
      modelScenario3$simulation$y)
      
########################################################
#EXAMPLE OF MODEL'S TRACKING RESIDUALS INITIALIZATION BY USING 
#THE RESCHECK SIMULATION'S OUTPUT VALUE "ConstantAdjusmentRESCHECK"

#define the model with LHS funs
myModel<-'MODEL

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

END'


#define model data
modelData<-list(
  cn=TSERIES(39.8,41.9,45,49.2,50.6,52.6,55.1,56.2,57.3,
             57.8,55,50.9,45.6,46.5,48.7,51.3,57.7,58.7,57.5,61.6,65,69.7,
             START=c(1920,1),FREQ=1),
  g=TSERIES(4.6,6.6,6.1,5.7,6.6,6.5,6.6,7.6,7.9,8.1,9.4,
            10.7,10.2,9.3,10,10.5,10.3,11,13,14.4,15.4,22.3,
            START=c(1920,1),FREQ=1),
  i=TSERIES(2.7,-.2,1.9,5.2,3,5.1,5.6,4.2,3,5.1,1,-3.4,
            -6.2,-5.1,-3,-1.3,2.1,2,-1.9,1.3,3.3,4.9,
            START=c(1920,1),FREQ=1),
  k=TSERIES(182.8,182.6,184.5,189.7,192.7,197.8,203.4,
            207.6,210.6,215.7,216.7,213.3,207.1,202,
            199,197.7,199.8,201.8,199.9,201.2,204.5,209.4,
            START=c(1920,1),FREQ=1),
  p=TSERIES(12.7,12.4,16.9,18.4,19.4,20.1,19.6,19.8,21.1,
            21.7,15.6,11.4,7,11.2,12.3,14,17.6,17.3,15.3,19,21.1,23.5,
            START=c(1920,1),FREQ=1),
  w1=TSERIES(28.8,25.5,29.3,34.1,33.9,35.4,37.4,37.9,39.2,
             41.3,37.9,34.5,29,28.5,30.6,33.2,36.8,41,38.2,41.6,45,53.3,
             START=c(1920,1),FREQ=1),
  y=TSERIES(43.7,40.6,49.1,55.4,56.4,58.7,60.3,61.3,64,67,
            57.7,50.7,41.3,45.3,48.9,53.3,61.8,65,61.2,68.4,74.1,85.3,
            START=c(1920,1),FREQ=1),
  t=TSERIES(3.4,7.7,3.9,4.7,3.8,5.5,7,6.7,4.2,4,7.7,7.5,
            8.3,5.4,6.8,7.2,8.3,6.7,7.4,8.9,9.6,11.6,
            START=c(1920,1),FREQ=1),
  time=TSERIES(NA,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,
               3,4,5,6,7,8,9,10,  
               START=c(1920,1),FREQ=1),   
  w2=TSERIES(2.2,2.7,2.9,2.9,3.1,3.2,3.3,3.6,3.7,4,4.2,
             4.8,5.3,5.6,6,6.1,7.4,6.7,7.7,7.8,8,8.5,
             START=c(1920,1),FREQ=1)
)


#example data transformation
modelData<-within(modelData,{
  i=exp(i);     #we have LOG(i)     in the model MDL definition
  cn=log(cn);   #we have EXP(cn)    in the model MDL definition
  y=CUMSUM(y)   #we have TSDELTA(y) in the model MDL definition
})

#load model and model data
model<-LOAD_MODEL(modelText=myModel)
model<-LOAD_MODEL_DATA(model,modelData)

#estimate model
model<-ESTIMATE(model)

#get initial tracking residuals in range 1925-1935
#we need to set ZeroErrorAC to TRUE due to error autocorrelation 
#in Consumption MDL definition
model<-SIMULATE(model,
                TSRANGE=c(1925,1,1935,1),
                simType='RESCHECK',
                ZeroErrorAC=TRUE)

#get init trac
initTrac<-model$ConstantAdjustmentRESCHECK

#dynamic simulation using initTrac as constant adjustments
#we remove first two periods from simulation range
#due to error autocorrelation in Consumption MDL equation
model<-SIMULATE(model,
                TSRANGE=c(1927,1,1935,1),
                ConstantAdjustment=initTrac)

#check simulated values are equals to historical ones in simulation range
for (idxV in model$vendog)
{
  print(max(abs(model$simulation[[idxV]]-
                  TSPROJECT(model$modelData[[idxV]],TSRANGE=c(1927,1,1935,1)))))
}

########################################################
#EXAMPLE OF FORWARD-LOOKING KLEIN-LIKE MODEL 
#HAVING RATIONAL EXPECTATION ON INVESTMENTS

#define model
kleinLeadModelDefinition<-
"MODEL 
COMMENT> Klein Model 1 of the U.S. Economy 

COMMENT> Consumption
BEHAVIORAL> cn
TSRANGE 1921 1 1941 1
EQ> cn =  a1 + a2*p + a3*TSLAG(p,1) + a4*(w1+w2) 
COEFF> a1 a2 a3 a4

COMMENT> Investment with TSLEAD
IDENTITY> i
EQ> i = (MOVAVG(i,2)+TSLEAD(i))/2

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
kleinLeadModelData<-list(
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
kleinLeadModel<-LOAD_MODEL(modelText=kleinLeadModelDefinition)
kleinLeadModel<-LOAD_MODEL_DATA(kleinLeadModel,kleinLeadModelData)

#estimate model
kleinLeadModel<-ESTIMATE(kleinLeadModel, quietly = TRUE)

#set expected value of 2 for Investment in 1931
#(note that simulation TSRANGE spans up to 1930)
kleinLeadModel$modelData$i[[1931,1]]<-2 

#simulate model
kleinLeadModel<-SIMULATE(kleinLeadModel
                  ,TSRANGE=c(1924,1,1930,1))
                  
#print simulated investments
TABIT(kleinLeadModel$simulation$i)

}
