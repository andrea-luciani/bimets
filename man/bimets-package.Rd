\name{bimets-package}

\alias{bimets}

\docType{package}
\title{
bimets :: Time Series And Econometric Modeling In R
}
\description{

BIMETS is a software framework developed by using \code{R} language and designed for time series analysis and econometric modeling, which allows creating and manipulating time series, specifying simultaneous equation models of any size by using a kind of high-level description language, and performing model estimation and structural stability analysis, deterministic and stochastic simulation and forecasting, optimal control.\cr\cr
Besides, BIMETS computational capabilities provide many tools to pre-process data and post-process results, designed for statisticians and economists. These operations are fully integrated with the \code{R} environment.\cr \cr
The package can be installed and loaded in \code{R} with the following commands (with \code{R>} as the \code{R} command prompt):
\preformatted{
R> install.packages('bimets')
R> library(bimets)
}
If you have general questions about using BIMETS, or for bug reports, please use the \href{https://github.com/andrea-luciani/bimets/issues}{git issue tracker} or write to the \href{mailto:andrea.luciani@bancaditalia.it}{maintainer}.

\tabular{c}{\bold{TIME SERIES }  }



BIMETS supports daily, weekly, monthly, quarterly, semiannual and yearly time series. Time series with a frequency of 24 and 36 periods per year are also supported. Time series are created by the \code{\link[=TIMESERIES]{TIMESERIES}} function.\cr\cr
Example: 
\preformatted{
R> #yearly time series
R> myTS <- TIMESERIES(1:10,START=as.Date('2000-01-01'),FREQ=1)

R> #monthly time series
R> myTS <- TIMESERIES(1:10,START=c(2002,3),FREQ='M')
}

The main BIMETS time series capabilities are:\cr\cr
- \emph{Indexing}\cr
- \emph{Aggregation / Disaggregation}\cr
- \emph{Manipulation}\cr

\tabular{c}{\emph{Time Series Indexing} }


The BIMETS package extends R indexing capabilities in order to ease time series analysis and manipulation. Users can access and modify time series data:\cr\cr
- \emph{by date}:  users can select and modify a single observation by date by using the syntax \code{ts['Date']}, or multiple observations by using \code{ts['StartDate/EndDate']}; \cr\cr
- \emph{by year-period}: users can select and modify observations by providing the year and the period requested, i.e. \code{ts[[Year,Period]]};\cr\cr
- \emph{by observation index}: users can select and modify observations by simply providing the array of requested indices, i.e. \code{ts[indices]};\cr\cr
Example:
\preformatted{
R> #create a daily time series
R> myTS <- TIMESERIES((1:100),START=c(2000,1),FREQ='D')

R> myTS[1:3]                      #get first three obs.
R> myTS['2000-01-12']             #get Jan 12, 2000 data
R> myTS['2000-02-03/2000-02-14']  #get Feb 3 up to Feb 14
R> myTS[[2000,14]]                #get year 2000 period 14

R> myTS['2000-01-15'] <- NA        #assign to Jan 15, 2000
R> myTS[[2000,42]] <- NA           #assign to Feb 11, 2000
R> myTS[[2000,100]] <- c(-1,-2,-3) #extend time series starting from period 100


}
\tabular{c}{\emph{Time Series Aggregation / Disaggregation} }
The BIMETS package provides advanced (dis)aggregation capabilities, having linear interpolation capabilities in disaggregation, and aggregation functions (e.g. \code{STOCK}, \code{SUM}, \code{AVE}, etc.) while reducing the time series frequency. \cr\cr
Example:
\preformatted{
R> #create a monthly time series
R> myMonthlyTS <- TIMESERIES(1:100,START=c(2000,1),FREQ='M')

R> #convert to yearly time series by using the average as aggregation fun
R> myYearlyTS <- YEARLY(myMonthlyTS,'AVE')

R> #convert to daily by using central interpolation as disaggregation fun
R> myDailyTS <- DAILY(myMonthlyTS,'INTERP_CENTER')




}


  
\tabular{c}{\emph{Time Series Manipulation} }


The BIMETS package provides, among others, the following time series manipulation capabilities:\cr\cr
- Time series extension \code{\link[=TSEXTEND]{TSEXTEND}} \cr 
- Time series merging \code{\link[=TSMERGE]{TSMERGE}} \cr
- Time series projection \code{\link[=TSPROJECT]{TSPROJECT}} \cr
- Lag \code{\link[=TSLAG]{TSLAG}}\cr
- Lead \code{\link[=TSLEAD]{TSLEAD}}\cr
- Lag differences: standard, percentage, and logarithmic \code{\link[=TSDELTA]{TSDELTA}} \code{\link[=TSDELTAP]{TSDELTAP}} \code{\link[=TSDELTALOG]{TSDELTALOG}}\cr
- Cumulative product \code{\link[=CUMPROD]{CUMPROD}} \cr
- Cumulative sum \code{\link[=CUMSUM]{CUMSUM}} \cr
- Moving average \code{\link[=MOVAVG]{MOVAVG}} \cr
- Moving sum \code{\link[=MOVSUM]{MOVSUM}} \cr
- Time series data presentation \code{\link[=TABIT]{TABIT}}\cr\cr

Example:
\preformatted{
R> #define two time series
R> myTS1 <- TIMESERIES(1:100,START=c(2000,1),FREQ='M')
R> myTS2 <- TIMESERIES(-(1:100),START=c(2005,1),FREQ='M')

R> #extend time series up to Apr 2020 with quadratic formula
R> myExtendedTS <- TSEXTEND(myTS1,UPTO=c(2020,4),EXTMODE='QUADRATIC')

R> #merge two time series with sum
R> myMergedTS <- TSMERGE(myExtendedTS,myTS2,fun='SUM')

R> #project time series on arbitrary time range
R> myProjectedTS <- TSPROJECT(myMergedTS,TSRANGE=c(2004,2,2006,4))

R> #lag and delta\% time series
R> myLagTS <- TSLAG(myProjectedTS,2)
R> myDeltaPTS <- TSDELTAP(myLagTS,2)

R> #moving average
R> myMovAveTS <- MOVAVG(myDeltaPTS,5)

R> #print data
R> TABIT(myMovAveTS,
      myTS1,
      TSRANGE=c(2004,8,2004,12)
      )

    Date, Prd., myMovAveTS , myTS1
Aug 2004, 8   ,            , 56
Sep 2004, 9   ,            , 57
Oct 2004, 10  , 3.849002   , 58
Nov 2004, 11  , 3.776275   , 59
Dec 2004, 12  , 3.706247   , 60
 
}


\tabular{c}{\bold{ECONOMETRIC MODELING} } 

BIMETS econometric modeling capabilities comprehend:\cr\cr
- \emph{Model Definition Language} \cr
- \emph{Estimation} \cr
- \emph{Structural Stability} \cr
- \emph{Simulation} \cr
- \emph{Stochastic Simulation} \cr
- \emph{Multipliers Analysis} \cr
- \emph{Endogenous Targeting } \cr
- \emph{Optimal Control } \cr\cr
We will go through each item of the list with a simple Klein model example (ref: \emph{"Economic Fluctuations in the United States 1921-1941"} by L. R. Klein, Wiley and Sons Inc., New York, 1950). \cr
\tabular{c}{\emph{Model Definition Language} }

BIMETS provides a language to specify an econometric model unambiguously. This section describes how to create a model and its general structure. The specification of an econometric model is translated and identified by keyword statements which are grouped in a model file, i.e. a plain text file or a \code{character} variable with a specific syntax. Collectively, these keyword statements constitute the BIMETS Model Description Language (from now on \code{\link[=MDL]{MDL}}). The model specifications consist of groups of statements. Each statement begins with a keyword. The keyword classifies the component of the model which is being specified.\cr\cr
Below is an example of Klein's model, which can either be stored in an \code{R} variable of class \code{character} or in a plain text file with an \code{MDL} compliant syntax. \cr\cr
The content of the \emph{klein1.txt} variable  is:
\preformatted{
R> klein1.txt <- "
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

END
"
}

Given: \cr \cr
- \code{cn} as \emph{Private Consumption Expenditure};   \cr
- \code{i} as \emph{Investment};    \cr
- \code{w1} as \emph{Wage Bill of the Private Sector (Demand for Labor)};   \cr
- \code{p} as \emph{Profits};  \cr
- \code{k} as \emph{Stock of Capital Goods};   \cr
- \code{y} as \emph{Gross National Product};  \cr  
- \code{w2} as \emph{Wage Bill of the Government Sector};   \cr
- \code{time} as an annual index of the passage of time;   \cr
- \code{g} as \emph{Government Expenditure plus Net Exports};   \cr
- \code{t} as \emph{Business Taxes}.   \cr \cr
\code{a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4} are coefficients to be estimated.  \cr \cr
\cr 

This system has only six equations, three of which must be fitted to assess the coefficients. It may not seem challenging to solve this system. However, the objective complexity emerges if you look at the incidence graph in the following figure, wherein endogenous variables are plotted in blue and exogenous variables are plotted in pink.\cr \cr

\figure{KleinIG.png} \cr \cr

Each edge states a simultaneous dependence from a variable to another, e.g. the \code{w1} equation depends on the current value of the \code{time} time series; complexity arises because in this model there are several circular dependencies, one of which is plotted in dark blue.  \cr \cr
A circular dependency in the incidence graph of a model implies that the model is a \emph{simultaneous} equations model. It must be estimated using ad-hoc procedures; moreover, it can be simulated, e.g. performing a forecast, only using an iterative algorithm.

As shown in the code, the model definition is quite intuitive. The first keyword is \code{MODEL}, while at the end of the model definition we can find the \code{END} keyword. Available tags in the definition of a generic BIMETS model are: 

- \bold{EQUATION>} or \bold{BEHAVIORAL>} indicate the beginning of a series of keyword statements describing a behavioral equation;\cr \cr
- \bold{IDENTITY>} indicates the beginning of a series of keyword statements describing an identity or technical equation;\cr \cr
- \bold{EQ>} specifies the mathematical expression for a behavioral equation or an identity equation;\cr \cr
- \bold{COEFF>} specifies the coefficient names used in the EQ> keyword statement of a behavioral equation;\cr \cr
- \bold{ERROR>} specifies an autoregressive process of a given order for the regression error;\cr \cr
- \bold{PDL>} defines an Almon polynomial distributed lag;\cr  \cr
- \bold{RESTRICT>} is a keyword that can be used to specify linear coefficient restrictions;\cr \cr
- \bold{IF>} is used to conditionally evaluate an identity during a simulation, depending on a logical expression's value. Thus, it is possible to have a model alternating between two or more identity specifications for each simulation period, depending upon results from other equations;\cr \cr
- \bold{IV>} specifies the mathematical expression for an instrumental variable used in a behavioral equation;\cr \cr
- \bold{COMMENT>} can be used to insert comments into a model;\cr \cr
The mathematical expression in the EQ> and IF> definitions can include the standard arithmetic operators, parentheses, and the following MDL time series functions: \cr \cr
- \code{TSLAG(ts,i)}: lag the \code{ts} time series by \code{i}-periods;\cr \cr
- \code{TSDELTA(ts,i)}: \code{i}-periods difference of the \code{ts} time series;\cr \cr
- \code{TSDELTAP(ts,i)}: \code{i}-periods percentage difference of the \code{ts} time series;\cr \cr
- \code{TSDELTALOG(ts,i)}: \code{i}-periods logarithmic difference of the \code{ts} time series;\cr \cr
- \code{MOVAVG(ts,i)}: \code{i}-periods moving average of the \code{ts} time series;\cr \cr
- \code{MOVSUM(ts,i)}: \code{i}-periods moving sum of the  \code{ts} time series;\cr \cr
- \code{LOG(ts)}: log of the \code{ts} time series.;\cr \cr
- \code{EXP(ts)}: exponential of the \code{ts} time series; \cr \cr
- \code{ABS(ts)}: absolute values of the \code{ts} time series;\cr \cr

More details are available in \code{\link[=MDL]{MDL}} and \code{\link[=LOAD_MODEL]{LOAD_MODEL}} help pages. \code{LOAD_MODEL()} is the BIMETS function that reads an MDL model file and creates an equivalent R data structure.\cr \cr
Back to Klein's model example, the BIMETS \code{\link[=LOAD_MODEL]{LOAD_MODEL}} function reads the \emph{klein1.txt} model as previously defined:
\preformatted{
R> kleinModel <- LOAD_MODEL(modelText = klein1.txt)

Analyzing behaviorals...
Analyzing identities...
Optimizing...
Loaded model "klein1.txt":
    3 behaviorals
    3 identities
    12 coefficients
...LOAD MODEL OK
}

As shown in the output, BIMETS counted 3 behavioral equations, 3 identities and 12 coefficients. Now in the \code{R} session there is a variable named \emph{kleinModel} that contains the model structure defined in the \emph{klein1.txt} variable. From now on, users can ask BIMETS about any details of this model.\cr\cr
For example, to gather information on the "\code{cn}" \emph{Consumption} behavioral equation: 
\preformatted{
R> kleinModel$behaviorals$cn

$eq
[1] "cn=a1+a2*p+a3*TSLAG(p,1)+a4*(w1+w2)"

$eqCoefficientsNames
[1] "a1" "a2" "a3" "a4"

$eqComponentsNames
[1] "cn" "p" "w1" "w2"

$tsrange
[1] 1921 1 1941 1

$eqRegressorsNames
[1] "1" "p" "TSLAG(p,1)" "(w1+w2)"

$eqSimExp
expression(cn[2, ] = cn__ADDFACTOR[2, ] + +cn__a1 * 1 + cn__a2 *
p[2, ] + cn__a3 * (p[1, ]) + cn__a4 * (w1[2, ] + w2[2, ]))

etc...
}

Users can always read (or carefully change) any model parameters. The \code{\link[=LOAD_MODEL]{LOAD_MODEL}} function parses behavioral and identity expressions of the \code{\link[=MDL]{MDL}} definition, but it also does a significant optimization. Properly reordering the model equations is a key preparatory step in the later phase of the simulation, in order to guarantee performance and convergence, if any, with the aim of minimizing the number of \emph{feedback} endogenous variables (see \code{\link[=SIMULATE]{SIMULATE}}).\cr\cr
The \code{\link[=LOAD_MODEL]{LOAD_MODEL}} function builds the model's incidence matrix, and uses this matrix to calculate the proper evaluation order of the model equations during the simulation.\cr\cr Back to the Klein's model example, the incidence matrix and the reordering of the equations are stored in the following variables:
\preformatted{
R> kleinModel$incidence_matrix

   cn i w1 y p k
cn 0  0 1  0 1 0
i  0  0 0  0 1 0
w1 0  0 0  1 0 0
y  1  1 0  0 0 0
p  0  0 1  1 0 0
k  0  1 0  0 0 0

R> kleinModel$vpre

NULL

R> kleinModel$vsim

[1] "w1" "p" "cn" "i" "y"

R> kleinModel$vfeed

[1] "y"

R> kleinModel$vpost

[1] "k"

}

While simulating the Klein's model, BIMETS will iterate on the computation of, in order,\cr \code{w1 -> p -> cn -> i -> y} (the \code{vsim} variables), by looking for convergence on \code{y} (the \code{vfeed} variable, only one in this example) that is the feedback variable. If the convergence is achieved, it will calculate \code{k} (the \code{vpost} variable). The \code{vpre} array in this example is empty; therefore, no equation has to be evaluated before the iterative algorithm.\cr\cr
More details on the equations reordering are available in \code{\link[=SIMULATE]{SIMULATE}} and \code{\link[=LOAD_MODEL]{LOAD_MODEL}} help pages.\cr\cr
Once the model has been parsed, users need to load the data of all the time series involved in the model, by using the \code{\link[=LOAD_MODEL_DATA]{LOAD_MODEL_DATA}} function. In the following example, the code defines a list of time series and loads this list into the Klein's model previously defined:
\preformatted{
R> kleinModelData <- list(  
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

R> kleinModel <- LOAD_MODEL_DATA(kleinModel,kleinModelData)
}
Since time series and other data (e.g. regressor coefficients, error coefficients, constant adjustments, targets, instruments, etc...) are stored in the model object, users can define multiple model objects - each with its own arbitrary data - in the same \code{R} session. BIMETS makes it possible to estimate, simulate and compare results from different models with different data sets. Furthermore, users can easily save an estimated or a simulated model as a standard \code{R} variable, thus reloading it later, having all available data and time series stored in it, i.e. endogenous and exogenous time series, estimated coefficients, constant adjustments, simulation options, simulated time series, calculated instruments, targets, etc. (see also \code{\link[=SIMULATE]{SIMULATE}}, \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}, \code{\link[=RENORM]{RENORM}}, \code{\link[=OPTIMIZE]{OPTIMIZE}})\cr\cr
An advanced MDL model example follows:
\preformatted{

R> #KLEIN MODEL WITH AUTOCORRELATION, RESTRICTIONS, 
R> #CONDITIONAL EVALUATIONS AND LHS FUNCTIONS

R> lhsKlein1.txt <- "
MODEL

COMMENT> Modified Klein Model 1 of the U.S. Economy with PDL,
COMMENT> autocorrelation on errors, restrictions, 
COMMENT> conditional evaluations and LHS functions on EQ

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

}

See \code{\link[=MDL]{MDL}} help page for details.

\tabular{c}{\emph{Estimation} }
The BIMETS \code{\link[=ESTIMATE]{ESTIMATE}} function estimates equations that are linear in the coefficients, as specified in the behavioral equations of the model object. Coefficients can be estimated for single equations or blocks of simultaneous equations. The estimation function supports:\cr\cr
- \emph{Ordinary Least Squares};\cr
- \emph{Instrumental Variables};\cr
- \emph{Deterministic linear restrictions on the coefficients};\cr
- \emph{Almon Polynomial Distributed Lags};\cr
- \emph{Autocorrelation of the errors};\cr
- \emph{Structural stability analysis (Chow tests)};\cr\cr
Restrictions procedure derives from Lagrange Multipliers' theory, while the Cochrane-Orcutt method allows accounting for residuals autocorrelation. \cr\cr
The estimation of the previously defined Klein's model is shown in the following example:  
\preformatted{
R> kleinModel <- ESTIMATE(kleinModel)
}
Users can also estimate a selection of behavioral equations:
\preformatted{
R> kleinModel <- ESTIMATE(kleinModel,eqList=c('cn'))

Estimate the Model klein1.txt:
the number of behavioral equations to be estimated is 1.
The total number of coefficients is 4.

_________________________________________

BEHAVIORAL EQUATION: cn
Estimation Technique: OLS

cn                  = 16.2366
                        T-stat. 12.46382 ***
                        
                    + 0.1929344 p
                        T-stat. 2.115273 *
                        
                    + 0.0898849 TSLAG(p,1)
                        T-stat. 0.9915824
                    
                    + 0.7962187 (w1+w2)
                        T-stat. 19.93342 ***


STATs:
R-Squared                       : 0.9810082
Adjusted R-Squared              : 0.9776567
Durbin-Watson Statistic         : 1.367474
Sum of squares of residuals     : 17.87945
Standard Error of Regression    : 1.02554
Log of the Likelihood Function  : -28.10857
F-statistic                     : 292.7076
F-probability                   : 7.993606e-15
Akaike's IC                     : 66.21714
Schwarz's IC                    : 71.43975
Mean of Dependent Variable      : 53.99524
Number of Observations          : 21
Number of Degrees of Freedom    : 17
Current Sample (year-period)    : 1921-1 / 1941-1


Signif. codes: *** 0.001 ** 0.01 * 0.05

...ESTIMATE OK
}
A similar output is shown for each estimated regression. Once the estimation is completed, coefficient values, residuals, statistics, etc. are stored in the model object.
\preformatted{
R> #print estimated coefficients
R> kleinModel$behaviorals$cn$coefficients

   [,1]
a1 16.2366003
a2 0.1929344
a3 0.0898849
a4 0.7962187

R> #print residuals
R> kleinModel$behaviorals$cn$residuals

Time Series:
Start = 1921
End = 1941
Frequency = 1
[1]  -0.323893544 -1.250007790 -1.565741401 -0.493503129  0.007607907
[6]   0.869096295  1.338476868  1.054978943 -0.588557053  0.282311734
[11] -0.229653489 -0.322131892  0.322281007 -0.058010257 -0.034662717
[16]  1.616497310 -0.435973632  0.210054350  0.989201310  0.785077489
[21] -2.173448309

R> #print a selection of estimate statistics
R> kleinModel$behaviorals$cn$statistics$DegreesOfFreedom

[1] 17

R> kleinModel$behaviorals$cn$statistics$StandardErrorRegression

[1] 1.02554

R> kleinModel$behaviorals$cn$statistics$CoeffCovariance

    a1            a2            a3            a4
a1  1.6970227814  0.0005013886 -0.0177068887 -0.0329172192
a2  0.0005013886  0.0083192948 -0.0052704304 -0.0013188865
a3 -0.0177068887 -0.0052704304  0.0082170486 -0.0006710788
a4 -0.0329172192 -0.0013188865 -0.0006710788  0.0015955167

R> kleinModel$behaviorals$cn$statistics$AdjustedRSquared
[1] 0.9776567

R> kleinModel$behaviorals$cn$statistics$LogLikelihood
[1] -28.10857
}

Below is an example of a model estimation that presents coefficient restrictions, PDL, error autocorrelation, and conditional equation evaluations:
\preformatted{

R> #define model
R> advancedKlein1.txt <- 
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

R> #load model and data
R> advancedKleinModel <- LOAD_MODEL(modelText=advancedKlein1.txt)

Analyzing behaviorals...
Analyzing identities...
Optimizing...
Loaded model "advancedKlein1.txt":
    3 behaviorals
    3 identities
    12 coefficients
...LOAD MODEL OK

R> advancedKleinModel <- LOAD_MODEL_DATA(advancedKleinModel,kleinModelData)

Load model data "kleinModelData" into model "advancedKlein1.txt"...
...LOAD MODEL DATA OK

R> #estimate model
R> advancedKleinModel <- ESTIMATE(advancedKleinModel)

Estimate the Model advancedKlein1.txt:
the number of behavioral equations to be estimated is 3.
The total number of coefficients is 13.

_________________________________________

BEHAVIORAL EQUATION: cn
Estimation Technique: OLS
Autoregression of Order  2  (Cochrane-Orcutt procedure)

Convergence was reached in  6  /  20  iterations.


cn                  =   14.82685    
                        T-stat. 7.608453    ***

                    +   0.2589094   p
                        T-stat. 2.959808    *

                    +   0.01423821  TSLAG(p,1)
                        T-stat. 0.1735191   

                    +   0.8390274   (w1+w2)
                        T-stat. 14.67959    ***

ERROR STRUCTURE:  AUTO(2) 

AUTOREGRESSIVE PARAMETERS:
Rho             Std. Error      T-stat.         
 0.2542111       0.2589487       0.9817045       
-0.05250591      0.2593578      -0.2024458       


STATs:
R-Squared                      : 0.9826778   
Adjusted R-Squared             : 0.9754602   
Durbin-Watson Statistic        : 2.256004    
Sum of squares of residuals    : 8.071633    
Standard Error of Regression   : 0.8201439   
Log of the Likelihood Function : -18.32275   
F-statistic                    : 136.1502    
F-probability                  : 3.873514e-10
Akaike's IC                    : 50.6455     
Schwarz's IC                   : 56.8781     
Mean of Dependent Variable     : 54.29444    
Number of Observations         : 18
Number of Degrees of Freedom   : 12
Current Sample (year-period)   : 1923-1 / 1940-1


Signif. codes:   *** 0.001  ** 0.01  * 0.05  



_________________________________________

BEHAVIORAL EQUATION: i
Estimation Technique: OLS

i                   =   0.5348561   
                        T-stat. 0.06197295  

                    +   0.6267204   p
                        T-stat. 4.835884    ***

                    +   0.3732796   TSLAG(p,1)
                        T-stat. 2.88029     *

                    -   0.0796483   TSLAG(k,1)
                        T-stat. -1.871304   

RESTRICTIONS:
b2+b3=1

RESTRICTIONS F-TEST:
F-value            : 5.542962       
F-prob(1,14)       : 0.03368297     


STATs:
R-Squared                      : 0.9009016   
Adjusted R-Squared             : 0.8876885   
Durbin-Watson Statistic        : 1.284709    
Sum of squares of residuals    : 23.40087    
Standard Error of Regression   : 1.249023    
Log of the Likelihood Function : -27.90251   
F-statistic                    : 68.18238    
F-probability                  : 2.954599e-08
Akaike's IC                    : 63.80501    
Schwarz's IC                   : 67.3665     
Mean of Dependent Variable     : 1.111111    
Number of Observations         : 18
Number of Degrees of Freedom   : 15
Current Sample (year-period)   : 1923-1 / 1940-1


Signif. codes:   *** 0.001  ** 0.01  * 0.05  



_________________________________________

BEHAVIORAL EQUATION: w1
Estimation Technique: OLS

w1                  =   2.916775    
                        T-stat. 1.808594    

                    +   0.4229623   (y+t-w2)
                        T-stat. 10.32315    ***

                    +   c3          TSLAG(y+t-w2,1)
                        PDL

                    +   0.1020647   time
                        T-stat. 3.048413    **

PDL:
c3 1 2

Distributed Lag Coefficient: c3
Lag     Coeff.         Std. Error     T-stat.        
0        0.1292072      0.06348684     2.035181      
1        0.01035948     0.04266269     0.2428229     
SUM      0.1395667      0.03801893    


STATs:
R-Squared                      : 0.9806112   
Adjusted R-Squared             : 0.9746454   
Durbin-Watson Statistic        : 2.038182    
Sum of squares of residuals    : 6.59422     
Standard Error of Regression   : 0.7122132   
Log of the Likelihood Function : -16.50329   
F-statistic                    : 164.3727    
F-probability                  : 5.454803e-11
Akaike's IC                    : 45.00658    
Schwarz's IC                   : 50.34881    
Mean of Dependent Variable     : 36.41667    
Number of Observations         : 18
Number of Degrees of Freedom   : 13
Current Sample (year-period)   : 1923-1 / 1940-1


Signif. codes:   *** 0.001  ** 0.01  * 0.05  


...ESTIMATE OK
}

\tabular{c}{\emph{Structural Stability} }
One of the main purposes of econometric modeling is its use for forecast and policy evaluation and, to this end, the stability of any behavioral equation parameters over time should be verified. In order to check for structural stability two different procedures, which can be derived from the so-called Chow-tests, are applied.\cr \cr
Given a sample of observations (i.e. the base \code{TSRANGE}) and selecting an arbitrary forward extension (i.e. the extended \code{TSRANGE}) we can perform the same regression by using these two time ranges.\cr\cr
In general, a stability analysis is carried on in the following ways:\cr
- comparing the parameter estimates arising from the two regressions: this is known as the covariance analysis;\cr
- checking the accuracy of the forecast for the dependent variable in the extended \code{TSRANGE}, using the estimates produced in the base \code{TSRANGE}: this is known as the predictive power test.\cr\cr
The test statistic follows the \code{F} distribution  and can be performed during the \code{ESTIMATE()} function execution by using the \code{CHOWTEST} argument set to \code{TRUE} (more details in the \code{\link[=ESTIMATE]{ESTIMATE}} help page).\cr \cr
Example:
\preformatted{
#chow test for the consumption equation
#base TSRANGE set to 1921/1935
R> kleinModelChow <- ESTIMATE(kleinModel
                       ,eqList='cn'
                       ,TSRANGE=c(1921,1,1935,1)
                       ,forceTSRANGE=TRUE
                       ,CHOWTEST=TRUE)

Estimate the Model klein1.txt:
the number of behavioral equations to be estimated is 1.
The total number of coefficients is 4.

_________________________________________

BEHAVIORAL EQUATION: cn
Estimation Technique: OLS

cn                  =   13.1275    
                        T-stat. 6.5046     ***

                    +   0.16698    p
                        T-stat. 2.18304    

                    +   0.0885684  TSLAG(p,1)
                        T-stat. 0.975042   

                    +   0.887964   (w1+w2)
                        T-stat. 12.61      ***


STATs:
R-Squared                      : 0.978728   
Adjusted R-Squared             : 0.972926   
Durbin-Watson Statistic        : 1.38       
Sum of squares of residuals    : 6.9186     
Standard Error of Regression   : 0.793072   
Log of the Likelihood Function : -15.4803   
F-statistic                    : 168.7      
F-probability                  : 1.77673e-09
Akaike's IC                    : 40.9606    
Schwarz's IC                   : 44.5009    
Mean of Dependent Variable     : 50.9133    
Number of Observations         : 15
Number of Degrees of Freedom   : 11
Current Sample (year-period)   : 1921-1 / 1935-1


Signif. codes:   *** 0.001  ** 0.01  * 0.05  



STABILITY ANALYSIS:
Behavioral equation: cn 

Chow test:
Sample (auto)      : 1936-1 / 1941-1 
F-value            : 4.48873       
F-prob(6,17)       : 0.00668723      

Predictive Power:

Date, Prd., Actual  , Predict   , Error      , Std. Error, T-stat         

1936, 1   ,  57.7   ,  56.5544  ,  1.14564   ,  1.01181  ,  1.13227      
1937, 1   ,  58.7   ,  59.931   , -1.23099   ,  1.0201   , -1.20673      
1938, 1   ,  57.5   ,  57.9721  , -0.472122  ,  0.968638 , -0.487409     
1939, 1   ,  61.6   ,  61.5207  ,  0.0793139 ,  1.20048  ,  0.0660685    
1940, 1   ,  65     ,  65.3957  , -0.395718  ,  1.24227  , -0.318545     
1941, 1   ,  69.7   ,  73.7965  , -4.09655   ,  1.6693   , -2.45405   
          

...ESTIMATE OK                      
}
\tabular{c}{\emph{Simulation} }


The simulation of an econometric model basically consists in solving the system of the equations describing the model for each time period in the specified time interval. Since the equations may not be linear in the variables, and since the graph derived from the incidence matrix may be cyclic, the usual methods based on linear algebra are not applicable. The simulation must be solved by using an iterative algorithm. \cr \cr
BIMETS simulation capabilities support:\cr\cr
- \emph{Static simulations}: a static multiple equation simulation, in which the historical values for the lagged endogenous variables are used in the solutions of subsequent periods; \cr\cr
- \emph{Dynamic simulations}: a dynamic simulation, in which the simulated values for the lagged endogenous variables are used in the solutions of subsequent periods;\cr\cr
- \emph{Forecast simulations}: similar to dynamic simulation, but during the initialization of the iterative algorithm the starting values of endogenous variables in a period are set equal to the simulated values of the previous period. This allows the simulation of future endogenous observations, i.e. the forecast;  \cr\cr
- \emph{Residuals check}: a single period, single equation simulation; simulated time series in output are just the computation of the RHS (right-hand-side) of their equation, by using the historical values of the involved time series and by accounting for error autocorrelation and PDLs, if any;\cr\cr
- \emph{Stochastic Simulation}: see \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}};\cr\cr
- \emph{Partial or total exogenization of endogenous variables}: in the provided time interval (i.e. partial exog.) or in whole simulation time range (i.e. total exog.), the values of the selected endogenous variables can be definitely set to their historical values, by excluding their equations from the iterative algorithm of simulation;\cr\cr
- \emph{Constant adjustment of endogenous variables (add-factors)}: adds up a new exogenous time series (i.e. the "constant adjustment") in the equation of the selected endogenous variables.\cr\cr
- \emph{Gauss-Seidel} and \emph{Newton-Raphson} simulation algorithms: the Gauss-Seidel algorithm is simple, robust, and works well for many backward-looking macro-econometric models. Equations are evaluated as-is in a proper order until the convergence, if any, is verified on feedback variables. It is slower than Newton algorithms for a very low convergence criterion, and fails to find a convergence for a small set of econometric models, even when a convergence exists. The Newton-Raphson algorithm allows users to solve a broader set of macro-econometric models than the Gauss-Seidel algorithm. Moreover, it is usually faster than the Gauss-Seidel algorithm (on modern computers, users must simulate an extensive econometric model with a low convergence criterion to appreciate the speedup). This type of algorithm requires the construction and the inversion of the Jacobian matrix for the feedback variables; thus, in some scenarios, numerical issues can arise, and users are required to manually exclude some feedback variables from the Jacobian matrix by using the \code{JacobianDrop} argument of the \code{\link[=SIMULATE]{SIMULATE}} procedure.\cr\cr
More details are available in the \code{\link[=SIMULATE]{SIMULATE}} help page. \cr \cr
Back to Kelin's model example, let's forecast the \emph{GNP} (i.e. the "\code{y}" endogenous variable, originally referred as "Net national income, measured in billions of 1934 dollars", pag. 141 in "Economic Fluctuations in the United States" by L. R. Klein, Wiley and Sons Inc., New York, 1950) up to 1943: 
\preformatted{
R> #FORECAST GNP in 1942:1944

R> #we need to extend exogenous variables in 1942 up to 1944
R> kleinModel$modelData <- within(kleinModel$modelData,{
            w2   = TSEXTEND(w2,  UPTO=c(1944,1),EXTMODE='CONSTANT')
            t    = TSEXTEND(t,   UPTO=c(1944,1),EXTMODE='CONSTANT')
            g    = TSEXTEND(g,   UPTO=c(1944,1),EXTMODE='CONSTANT')
            time = TSEXTEND(time,UPTO=c(1944,1),EXTMODE='LINEAR')
            })

 
R> #simulate model
R> kleinModel <- SIMULATE(kleinModel
            ,simType='FORECAST'
            ,TSRANGE=c(1941,1,1944,1)
            ,simConvergence=0.00001
            ,simIterLimit=100
            )
  
Simulation: 100.00 \%
...SIMULATE OK

R> #get forecasted GNP
R> TABIT(kleinModel$simulation$y)

Date, Prd., kleinModel$simulation$y
1941, 1   ,  95.41613      
1942, 1   ,  106.8923      
1943, 1   ,  107.4302      
1944, 1   ,  100.7512     
}

\figure{KleinGNP.png} \cr \cr


Below is an example of advanced simulation using the \code{NEWTON} algorithm:

\preformatted{

R> #DYNAMIC NEWTON SIMULATION EXAMPLE 
R> #WITH EXOGENIZATION AND CONSTANT ADJUSTMENTS
 
R> #define exogenization list
R> #'cn' exogenized in 1923-1925
R> #'i' exogenized in whole TSRANGE
R> exogenizeList <- list(
                  cn = c(1923,1,1925,1),
                  i  = TRUE
                  )
 
R> #define add-factor list
R> #cn add-factor will be 1 in 1923 and -1 in 1924
R> #y add-factor will be 0.1 in 1926, -0.1 in 1927 and -0.5 in 1928
R> constantAdjList <- list(
                 cn = TIMESERIES(1,-1,START=c(1923,1),FREQ='A'),
                 y  = TIMESERIES(0.1,-0.1,-0.5,START=c(1926,1),FREQ='A')
                 )
 
R> #simulate model
R> kleinModel <- SIMULATE(kleinModel
                  ,simAlgo='NEWTON'
                  ,simType='DYNAMIC'
                  ,TSRANGE=c(1923,1,1941,1)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
                  ,Exogenize=exogenizeList
                  ,ConstantAdjustment=constantAdjList 
                  )
}

\tabular{c}{\emph{Stochastic Simulation}}
Forecasts produced by structural econometric models are subject to several sources of error, such as random disturbance term of each stochastic equation, errors in estimated coefficients, errors in forecasts of exogenous variables, errors in preliminary data and mis-specification of the model.\cr\cr
The forecast error depending on the structural disturbances can be analyzed by using the stochastic simulation procedure.\cr\cr
The deterministic simulation is the simultaneous solution of an econometric model obtained by applying, for each stochastic (behavioral) equation, the expected values of the structural disturbances, which are all zero by assumption. In the BIMETS \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}} stochastic simulation, the structural disturbances are given values that have specified stochastic properties. The error terms of the estimated behavioral equation of the model are appropriately perturbed. Identity equations and exogenous variables can be as well perturbed by disturbances that have specified stochastic properties. The model is then solved for each data set with different values of the disturbances. Finally, mean and standard deviation are computed for each simulated endogenous variable. \cr\cr
In terms of computational efficiency, the procedure takes advantage of the fact that multiple datasets are bound together in matrices. Therefore, to achieve a global convergence, the iterative simulation algorithm is executed once for all perturbed datasets. This solution can be viewed as a sort of a SIMD (i.e. Single Instruction Multiple Data) parallel simulation: \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}} function transforms time series into matrices; consequently, the procedure can easily bind multiple datasets by column. At the same time, a single run ensures a fast code execution. Finally, each column in the output matrices represents a stochastic realization.\cr\cr
By using the \code{StochStructure} argument of this function, users can define a stochastic structure for the disturbances. For each variable of the model, users can provide a distinct probability distribution for the disturbance, and a specific time range of application. Mean and standard deviation for each simulated endogenous time series will be stored in the \code{stochastic_simulation} element of the output model object; all the stochastic realizations will be stored in the \code{simulation_MM} element of the output model object as named matrices.

In the following figure, the advanced Klein model (see code example), has been perturbed during the forecast operation by applying a normal disturbance to the endogenous \emph{Consumption} behavioral \code{cn} add-factor in year 1942, and a uniform disturbance to the exogenous \emph{Government Expenditure} time series \code{g} along all the simulation \code{TSRANGE}. The normal disturbance applied to the \code{cn} behavioral has a zero mean and a standard deviation equal to the behavioral regression standard error,\cr i.e. \code{advancedKleinModel$behaviorals$cn$statistics$StandardErrorRegression}, thus roughly replicating the \code{\link[=ESTIMATE]{ESTIMATE}} regression error during the current perturbation (not accounting for inter-equations cross-covariance).\cr \cr 
\figure{StochKleinGNP.png} \cr \cr

\preformatted{

R> #we want to perform a stochastic forecast of the GNP up to 1944
R> #we will add normal disturbances to endogenous Consumption 'cn' 
R> #in 1942 by using its regression standard error
R> #we will add uniform disturbances to exogenous Government Expenditure 'g'
R> #in whole TSRANGE
R> nSD=advancedKleinModel$behaviorals$cn$statistics$StandardErrorRegression
R> myStochStructure <- list(
  cn=list(
        TSRANGE=c(1942,1,1942,1),
        TYPE='NORM',
        PARS=c(0,nSD)
        ),
  g=list(
        TSRANGE=TRUE,
        TYPE='UNIF',
        PARS=c(-1,1)
        )
  )

R> #we need to extend exogenous variables up to 1944
R> advancedKleinModel$modelData <- within(advancedKleinModel$modelData,{
    w2    = TSEXTEND(w2,  UPTO=c(1944,1),EXTMODE='CONSTANT')
    t     = TSEXTEND(t,   UPTO=c(1944,1),EXTMODE='LINEAR')
    g     = TSEXTEND(g,   UPTO=c(1944,1),EXTMODE='CONSTANT')
    k     = TSEXTEND(k,   UPTO=c(1944,1),EXTMODE='LINEAR')
    time  = TSEXTEND(time,UPTO=c(1944,1),EXTMODE='LINEAR')
  })

R> #stochastic model forecast
R> advancedKleinModel <- STOCHSIMULATE(advancedKleinModel
                      ,simType='FORECAST'
                      ,TSRANGE=c(1941,1,1944,1)
                      ,StochStructure=myStochStructure
                      ,StochSeed=123
                      )
                      
R> #print mean and standard deviation of forecasted GNP
R> with(advancedKleinModel$stochastic_simulation,TABIT(y$mean, y$sd))

      Date, Prd., y$mean         , y$sd           

      1941, 1   ,  125.5045      ,  4.250935      
      1942, 1   ,  173.2946      ,  9.2632        
      1943, 1   ,  185.9602      ,  11.87774      
      1944, 1   ,  141.0807      ,  11.6973  

R> #print the unperturbed forecasted GNP along with the
R> #first 5 perturbed realizations
R> with(advancedKleinModel$simulation_MM,print(y[,1:6]))


}

\tabular{c}{\emph{Multipliers Analysis}}
The BIMETS \code{\link[=MULTMATRIX]{MULTMATRIX}} function computes the matrix of both impact and interim multipliers, for a selected set of endogenous variables (i.e. \code{TARGET}) with respect to a selected set of exogenous variables (i.e. \code{INSTRUMENT}), by subtracting the results from different simulations in each period of the provided time range (i.e. \code{TSRANGE}). The simulation algorithms are the same as those used for the \code{\link[=SIMULATE]{SIMULATE}} operation. \cr\cr
The \code{MULTMATRIX} procedure is articulated as follows:\cr\cr
1. simultaneous simulations are done;\cr\cr
2. the first simulation establishes the base line solution (without shocks);\cr\cr
3. the other simulations are done with shocks applied to each of the \code{INSTRUMENT} one at a time for every period in \code{TSRANGE};\cr\cr
4. each simulation follows the defaults described in the \code{\link[=SIMULATE]{SIMULATE}} help page, but has to be \code{STATIC} for the IMPACT multipliers and \code{DYNAMIC} for INTERIM multipliers;\cr\cr
5. given \code{MM_SHOCK} shock amount as a very small positive number, derivatives are computed by subtracting the base line solution of the \code{TARGET} from the shocked solution, then dividing by the value of the base line \code{INSTRUMENT} times the \code{MM_SHOCK};\cr\cr\cr
BIMETS users can also declare an endogenous variable as the \code{INSTRUMENT} variable. In this case, the constant adjustment (see \code{\link[=SIMULATE]{SIMULATE}}) related to the provided endogenous variable will be used as the \code{INSTRUMENT} exogenous variable. \cr\cr
Back to our Klein's model example, we can calculate impact multipliers of \emph{Government Expenditure} "\code{g}" and \emph{Government Wage Bill} "\code{w2}" with respect of \emph{Consumption} "\code{cn}" and \emph{Gross National Product} "\code{y}" in the year 1941 by using the previously estimated model: 
\preformatted{
R> kleinModel <- MULTMATRIX(kleinModel,
                        symType='STATIC',
                        TSRANGE=c(1941,1,1941,1),
                        INSTRUMENT=c('w2','g'),
                        TARGET=c('cn','y')
                        )
Multiplier Matrix: 100.00 \%
...MULTMATRIX OK

R> kleinModel$MultiplierMatrix

           w2_1        g_1
cn_1  0.4540346   1.671956
y_1   0.2532000   3.653260
}
Results show that the impact multiplier of "\code{y}" with respect to "\code{g}" is +3.65. If we change the \emph{Government Expenditure} "\code{g}" value in 1941 from 22.3 (his historical value) to 23.3 (+1), then the simulated \emph{Gross National Product} "\code{y}" in 1941 changes from 95.2 to 99, thusly roughly confirming the +3.65 impact multiplier. Note that "\code{g}" only appears once in the model definition, and only in the "\code{y}" equation, with a coefficient equal to one (Keynes would approve).\cr\cr
An interim-multiplier example follows: 
\preformatted{
R> #multi-period interim multipliers
R> kleinModel <- MULTMATRIX(kleinModel,
                   TSRANGE=c(1940,1,1941,1),
                   INSTRUMENT=c('w2','g'),
                   TARGET=c('cn','y'))
Multiplier Matrix: 100.00 \%
...MULTMATRIX OK

R> #output multipliers matrix (note the zeros where the period
R> #of the INSTRUMENT is greater than the period of the TARGET)
R> kleinModel$MultiplierMatrix

            w2_1       g_1       w2_2      g_2
cn_1   0.4478202  1.582292  0.0000000 0.000000
y_1    0.2433382  3.510971  0.0000000 0.000000
cn_2  -0.3911001  1.785042  0.4540346 1.671956
y_2   -0.6251177  2.843960  0.2532000 3.653260
}


\tabular{c}{\emph{Endogenous Targeting}}

The endogenous targeting of econometric models consists of solving the model while interchanging the role of one or more endogenous variables with an equal number of exogenous variables.\cr\cr
The BIMETS \code{\link[=RENORM]{RENORM}}  procedure determines the values for the \code{INSTRUMENT} exogenous variables that allow the objective \code{TARGET} endogenous values to be achieved, with respect to the constraints given by the model equations (see \code{\link[=MDL]{MDL}}).\cr\cr
This is an approach to economic and monetary policy analysis, and is based on two assumptions:\cr\cr
1. there exists a desired level for a set of the \code{n} endogenous variables defined as \code{TARGET};\cr
2. there exists a set of the \code{n} exogenous variables defined as \code{INSTRUMENT};\cr\cr
Given these premises, the endogenous targeting process consists in determining the values of the exogenous variables chosen as \code{INSTRUMENT} allowing us to achieve the desired values for the endogenous variables designated as \code{TARGET}. In other words the procedure allows users to exchange the role of exogenous and endogenous among a set of variables pairs. \cr\cr
Given a list of exogenous \code{INSTRUMENT} variables and a list of \code{TARGET} endogenous time series, the iterative procedure can be split into the following steps:\cr\cr
1. Computation of the multipliers matrix \code{MULTMAT} of the \code{TARGET} endogenous variables with respect to the \code{INSTRUMENT} exogenous variables (this is a square matrix by construction);\cr\cr
2. Solution of the linear system (if any): 

\eqn{V_{exog}(i+1) = V_{exog}(i) +} \code{MULTMAT} \eqn{^{-1} * (V_{endog}(i) -} \code{TARGET} \eqn{)}, where \eqn{V_{exog}(i)} are the exogenous variables in the \code{INSTRUMENT} list and \eqn{V_{endog}(i)} are the endogenous variables that have a related target in the \code{TARGET} list, given \code{i} the current iteration;\cr\cr
3. Simulation of the model with the new set of exogenous variables computed in step 2, then a convergence check by comparing the subset of endogenous variables arising from this simulation and the related time series in \code{TARGET} list. If the convergence condition is satisfied, or the maximum number of iterations is reached, the algorithm will stop, otherwise it will go back to step 1;\cr\cr
Users can also declare an endogenous variable as an \code{INSTRUMENT} variable. In this case, the constant adjustment (see \code{\link[=SIMULATE]{SIMULATE}}) related to the provided endogenous variable will be used as the instrument exogenous variable. This procedure is particularly suited for the automatic computation of the add-factors needed to fine tune the model into a baseline path and to improve the forecasting accuracy.\cr\cr
If the convergence condition is satisfied, the \code{RENORM} procedure will return the \code{INSTRUMENT} time series allowing us to achieve the desired values for endogenous variables designated as \code{TARGET}.\cr\cr
Back to our Klein's model example, we can perform the endogenous targeting of the previously estimated model. First of all, the targets must be defined:
\preformatted{

R> #we want an arbitrary value on Consumption of 66 in 1940 and 78 in 1941
R> #we want an arbitrary value on GNP of 77 in 1940 and 98 in 1941

R> kleinTargets  <-  list(
              cn = TIMESERIES(66,78,START=c(1940,1),FREQ=1),
              y  = TIMESERIES(77,98,START=c(1940,1),FREQ=1)
              )
}
Then, we can perform the model endogenous targeting by using the "\code{w2}" (\emph{Wage Bill of the Government Sector}) and the "\code{g}" (\emph{Government Expenditure}) exogenous variables as \code{INSTRUMENT}, in the years 1940 and 1941:
\preformatted{
R> kleinModel <- RENORM(kleinModel
                   ,INSTRUMENT = c('w2','g')
                   ,TARGET = kleinTargets
                   ,TSRANGE = c(1940,1,1941,1)
                   ,simIterLimit = 100
                    )
}
Once \code{RENORM} completes, the calculated values of exogenous \code{INSTRUMENT} allowing us to achieve the desired endogenous \code{TARGET} values are stored in the model:
\preformatted{
R> with(kleinModel,TABIT(modelData$w2,
                      renorm$INSTRUMENT$w2,
                      modelData$g,
                      renorm$INSTRUMENT$g,
                      TSRANGE=c(1940,1,1941,1)
                      )
          )
     
Date, Prd., modelData$w2, renorm$INSTRUMENT$w2, modelData$g, renorm$INSTRUMENT$g
1940,  1  , 8           , 7.413331            , 15.4       , 16.1069
1941,  1  , 8.5         , 9.3436              , 22.3       , 22.65985
     
}

So, if we want to achieve on "\code{cn}" (\emph{Consumption}) an arbitrary simulated value of 66 in 1940 and 78 in 1941, and if we want to achieve on "\code{y}" (\emph{GNP}) an arbitrary simulated value of 77 in 1940 and 98 in 1941, we need to change exogenous "\code{w2}" (\emph{Wage Bill of the Government Sector}) from 8 to 7.41 in 1940 and from 8.5 to 9.34 in 1941, and we need to change exogenous "\code{g}" (\emph{Government Expenditure}) from 15.4 to 16.1 in 1940 and from 22.3 to 22.66 in 1941. \cr\cr

                Let's verify:
\preformatted{
R> #create a new model
R> kleinRenorm <- kleinModel

R> #get instruments to be used
R> newInstruments <- kleinModel$renorm$INSTRUMENT

R> #change exogenous by using new instruments data
R> kleinRenorm$modelData <- within(kleinRenorm$modelData,
                 {
                   w2[[1940,1]]=newInstruments$w2[[1940,1]]
                   w2[[1941,1]]=newInstruments$w2[[1941,1]]
                   g[[1940,1]] =newInstruments$g[[1940,1]]
                   g[[1941,1]] =newInstruments$g[[1941,1]]
                 }
                )
R> #users can also replace last two commands with:
R> #kleinRenorm$modelData <- kleinRenorm$renorm$modelData

R> #simulate the new model
R> kleinRenorm <- SIMULATE(kleinRenorm
                      ,TSRANGE=c(1940,1,1941,1)
                      ,simConvergence=0.00001
                      ,simIterLimit=100
                      )
Simulation: 100.00 \%
...SIMULATE OK

R> #verify targets are achieved
R> with(kleinRenorm$simulation,
     TABIT(cn,y)
     )
     
Date, Prd., cn        , y
1940,  1  , 66.01116  , 77.01772
1941,  1  , 78.02538  , 98.04121


}

\tabular{c}{\emph{Optimal Control}}

An approach to policy evaluation is via a so-called "social welfare function". This approach relaxes the assumptions of the instruments-targets framework, i.e. the \code{\link[=RENORM]{RENORM}} procedure. Rather than assuming specific desired targets for some endogenous variables, it assumes the existence of a social welfare function determining a scalar measure of performance based on both endogenous and policy (exogenous) variables.
\cr\cr
The social welfare function can incorporate information about tradeoffs in objectives that are not allowed by the \code{\link[=RENORM]{RENORM}} instruments-targets approach.
\cr\cr
BIMETS supplies the \code{\link[=OPTIMIZE]{OPTIMIZE}} procedure in order to perform optimal control exercises on econometric models.
\cr\cr
The optimization consists of maximizing a social welfare function, i.e. the objective-function, depending on exogenous and (simulated) endogenous variables, subject to user constraints plus the constraints imposed by the econometric model equations. Users are allowed to define constraints and objective-functions of any degree, and are allowed to provide different constraints and objective-functions in different optimization time periods.
\cr\cr
The core of the \code{\link[=OPTIMIZE]{OPTIMIZE}} procedure is based on a Monte Carlo method that takes advantage of the \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}} procedure. Policy variables, i.e. \code{INSTRUMENT}, are uniformly perturbed in the range defined by the user-provided boundaries, then the \code{INSTRUMENT} values that i) verify the user-provided constraints and ii) maximize the objective-functions are selected and stored into the \code{optimize} element of the output BIMETS model.
\cr\cr

In the following figure, the scatter plot is populated with \code{2916} objective function stochastic realizations, computed by using the example code at the end of this section; the \code{210.58} local maximum  is highlighted\cr(i.e. \code{advancedKleinModel$optimize$optFunMax} in code example).\cr\cr In this example:\cr\cr 
i) The objective function definition is:\cr 
\eqn{f(y,cn,g) = (y-110)+(cn-90)*|cn-90|-\sqrt{g-20}} \cr
given \eqn{y} as the simulated \emph{Gross National Product}, \eqn{cn} as the simulated \emph{Consumption} and \eqn{g} as the exogenous \emph{Government Expenditure}: the basic idea is to maximize \emph{Consumption}, and secondarily the \emph{Gross National Product}, while reducing the \emph{Government Expenditure};\cr\cr 
ii) The \code{INSTRUMENT} variables are the \eqn{cn} \emph{Consumption} "booster" (i.e. the add-factor, not to be confused with the simulated \emph{Consumption} in the objective function) and the \eqn{g} \emph{Government Expenditure}, defined over the following domains: \eqn{ cn \in (-5,5)}, \eqn{g \in (15,25)};\cr\cr
iii) The following restrictions are applied to the \code{INSTRUMENT}: \eqn{g + cn^2/2 < 27 \wedge g + cn > 17}, given \eqn{cn} as the \emph{Consumption} "booster" (i.e. the add-factor) and \eqn{g} as the \emph{Government Expenditure};

\figure{OptKlein.png} \cr \cr
The figure clearly shows that non-linear restrictions have been applied, and that non-computable objective functions have been discarded, e.g. the stochastic realizations having \eqn{g<20} due to the square root operation in the objective function, given instrument \eqn{g \in (15,25)}.

An example of an optimal control exercise applied to the previoously defined \code{advancedKleinModel} follows:
\preformatted{

R> #reset time series of the model object that have been 
R> #modified in the stochastic simulation section
R> advancedKleinModel <- LOAD_MODEL_DATA(advancedKleinModel,kleinModelData)

R> #we want to maximize the non-linear objective function:
R> #f()=(y-110)+(cn-90)*ABS(cn-90)-(g-20)^0.5
R> #in 1942 by using INSTRUMENT cn in range (-5,5) 
R> #(cn is endogenous so we use the add-factor)
R> #and g in range (15,25)
R> #we will also impose the following non-linear restriction:
R> #g+(cn^2)/2<27 & g+cn>17

R> #we need to extend exogenous variables up to 1942
R> advancedKleinModel$modelData <- within(advancedKleinModel$modelData,{
    w2    = TSEXTEND(w2,  UPTO=c(1942,1),EXTMODE='CONSTANT')
    t     = TSEXTEND(t,   UPTO=c(1942,1),EXTMODE='LINEAR')
    g     = TSEXTEND(g,   UPTO=c(1942,1),EXTMODE='CONSTANT')
    k     = TSEXTEND(k,   UPTO=c(1942,1),EXTMODE='LINEAR')
    time  = TSEXTEND(time,UPTO=c(1942,1),EXTMODE='LINEAR')
})

R> #define INSTRUMENT and boundaries
R> myOptimizeBounds <- list(
    cn=list(TSRANGE=TRUE,
            BOUNDS=c(-5,5)),
    g=list(TSRANGE=TRUE,
           BOUNDS=c(15,25))
)

R> #define restrictions
R> myOptimizeRestrictions <- list(
    myRes1=list(
        TSRANGE=TRUE,
        INEQUALITY='g+(cn^2)/2<27 & g+cn>17')
)

R> #define objective function
R> myOptimizeFunctions <- list(
    myFun1=list(
        TSRANGE=TRUE,
        FUNCTION='(y-110)+(cn-90)*ABS(cn-90)-(g-20)^0.5')
)

R> #Monte-Carlo optimization by using 10000 stochastic realizations
R> #and 1E-4 convergence criterion 
R> advancedKleinModel <- OPTIMIZE(advancedKleinModel
                          ,simType = 'FORECAST'
                          ,TSRANGE=c(1942,1,1942,1)
                          ,simConvergence= 1E-4
                          ,simIterLimit  = 1000
                          ,StochReplica  = 10000
                          ,StochSeed = 123
                          ,OptimizeBounds = myOptimizeBounds
                          ,OptimizeRestrictions = myOptimizeRestrictions
                          ,OptimizeFunctions = myOptimizeFunctions)
OPTIMIZE(): optimization boundaries for the add-factor of endogenous 
            variable "cn" are (-5,5) from year-period 1942-1 to 1942-1.
OPTIMIZE(): optimization boundaries for the exogenous 
            variable "g" are (15,25) from year-period 1942-1 to 1942-1.
OPTIMIZE(): optimization restriction "myRes1" is active 
            from year-period 1942-1 to 1942-1.
OPTIMIZE(): optimization objective function "myFun1" is active 
            from year-period 1942-1 to 1942-1.

Optimize:     100.00 \%
OPTIMIZE(): 2916 out of 10000 objective function realizations (29\%) 
            are finite and verify the provided restrictions.
...OPTIMIZE OK

R> #print local maximum
R> advancedKleinModel$optimize$optFunMax
[1] 210.5755

R> #print INSTRUMENT that allow local maximum to be achieved
R> advancedKleinModel$optimize$INSTRUMENT
$cn
Time Series:
Start = 1942 
End = 1942 
Frequency = 1 
[1] 2.032203

$g
Time Series:
Start = 1942 
End = 1942 
Frequency = 1 
[1] 24.89773

R> #LET'S VERIFY RESULTS
R> #copy into modelData the computed INSTRUMENT 
R> #that allow to maximize the objective function 
R> advancedKleinModel$modelData <- advancedKleinModel$optimize$modelData

R> #simulate the model by using the new INSTRUMENT
R> #note: we used cn add-factor as OPTIMIZE instrument, so we need 
R> #to pass the computed cn add-factor to the SIMULATE call
R> newConstantAdjustment <- advancedKleinModel$optimize$ConstantAdjustment
R> advancedKleinModel <- SIMULATE(advancedKleinModel
                  ,simType = 'FORECAST'
                  ,TSRANGE = c(1942,1,1942,1)
                  ,simConvergence = 1E-5
                  ,simIterLimit = 1000
                  ,ConstantAdjustment = newConstantAdjustment)

R> #calculate objective function by using the SIMULATE output time series
R> #(y-110)+(cn-90)*ABS(cn-90)-(g-20)^0.5
R> y  <- advancedKleinModel$simulation$y
R> cn <- advancedKleinModel$simulation$cn
R> g  <- advancedKleinModel$modelData$g
R> optFunTest <- (y-110)+(cn-90)*abs(cn-90)-(g-20)^0.5

R> #verify computed max is equal to optimization max
R> #(in the following command TSPROJECT could be omitted because
R> #myFun1$TSRANGE = TRUE)
R> abs(sum(TSPROJECT(optFunTest
              ,TSRANGE=c(1942,1,1942,1)
              ,ARRAY = TRUE)
        ) - advancedKleinModel$optimize$optFunMax)  < 1E-4
[1] TRUE
}
}
\details{

\tabular{ll}{\cr
Package: \tab bimets - Time Series And Econometric Modeling In R\cr
Type: \tab Package\cr
License: \tab GPL-3\cr
} 


BIMETS estimation and simulation results have been compared to the output results of leading commercial econometric software by using several large and complex models.\cr\cr
The models used in the comparison have more than:\cr\cr
+100  behavioral equations;\cr
+700  technical identities;\cr
+500  coefficients;\cr
+1000 time series of endogenous and exogenous variables;\cr\cr
In these models, there are equations with restricted coefficients, polynomial distributed lags, error autocorrelation, and conditional evaluation of technical identities; all models have been simulated in \emph{static}, \emph{dynamic}, and \emph{forecast} mode, with exogenization and constant adjustments of endogenous variables through the use of BIMETS capabilities.\cr\cr
In the +800 endogenous simulated time series over the +20 simulated periods (i.e. more than 16.000 simulated observations), the average \emph{percentage} difference between BIMETS and leading commercial software results has a magnitude of \eqn{10^{-7} \% }{10^(-7) \%}. The difference between results calculated by using different commercial software has the same average magnitude.\cr\cr
BIMETS stands for Bank of Italy Model Easy Time Series; it does not depend on compilers or third-party software so it can be freely downloaded and installed on Linux, MS Windows(R) and Mac OSX(R), without any further requirements. \cr\cr
More details in:\cr\cr
- \code{\link[=MDL]{MDL}}\cr
- \code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
- \code{\link[=ESTIMATE]{ESTIMATE}}\cr
- \code{\link[=SIMULATE]{SIMULATE}}\cr
- \code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}\cr
- \code{\link[=MULTMATRIX]{MULTMATRIX}}\cr
- \code{\link[=RENORM]{RENORM}}\cr
- \code{\link[=OPTIMIZE]{OPTIMIZE}}\cr

Disclaimer: \emph{The views and opinions expressed in these pages are those of the authors and do not necessarily reflect the official policy or position of the Bank of Italy. Examples of analysis performed within these pages are only examples. They should not be utilized in real-world analytic products as they are based only on very limited and dated open source information. Assumptions made within the analysis are not reflective of the position of the Bank of Italy.}\cr
}
\author{ Andrea Luciani  <andrea.luciani@bancaditalia.it> \cr
Roberto Stok  <roberto.stok@bancaditalia.it> 
\cr \cr 
}

