\name{bimets-package}

\alias{bimets}

\docType{package}
\title{
bimets :: Time Series And Econometric Modeling In R
}
\description{

\if{html}{(Note: this is the html version of the reference manual. Please consider reading the pdf version of this reference manual, wherein there are figures and the mathematical expressions are better formatted than in html.)\cr\cr}

\bold{bimets} is a software framework developed by using \code{R} language and designed for time series analysis and econometric modeling, which allows creating and manipulating time series, specifying simultaneous equation models of any size by using a kind of high-level description language, and performing model estimation, simulation and forecasting.\cr\cr
In addition, \bold{bimets} computational capabilities provide many tools to pre-process data and post-process results, designed for statisticians and economists. These operations are fully integrated with the \code{R} environment.\cr \cr
The package can be installed and loaded in \code{R} with the following commands (with \code{R>} as the \code{R} command prompt):
\preformatted{
R> install.packages('bimets')
R> library(bimets)
}


\tabular{c}{\bold{TIME SERIES }  }



\bold{bimets} supports daily, weekly, monthly, quarterly, semiannual and yearly time series. Time series with a frequency of 24 and 36 periods per year are also supproted. The time series are created by the \code{\link[=TIMESERIES]{TIMESERIES}} function.\cr\cr
Example: 
\preformatted{
R> #yearly time series
R> myTS <- TIMESERIES(1:10,START=as.Date('2000-01-01'),FREQ=1)

R> #monthly time series
R> myTS <- TIMESERIES(1:10,START=c(2002,3),FREQ='M')
}

The main \bold{bimets} time series capabilities are:\cr\cr
- \emph{Indexing}\cr
- \emph{Aggregation / Disaggregation}\cr
- \emph{Manipulation}\cr

\tabular{c}{\emph{Time Series Indexing} }


The \bold{bimets} package extends R indexing capabilities in order to ease time series analysis and manipulation. Users can access and modify time series data:\cr\cr
- \emph{by date}:  users can select and modify a single observation by date by using the syntax \code{ts['Date']}, or multiple observations by using \code{ts['StartDate/EndDate']}; \cr\cr
- \emph{by year-period}: users can select and modify observations by providing the year and the period requested, i.e. \code{ts[[Year,Period]]};\cr\cr
- \emph{by observation index}: users can select and modify observations by simply providing the array of requested indexes, i.e. \code{ts[indexes]};\cr\cr
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
\tabular{c}{\emph{Time Series Aggregation/Disaggregation} }
The \bold{bimets} package provides advanced (dis)aggregation capabilities, with several linear interpolation capabilities in disaggregation, and many aggregation functions (e.g. \code{STOCK}, \code{SUM}, \code{AVE}, etc.) while reducing the time series frequency. \cr\cr
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


The \bold{bimets} package provides, among others, the following time series manipulation capabilities:\cr\cr
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
R> myExtendedTS <- TSEXTEND(myTS1,UPTO = c(2020,4),EXTMODE = 'QUADRATIC')

R> #merge two time series with sum
R> myMergedTS <- TSMERGE(myExtendedTS,myTS2,fun = 'SUM')

R> #project time series on arbitrary time range
R> myProjectedTS <- TSPROJECT(myMergedTS,TSRANGE = c(2004,2,2006,4))

R> #lag and delta% time series
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

\bold{bimets} econometric modeling capabilities comprehend:\cr\cr
- \emph{Model Definition Language} \cr
- \emph{Estimation} \cr
- \emph{Simulation} \cr
- \emph{Multipliers Analysis} \cr
- \emph{Endogenous Targeting } \cr\cr
We will go through each item of the list with a simple Klein model example (ref: \emph{"Economic Fluctuations in the United States 1921-1941"} by L. R. Klein, Wiley and Sons Inc., New York, 1950). \cr
\tabular{c}{\emph{Model Definition Language} }

\bold{bimets} provides a language to unambiguously specify an econometric model. This section describes how to create a model and its general structure. The specification of an econometric model is translated and identified by keyword statements which are grouped in a model file, i.e. a plain text file with a specific syntax. Collectively, these keyword statements constitute the \bold{bimets} Model Description Language (from now on \code{\link[=MDL]{MDL}}). The model specifications consist of groups of statements. Each statement begins with a keyword. The keyword classifies the component of the model which is beign specified.\cr\cr
Below is an example of Klein's model, that can either be stored in an \code{R} variable of class \code{character} or in a plain text file with a \code{MDL} compliant syntax. \cr\cr
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
- \code{cn} as private consumption expenditure;   \cr
- \code{i} as investment;    \cr
- \code{w1} as wage bill of the private sector (demand for labor);   \cr
- \code{p} as profits;  \cr
- \code{k} as stock of capital goods;   \cr
- \code{y} as gross national product;  \cr  
- \code{w2} as wage bill of the government sector;   \cr
- \code{time} as an index of the passage of time, e.g. 1931 = zero;   \cr
- \code{g} as government expenditure plus net exports;   \cr
- \code{t} as business taxes.   \cr \cr
\code{a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4} are coefficient to be estimated.  \cr \cr
\cr 
\if{latex}{
This system has only 6 equations, three of which must be fitted in order to assess the coefficients. It may not seem so difficult to solve this system, but the real complexity emerges if you look at the incidence graph in the following figure, wherein endogenous variables are plotted in blue and exogenous variables are plotted in pink.\cr \cr

\figure{KleinIG.png}{options: width=14cm} \cr \cr

Each edge states a simultaneous dependence from a variable to another, e.g. the \code{w1} equation depends on the current value of the \code{time} time series; complexity arises because in this model there are several circular dependencies, one of which is plotted in dark blue.  \cr \cr
A circular dependency in the incidence graph of a model implies that the model is a \emph{simultaneous} equations model and that it must be estimated by using ad-hoc procedures; moreover it can be simulated, i.e. performing a forecast, only by using an iterative algorithm.  
}
\if{html}{
Please note that there are circular dependencies between equations of the model, i.e. \code{p <- w1 <- y <- p}. Circular dependencies imply that the model simulation must be solved with an iterative algorithm.\cr\cr
} 
As shown in the code, the model definition is quite intuitive. The first keyword is \code{MODEL}, while at the end of the model definition we can find the \code{END} keyword. Available tags in the definition of a generic \bold{bimets} model are: 



- \bold{EQUATION>} or \bold{BEHAVIORAL>} indicate the beginning of a series of keyword statements describing a behavioral equation;\cr \cr
- \bold{IDENTITY>} indicates the beginning of a series of keyword statements describing an identity or technical equation;\cr \cr
- \bold{EQ>} specifies the mathematical expression for a behavioral equation or an identity equation;\cr \cr
- \bold{COEFF>} specifies the coefficient names used in the EQ> keyword statement of a behavioral equation;\cr \cr
- \bold{ERROR>} specifies an autoregressive process of a given order for the regression error;\cr \cr
- \bold{PDL>} defines an Almon polynomial distributed lag;\cr  \cr
- \bold{RESTRICT>} is a keyword that can be used to specify linear coefficient restrictions;\cr \cr
- \bold{IF>} is used to conditionally evaluate an identity during a simulation, depending on the value of a logical expression. Thus, it is possible to have a model alternating between two or more identity specifications for each simulation period, depending upon results from other equations;\cr \cr
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

More details are available in \code{\link[=MDL]{MDL}} and \code{\link[=LOAD_MODEL]{LOAD_MODEL}} help pages. \code{LOAD_MODEL()} is the \bold{bimets} function that reads an MDL model file and creates an equivalent R data structure.\cr \cr
Back to Klein's model example, the \bold{bimets} \code{\link[=LOAD_MODEL]{LOAD_MODEL}} function reads the \emph{klein1.txt} model as previously defined:
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

As shown in the output, \bold{bimets} counted 3 behavioral equations, 3 identities and 12 coefficients. Now in the \code{R} session there is a variable named \emph{kleinModel} that contains the model structure defined in the \emph{klein1.txt} variable. From now on, the user can ask \bold{bimets} about any details of this model.\cr\cr
For example, to gather information on the "\code{cn}" Consumption behavioral equation: 
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

Users can always read (or carefully change) any model parameters. The \code{\link[=LOAD_MODEL]{LOAD_MODEL}} function parses behavioral and identity expressions of the \code{\link[=MDL]{MDL}} definition, but it also does an important optimization. Properly reordering the model equations is a key preparatory step in the later phase of the simulation, in order to guarantee performance and convergence, if any, with the aim of minimizing the number of feedback endogenous variables.\cr\cr
The \code{\link[=LOAD_MODEL]{LOAD_MODEL}} function builds the incidence matrix of the model, and uses this matrix to calculate the proper evaluation order of the model equations during the simulation.\cr\cr Back to the Klein's model example, the incidence matrix and the reordering of the equations are stored in the following variables:
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

While simulating the Klein's model, \bold{bimets} will iterate on the computation of, in order,\cr \code{w1 -> p -> cn -> i -> y} (the \code{vsim} variables), by looking for convergence on \code{y} (the \code{vfeed} variable, only one in this example) that is the feedback variable. If the convergence is achieved, it will calculate \code{k} (the \code{vpost} variable). The \code{vpre} array in this example is empty, that is no equation has to be evaluated before the iterative algorithm.\cr\cr
More details on the equations reordering are available in \code{\link[=MDL]{MDL}} and \code{\link[=LOAD_MODEL]{LOAD_MODEL}} help pages.\cr\cr
Once the model has been parsed, users needs to load the data of all the time series involved in the model, by using the \code{\link[=LOAD_MODEL_DATA]{LOAD_MODEL_DATA}} function. In the following example, the code defines a list of time series and loads this list into the Klein's model previously defined:
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
Since time series and other data (e.g. regressor coefficients, error coefficients, constant adjustments, targets, instruments, etc...) are stored into the model object, users can define multiple model objects - each with its own arbitrary data - in the same \code{R} session. \bold{bimets} makes it possible to estimate, simulate and compare results from different models with different data sets. Furthermore, users can easily save an estimated or a simulated model as a standard \code{R} variable, thus reloading it later, having all available data and time series stored into it, i.e. endogenous and exogenous time series, estimated coefficients, constant adjustments, simulation options, simulated time series, calculated instruments, targets, etc. (see also \code{\link[=RENORM]{RENORM}} and \code{\link[=SIMULATE]{SIMULATE}})\cr\cr
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
The \bold{bimets} \code{\link[=ESTIMATE]{ESTIMATE}} function estimates equations that are linear in the coefficients, as specified in the behavioral equations of the model object. Coefficients can be estimated for single equations or blocks of simultaneous equations. The estimation function supports:\cr\cr
- \emph{Ordinary Least Squares};\cr
- \emph{Instrumental Variables};\cr
- \emph{Deterministic linear restrictions on the coefficients};\cr
- \emph{Almon Polynomial Distributed Lags};\cr
- \emph{Autocorrelation of the errors};\cr
- \emph{Structural stability analysis (Chow tests)};\cr\cr
Restrictions procedure derives from the theory of Lagrange Multipliers, while the Cochrane-Orcutt method allows to account for residuals autocorrelation. \cr\cr
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
A similar output is shown for each estimated regression. Once the estimation is completed, coefficient values, residuals, statistics, etc. are stored into the model object.
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

Below is an example of a model estimation that presents coefficient restrictions, PDL, error autocorrelation and conditional equation evaluations:
\preformatted{

R> #define model
R> advancedKlein1.txt <- 
"MODEL

COMMENT> Modified Klein Model 1 of the U.S. Economy with PDL, 
COMMENT> autocorrelation on errors, restrictions and 
COMMETN> conditional equation evaluations

COMMENT> Consumption with autocorrelation on errors
BEHAVIORAL> cn
TSRANGE 1925 1 1941 1
EQ> cn =  a1 + a2*p + a3*TSLAG(p,1) + a4*(w1+w2) 
COEFF> a1 a2 a3 a4
ERROR> AUTO(2)

COMMENT> Investment with restrictions
BEHAVIORAL> i
TSRANGE 1923 1 1941 1
EQ> i = b1 + b2*p + b3*TSLAG(p,1) + b4*TSLAG(k,1)
COEFF> b1 b2 b3 b4
RESTRICT> b2 + b3 = 1

COMMENT> Demand for Labor with PDL
BEHAVIORAL> w1 
TSRANGE 1925 1 1941 1
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

.CHECK_MODEL_DATA(): warning, there are undefined values in time series "time".

Estimate the Model advancedKlein1.txt:
the number of behavioral equations to be estimated is 3.
The total number of coefficients is 13.

_________________________________________

BEHAVIORAL EQUATION: cn
Estimation Technique: OLS
Autoregression of Order 2 (Cochrane-Orcutt procedure)

Convergence was reached in  9  /  20  iterations.


cn                  =   19.01352    
                        T-stat. 12.13083    ***

                    +   0.3442816   p
                        T-stat. 3.533253    **

                    +   0.03443117  TSLAG(p,1)
                        T-stat. 0.3937881   

                    +   0.6993905   (w1+w2)
                        T-stat. 14.0808     ***

ERROR STRUCTURE:  AUTO(2) 

AUTOREGRESSIVE PARAMETERS:
Rho             Std. Error      T-stat.         
 0.05743131      0.3324101       0.1727725       
 0.007785936     0.2647013       0.02941404      


STATs:
R-Squared                      : 0.985263    
Adjusted R-Squared             : 0.9785644   
Durbin-Watson Statistic        : 1.966609    
Sum of squares of residuals    : 9.273455    
Standard Error of Regression   : 0.9181728   
Log of the Likelihood Function : -18.97047   
F-statistic                    : 147.0844    
F-probability                  : 1.090551e-09
Akaike's IC                    : 51.94093    
Schwarz's IC                   : 57.77343    
Mean of Dependent Variable     : 55.71765    
Number of Observations         : 17
Number of Degrees of Freedom   : 11
Current Sample (year-period)   : 1925-1 / 1941-1


Signif. codes:   *** 0.001  ** 0.01  * 0.05  



_________________________________________

BEHAVIORAL EQUATION: i
Estimation Technique: OLS

i                   =   2.868104    
                        T-stat. 0.3265098   

                    +   0.5787626   p
                        T-stat. 4.456542    ***

                    +   0.4212374   TSLAG(p,1)
                        T-stat. 3.243579    **

                    -   0.09160307  TSLAG(k,1)
                        T-stat. -2.11748    

RESTRICTIONS:
b2+b3=1

RESTRICTIONS F-TEST:
F-value            : 8.194478       
F-prob(1,15)       : 0.0118602      


STATs:
R-Squared                      : 0.8928283   
Adjusted R-Squared             : 0.8794319   
Durbin-Watson Statistic        : 1.173106    
Sum of squares of residuals    : 26.76483    
Standard Error of Regression   : 1.293368    
Log of the Likelihood Function : -30.215     
F-statistic                    : 66.64659    
F-probability                  : 1.740364e-08
Akaike's IC                    : 68.43001    
Schwarz's IC                   : 72.20776    
Mean of Dependent Variable     : 1.310526    
Number of Observations         : 19
Number of Degrees of Freedom   : 16
Current Sample (year-period)   : 1923-1 / 1941-1


Signif. codes:   *** 0.001  ** 0.01  * 0.05  



_________________________________________

BEHAVIORAL EQUATION: w1
Estimation Technique: OLS

w1                  =   1.103637    
                        T-stat. 0.7290738   

                    +   0.4358984   (y+t-w2)
                        T-stat. 11.35698    ***

                    +   c3          TSLAG(y+t-w2,1)
                        PDL

                    +   0.1363549   time
                        T-stat. 3.398964    **

PDL:
c3 1 2

Distributed Lag Coefficient: c3
Lag     Coeff.         Std. Error     T-stat.        
0        0.1212886      0.06620502     1.832015      
1        0.0354339      0.04657983     0.7607135     
SUM      0.1567225      0.04163457    


STATs:
R-Squared                      : 0.9891508   
Adjusted R-Squared             : 0.9855344   
Durbin-Watson Statistic        : 2.219659    
Sum of squares of residuals    : 6.3545      
Standard Error of Regression   : 0.7276962   
Log of the Likelihood Function : -15.75753   
F-statistic                    : 273.5171    
F-probability                  : 1.130929e-11
Akaike's IC                    : 43.51506    
Schwarz's IC                   : 48.51434    
Mean of Dependent Variable     : 37.69412    
Number of Observations         : 17
Number of Degrees of Freedom   : 12
Current Sample (year-period)   : 1925-1 / 1941-1


Signif. codes:   *** 0.001  ** 0.01  * 0.05  

...ESTIMATE OK
}

\tabular{c}{\emph{Structural Stability} }
One of the main purposes of the econometric modeling is its use for forecast and policy evaluation and, to this end, the stability of a behavioral equation parameters over time should be verified. In order to check for structural stability two different procedures, which can be derived from the so called Chow-tests, are applied.\cr \cr
Given a sample of observations (i.e. the base \code{TSRANGE}) and selecting an arbitrary forward extension (i.e. the extended \code{TSRANGE}) we can perform the same regression by using these two time ranges.\cr\cr
In general a stability analysis is carried on in the following ways:\cr
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


The simulation of an econometric model basically consists in solving the system of the equations describing the model for each time period in the specified time interval. Since the equations may also not be linear in the variables, and since the graph derived from the incidence matrix may be cyclic, the usual methods based on linear algebra are not applicable, and the simulation must be solved by using an iterative algorithm. \cr \cr
\bold{bimets} simulation capabilities support:\cr\cr
- \emph{Static simulations}: a static multiple equation simulation, in which the historical values for the lagged endogenous variables are used in the solutions of subsequent periods; \cr\cr
- \emph{Dynamic simulations}: a dynamic simulation, in which the simulated values for the lagged endogenous variables are used in the solutions of subsequent periods;\cr\cr
- \emph{Forecast simulations}: similar to dynamic simulation, but during the initialization of the iterative algorithm the starting values of endogenous variables in a period are set equal to the simulated values of the previous period. This allows the simulation of future endogenous observations, i.e. the forecast;  \cr\cr
- \emph{Residuals check}: a single period, single equation simulation; simulated time series in output are just the computation of the RHS (right-hand-side) of their equation, by using the historical values of the involved time series and by accounting for error autocorrelation and PDLs, if any;\cr\cr
- \emph{Partial or total exogenization of endogenous variables}: in the provided time interval (i.e. partial exog.) or in the whole simulation time range (i.e. total exog.), the values of the selected endogenous variables can be definitely set to their historical values, by excluding their equations from the iterative algorithm of simulation;\cr\cr
- \emph{Constant adjustment of endogenous variables (add-factors)}: adds another exogenous time series - the "constant adjustment" - in the equation of the selected endogenous variables;\cr\cr
- \emph{Backfill of simulated time series}: specifies the periods of historical data prior to the simulation time interval which are to be included in the simulation results;\cr\cr
More details are available in the \code{\link[=SIMULATE]{SIMULATE}} help page. \cr \cr

Back to Kelin's model example, let's forecast the GNP (i.e. the "\code{y}" endogenous variable, originally referred as "Net national income, measured in billions of 1934 dollars", pag. 141 in "Economic Fluctuations in the United States" by L. R. Klein, Wiley and Sons Inc., New York, 1950) up to 1943: 
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
  
Simulation: 100.00%
...SIMULATE OK

R> #get forecasted GNP
R> TABIT(kleinModel$simulation$y)

Date, Prd., kleinModel$simulation$y
1941, 1   ,  95.41613      
1942, 1   ,  106.8923      
1943, 1   ,  107.4302      
1944, 1   ,  100.7512     
}
\if{latex}{
\figure{KleinGNP.png}{options: width=14cm} \cr \cr
}

Below is an example of advanced simulation:

\preformatted{

R> #STATIC SIMULATION EXAMPLE WITH EXOGENIZATION AND CONSTANT ADJUSTMENTS
 
R> #define exogenization list
R> #'cn' exogenized in 1923-1925
R> #'i' exogenized in the whole TSRANGE
R> exogenizeList <- list(
                  cn = c(1923,1,1925,1),
                  i  = TRUE
                  )
 
R> #define add-factor list
R> constantAdjList <- list(
                 cn = TIMESERIES(1,-1,START=c(1923,1),FREQ='A'),
                 y  = TIMESERIES(0.1,-0.1,-0.5,START=c(1926,1),FREQ='A')
                 )
 
R> #simulate model
R> kleinModel <- SIMULATE(kleinModel
                  ,simType='STATIC'
                  ,TSRANGE=c(1923,1,1941,1)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
                  ,Exogenize=exogenizeList
                  ,ConstantAdjustment=constantAdjList 
                  )
}


\tabular{c}{\emph{Multipliers Analysis}}
The \bold{bimets} \code{\link[=MULTMATRIX]{MULTMATRIX}} function computes the matrix of both impact and interim multipliers, for a selected set of endogenous variables (i.e. \code{TARGET}) with respect to a selected set of exogenous variables (i.e. \code{INSTRUMENT}), by subtracting the results from different simulations in each period of the provided time range (i.e. \code{TSRANGE}). The simulation algorithms are the same as those used for the \code{\link[=SIMULATE]{SIMULATE}} operation. \cr\cr
The \code{MULTMATRIX()} procedure is articulated as follows:\cr\cr
1. simultaneous simulations are done;\cr\cr
2. the first simulation establishes the base line solution (without shocks);\cr\cr
3. the other simulations are done with shocks applied to each of the \code{INSTRUMENT} one at a time for every period in \code{TSRANGE};\cr\cr
4. each simulation follows the defaults described in the \emph{Simulation} section, but has to be \code{STATIC} for the IMPACT multipliers and \code{DYNAMIC} for INTERIM multipliers;\cr\cr
5. given the \code{MM_SHOCK} shock amount as a very small positive number, derivatives are computed by subtracting the base line solution of the \code{TARGET} from the shocked solution, then dividing by the value of the base line \code{INSTRUMENT} times the \code{MM_SHOCK};\cr\cr\cr
\bold{bimets} users can also declare an endogenous variable as the \code{INSTRUMENT} variable. In this case, the constant adjustment related to the provided endogenous variable will be used as the \code{INSTRUMENT} exogenous variable. \cr\cr
Back to our Klein's model example, we can calculate impact multipliers of Government non-Wage Spending "\code{g}" and Government Wage Bill "\code{w2}" with respect of Consumption "\code{cn}" and Gross National Product "\code{y}" in the year 1941 by using the previously estimated model: 
\preformatted{
R> kleinModel <- MULTMATRIX(kleinModel,
                        TSRANGE=c(1941,1,1941,1),
                        INSTRUMENT=c('w2','g'),
                        TARGET=c('cn','y')
                        )
Multiplier Matrix: 100.00%
...MULTMATRIX OK

R> kleinModel$MultiplierMatrix

           w2_1        g_1
cn_1  0.4540346   1.671956
y_1   0.2532000   3.653260
}
Results show that the impact multiplier of "\code{y}" with respect to "\code{g}" is +3.65. If we change the Government non-Wage Spending "\code{g}" value in 1941 from 22.3 (his historical value) to 23.3 (+1), then the simulated Gross National Product "\code{y}" in 1941 changes from 95.2 to 99, thusly roughly confirming the +3.65 impact multiplier. Note that "\code{g}" only appears once in the model definition, and only in the "\code{y}" equation, with a coefficient of one (Keynes would approve).\cr\cr
An interim-multiplier example follows: 
\preformatted{
R> #multi-period interim multipliers
R> kleinModel <- MULTMATRIX(kleinModel,
                   TSRANGE=c(1940,1,1941,1),
                   INSTRUMENT=c('w2','g'),
                   TARGET=c('cn','y'))
Multiplier Matrix: 100.00%
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
The \bold{bimets} \code{\link[=RENORM]{RENORM}}  procedure determines the values for the \code{INSTRUMENT} exogenous variables which allow the objective \code{TARGET} endogenous variables to be achieved, with respect to the constraints given by the model \code{MDL} definition.\cr\cr
This is an approach to economic and monetary policy analysis, and is based on two assumptions:\cr\cr
1. there exists a desired level for a set of \code{n} endogenous variables of the model defined as \code{TARGET};\cr
2. there exists a set of \code{n} exogenous variables defined as \code{INSTRUMENT};\cr\cr
Given these premises, the endogenous targeting process consists in determining the values of the exogenous variables chosen as \code{INSTRUMENT} allowing us to achieve the desired values for the endogenous variables designated as \code{TARGET}. In other words the procedure allows users to exchange the role of exogenous and endogenous among a set of variables pairs. \cr\cr
Given a list of exogenous \code{INSTRUMENT} variables and a list of \code{TARGET} endogenous time series, the iterative procedure can be split into the following steps:\cr\cr
1. Computation of the multipliers matrix \code{MULTMAT} of the \code{TARGET} endogenous variables with respect to the \code{INSTRUMENT} exogenous variables (this is a square matrix by construction);\cr\cr
2. Solution of the linear system (if any): 
\if{html}{
\code{V.Exog(i+1) = V.Exog(i) + MULTMAT^(-1) * (V.Endog(i) - TARGET )}, where \code{V.Exog(i)} are the exogenous variables in the \code{INSTRUMENT} list and \code{V.Endog(i)} are the endogenous variables that have a related target in the \code{TARGET} list, given \code{i} the current iteration}
\if{latex}{
\eqn{V_{exog}(i+1) = V_{exog}(i) +} \code{MULTMAT} \eqn{^{-1} * (V_{endog}(i) -} \code{TARGET} \eqn{)}, where \eqn{V_{exog}(i)} are the exogenous variables in the \code{INSTRUMENT} list and \eqn{V_{endog}(i)} are the endogenous variables that have a related target in the \code{TARGET} list, given \code{i} the current iteration}
;\cr\cr
3. Simulation of the model with the new set of exogenous variables computed in step 2, then a convergence check by comparing the subset of endogenous variables arising from this simulation and the related time series in \code{TARGET} list. If the convergence condition is satisfied, or the maximum number of iterations is reached, the algorithm will stop, otherwise it will go back to step 1;\cr\cr
Users can also declare an endogenous variable as an \code{INSTRUMENT} variable. In this case, the constant adjustment related to the provided endogenous variable will be used as the instrument exogenous variable. This procedure is particularly suited for the automatic computation of the add-factors needed to fine tune the model into a baseline path and to improve the forecasting accuracy.\cr\cr
If the convergence condition is satisfied, the \code{RENORM} procedure will return the requested \code{INSTRUMENT} time series allowing to achieve the desired values for endogenous variables designated as \code{TARGET}.\cr\cr
Back to our Klein's model example, we can perform the endogenous targeting of the previously estimated model. First of all, the targets must be defined:
\preformatted{

R> #we want an arbitrary value on Consumption of 66 in 1940 and 78 in 1941
R> #we want an arbitrary value on GNP of 77 in 1940 and 98 in 1941

R> kleinTargets  <-  list(
              cn = TIMESERIES(66,78,START=c(1940,1),FREQ=1),
              y  = TIMESERIES(77,98,START=c(1940,1),FREQ=1)
              )
}
Then, we can perform the model endogenous targeting by using the "\code{w2}" (Wage Bill of the Government Sector) and the "\code{g}" (Government non-Wage Spending) exogenous variables as \code{INSTRUMENT}, in the years 1940 and 1941:
\preformatted{
R> kleinModel <- RENORM(kleinModel
                   ,INSTRUMENT = c('w2','g')
                   ,TARGET = kleinTargets
                   ,TSRANGE = c(1940,1,1941,1)
                   ,simIterLimit = 100
                    )
}
Once \code{RENORM} completes, the calculated values of exogenous \code{INSTRUMENT} allowing us to achieve the desired endogenous \code{TARGET} values are stored into the model:
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

So, if we want to achieve on "\code{cn}" (Consumption) an arbitrary simulated value of 66 in 1940 and 78 in 1941, and if we want to achieve on "\code{y}" (GNP) an arbitrary simulated value of 77 in 1940 and 98 in 1941, we need to change exogenous "\code{w2}" (Wage Bill of the Government Sector) from 8 to 7.41 in 1940 and from 8.5 to 9.34 in 1941, and we need to change exogenous "\code{g}" (Government non-Wage Spending) from 15.4 to 16.1 in 1940 and from 22.3 to 22.66 in 1941. \cr\cr

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
Simulation: 100.00%
...SIMULATE OK

R> #verify targets are achieved
R> with(kleinRenorm$simulation,
     TABIT(cn,y)
     )
     
Date, Prd., cn        , y
1940,  1  , 66.01116  , 77.01772
1941,  1  , 78.02538  , 98.04121


}

\bold{bimets} estimation and simulation results have been compared to the output results of leading commercial econometric software, by using several large and complex models.\cr\cr
The models used in the comparison have more than:\cr\cr
+100  behavioral equations;\cr
+700  technical identities;\cr
+500  coefficients;\cr
+1000 time series of endogenous and exogenous variables;\cr\cr
In these models we can find equations with restricted coefficients, polynomial distributed lags, error autocorrelation and conditional evaluation of technical identities; all models have been simulated in \emph{static}, \emph{dynamic}, and \emph{forecast} mode, with exogenization and constant adjustments of endogenous variables through the use of \bold{bimets} capabilities.\cr\cr
In the +800 endogenous simulated time series over the +20 simulated periods (i.e. more than 16.000 simulated observations), the average \emph{percentage} difference between \bold{bimets} and leading commercial software results has a magnitude of \eqn{10^{-7} \% }{10^(-7) \%}. The difference between results calculated by using different commercial software has the same average magnitude.\cr\cr

\bold{bimets} stands for Bank of Italy Model Easy Time Series; it does not depend on compilers or third-party software so it can be freely downloaded and installed on Linux, MS Windows(R) and Mac OSX(R), without any further requirements. \cr\cr

More details in:\cr\cr
- \code{\link[=MDL]{MDL}}\cr
- \code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
- \code{\link[=ESTIMATE]{ESTIMATE}}\cr
- \code{\link[=SIMULATE]{SIMULATE}}\cr
- \code{\link[=MULTMATRIX]{MULTMATRIX}}\cr
- \code{\link[=RENORM]{RENORM}}\cr


}
\details{
\tabular{ll}{
Package: \tab bimets - Time Series And Econometric Modeling In R\cr
Type: \tab Package\cr
License: \tab GPL-3\cr
} 
Disclaimer: \emph{The views and opinions expressed in these pages are those of the authors and do not necessarily reflect the official policy or position of the Bank of Italy. Examples of analysis performed within these pages are only examples. They should not be utilized in real-world analytic products as they are based only on very limited and dated open source information. Assumptions made within the analysis are not reflective of the position of the Bank of Italy.}\cr
}
\author{ Andrea Luciani  <andrea.luciani@bancaditalia.it> \cr
Roberto Stok  <roberto.stok@bancaditalia.it> 
\cr \cr 
}

