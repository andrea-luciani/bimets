# bimets - Time Series And Econometric Modeling In R


**bimets** is an R package developed with the aim of easing time series analysis and building up a framework that facilitates the definition, estimation and simulation of simultaneous equation models.

**bimets** does not depend on compilers or third-party software so it can be freely downloaded and installed on Linux, MS Windows(R) and Mac OSX(R), without any further requirements. 

<!-- For an introduction and examples, visit the [bimets Dev Center](http://bimets.github.com/). 

For bug reports, please use the [issue tracker](https://github.com/bimets/issues
-->

If you have general questions about using **bimets**, or for bug reports, please write to the mantainer: [Andrea.Luciani@bancaditalia.it](mailto:andrea.luciani@bancaditalia.it)

## Features



**TIME SERIES:**

- supports daily, weekly, monthly, quarterly, semiannual, yearly time series, and frequency of 24 and 36 periods per year.
- indexing *by date* - users can select and modify a single observation by using the syntax `ts['Date']`, or multiple observations by using `ts['StartDate/EndDate']`.
- indexing *by year-period* - users can select and modify observations by providing a two-dimensional numerical array composed by the year and the period, e.g. `ts[[Year,Period]]`.
- indexing *by observation index* - users can select and modify observations by providing the array of requested indexes (core R), e.g. `ts[indexes]`.
- *Aggregation/Disaggregation* - the package provides advanced (dis)aggregation capabilities, with several linear interpolation capabilities in disaggregation, and many aggregation functions (e.g. `STOCK`, `SUM`, `AVE`, etc.) while reducing the time series frequency.
- *Manipulation* - the package provides, among others, the following time series manipulation capabilities: 
time series extension `TSEXTEND()`, 
time series merging `TSMERGE()`, 
time series projection `TSPROJECT()`, 
lag `TSLAG()`, 
lag differences absolute and percentage `TSDELTA()` `TSDELTAP()`, 
cumulative product `CUMPROD()`, 
cumulative sum `CUMSUM()`, 
moving average `MOVAVG()`, 
moving sum `MOVSUM()`, 
time series data presentation `TABIT()` 


Example:
```r
#create ts
myTS=TIMESERIES((1:100),START=c(2000,1),FREQ='D');
 
myTS[1:3];                    #get first three obs.
myTS['2000-01-12'];           #get Jan 12, 2000 data
myTS['2000-02-03/2000-03-04'] #get Feb 3 up to Mar 4
myTS[[2000,14]];              #get year 2000 period 14
myTS[[2032,1]];               #get year 2032 period 1 (out of range)
    
myTS['2000-01-15']=NA;        #assign to Jan 15, 2000
myTS[[2000,3]]=NA;            #assign to Jan 3, 2000
myTS[[2000,42]] = NA          #assign to Feb 11, 2000
myTS[[2000,100]]= c(-1,-2,-3);#assign array starting from 2000 					
                              #period 100 (i.e. extend series)
                              
#aggregation/disaggregation
myMonthlyTS=TIMESERIES(1:100,START=c(2000,1),FREQ='M');
myAnnualTS=ANNUAL(myMonthlyTS,'AVE');
myDailyTS=DAILY(myMonthlyTS,'INTERP_CENTER');

#manipulation
myTS1=TIMESERIES(1:100,START=c(2000,1),FREQ='M');
myTS2=TIMESERIES(-(1:100),START=c(2005,1),FREQ='M');
myExtendedTS=TSEXTEND(myTS1,UPTO = c(2020,4),EXTMODE = 'QUADRATIC');
myMergedTS=TSMERGE(myExtendedTS,myTS2,fun = 'SUM');
myProjectedTS=TSPROJECT(myMergedTS,TSRANGE = c(2004,2,2006,4));
myLagTS=TSLAG(myProjectedTS,2);
myDeltaPTS=TSDELTAP(myLagTS,2);
myMovAveTS=MOVAVG(myDeltaPTS,5);
TABIT(myMovAveTS,myTS1);

#     DATE, PER, myMovAveTS     , myTS1          
# 
# Jan 2000, 1  ,                ,  1             
# Feb 2000, 2  ,                ,  2             
# Mar 2000, 3  ,                ,  3             
# ...
# Sep 2004, 9  ,                ,  57            
# Oct 2004, 10 ,  3.849002      ,  58            
# Nov 2004, 11 ,  3.776275      ,  59            
# Dec 2004, 12 ,  3.706247      ,  60            
# Jan 2005, 1  ,  3.638771      ,  61            
# Feb 2005, 2  ,  3.573709      ,  62            
# Mar 2005, 3  ,  3.171951      ,  63            
# Apr 2005, 4  ,  2.444678      ,  64            
# May 2005, 5  ,  1.730393      ,  65            
# Jun 2005, 6  ,  1.028638      ,  66            
# Jul 2005, 7  ,  0.3389831     ,  67            
# Aug 2005, 8  ,  0             ,  68            
# Sep 2005, 9  ,  0             ,  69            
# Oct 2005, 10 ,  0             ,  70            
# ...
# Mar 2008, 3  ,                ,  99            
# Apr 2008, 4  ,                ,  100 

```



**MODELING:**

**bimets** econometric modeling capabilities comprehend: 

- *Model Definition Language* - the specification of an econometric model is translated and identified by keywords statements which are grouped in a model file, i.e. a plain text file or a `character` R variable with a specific syntax. Collectively, these keyword statements constitute a kind of a **bimets** Model Description Language (i.e. `MDL`). The MDL syntax allows to define behaviorals equations, technical equations, conditional evaluations during the simulation, and other model properties. 
- *Estimation* - the estimation function `ESTIMATE()` supports Ordinary Least Squares, Instrumental Variables, deterministic linear restrictions on the coefficients, Almon Polynomial Distributed Lags (i.e. `PDL`), autocorrelation of the errors, structural stability analysis (Chow tests).
- *Simulation* - the simulation function `SIMULATE()` supports static, dynamic and forecast simulations, residuals check, partial or total exogenization of endogenous variables, constant adjustment of endogenous variables (i.e. add-factors).
- *Multipliers Evaluation* - the multipliers evaluation function `MULTMATRIX()` computes the matrix of both impact and interim multipliers for a selected set of endogenous variables, i.e. the `TARGET`, with respect to a selected set of exogenous variables, i.e. the `INSTRUMENT`.
- *Renormalization* - the renormalization function `RENORM()` performs the renormalization of econometric models, which consists of solving the model while interchanging the role of one or more endogenous variables with an equal number of exogenous variables. The procedure determines the values for the `INSTRUMENT` exogenous variables which allow to achieve the desired values for the `TARGET` endogenous variables, subject to the constraints given by the equations of the model. This is an approach to economic and monetary policy analysis,

A Klein's model example, having restrictions, error autocorrelation and conditional evaluations, follows:

```r

#define the Klein model
klein1.txt="MODEL

COMMENT> Modified Klein Model 1 of the U.S. Economy with PDL, 
COMMENT> autocorrelation on errors, restrictions and conditional equation evaluations

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

#load the model
kleinModel=LOAD_MODEL(modelText = klein1.txt);

# Loading model: "klein1.txt"...
# Analyzing behaviorals...
# Analyzing identities...
# Optimizing...
# Loaded model "klein1.txt":
#     3 behaviorals
#     3 identities
#    12 coefficients
# ...LOAD MODEL OK

kleinModel$behaviorals$cn
# $eq
# [1] "cn=a1+a2*p+a3*TSLAG(p,1)+a4*(w1+w2)"
# 
# $eqCoefficientsNames
# [1] "a1" "a2" "a3" "a4"
# 
# $eqComponentsNames
# [1] "cn" "p"  "w1" "w2"
# 
# $tsrange
# [1] 1925    1 1941    1
# 
# $eqRegressorsNames
# [1] "1"         "p"        "TSLAG(p,1)" "(w1+w2)" 
# 
# $eqSimExp
# expression(cn[2,]=cn__ADDFACTOR[2,]+cn__a1+cn__a2*p[2,]+cn__a3*...
# 
# ...and more

kleinModel$incidence_matrix

#    cn i w1 y p k
# cn  0 0  1 0 1 0
# i   0 0  0 0 1 0
# w1  0 0  0 1 0 0
# y   1 1  0 0 0 0
# p   0 0  1 1 0 0
# k   0 1  0 0 0 0

#define data
kleinModelData=list(  
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

kleinModel=LOAD_MODEL_DATA(kleinModel,kleinModelData);
# Load model data "kleinModelData" into model "klein1.txt"...
# ...LOAD MODEL DATA OK
 
 
kleinModel=ESTIMATE(kleinModel)
#.CHECK_MODEL_DATA(): warning, there are undefined values in time series "time".
#
#Estimate the Model klein1.txt:
#the number of behavioral equations to be estimated is 3.
#The total number of coefficients is 13.
#
#_________________________________________
#
#BEHAVIORAL EQUATION: cn
#Estimation Technique: OLS
#Estimation Technique: OLS
#Autoregression of Order  2  (Cochrane-Orcutt procedure)
#
#Convergence was reached in  9  /  20  iterations.
#
#
#cn                  =   19.01352    
#                        T-stat. 12.13083    ***
#
#                    +   0.3442816   p
#                        T-stat. 3.533253    **
#
#                    +   0.03443117  TSLAG(p,1)
#                        T-stat. 0.3937881   
#
#                    +   0.6993905   (w1+w2)
##                        T-stat. 14.0808     ***
#
#ERROR:  AUTO(2) 
#
#AUTOREGRESSIVE PARAMETERS:
#Rho             Std. Error      T-stat.         
# 0.05743131      0.3324101       0.1727725       
# 0.007785936     0.2647013       0.02941404      
#
#
#STATs:
#R-Squared                      : 0.985263    
#Adjusted R-Squared             : 0.9785644   
#Durbin-Watson Statistic        : 1.966609    
#Sum of squares of residuals    : 9.273455    
#Standard Error of Regression   : 0.9181728   
#Log of the Likelihood Function : -18.97047   
#F-statistic                    : 147.0844    
#F-probability                  : 1.090551e-09
#Akaike's IC                    : 51.94093    
#Schwarz's IC                   : 57.77343    
#Mean of Dependent Variable     : 55.71765    
#Number of Observations         : 17
#Number of Degrees of Freedom   : 11
#Current Sample (year-period)   : 1925-1 / 1941-1
#
#
#Signif. codes:   *** 0.001  ** 0.01  * 0.05  
#
# 
# ...similar output for the all the regressions.

#simulate GDP in 1925-1930
kleinModel=SIMULATE(kleinModel, 
                      TSRANGE=c(1925,1,1930,1), 
                      simIterLimit = 100)

# Simulation:    100.00%
# ...SIMULATE OK

#print simulated gdp
TABIT(kleinModel$simulation$y)
#
#DATE, PER, kleinModel$simulation$y
#
#      1925, 1  ,  62.74953      
#      1926, 1  ,  56.46665      
#      1927, 1  ,  48.3741       
#      1928, 1  ,  55.58927      
#      1929, 1  ,  73.35799      
#      1930, 1  ,  74.93561  

#get multiplier matrix in 1941
kleinModel=MULTMATRIX(kleinModel,
                        TSRANGE=c(1941,1,1941,1),
                        INSTRUMENT=c('w2','g'),
                        TARGET=c('cn','y'),
                        simIterLimit = 100)

# Multipliter Matrix:    100.00%
# ...MULTMATRIX OK

kleinModel$MultiplierMatrix
#           w2_1      g_1
#cn_1 -0.1596758 2.853391
#y_1  -0.7216553 5.720007

#we want an arbitrary value on Consumption of 66 in 1940 and 78 in 1941
#we want an arbitrary value on GNP of 77 in 1940 and 98 in 1941
kleinTargets = list(
                    cn = TIMESERIES(66,78,START=c(1940,1),FREQ=1),
                    y  = TIMESERIES(77,98,START=c(1940,1),FREQ=1)
                    )

#renormalize the model              
kleinModel=RENORM(kleinModel
                   ,INSTRUMENT = c('w2','g')
                   ,TARGET = kleinTargets
                   ,TSRANGE = c(1940,1,1941,1)
                   ,simIterLimit = 100
 );

# Convergence reached in 3 iterations.
# ...RENORM OK

#The calculated values of exogenous INSTRUMENT 
#that allow to achieve the desired endogenous TARGET values
#are stored into the model:

with(kleinModel,TABIT(modelData$w2,
                      renorm$INSTRUMENT$w2,
                      modelData$g,
                      renorm$INSTRUMENT$g))

# DATE, PER, modelData$w2, renorm$w2, modelData$g, renorm$g
# 
#       ...
# 
#       1938, 1  ,        7.7,           ,         13,           
#       1939, 1  ,        7.8,           ,       14.4,           
#       1940, 1  ,          8,   8.857669,       15.4,    15.81276
#       1941, 1  ,        8.5,   12.18823,       22.3,    21.83899

#So, if we want to achieve on "cn" (Consumption) an arbitrary simulated value of 66 in 1940
#and 78 in 1941, and if we want to achieve on "y" (GNP) an arbitrary simulated value of 77
#in 1940 and 98 in 1941, we need to change exogenous "w2" (Wage Bill of the Government
#Sector) from 8 to 8.86 in 1940 and from 8.5 to 12.19 in 1941, and we need to change exogenous
#"g"(Government non-Wage Spending) from 15.4 to 15.81 in 1940 and from 22.3 to 21.84 in 1941.

#Let's verify:

#create a new model
kleinRenorm=kleinModel

#update the required INSTRUMENT
kleinRenorm$modelData=kleinRenorm$renorm$modelData

#simulate the new model
kleinRenorm=SIMULATE(kleinRenorm
                  ,TSRANGE=c(1940,1,1941,1)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
                  )
#Simulation:    100.00%
#...SIMULATE OK

#verify TARGETs are achieved
with(kleinRenorm$simulation,
    TABIT(cn,y)
    )
    
#    DATE, PER, cn             , y              
#
#    1940, 1  ,  66.02157      ,  77.03568      
#    1941, 1  ,  78.05216      ,  98.09119 

```

**bimets** estimation and simulation results have been compared to the output results of leading commercial econometric software, by using several large and complex models.

The models used in the comparison have more than:

- +100  behavioral equations;
- +700  technical identities;
- +500  coefficients;
- +1000 time series of endogenous and exogenous variables;

In these models we can find equations with restricted coefficients, polynomial distributed lags, error autocorrelation and conditional evaluation of technical identities; all models have been simulated in static, dynamic, and forecast mode, with exogenization and constant adjustments of endogenous variables, through the use of **bimets** capabilities.

In the +800 endogenous simulated time series over the +20 simulated periods (i.e. more than 16.000 simulated observations), the average percentage difference between **bimets** and leading commercial software results has a magnitude of `10E-7 %`. The difference between results calculated by using different commercial software has the same average magnitude.

## Installation

The package can be installed and loaded in R with the following commands:

```r
install.packages("bimets")
library(bimets)
```
<!--
To install the latest development builds directly from GitHub, run this instead:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("bimets")
```
-->

## Guidelines for contributing

We welcome contributions to the **bimets** package. In the case please write to the mantainer: [Andrea.Luciani@bancaditalia.it](mailto:andrea.luciani@bancaditalia.it). 

## License

The **bimets** package is licensed under the GPL-3

Disclaimer: *The views and opinions expressed in these pages are those of the authors and do not necessarily reflect the official policy or position of the Bank of Italy. Examples of analysis performed within these pages are only examples. They should not be utilized in real-world analytic products as they are based only on very limited and dated open source information. Assumptions made within the analysis are not reflective of the position of the Bank of Italy.*
