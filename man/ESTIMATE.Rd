\name{ESTIMATE}
\alias{ESTIMATE}


\title{
Estimate a BIMETS model object
}
\description{
\if{html}{(Note: this is the html version of the reference manual. Please consider reading the pdf version of this reference manual, wherein there are figures and the mathematical expressions are better formatted than in html.)\cr\cr}
This function estimates equations that are linear in the coefficients, as specified in the behavioral equations of the input model object. Coefficients can be estimated for single equations or blocks of simultaneous equations. Coefficients restriction procedure derives from the theory of Lagrange Multipliers, while the Cochrane-Orcutt method allows to account for residuals autocorrelation.\cr\cr The estimation function supports:\cr\cr
- \emph{Ordinary Least Squares};\cr
- \emph{Instrumental Variables};\cr
- \emph{Deterministic linear restrictions on the coefficients};\cr
- \emph{Almon Polynomial Distributed Lags};\cr
- \emph{Autocorrelation of the errors};\cr
- \emph{Structural stability analysis (Chow tests)};\cr\cr Further details on estimation calculus can be found in \code{\link[=MDL]{MDL}} help page; further details on Chow test can be found below in this section;


}
\usage{
ESTIMATE(model=NULL, 
         eqList=NULL, 
         TSRANGE=NULL,
         forceTSRANGE=FALSE,
         estTech='OLS',
         IV=NULL,
         quietly=FALSE,
         showWarnings=FALSE,
         tol=.Machine$double.eps,
         digits=getOption('digits'),
         centerCOV=TRUE,
         CHOWTEST=FALSE,
         CHOWPAR=NULL,
         avoidCompliance=FALSE,
         ...)

}

\arguments{
  \item{model}{
  The BIMETS model object to be estimated (see also \code{\link[=LOAD_MODEL]{LOAD_MODEL}}).
  }
  \item{eqList}{
  The \code{character} array of behavioral names to be estimated. If it is \code{NULL} then all the behaviorals of the model will be estimated.
  }
  \item{TSRANGE}{
  The time range of the estimation, as a four dimensional numerical array, \cr i.e. \code{TSRANGE=c(start_year, start_period, end_year, end_period)}. The \code{TSRANGE} provided in the behavioral \code{\link[=MDL]{MDL}} definition takes precedence over this function argument.
  }
   \item{forceTSRANGE}{
  If \code{TRUE}, the \code{TSRANGE} defined in the previous argument takes precedence over the \code{TSRANGE} provided in the behavioral \code{\link[=MDL]{MDL}} definition.
  }
   \item{estTech}{
   The estimation technique used in the regression. Ordinary Least Squares \code{OLS} and Instrumental Variables estimation \code{IV} are supported.
   }
    \item{IV}{
   The \code{character} array built with the Instrumental Variable expressions, in the case of Instrumental Variables estimation (see examples).
   }
   \item{quietly}{
   If \code{TRUE}, information messages will be suppressed, e.g. results and regression statistics.
   }
  \item{showWarnings}{
  If \code{TRUE} and if a model time series has a missing value then warning message will be shown.
  }
  \item{tol}{
  The tolerance for detecting linear dependencies in the columns of a matrix while an inversion is requested. The default is \code{.Machine$double.eps}.
  }
  \item{digits}{
  Controls the number of digits to print when printing coefficients and statistics of the estimation. Valid values are 1 to 22 with a default of 7.
  }
  \item{centerCOV}{
  If \code{TRUE}, the function subtracts the mean from the residuals before calculating the residual covariance matrix.
  }
  \item{CHOWTEST}{
  If \code{TRUE}, the structural stability analysis will be performed.
  }
  \item{CHOWPAR}{
  Indicates the last year-period where the stability test is performed. If \code{NULL} it will be automatically calculated by using all available time series data. It must be provided as an integer array, e.g. \code{c(YEAR,PERIOD)}.
  }
    \item{avoidCompliance}{
  If \code{TRUE}, compliance control check of model time series will be skipped. See \code{\link[=is.bimets]{is.bimets}}
  }

   \item{...}{
Backward compatibility.
}
  
  }

\value{
If \code{outputText=TRUE}, for each behavioral in the \code{eqList} this function will print out:\cr\cr
- the name of the estimated behavioral;\cr
- the estimation technique used;\cr
- the autocorrelation order of the error, if any, and the iterations count required to achieve the convergence;\cr
- the estimated equation with calculated coefficients and regressor expression; for each coefficient the T-statistic and the significance will be printed out;\cr
- the restriction equations imposed on the coefficients, if any;\cr
- the F-test for the restrictions, including the PDL restrictions, if any;\cr
- the final autocorrelation parameters for the error, along with their standard error, the T-statistic and the significance;\cr
- the \emph{R-Squared} and the \emph{Adjusted R-Squared};\cr
- the \emph{Durbin-Watson Statistic};\cr
- the \emph{Sum of squares of residuals};\cr
- the \emph{Standard Error of Regression};\cr
- the \emph{Log of the Likelihood Function};\cr
- the \emph{F-statistic} and the \emph{F-probability};\cr
- the \emph{AIC} and the \emph{BIC};\cr
- the \emph{Mean of the Dependent Variable};\cr
- the \emph{Number of Observations};\cr
- the \emph{Number of Degrees of Freedom};\cr
- the \emph{Current Sample}, i.e. the \code{TSRANGE} of estimation;\cr

For each behavioral in the \code{eqList} this function will add 4 new named elements into the related behavioral of the output model object:\cr\cr
1) \code{coefficients}: a numerical array built with the estimated coefficients;\cr
2) \code{errorCoefficients}: a numerical array built with the estimated coefficient for the error autoregression, if the \code{ERROR>} structure has been provided in the model \code{\link[=MDL]{MDL}} definition;\cr
3) \code{residuals}: the time series of the regression residuals. If an \code{ERROR>} structure has been provided in the behavioral definition, the related residuals will be calculated as described in the Cochrane-Orcutt procedure (see \code{\link[=MDL]{MDL}}).\cr
3) \code{residuals_no_error_correction}:  if an \code{ERROR>} structure has been provided in the behavioral definition, the residuals caclucated by using the original dependant and independent varibles are stored into this list element.\cr
4) \code{statistics}: a list built with the parameters and the statistics of the estimation, e.g.:\cr
- \code{TSRANGE}: TSRANGE requested in the latest estimation of the behavioral;\cr
- \code{estimationTechinque}: estimation technique requested in the latest estimation of the behavioral;\cr
- \code{CoeffCovariance}: coefficients covariance;\cr
- \code{StandardErrorRegression} and \code{StandardErrorRegressionNotCentered}: standard error of the regression (centered and not-centered);\cr
- \code{CoeffTstatistic}: T-statistic of the coefficients;\cr
- \code{RSquared}: R-Squared;\cr
- \code{AdjustedRSquared}: adjusted R-Squared;\cr
- \code{DegreesOfFreedom}: degrees of freedom of the regression;\cr
- \code{CoeffPvalues}: coefficients p-values;\cr
- \code{LogLikelihood}:  Log of the Likelihood Function;\cr
- \code{Fstatistics}: F-statistics;\cr
- \code{RhosTstatistics}: rhos T-statistic (if any);\cr
- \code{FtestRestrValue}: F-test value for the restrictions;\cr
- \code{FtestRestrProbability}: F-test probability for the restrictions;\cr
- \code{AIC}: Akaike's Information Criterion;\cr
- \code{BIC}: Schwarz's Information Criterion;\cr
- \code{matrixX}: the regressors matrix;\cr
- \code{vectorY}: the dependent variable;\cr
- \code{matrixX_error_corrected}: the regressors matrix arising from the Cochrane-Orcutt procedure;\cr
- etc.
\cr\cr

\emph{Structural Stability - Chow test}\cr\cr
One of the main purposes of the econometric modeling is its use for forecast and policy evaluation and, to this end, the stability of a behavioral equation parameters over time should be verified. In order to check for structural stability two different procedures, which can be derived from the so called Chow-tests, are applied.\cr
\if{html}{
Given a sample of \eqn{T_0 = t_k,...,t_n} observations (i.e. the base \code{TSRANGE}) and selecting an arbitrary forward extension in \eqn{T_1 = t_k,...,t_n,...,t_m} observations (i.e. the extended \code{TSRANGE}) in the general case we have the following two regressions: \cr\cr
1) \eqn{Y_0 = b_0*X_0+eps_0,  eps_0  in N(0,sigma_0^2) }, having time series projected on the base \code{TSRANGE}\cr
2) \eqn{Y_1 = b_1*X_1+eps_1,  eps_1  in N(0,sigma_1^2) }, having time series projected on the extended \code{TSRANGE}\cr
}
\if{latex}{
Given a sample of \eqn{T_{0} = t_{k},...,t_{n}} observations (i.e. the base \code{TSRANGE}) and selecting an arbitrary forward extension in \eqn{T_{1} = t_{k},...,t_{n},...,t_{m}} observations (i.e. the extended \code{TSRANGE}) in the general case we have the following two regressions: \cr\cr
1) \eqn{Y_{0} = \beta_{0}*X_{0}+\epsilon_{0},  \quad    \epsilon_{0} \sim \mathcal{N}(0,\,\sigma_{0}^{2}) }, having time series projected on the base \code{TSRANGE}\cr
2) \eqn{Y_{1} = \beta_{1}*X_{1}+\epsilon_{1},  \quad    \epsilon_{1} \sim \mathcal{N}(0,\,\sigma_{1}^{2}) }, having time series projected on the extended \code{TSRANGE}\cr
}
In general a stability analysis is carried on in the following ways:\cr
- comparing the parameter estimates arising from the two regressions: this is known as the covariance analysis;\cr
- checking the accuracy of the forecast for the dependent variable in the extended \code{TSRANGE}, using the estimates produced in the base \code{TSRANGE}: this is known as the predictive power test.\cr\cr
The first Chow test is calculated as:
\if{html}{
\eqn{tau = ( SSR_1-SSR_0 ) / SSR_0 * DoF_1 / ( DoF_1-DoF_0 ) }, \cr \cr with \eqn{SSR_i} as the sum of squared residuals and \eqn{DoF_i} as the number of degrees of fredoom in the regression \eqn{i=0,1}. \cr
}
\if{latex}{
\eqn{\tau = \frac{SSR_{1}-SSR_{0}}{SSR_{0}} \frac{DoF_{1}}{DoF_{1}-DoF_{0}} }, \cr \cr with \eqn{SSR_{i}} as the sum of squared residuals and \eqn{DoF_{i}} as the number of degrees of fredoom in the regression \eqn{i=0,1}. \cr
}

The test is completed by calculating the following time series on the extended \code{TSRANGE}:\cr
- the forecast error;\cr
- the standard error of forecast;\cr
- the t-statistic for the error;\cr\cr
The standard error of the forecast for the \eqn{t_j} observation in the extended \code{TSRANGE} is computed according to: \cr

\if{html}{
\eqn{SE_j = sigma_0 * sqrt(1+t(x_j) * ( t(X_0) * X_0)^(-1) * x_j) } \cr\cr having \eqn{x_j} as the independent values (i.e. regressors) on the \eqn{t_j} observation in the \eqn{T_1} extended \code{TSRANGE},  with \eqn{n < j <= m}. \cr\cr
The null hypothesis for \eqn{tau} is: \cr\cr
\eqn{ H* : beta_1 = beta_0}, given \eqn{sigma_1^2 = sigma_0^2} \cr\cr
The test statistic \eqn{tau} follows the \eqn{F} distribution with  \eqn{ ( DoF_1-DoF_0 ) } and \eqn{DoF_1} degrees of freedom, and can be performed during the \code{ESTIMATE()} function execution by using the \code{CHOWTEST} argument set to \code{TRUE}.
}
\if{latex}{
\eqn{SE_{j} = \sigma_{0} \sqrt{1+x_j^\top * ( X_{0}^\top * X_{0}^{ })^{-1} * x_j} } \cr\cr having \eqn{x_j} as the independent values (i.e. regressors) on the \eqn{t_{j}} observation in the \eqn{T_{1}} extended \code{TSRANGE},  with \eqn{n < j \leq m}. \cr\cr
The null hypothesis for \eqn{\tau} is: \cr\cr
\eqn{ H^{*} : \beta_{1} = \beta_{0}}, given \eqn{\sigma_{1}^{2} = \sigma_{0}^{2}} \cr\cr
The test statistic \eqn{\tau} follows the \eqn{F} distribution with  \eqn{ ( DoF_{1}-DoF_{0} ) } and \eqn{DoF_{1}} degrees of freedom, and can be performed during the \code{ESTIMATE()} function execution by using the \code{CHOWTEST} argument set to \code{TRUE}.

}

If \code{CHOWTEST} is \code{TRUE}, for each behavioral in the \code{eqList} the output model will have the following named element:\cr
- \code{ChowTest}: it contains the statistics and the time series computed during the last structural analysis performed on the related behavioral.\cr




}

\seealso{
\code{\link[=MDL]{MDL}}\cr
\code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
\code{\link[=SIMULATE]{SIMULATE}}\cr
\code{\link[=MULTMATRIX]{MULTMATRIX}}\cr
\code{\link[=RENORM]{RENORM}}\cr
\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
\code{\link[=summary.BIMETS_MODEL]{summary}}\cr
}

\examples{

#define model
myModelDefinition=
"MODEL

COMMENT> Modified Klein Model 1 of the U.S. Economy with PDL,
COMMENT> autocorrelation on errors, restrictions and conditional evaluations

COMMENT> Consumption
BEHAVIORAL> cn
TSRANGE 1925 1 1941 1
EQ> cn = a1 + a2*p + a3*TSLAG(p,1) + a4*(w1+w2)
COEFF> a1 a2 a3 a4
ERROR> AUTO(2)

COMMENT> Investment
BEHAVIORAL> i
TSRANGE 1923 1 1941 1
EQ> i = b1 + b2*p + b3*TSLAG(p,1) + b4*TSLAG(k,1)
COEFF> b1 b2 b3 b4
RESTRICT> b2 + b3 = 1

COMMENT> Demand for Labor
BEHAVIORAL> w1
TSRANGE 1925 1 1941 1
EQ> w1 = c1 + c2*(y+t-w2) + c3*TSLAG(y+t-w2,1) + c4*time
COEFF> c1 c2 c3 c4
PDL> c3 1 3

COMMENT> Gross National Product
IDENTITY> y
EQ> y = cn + i + g - t

COMMENT> Profits
IDENTITY> p
EQ> p = y - (w1+w2)

COMMENT> Capital Stock with switches
IDENTITY> k
EQ> k = TSLAG(k,1) + i
IF> i > 0
IDENTITY> k
EQ> k = TSLAG(k,1)
IF> i <= 0
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


#load model 
myModel=LOAD_MODEL(modelText=myModelDefinition)

#load data into the model
myModel=LOAD_MODEL_DATA(myModel,myModelData,showWarnings = TRUE)

#################################################
#OLS case

#estimate the model
myModel=ESTIMATE(myModel)

#HERE BELOW THE OUTPUT OF THE ESTIMATION (COMMENTED OUT):
#.CHECK_MODEL_DATA(): warning, there are undefined values in time series "time".
#
#Estimate the Model myModelDefinition:
#the number of behavioral equations to be estimated is 3.
#The total number of coefficients is 14.
#
#_________________________________________
#
#BEHAVIORAL EQUATION: cn
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
#                        T-stat. 14.0808     ***
#
#ERROR:  AUTO(2) 
#
#AUTOREGRESSIVE PARAMETERS:
#Rho             Std. Error      T-stat.         
 #0.05743131      0.3324101       0.1727725       
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
#
#_________________________________________
#
#BEHAVIORAL EQUATION: i
#Estimation Technique: OLS
#
#i                   =   2.868104    
#                        T-stat. 0.3265098   
#
#                    +   0.5787626   p
#                        T-stat. 4.456542    ***
#
#                    +   0.4212374   TSLAG(p,1)
#                        T-stat. 3.243579    **
#
#                    -   0.09160307  TSLAG(k,1)
#                        T-stat. -2.11748    
#
#RESTRICTIONS:
#b2+b3=1
#
#RESTRICTIONS F-TEST:
#F-value            : 8.194478       
#F-prob(1,15)       : 0.0118602      
#
#
#STATs:
#R-Squared                      : 0.8928283   
#Adjusted R-Squared             : 0.8794319   
#Durbin-Watson Statistic        : 1.173106    
#Sum of squares of residuals    : 26.76483    
#Standard Error of Regression   : 1.293368    
#Log of the Likelihood Function : -30.215     
#F-statistic                    : 66.64659    
#F-probability                  : 1.740364e-08
#Akaike's IC                    : 68.43001    
#Schwarz's IC                   : 72.20776    
#Mean of Dependent Variable     : 1.310526    
#Number of Observations         : 19
#Number of Degrees of Freedom   : 16
#Current Sample (year-period)   : 1923-1 / 1941-1
#
#
#Signif. codes:   *** 0.001  ** 0.01  * 0.05  
#
#
#
#_________________________________________
#
#BEHAVIORAL EQUATION: w1
#Estimation Technique: OLS
#
#w1                  =   1.12869     
#                        T-stat. 0.6479266   
#
#                    +   0.4398767   (y+t-w2)
#                        T-stat. 12.01268    ***
#
#                    +   c3          TSLAG(y+t-w2,1)
#                        PDL
#
#                    +   0.1368206   time
#                        T-stat. 3.373905    **
#
#PDL:
#c3 1 3
#
#Distributed Lag Coefficient: c3
#Lag     Coeff.         Std. Error     T-stat.        
#0        0.1076812      0.04283967     2.513586      *
#1        0.05074557     0.01291231     3.930015      **
#2       -0.00619005     0.03110492    -0.1990055     
#SUM      0.1522367      0.03873693    
#
#RESTRICTIONS F-TEST:
#F-value            : 0.06920179     
#F-prob(1,11)       : 0.7973647      
#
#
#STATs:
#R-Squared                      : 0.9890855   
#Adjusted R-Squared             : 0.9854474   
#Durbin-Watson Statistic        : 2.174168    
#Sum of squares of residuals    : 6.392707    
#Standard Error of Regression   : 0.7298805   
#Log of the Likelihood Function : -15.80848   
#F-statistic                    : 271.8645    
#F-probability                  : 1.172284e-11
#Akaike's IC                    : 43.61697    
#Schwarz's IC                   : 48.61625    
#Mean of Dependent Variable     : 37.69412    
#Number of Observations         : 17
#Number of Degrees of Freedom   : 12
#Current Sample (year-period)   : 1925-1 / 1941-1
#
#
#Signif. codes:   *** 0.001  ** 0.01  * 0.05  
#
#
#...ESTIMATE OK

#get residuals of 'cn'
myModel$behaviorals$cn$residuals
#Time Series:
#Start = 1925 
#End = 1941 
#Frequency = 1 
# [1] -0.88562504  0.25109884  0.66750111  ...
#[17] -1.41795908

#get residuals of 'i'
myModel$behaviorals$i$residuals
#Time Series:
#Start = 1923 
#End = 1941 
#Frequency = 1 
# [1]  1.464518775 -1.469763968  0.078674017  ...
#[16] -2.425079127 -0.698071507 -1.352967430 -1.724306054

#get estimation coefficients of 'cn' and 'w1'
myModel$behaviorals$cn$coefficients
#          [,1]
#a1 19.01352476
#a2  0.34428157
#a3  0.03443117
#a4  0.69939052

myModel$behaviorals$cn$errorCoefficients
#             [,1]
#RHO_1 0.057431312
#RHO_2 0.007785936

myModel$behaviorals$w1$coefficients
#                [,1]
#c1        1.12869024
#c2        0.43987666
#c3        0.10768118
#c3_PDL_1  0.05074557
#c3_PDL_2 -0.00619005
#c4        0.13682057


#################################################
#IV case

#estimation of Consumption "cn" with arbitrary IVs
#and error autocorrelation
myModel=ESTIMATE(myModel,
                 eqList = 'cn',
                 estTech = 'IV', 
                 IV=c('1',
                      'TSLAG(y)',
                      'TSLAG(w1)*pi+0.5',
                      'exp(w2)'))
#Estimate the Model myModelDefinition:
#the number of behavioral equations to be estimated is 1.
#The total number of coefficients is 4.
#
#_________________________________________
#
#BEHAVIORAL EQUATION: cn
#Estimation Technique: IV
#Autoregression of Order  2  (Cochrane-Orcutt procedure)
#
#Convergence was reached in  7  /  20  iterations.
#
#
#cn                  =   18.07073    
#                        T-stat. 11.72958    ***
#
#                    +   0.2530483   p
#                        T-stat. 1.583881    
#
#                    +   0.08631646  TSLAG(p,1)
#                        T-stat. 0.7556204   
#
#                    +   0.7363227   (w1+w2)
#                        T-stat. 13.11572    ***
#
#ERROR:  AUTO(2) 
#
#AUTOREGRESSIVE PARAMETERS:
#Rho             Std. Error      T-stat.         
#0.01559806      0.343195        0.04544955      
#-0.1196327       0.283432       -0.422086        
#
#
#STATs:
#R-Squared                      : 0.9843186   
#Adjusted R-Squared             : 0.9771907   
#Durbin-Watson Statistic        : 1.917329    
#Sum of squares of residuals    : 9.867739    
#Standard Error of Regression   : 0.9471363   
#Log of the Likelihood Function : -19.49844   
#F-statistic                    : 138.0938    
#F-probability                  : 1.532807e-09
#Akaike's IC                    : 52.99689    
#Schwarz's IC                   : 58.82938    
#Mean of Dependent Variable     : 55.71765    
#Number of Observations         : 17
#Number of Degrees of Freedom   : 11
#Current Sample (year-period)   : 1925-1 / 1941-1
#
#
#Signif. codes:   *** 0.001  ** 0.01  * 0.05  
#
#
#...ESTIMATE OK




#estimation of Investment "i" with arbitrary IVs
#and coefficient restrictions
myModel=ESTIMATE(myModel,
                 eqList = 'i',
                 estTech = 'IV', 
                 IV=c('1',
                      'TSLAG(w2)',
                      'TSLAG(w1)*pi+0.5',
                      'exp(w2)'))

#.CHECK_MODEL_DATA(): warning, there are undefined values in time series "time".
#
#Estimate the Model myModelDefinition:
#the number of behavioral equations to be estimated is 1.
#The total number of coefficients is 4.
#
#_________________________________________
#
#BEHAVIORAL EQUATION: i
#Estimation Technique: IV
#
#i                   =   34.517544    
#                        T-stat. 1.264388   
#
#                    +   0.3216326   p
#                        T-stat. 0.8648297   
#
#                    +   0.6783672   TSLAG(p,1)
#                        T-stat. 1.824043    
#
#                    -   0.2475568   TSLAG(k,1)
#                        T-stat. -1.842520   
#
#RESTRICTIONS:
#b2+b3=1
#
#RESTRICTIONS F-TEST:
#F-value            : 2.465920       
#F-prob(1,15)       : 0.137190
#
#
#STATs:
#R-Squared                      : 0.805773   
#Adjusted R-Squared             : 0.781494   
#Durbin-Watson Statistic        : 0.940534   
#Sum of squares of residuals    : 48.50580  
#Standard Error of Regression   : 1.741152    
#Log of the Likelihood Function : -35.86365   
#F-statistic                    : 33.18894    
#F-probability                  : 2.025229e-06
#Akaike's IC                    : 79.72731     
#Schwarz's IC                   : 83.50506    
#Mean of Dependent Variable     : 1.310526    
#Number of Observations         : 19
#Number of Degrees of Freedom   : 16
#Current Sample (year-period)   : 1923-1 / 1941-1
#
#
#Signif. codes:   *** 0.001  ** 0.01  * 0.05  
#
#
#...ESTIMATE OK

##############################################################
#CHOW TEST on w1

#base TSRANGE set to 1925 / 1935
myModel=ESTIMATE(myModel,
                  eqList='w1',
                  TSRANGE=c(1925,1,1935,1),
                  forceTSRANGE=TRUE,
                  CHOWTEST=TRUE)

#Estimate the Model myModelDefinition:
#the number of behavioral equations to be estimated is 1.
#The total number of coefficients is 6.
#
#_________________________________________
#
#BEHAVIORAL EQUATION: w1
#Estimation Technique: OLS
#
#w1                = -   4.48873    
#                        T-stat. -2.47402   *
#
#                    +   0.545102   (y+t-w2)
#                        T-stat. 15.3462    ***
#
#                    +   c3         TSLAG(y+t-w2,1)
#                        PDL
#
#                    +   0.292018   time
#                        T-stat. 5.58588    **
#
#PDL:
#c3 1 3
#
#Distributed Lag Coefficient: c3
#Lag     Coeff.        Std. Error    T-stat.       
#0        0.0413985     0.0336676     1.22963      
#1        0.0493551     0.00742323    6.64873      ***
#2        0.0573116     0.0265487     2.15873      
#SUM      0.148065      0.0222697    
#
#RESTRICTIONS F-TEST:
#F-value            : 3.35954       
#F-prob(1,5)        : 0.126295      
#
#
#STATs:
#R-Squared                      : 0.995931   
#Adjusted R-Squared             : 0.993219   
#Durbin-Watson Statistic        : 2.43313    
#Sum of squares of residuals    : 0.737093   
#Standard Error of Regression   : 0.350498   
#Log of the Likelihood Function : -0.742173  
#F-statistic                    : 367.183    
#F-probability                  : 2.68564e-07
#Akaike's IC                    : 13.4843    
#Schwarz's IC                   : 15.8717    
#Mean of Dependent Variable     : 34.9909    
#Number of Observations         : 11
#Number of Degrees of Freedom   : 6
#Current Sample (year-period)   : 1925-1 / 1935-1
#
#
#Signif. codes:   *** 0.001  ** 0.01  * 0.05  
#
#
#
#STABILITY ANALYSIS:
#Behavioral equation: w1 
#
#Chow test:
#Sample (auto)      : 1936-1 / 1941-1 
#F-value            : 15.3457       
#F-prob(6,12)       : 0.00534447    
#
#Predictive Power:
#
#DATE, PER, ACTUAL        , PREDICT       , ERROR         , STDERR        , TSTAT         
#
#1936, 1  ,  36.8         ,  38.439       , -1.63901      ,  0.547471     , -2.99378      
#1937, 1  ,  41           ,  40.824       ,  0.176033     ,  0.630905     ,  0.279017     
#1938, 1  ,  38.2         ,  39.6553      , -1.4553       ,  0.672192     , -2.165        
#1939, 1  ,  41.6         ,  45.0547      , -3.45466      ,  0.834433     , -4.14012      
#1940, 1  ,  45           ,  49.0118      , -4.01179      ,  0.966472     , -4.15096      
#1941, 1  ,  53.3         ,  56.6727      , -3.37275      ,  1.23486      , -2.73127   
#     
#
#...ESTIMATE OK
}
