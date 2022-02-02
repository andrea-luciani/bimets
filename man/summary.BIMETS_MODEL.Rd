\name{summary.BIMETS_MODEL}
\alias{print.BIMETS_MODEL}
\alias{summary.BIMETS_MODEL}
\alias{summary}
\alias{print}


\title{
Print basic information about a BIMETS model
}
\description{
This function prints basic information about a BIMETS model, e.g. behaviorals and identities count, coefficients count, the presence of estimated coefficients or simulated time series.
}
\usage{
\method{summary}{BIMETS_MODEL}(object,...)
\method{print}{BIMETS_MODEL}(x,...)
}

\arguments{
  \item{object}{
A BIMET model. 
}
  \item{x}{
A BIMET model. 
}
\item{...}{ 
Arguments list for the generic method.
}


  
}

\value{
This function prints basic information about a BIMETS model, i.e.:\cr\cr
- the name of the model;\cr
- the behaviorals count;\cr
- the identities count;\cr
- the coefficients count;\cr
- the check for the compliance of the model data;\cr
- the check for the coefficients definition in all the behaviorals;\cr
- the check for the definition of a simulated time series for each related endogenous variable of the model;\cr\cr
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

#model summary
summary(myModel)

#BIMETS MODEL
#-----------------------------------
#name:             myModelDefinition
#behaviorals:      3
#identities:       3
#coefficients:     12
#model data:       not OK
#.CHECK_MODEL_DATA(): model has no data. Please use LOAD_MODEL_DATA().
#fully estimated:  FALSE
#simulated:        FALSE

#load data into the model
myModel=LOAD_MODEL_DATA(myModel,myModelData,showWarnings = TRUE)

#estimate the model
myModel=ESTIMATE(myModel)

#model summary
print(myModel)

#BIMETS MODEL
#-----------------------------------
#name:             myModelDefinition
#behaviorals:      3
#identities:       3
#coefficients:     12
#model data:       OK
#fully estimated:  TRUE
#simulated:        FALSE

}
