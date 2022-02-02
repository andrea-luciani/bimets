\name{LOAD_MODEL}
\alias{LOAD_MODEL}


\title{
Load a BIMETS model description file
}
\description{
\if{html}{(Note: this is the html version of the reference manual. Please consider reading the pdf version of this reference manual, wherein there are figures and the mathematical expressions are better formatted than in html.)\cr\cr}

This function parses a \code{\link[=MDL]{MDL}} model definition and creates an equivalent R data structure that can be estimated and simulated. The input model definition can be either an external plain text file or a \code{character} variable.
}
\usage{
LOAD_MODEL( modelFile=NULL, 
            modelText=NULL, 
            showWarnings=TRUE, 
            quietly=FALSE,
            oldStyleModel=FALSE, 
            ...)
}

\arguments{
  \item{modelFile}{
  The path to the text file containing the \code{\link[=MDL]{MDL}} model definition.
  }
  \item{modelText}{
  The \code{character} variable containing the \code{\link[=MDL]{MDL}} model definition. \code{modelText} takes precedence over \code{modelFile} if both are defined.
  }
  \item{showWarnings}{
  If \code{TRUE} a warning message will be shown if the model has a null set of endogenous variables or a null set of exogenous variables.
  }
  
  \item{quietly}{
  If \code{TRUE}, information messages will be suppressed.
  }
  
     \item{oldStyleModel}{
    Backward compatibility.
    }
    
   \item{...}{
    Backward compatibility.
    }
  }

\value{
This function returns a BIMETS model object containing all the information gathered from the parsing of the input model definition. \cr\cr
A BIMETS model created with the \code{LOAD_MODEL} function can be viewed as a complex R \code{list()} containing the following elements (see examples):\cr\cr
- \bold{rawData} and \bold{cleanModel}: string arrays containing the original model definition. \code{cleanModel} is a clean version of the model definition, i.e. without comments, blank lines, etc.; \cr\cr
- \bold{behaviorals} and \bold{identities}: sub-lists containing all the information gathered from the behavioral and the identity definitions. This sub lists are described later in this page; \cr\cr
- \bold{vendog} and \bold{vexog}: string array containing the names of the endogenous and exogenous variables of the model; the former is also subsetted into \bold{vendogBehaviorals} and \bold{vendogIdentities}\cr\cr
- \bold{totNumEqs}, \bold{totNumIds} and \bold{eqCoeffNum}: integer variables containing the behaviorals count, the identities count and the coefficients count of the model;\cr\cr
- \bold{incidence_matrix}: the incidence matrix built from the model equations; it is a square matrix in which each row and each column represent an endogenous variable. If the \code{(i,j)} element is equal to \code{1} than in the model definition the current value of the endogenous variable referred by the \code{i}-row directly depends on the current value of the endogenous variable referred by the \code{j}-column. (see example)\cr\cr
- \bold{vpre}, \bold{vsim}, \bold{vfeed} and \bold{vpost}: the simulation process takes advantage of an appropriate ordering of the equations to increase the performances by iteratively solving only one subset of equations, while the other equations are solved straightforwardly.  (Ref: \emph{Don Gallo - Solving large sparse systems of equations in econometric models - Journal of Forecasting 1987} and \emph{Numerical methods for simulation and optimal control of large-scale macroeconomic models - Nepomiastchy, Rachidi, Ravelli - 1980}). The optimal reordering of the model equations is achieved by using an iterative algorithm applied to the incidence matrix, that produces 4 ordered arrays of endogenous variables: \cr 1) \code{vpre} is the ordered string array containing the names of the endogenous pre-recursive variables that must be computed (by using their \code{EQ>} definition) before the simulation iterative algorithm takes place; \cr 2) \code{vsim} is the ordered string array containing the names of the endogenous variables that must be sequentially computed during each iteration of the simulation algorithm;\cr 3) \code{vfeed} is the ordered string array containing the names of the endogenous feedback variables to be evaluated for convergence during the simulation iterative algorithm; \cr 4) \code{vpost} is the ordered string array containing the names of the endogenous post-recursive variables that must be computed after the execution and the convergence of the iterative algorithm in step 3 (more details in \code{\link[=bimets]{BIMETS package}} and \code{\link[=SIMULATE]{SIMULATE}} help pages);\cr\cr
- \bold{max_lag}: the max lag of the model, i.e. the highest number of periods a time series of the model is lagged by in the \code{MDL} definition. It also accounts for PDLs and for the order of the error autocorrelation, if any; \cr\cr
- \bold{modelName}: the name of the model, copied from the input file name or from the input \code{character} variable name containing the model definition; \cr\cr
BEHAVIORALS and IDENTITIES\cr\cr
The elements \bold{'behaviorals'} and \bold{'identities'} of the BIMETS model are named lists that contain information on behaviorals and identities of the model. In both of this two lists, the name of each element is the name of the behavioral or the identity the data refer to, as specified in the model definition file: e.g. given a BIMETS model named \code{myModel}, the information on a behavioral named \code{cn} (i.e there exists a "BEHAVIORAL> cn" in the \code{MDL} definition of the model) is stored into  \code{myModel$behaviorals$cn}.\cr\cr
Behavioral elements have the following components:\cr\cr
- \bold{eq}: the equation of the behavioral, as a \code{character} variable;\cr\cr
- \bold{eqCoefficientsNames}: the names of the coefficients (the original ones and eventually the ones created by the \code{PDL>} expansion);\cr\cr
- \bold{eqCoefficientsNamesOriginal}: the names of the original coefficients;\cr\cr
- \bold{eqComponentsNames}: the names of endogenous and exogenous variables that appear in the behavioral equation;\cr\cr
- \bold{eqComponentsNamesBehaviorals}: the names of behavioral endogenous variables that appear in the behavioral equation;\cr\cr
- \bold{eqComponentsNamesIdentities}: the names of identity endogenous variables that appear in the behavioral equation;\cr\cr
- \bold{eqComponentsNamesExogenous}: the names of exogenous variables that appear in the behavioral equation;\cr\cr
- \bold{tsrange}: the estimation time range as a 4 integer array;\cr\cr
- \bold{eqRegressorsNames}: a \code{character} array containing the regressor expressions (the original ones and eventually the ones created by the \code{PDL>} expansion); \cr\cr
- \bold{eqRegressorsNamesOriginal}:  a \code{character} array containing the expressions of the original regressors;\cr\cr
- \bold{errorRaw}: the original definition of the error autocorrelation, if any (see \code{\link[=MDL]{MDL}});\cr\cr
- \bold{errorType}: the type of the error structure;\cr\cr
- \bold{errorDim}: the dimension of the error autocorrelation;\cr\cr
- \bold{eqSimExp}: the \code{R} optimized \code{expression} of the behavioral equation that is used in the simulation algorithm;\cr\cr
- \bold{matrixR}: the \code{R} Lagrange matrix that is used in restriction analysis (see \code{\link[=MDL]{MDL}});\cr\cr
- \bold{vectorR}: the \code{r} Lagrange vector that is used in restriction analysis (see \code{\link[=MDL]{MDL}});\cr\cr
- \bold{restrictRaw}: the original definition of the coefficient restrictions, if any.\cr\cr
- \bold{pdlRaw}: the original definition of the PDL restrictions, if any (see examples and \code{\link[=MDL]{MDL}}).\cr\cr
- \bold{pdlRestrictionMatrix}: the \code{R} Lagrange matrix that is used in PDL restriction analysis (see examples and \code{\link[=MDL]{MDL}});\cr\cr
For example, given a BIMETS model named \code{myModel}, the information on a techincal identity named \code{y} (i.e there exists an "IDENTITY> y" in the \code{MDL} definition of the model) is stored in \code{myModel$identities$y}.\cr\cr Identity elements have the following components:\cr\cr
- \bold{eqRaw}: the original equations  of the identity (more than one if the identity has multiple equations and has \code{IF>} conditions), as a \code{character} variable (see examples and \code{\link[=MDL]{MDL}});\cr\cr
- \bold{ifRaw}: the original \code{IF>} conditions, if any, of the identity, as a \code{character} variable;\cr\cr
- \bold{eqFull}: the full expression of the identity, composed with \code{IF>} conditions and related equations (see examples), as a \code{character} variable;\cr\cr
- \bold{eqComponentsNames}: the names of endogenous and exogenous variables that appear in the identity equation;\cr\cr
- \bold{eqComponentsNamesBehaviorals}: the names of behavioral endogenous variables that appear in the identity equation;\cr\cr
- \bold{eqComponentsNamesIdentities}: the names of identity endogenous variables that appear in the identity equation;\cr\cr
- \bold{eqSimExp}: the \code{R} optimized \code{expression} of the identity equation that is used in the simulation algorithm;\cr\cr
- \bold{hasIF}: boolean, \code{TRUE} if the identity has an \code{IF>} condition;\cr\cr




}

\seealso{
\code{\link[=MDL]{MDL}}\cr
\code{\link[=LOAD_MODEL_DATA]{LOAD_MODEL_DATA}}\cr
\code{\link[=ESTIMATE]{ESTIMATE}}\cr
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

#load model 
myModel=LOAD_MODEL(modelText=myModelDefinition)

#retrieve model structure...
#get definition
myModel$cleanModel
# [1] "BEHAVIORAL> cn"                                        
# [2] "TSRANGE 1925 1 1941 1"                                 
# [3] "EQ> cn = a1 + a2*p + a3*TSLAG(p,1) + a4*(w1+w2)"       
# [4] "COEFF> a1 a2 a3 a4"                                    
# [5] "ERROR> AUTO(2)"                                        
# [6] "BEHAVIORAL> i"                                         
# [7] "TSRANGE 1923 1 1941 1"                                 
# [8] "EQ> i = b1 + b2*p + b3*TSLAG(p,1) + b4*TSLAG(k,1)"     
# [9] "COEFF> b1 b2 b3 b4"                                    
#[10] "RESTRICT> b2 + b3 = 1"                                 
#[11] "BEHAVIORAL> w1"                                        
#[12] "TSRANGE 1925 1 1941 1"                                 
#[13] "EQ> w1 = c1 + c2*(y+t-w2) + c3*TSLAG(y+t-w2,1)+c4*time"
#[14] "COEFF> c1 c2 c3 c4"                                    
#[15] "PDL> c3 1 3"                                           
#[16] "IDENTITY> y"                                           
#[17] "EQ> y = cn + i + g - t"                                
#[18] "IDENTITY> p"                                           
#[19] "EQ> p = y - (w1+w2)"                                   
#[20] "IDENTITY> k"                                           
#[21] "EQ> k = TSLAG(k,1) + i"                                
#[22] "IF> i > 0"                                             
#[23] "IDENTITY> k"                                           
#[24] "EQ> k = TSLAG(k,1)"                                    
#[25] "IF> i <= 0"                                            

#get endogenous and exogenous
myModel$vendog
#[1] "cn" "i"  "w1" "y"  "p"  "k" 
myModel$vexog
#[1] "w2"   "t"    "time" "g"   

#get behaviorals, identities and coefficients count
myModel$totNumEqs
#[1] 3
myModel$totNumIds
#[1] 3
myModel$eqCoeffNum
#[1] 12

#get the incidence matrix
myModel$incidence_matrix
#   cn i w1 y p k
#cn  0 0  1 0 1 0
#i   0 0  0 0 1 0
#w1  0 0  0 1 0 0
#y   1 1  0 0 0 0
#p   0 0  1 1 0 0
#k   0 1  0 0 0 0

#get the optimal reordering arrays
myModel$vpre
#NULL
myModel$vsim
#[1] "w1" "p"  "cn" "i"  "y" 
myModel$vfeed
#[1] "y"
myModel$vpost
#[1] "k"

#get the model max lag and the model name
myModel$max_lag
#[1] 3
myModel$modelName
#myModelDefinition



#get infos on behavioral w1

myModel$behaviorals$w1$eq
#[1] "w1=c1+c2*(y+t-w2)+c3*TSLAG(y+t-w2,1)+c4*time"

myModel$behaviorals$w1$eqCoefficientsNames
#[1] "c1"       "c2"       "c3"       "c3_PDL_1" "c3_PDL_2" "c4"  

myModel$behaviorals$w1$eqCoefficientsNamesOriginal
#[1] "c1" "c2" "c3" "c4"

myModel$behaviorals$w1$eqComponentsNames
#[1] "t"    "time" "w1"   "w2"   "y"   

myModel$behaviorals$w1$tsrange
#[1] 1925    1 1941    1

myModel$behaviorals$w1$eqRegressorsNames
#[1] "1"                       "(y+t-w2)"               
#[3] "TSLAG(y+t-w2,1)"  "TSLAG(TSLAG(y+t-w2,1),1)" "TSLAG(TSLAG(y+t-w2,1),2)" "time"

myModel$behaviorals$w1$eqRegressorsNamesOriginal
#[1] "1"                "(y+t-w2)"       
#[3] "TSLAG(y+t-w2,1)" "time"

myModel$behaviorals$w1$pdlRaw
#[1] "c3 1 3;"

myModel$behaviorals$w1$pdlRestrictionMatrix
#     [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]    0    0    1   -2    1    0



#get infos on behavioral cn

myModel$behaviorals$cn$errorRaw
#[1] "AUTO(2)"

myModel$behaviorals$cn$errorType
#[1] "AUTO"

myModel$behaviorals$cn$errorDim
#[1] 2

myModel$behaviorals$cn$eqSimExp
#expression(cn[4,]=cn__ADDFACTOR[4,]+cn__a1+cn__a2*p[4,]+cn__a3*(p[3,])+
#cn__a4*(w1[4,]+w2[4,])+cn__RHO_1*(cn[3,]-(cn__ADDFACTOR[3,]+
#cn__a1+cn__a2*p[3,]+cn__a3*(p[2,])+cn__a4*(w1[3,]+w2[3,])))+
#cn__RHO_2*(cn[2,]-(cn__ADDFACTOR[2,]+cn__a1+cn__a2*p[2,]+
#cn__a3*(p[1,])+cn__a4*(w1[2,]+w2[2,]))))



#get infos on behavioral i

myModel$behaviorals$i$matrixR
#    [,1] [,2] [,3] [,4]
#[1,]   0    1    1    0

myModel$behaviorals$i$vectorR
#[1] 1

myModel$behaviorals$i$restrictRaw
#[1] "b2+b3=1;"



#get infos on identitiy k

myModel$identities$k$eqRaw
#[1] "k=TSLAG(k,1)+i;k=TSLAG(k,1);"

myModel$identities$k$ifRaw
#[1] "i > 0;i <= 0;"

myModel$identities$k$eqFull
#[1] "__IF__ (i > 0) __THEN__ k=TSLAG(k,1)+i;__IF__ (i <= 0) __THEN__ k=TSLAG(k,1);"

myModel$identities$k$eqComponentsNames
#[1] "i" "k"

myModel$identities$k$eqSimExp
#expression(k[4,]=.MODEL_VIF(k[4,],i[4,] > 0,k_ADDFACTOR[4,]+
#(k[3,])+i[4,]),k[4,]=.MODEL_VIF(k[4,],i[4,] <= 0,
#k_ADDFACTOR[4,]+(k[3,])))

myModel$identities$k$hasIF
#[1] TRUE


}
