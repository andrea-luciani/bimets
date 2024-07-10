\name{MDL}
\alias{MDL}


\title{
BIMETS Model Description Language
}
\description{

BIMETS provides a language to unambiguously specify an econometric model. This page describes how to create a model and its general structure. The specification of an econometric model is translated and identified by keyword statements which are grouped in a model file, i.e. a plain text file or a \code{character} variable with a specific syntax. Collectively, these keyword statements constitute the BIMETS Model Description Language (from now on \code{MDL}). The model specifications consist of groups of statements. Each statement begins with a keyword. The keyword classifies the component of the model which is being specified.\cr\cr
Below is an example of a Klein's model  with an \code{MDL} compliant syntax which can either be stored in a \code{character} variable or in a plain text file.\cr\cr 
For more realistic scenarios, several advanced econometric exercises on the US Federal Reserve FRB/US econometric model (e.g., dynamic simulation in a monetary policy shock, rational expectations, endogenous targeting, stochastic simulation, etc.) are available in the \href{https://cran.r-project.org/package=bimets/vignettes/frb2bimets.pdf}{"US Federal Reserve quarterly model (FRB/US) in R with bimets"} vignette. \cr \cr
The content of the \emph{klein1.txt} variable  is:
\preformatted{
R> klein1.txt="
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
Please note that there are circular dependencies between equations of the model, e.g. \code{p <- w1 <- y <- p} as shown in the \code{\link[=bimets]{"BIMETS package"}} section figure in the \href{https://CRAN.R-project.org/package=bimets/bimets.pdf}{pdf version} of this reference manual. Circular dependencies imply that the model simulation must be solved with an iterative algorithm.\cr\cr
As shown, the model definition is quite intuitive. The first keyword is \code{MODEL}, while at the end of the model definition we can find the \code{END} keyword. Available tags in the definition of a generic BIMETS model are: 
\cr\cr
- \bold{EQUATION>} or \bold{BEHAVIORAL>} indicate the beginning of a series of keyword statements describing a behavioral equation. The behavioral statement general form is: \cr
\code{BEHAVIORAL> name  [TSRANGE startYear, startPeriod, endYear, endPeriod]}  \cr
where \code{name} is the name of the behavioral equation and the optional \code{TSRANGE} specifies that the provided time interval must be used to estimate the coefficients. The optional \code{TSRANGE} is defined as a 4-dimensional numerical array built with starting year, starting period, ending year, and ending period.\cr\cr
Given \eqn{Y=\beta*X+\epsilon}{Y = b * X + RES}, where \eqn{Y}{Y} are the historical values of the dependent variable and \eqn{X}{X} are the historical values of the regressors, if the requested estimation method is \code{OLS} (Ordinary Least Squares), in the general case (i.e. no restrictions nor error auto-correlation, as described later) the coefficients will be calculated as: \eqn{\beta_{OLS}=(X' * X) ^{-1} * X' * Y}{b_ols = (X' * X )^(-1) * X' * Y}.\cr\cr
If the requested estimation method is \code{IV} (Instrumental Variables), given \eqn{Z}{Z} the matrix built with instrumental variables as columns \eqn{Z_i}{Z_i}, that should not be correlated to the disturbance terms, i.e. \eqn{E[ \epsilon ' * Z_i] = 0}{E[ RES' * Z_i ] = 0}, the coefficients will be either calculated as \cr \eqn{\beta_{IV}=(Z' * X) ^{-1} * Z' * Y}{b_iv=(Z' * X)^(-1) * Z' * Y}, or more generally as: \eqn{\beta_{IV}=(\hat{X}' * \Omega^{-1} * \hat{X}) ^{-1} * \hat{X}' * \Omega^{-1} * Y}{b_iv=(X_hat' * Omega^(-1) * X_hat)^(-1) * X_hat' * Omega^(-1) * Y} where \eqn{\hat{X} = Z * (Z' * Z)^{-1} * Z' * X}{X_hat = Z * (Z' * Z)^(-1) * Z' * X} and \eqn{\Omega = \sigma^{2} * I}{Omega = sigma^(2) * I},  \eqn{\sigma^{2} = E[ \epsilon' * \epsilon]}{sigma^(2) = E[ RES' * RES ]}\cr\cr
- \bold{IDENTITY>} indicates the beginning of a series of keyword statements describing an identity or technical equation. The identity statement general form is: \cr
\code{IDENTITY> name}\cr where \code{name} is the identity name.\cr\cr
- \bold{EQ>} specifies the mathematical expression for a behavioral or an identity equation. \cr\cr
The equation statement general form for a behavioral equation is:\cr
\code{EQ> LHS = coeff1*f1 + coeff2*f2 + coeff3*f3 + ...}\cr
where \code{LHS} is a function of the behavioral variable, \cr \code{coeff1, coeff2, coeff3, ...} are the coefficient names of the equation and \cr \code{f1, f2, f3, ...} are functions of variables.\cr\cr
The equation statement general form for an identity equation is:\cr \code{EQ> LHS = f1 + f2 + f3 + ...} \cr where \code{LHS} is a function of the identity variable and \cr \code{f1, f2, f3, ...} are functions of variables.\cr\cr
The following \code{MDL} functions can be used in the \code{LHS} left-hand side of the equation, with \code{name} as the name of the behavioral or the identity variable: \cr\cr
- \code{name} - i.e. the identity function;\cr \cr
- \code{TSDELTA(name,i)} - \code{i}-periods difference of the \code{name} time series;\cr \cr
- \code{TSDELTAP(name,i)} - \code{i}-periods percentage difference of the \code{name} time series;\cr \cr
- \code{TSDELTALOG(name,i)} - \code{i}-periods logarithmic difference of the \code{name} time series;\cr \cr
- \code{LOG(name)} - log of the \code{name} time series;\cr \cr
- \code{EXP(name)} - exponential of the \code{name} time series. \cr \cr
On the other side, the mathematical expression available for use in the \code{RHS} right-hand side of the \code{EQ>} equation and in the \code{IV>} expression described later in this page (i.e. \code{f1, f2, f3, ...}) can include the standard arithmetic operators, parentheses and the following \code{MDL} functions:\cr \cr
- \code{TSLAG(ts,i)} - lag the \code{ts} time series by \code{i}-periods;\cr \cr
- \code{TSLEAD(ts,i)} - lead the \code{ts} time series by \code{i}-periods;\cr \cr
- \code{TSDELTA(ts,i)} - \code{i}-periods difference of the \code{ts} time series;\cr \cr
- \code{TSDELTAP(ts,i)} - \code{i}-periods percentage difference of the \code{ts} time series;\cr \cr
- \code{TSDELTALOG(ts,i)} - \code{i}-periods logarithmic difference of the \code{ts} time series;\cr \cr
- \code{MOVAVG(ts,i)} - \code{i}-periods moving average of the \code{ts} time series;\cr \cr
- \code{MOVSUM(ts,i)} - \code{i}-periods moving sum of the \code{ts} time series;\cr \cr
- \code{LOG(ts)} - log of the \code{ts} time series;\cr \cr
- \code{EXP(ts)} - exponential of the \code{ts} time series; \cr \cr
- \code{ABS(ts)} - absolute values of the \code{ts} time series.\cr \cr
Note that BIMETS classifies a model as a forward-looking model if any model equation contains the \code{TSLEAD} time series function. More details about forward-looking models are available in the "Rational Expectations Models" section of the \code{\link[=SIMULATE]{SIMULATE}} help pages.\cr \cr
\code{MDL} function names are reserved names. They cannot be used as variable or coefficient names. The coefficient names are specified in a subsequent \code{COEFF>} keyword statement within a behavioral equation. By definition, identities do not have any coefficient that must be assessed. Any name not specified as a coefficient name or mentioned on the list of the available \code{MDL} functions is assumed to be a variable.\cr\cr
- \bold{COEFF>} specifies the coefficient names used in the EQ> keyword statement of a behavioral equation. The coefficients statement general form is:\cr
\code{COEFF> coeff0 coeff1 coeff2 ... coeffn}. \cr The coefficients order in this statement must be the same as it appears in the behavioral equation.\cr\cr
- \bold{ERROR>} specifies an autoregressive process of a given order for the regression error. The error statement general form is:\cr
\code{ERROR> AUTO(n)} \cr where \code{n} is the order of the autoregressive process for the error.\cr\cr During an estimation, users must ensure that the required data are available for the specified error structure: \code{n} periods of data before the time interval specified by \code{TSRANGE} must be defined in any time series involved in the regression.\cr\cr The solution requires an iterative algorithm. Given \eqn{Y_{1}=\beta_{1}*X_{1}+\epsilon_{1}}{Y_1 = b_1 * X_1 + RES_1}, where \eqn{Y_{1}}{Y_1} are the historical values of the dependent variable and \eqn{X_{1}}{X_1} are the historical values of the regressors, the iterative algorithm is based on the Cochrane-Orcutt procedure:\cr \cr
1) Make an initial estimation by using the original TSRANGE extended backward \code{n} periods (given \code{n} as the autocorrelation order).\cr\cr
2) Estimate the error autocorrelation coefficients \eqn{\rho_{i}=\rho_{i,1},...,\rho_{i,n}}{RHO_i=RHO_i[1],...,RHO_i[n]} with \eqn{i=1} by regressing the residuals \eqn{\epsilon_{i}}{RES_i} on their lagged values by using the auxiliary model: \cr \eqn{\epsilon_{i}=\rho_{i,1}*TSLAG(\epsilon_{i},1)+...+\rho_{i,n}*TSLAG(\epsilon_{i},n)}{RES_i=RHO_i[1]*TSLAG(RES_i,1)+...+RHO_i[n]*TSLAG(RES_i,n)}\cr\cr
3) Transform the data for the dependent and the independent variables by using the estimated \eqn{\rho_{i}}{RHO_i}. The new dependent variable will be: \eqn{Y_{i+1}=P_i*Y_i}{Y_(i+1)=P_i*Y_i}, and the new independent variables will be \eqn{X_{i+1}=P_i*X_i}{X_(i+1)=P_i*X_i} with the matrix \eqn{P_i}{P_i} defined as:

\deqn{ P_i = \left( \begin{array}{ccccccc}
1 & 0 & 0 & 0 & ... & 0 & 0 \cr 
-\rho_{i,1} & 1 & 0 & 0 & ... & 0 & 0 \cr 
-\rho_{i,2} & -\rho_{i,1} & 1 & 0 & ... & 0 & 0 \cr 
 &  &  & ... &  &  &  \cr 
0 & 0 & ... & -\rho_{i,n} & ... & -\rho_{i,1} & 1 
\end{array} \right) } \cr
4) Run another estimation on the original model \eqn{Y_{i+1}=\beta_{i+1}*X_{i+1}+\epsilon_{i+1}}{Y_(i+1) = b_(i+1) * X_(i+1) + RES_(i+1)} by using the suitable \code{TSRANGE} and the transformed data coming out of step 3 and compute the new time series for the residuals. \cr\cr
5) Estimate the new autocorrelation coefficients \eqn{\rho_{i+1}=\rho_{i+1,1},...,\rho_{i+1,n}}{RHO_(i+1)=RHO_(i+1)[1],...,RHO_(i+1)[n]}, by regressing the new residuals arising from step 4 (similar to step 2)\cr\cr
6) Carry out the convergence check through a comparison among the previous \eqn{\rho_{i}}{RHO_i} and the new ones arising from steps 5. \cr If \eqn{all(abs(\rho_{i+1}-\rho_{i})<\delta)}{all(abs(RHO_(i+1)-RHO_i) < d)}, where \eqn{\rho_{i}}{RHO_i} is the \eqn{\rho}{RHO} vector at the iteration \eqn{i} and \eqn{\delta}{d} is a small convergence factor, then exit otherwise repeat from step 3 with \code{i <- i+1}. \cr\cr
- \bold{RESTRICT>} is a keyword that can be used to specify linear coefficient restrictions. A deterministic restriction can be applied to any equation coefficient. Any number of \code{RESTRICT>} keywords is allowed for each behavioral equation.\cr\cr A deterministic (exact) coefficient restriction sets a linear expression containing one or more coefficients equal to a constant. The restriction only affects the coefficients of the behavioral equation in which it is specified. The restriction statement general form is:
\preformatted{
RESTRICT> linear_combination_of_coefficients_1 = value_1
...
linear_combination_of_coefficients_n = value_n
} where \code{linear_combination_of_coefficients_i, i=1..n} is a linear combination of the coefficient(s) to be restricted and \code{value_i} is the in-place scalar value to which the linear combination of the coefficients is set equal. Each linear combination can be set equal to a different value.\cr\cr \code{MDL} example:
\preformatted{
RESTRICT> coeff1 = 0
coeff2 = 10.5
coeff3-3*coeff4+1.2*coeff5 = 0
}
In many econometric packages, linear restrictions have to be coded by hand in the equations. BIMETS allows users to write down the restriction in a natural way, thus applying a constrained minimization. This procedure, although it leads to approximate numerical estimates, allows an easy implementation.\cr\cr
The theory behind this procedure is that of the Lagrange multipliers. Presented here is an example of its implementation.\cr\cr Suppose that we have an equation defined as:
\preformatted{
EQUATION> Y TSRANGE 2010 1 2015 4 
EQ> Y = C1*X1 + C2*X2 + C3*X3 
COEFF> C1 C2 C3 
RESTRICT> 1.1*C1 + 1.3*C3 = 2.1 
1.2*C2 = 0.8
}
Coefficients \code{C1, C2, C3} are to be estimated. They are subject to the linear constraints specified by the \code{RESTRICT>} keyword statement. In the case of \code{OLS} estimation, this is carried out in the following steps:\cr\cr
1) Compute the cross-product matrices \eqn{X' X } and \eqn{X' Y} where \eqn{X} is a matrix with dimension \cr \code{[NOBS x NREG]} containing the values of the independent variables (regressors) historical observations (and a vector of ones for the constant term, if any), and where \eqn{Y} is a \code{NOBS} elements vector of the dependent variable (regressand) historical observations; \code{NOBS} is the number of observations available on the \code{TSRANGE} specified in the behavioral equation, and \code{NREG} is the number of regressors, or coefficients;\cr\cr
2) Build the restriction matrices. In the example:\cr\cr
\eqn{R = \left( \begin{array}{ccc}1.1 & 0 & 1.3  \cr 0 & 1.2 & 0 \end{array} \right) } \cr \cr
and \cr \cr
\eqn{r = \left( \begin{array}{c} 2.1  \cr 0.8  \end{array} \right)} \cr \cr

\code{R} is a matrix of \code{[NRES x NREG]} size, and \code{r} is a vector of \code{[NRES]} length, where \code{NRES} is the number of restrictions;\cr\cr
3) Compute the scaling factors for the augmentation to be performed in the next step:\cr
\deqn{Rscale[i]=\frac{mean(X' X)}{max(abs(R[i,]))}}{Rscale[i]=mean(X' X)/max(abs(R[i,]))}
where \eqn{R[i,]} is the i-th row of the \code{R} matrix.\cr\cr
Assuming \eqn{mean(X' X) = 5000}, in the example above we will have:\cr
\eqn{Rscale[1]=5000 / 1.3} \cr
\eqn{Rscale[2]=5000 / 1.2} \cr\cr
The augmented matrices will then be defined as:\cr\cr
\eqn{R_{aug} = \left( \begin{array}{ccc} 1.1 * Rscale[1] & 0 & 1.3 * Rscale[1] \cr 0 & 1.2 * Rscale[2] & 0 \end{array} \right) }

and \cr \cr
\eqn{r_{aug} = \left( \begin{array}{c} 2.1 * Rscale[1] \cr 0.8 * Rscale[2]  \end{array} \right) } \cr


4) Compute the so-called "augmented" cross-product matrix \eqn{(X' X)_{aug}}{(X' X)_aug} by adding to the cross-product matrix \eqn{(X' X)} a total of \code{NRES} rows and \code{NRES} columns:
\cr\cr
\eqn{(X' X)_{aug} = \left( \begin{array}{cc} X' X & R_{aug}' \cr R_{aug} & 0 \end{array} \right) }
\cr\cr
5) Similarly, compute the so-called "augmented" cross-product matrix \eqn{(X' Y)_{aug}}{(X' Y)_aug} by adding a total of \code{NRES} elements to the cross-product matrix \eqn{(X' Y)}:
\cr\cr
\eqn{(X' Y)_{aug}=\left( \begin{array}{c} X' Y \cr r_{aug}  \end{array} \right) }
\cr\cr
6) Calculate the \eqn{\hat{\beta}_{aug}}{beta_hat_aug} augmented coefficients by regressing the \eqn{(X' Y)_{aug}}{(X' Y)_aug} on the \eqn{(X' X)_{aug}}{(X' X)_aug}.\cr\cr The first \code{NREG} values of the augmented coefficients \eqn{\hat{\beta}_{aug}}{beta_hat_aug} array are the estimated coefficients with requested restrictions. The last \code{NRES} values are the errors we have on the deterministic restrictions.\cr\cr
In the case of \code{IV} estimation, the procedure is the same as in the \code{OLS} case, but the matrix \eqn{X}{X} has to be replaced with the matrix \eqn{\hat{X}}{X_hat}, as previously defined in the \code{BEHAVIORAL>} keyword.\cr\cr
- \bold{PDL>} is a keyword that defines an Almon polynomial distributed lag to be used in estimation. Almon Polynomial distributed lags are specific kind of deterministic restrictions imposed on the coefficients of the distributed lags of a specific regressor. Multiple PDLs on a single behavioral equation can be defined. \cr\cr The PDL> statement general form is:\cr \code{PDL> coeffname degree laglength [N] [F]}\cr where \code{coeffname} is the name of a coefficient, \code{degree} is an integer scalar specifying the degree of the polynomial, \code{laglength} is an integer scalar specifying the length of the polynomial (in number of time periods), the optional \code{N} (i.e. "nearest") means that the nearest lagged term of the expansion, i.e., the first term, is restricted to zero, and the optional \code{F} (i.e. "farthest") means that the farthest lagged term of the expansion, i.e., the last term, is restricted to zero; the \code{PDL>} keyword statement thusly defined applies an Almon polynomial distributed lag to the regressor associated with the \code{coeffname} coefficient, of \code{laglength} length and \code{degree} degree, by providing the appropriate expansion and the deterministic restrictions for the degree and length specified. These expansions are not explicitly shown to the user, i.e., the original model is not changed.\cr\cr \code{laglength} must be greater than \code{degree} (see example below).\cr\cr A PDL term can be further referenced in a \code{RESTRICT>} keyword statement by using the following syntax: \code{LAG(coefname, pdllag)}.\cr\cr Example: \code{RESTRICT> LAG(coeff2,3) = 0} means that, during the estimation, the regressor related to the coefficient \code{coeff2} and lagged by 3 periods in the PDL expansion must have a coefficient equal to zero. This example also implies that a \code{PDL> coeff2 x y} with \code{y > 3} has been declared in the same behavioral. \cr\cr
The implementing rules are the following:\cr\cr
1) Read off the \code{laglength} of the PDL keyword and expand the column of the regressor related to \code{coeffname} in the matrix \code{X} (i.e. the original regressors matrix) with the lagged values of the regressor, from left to right, starting form the lag 1 to the lag \code{laglength-1}. The matrix \code{X} will now have a \code{[NOBS x (NREG+laglength-1)]} size, with \code{NOBS} as the number of observations in the specified \code{TSRANGE} and \code{NREG} as the number of regressors, or coefficients.\cr\cr
2) Build the restriction matrix \code{R} with the following \code{[ Nrow x Ncol ]} dimensions:\cr
\code{Nrow = laglength - ( degree + 1 )}\cr
\code{Ncol = NREG + laglength - 1}\cr\cr
This matrix's elements will be zero except for the (\code{laglength})-columns related to the section of the expanded columns in the \code{X} matrix. For every row we will have to insert \code{degree+2} numbers different from zero.\cr\cr
The \code{degree+2} numbers are taken form the Tartaglia's-like triangle:
\preformatted{
1 -2  1
1 -3  3 -1
1 -4  6 -4   1
1 -5 10 -10  5  1
... ... ... ...
}
where in the \code{i}-th row we find the numbers for a PDL of \code{degree=i}.\cr\cr
The \code{r} vector giving the knows terms for the restrictions is a vector of\cr \code{NRES = laglength - (degree + 1)} elements equal to zero.\cr\cr
An example will clarify:
\preformatted{
EQUATION> Y TSRANGE 2010 1 2015 4 
EQ> Y = C1*X1 + C2*X2 + C3*X3 
COEFF> C1 C2 C3 
PDL> C2 2 5
} then \cr \cr
\eqn{R = \left( \begin{array}{ccccccc} 0 & 1 & -3 & 3 & 1 & 0 & 0 \cr 0 & 0 & 1 & -3 & 3 & 1 & 0 \end{array} \right) }
\cr \cr and \cr \cr
\eqn{r = \left( \begin{array}{c} 0  \cr 0 \end{array} \right) }
\cr\cr \cr
The expanded regressors are:\cr \code{X1, X2, TSLAG(X2,1), TSLAG(X2,2), TSLAG(X2,3), TSLAG(X2,4), X3}.\cr\cr The scaling factor is given, as in the standard restriction case, by: \eqn{mean(X' X) / max(abs(R[i,]))}{mean(X' X) / max(abs(R[i,]))} \cr\cr

- \bold{IF>} keyword is used to conditionally evaluate an identity during a simulation, depending on a logical expression's value. Thus, it is possible to have a model alternating between two or more identity specifications for each simulation period, depending upon results from other equations.\cr \cr
The IF> statement general form is:\cr
\code{IF> logical_expression}\cr\cr The \code{IF>} keyword must be specified within an identity group; this keyword causes the equation specified in the identity group to be evaluated during the current simulation period only when the \code{logical_expression} is \code{TRUE}.\cr\cr Only one \code{IF>} keyword is allowed in an identity group. Further occurrences produce an error message, and processing stops.\cr\cr
The \code{logical_expression} can be composed of constants, endogenous variables, exogenous variables, an expression among variables, combinations of the logical operators; mathematical operators and the \code{MDL} functions listed in the \code{EQ>} section are allowed.\cr\cr
In the following \code{MDL} example, the value of the endogenous \code{myIdentity} variable is specified with two complementary conditional identities, depending on the \code{TSDELTA()} result:
\preformatted{
IDENTITY> myIdentity
IF> TSDELTA(myEndog*(1-myExog)) > 0
EQ> myIdentity = TSLAG(myIdentity)+1

IDENTITY> myIdentity
IF> TSDELTA(myEndog*(1-myExog)) <= 0
EQ> myIdentity = TSLAG(myIdentity)
}
- \bold{IV>} specifies the mathematical expression for an instrumental variable used in a behavioral equation. \cr\cr
The general form for an instrumental variable expression is:\cr \code{IV> f1 + f2 + f3 + ...} \cr \code{f1, f2, f3, ...} are functions of variables.\cr\cr
The mathematical expression available for use in the \code{IV>} definition are those already described in the \code{EQ>} section.\cr \cr
- \bold{COMMENT>} can be used to insert comments into a model. The general form of this keyword is:\cr
\code{COMMENT> text}\cr \cr
The \code{text} following the \code{COMMENT>} keyword is ignored during all processing and must lie in the same line. Comments cannot be inserted within another keyword statement. A dollar sign in the first position of a line is equivalent to using the COMMENT> keyword, as in this exmaple:\cr
\code{
$This is a comment
}
\cr\cr No other keywords are currently allowed in the \code{MDL} syntax.\cr\cr

}





\seealso{
\code{\link[=LOAD_MODEL]{LOAD_MODEL}}\cr
\code{\link[=ESTIMATE]{ESTIMATE}}\cr
\code{\link[=SIMULATE]{SIMULATE}}\cr
\code{\link[=STOCHSIMULATE]{STOCHSIMULATE}}\cr
\code{\link[=MULTMATRIX]{MULTMATRIX}}\cr
\code{\link[=RENORM]{RENORM}}\cr
\code{\link[=OPTIMIZE]{OPTIMIZE}}\cr
\code{\link[=TSERIES]{TIMESERIES}}\cr
\code{\link[=idxOver]{BIMETS indexing}}\cr
\code{\link[=bimetsConf]{BIMETS configuration}}\cr
\code{\link[=summary.BIMETS_MODEL]{summary}}\cr
}

\examples{

#########################################################
#KLEIN MODEL WITH AUTOCORRELATION, RESTRICTIONS AND 
#CONDITIONAL EVALUATIONS

#define model
myModel<-
"MODEL

COMMENT> Modified Klein Model 1 of the U.S. Economy with PDL, 
COMMENT> autocorrelation on errors, restrictions and conditional evaluations

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
EQ> w1 = c1 + c2*(y+t-w2) + c3*TSLAG(y+t-w2,1)+c4*time
COEFF> c1 c2 c3 c4
PDL> c3 1 2

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
modelData<-list(
  cn    =TIMESERIES(39.8,41.9,45,49.2,50.6,52.6,55.1,56.2,57.3,57.8,55,50.9,
            45.6,46.5,48.7,51.3,57.7,58.7,57.5,61.6,65,69.7,
            START=c(1920,1),FREQ=1),
  g     =TIMESERIES(4.6,6.6,6.1,5.7,6.6,6.5,6.6,7.6,7.9,8.1,9.4,10.7,10.2,9.3,10,
            10.5,10.3,11,13,14.4,15.4,22.3,
            START=c(1920,1),FREQ=1),
  i     =TIMESERIES(2.7,-.2,1.9,5.2,3,5.1,5.6,4.2,3,5.1,1,-3.4,-6.2,-5.1,-3,-1.3,
            2.1,2,-1.9,1.3,3.3,4.9,
            START=c(1920,1),FREQ=1),
  k     =TIMESERIES(182.8,182.6,184.5,189.7,192.7,197.8,203.4,207.6,210.6,215.7,
            216.7,213.3,207.1,202,199,197.7,199.8,201.8,199.9,
            201.2,204.5,209.4,
            START=c(1920,1),FREQ=1),
  p     =TIMESERIES(12.7,12.4,16.9,18.4,19.4,20.1,19.6,19.8,21.1,21.7,15.6,11.4,
            7,11.2,12.3,14,17.6,17.3,15.3,19,21.1,23.5,
            START=c(1920,1),FREQ=1),
  w1    =TIMESERIES(28.8,25.5,29.3,34.1,33.9,35.4,37.4,37.9,39.2,41.3,37.9,34.5,
            29,28.5,30.6,33.2,36.8,41,38.2,41.6,45,53.3,
            START=c(1920,1),FREQ=1),
  y     =TIMESERIES(43.7,40.6,49.1,55.4,56.4,58.7,60.3,61.3,64,67,57.7,50.7,41.3,
            45.3,48.9,53.3,61.8,65,61.2,68.4,74.1,85.3,
            START=c(1920,1),FREQ=1),
  t     =TIMESERIES(3.4,7.7,3.9,4.7,3.8,5.5,7,6.7,4.2,4,7.7,7.5,8.3,5.4,6.8,7.2,
            8.3,6.7,7.4,8.9,9.6,11.6,
            START=c(1920,1),FREQ=1),
  time  =TIMESERIES(NA,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,  
            START=c(1920,1),FREQ=1),   
  w2    =TIMESERIES(2.2,2.7,2.9,2.9,3.1,3.2,3.3,3.6,3.7,4,4.2,4.8,5.3,5.6,6,6.1,
            7.4,6.7,7.7,7.8,8,8.5,
            START=c(1920,1),FREQ=1)
)

#load model and model data
model<-LOAD_MODEL(modelText=myModel)
model<-LOAD_MODEL_DATA(model,modelData)

#estimate model
model<-ESTIMATE(model)

#simulate model
model<-SIMULATE(model
               ,TSRANGE=c(1923,1,1941,1)
               ,simConvergence=0.00001
               ,simIterLimit=100
)


#########################################################
#KLEIN MODEL WITH LHS FUNCTIONS


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

#simulate model
model<-SIMULATE(model
               ,TSRANGE=c(1925,1,1930,1)
               ,simConvergence=0.00001
               ,simIterLimit=100
)

#########################################################
#SIMPLE MODEL WITH IV


#define the model with IVs
myShortModelDefinition<-"
MODEL
COMMENT> Consumption with IV
BEHAVIORAL> cn
TSRANGE 1925 1 1941 1
EQ> cn = a1 + a2*p + a3*TSLAG(p,1) + a4*(w1+w2)
COEFF> a1 a2 a3 a4
IV> 1
IV> TSLAG(y)
IV> TSLAG(w1)*pi+0.5
IV> exp(w2)
END
" 

#load model 
myShortModel<-LOAD_MODEL(modelText=myShortModelDefinition)

}
