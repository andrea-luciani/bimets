# bimets 3.0.1

May. 2023

* added `simAlgo` argument in `SIMULATE`
* added `NEWTON` option in `simAlgo` argument in `SIMULATE`
* added `FULLNEWTON` option in `simAlgo` argument in `SIMULATE`
* added `JacobianDrop` argument in `SIMULATE`
* added `JACOBIAN_SHOCK` argument in `SIMULATE`
* optimized coefficients management in `SIMULATE` (in order to be compliant with this optimization, models created with previous `bimets` versions and stored in binary file format as `.RData` must be recreated via `LOAD_MODEL` procedure)

# bimets 2.3.0 

Dec. 2022

* added a new `MATRIX` option of `StochStructure` in `STOCHSIMULATE`

# bimets 2.2.0 

Aug. 2022

* fixed iteration count in error message in `RENORM`
* fixed `R-devel KaTeX` compliance in help pages html rendering

# bimets 2.1.1

Jun. 2022

* extended report in `__RENORM_PARAMETERS__`
* fixed vignette in `MDL EQ RHS` function list
* fixed unexpected warning in `STOCHSIMULATE`

# bimets 2.1.0

Apr. 2022

* added `VERIFY_MAGNITUDE`
* added `GETRANGE`
* fixed bug in `ESTIMATE` if `CHOWTEST`, `tol` and `IV` enabled
* reduced tolerance in matrix inversion 
* fixed `R-devel` compliance in class comparison

# bimets 2.0.2

Feb. 2022

* added workaround in `stats::window` bug in daily series
* fixed minor bug and typos in vignette
* added git support

# bimets 2.0.1

Jan. 2022

* added `STOCHSIMULATE`
* added `OPTIMIZE`

# bimets 1.5.3

Mar. 2021

* fixed information messages in `MULTMATRIX`
* added missings check in lagged time series in `SIMULATE`
* fixed error messages in `SIMULATE` and `RENORM`
* fixed bug in `IF>` condition having `<-` (i.e. `R` assign operator)

# bimets 1.5.2

Dec. 2020

* F-prob in Chow Test output moved from `[0,100]` to `[0,1]`
* `IV>` definition allowed in `MDL`
* added `forceIV` in `ESTIMATE`
* fixed error messages in `SIMULATE` 
* added alias `YEARLY` for `ANNUAL`

# bimets 1.5.1

Jun. 2020

* fixed typos in `MDL LHS` function examples
* fixed bug `TSERIES()` with `character` input data

# bimets 1.5.0

May 2020

* added `LHS` functions in `MDL EQ>` definition
* added `avoidCompliance` in `ESTIMATE`, `SIMULATE`, `RENORM`
* added `residuals_no_error_correction` in `ESTIMATE`
* added `TSDELTALOG` time series function
* removed `stat:time` from code
* fixed typos in `README.md`

# bimets 1.4.3

Mar. 2020

* added structural stability analysis in `ESTIMATE`
* added endogenous selection in `SIMULATE` of type `RESCHECK`
* removed check of existence for all vendog and vexog in `CHECK_MODEL_DATA`
* added `forceTSRANGE` in `ESTIMATE`
* added frequency and more details in model object
* added mode details in behaviorals and `ESTIMATE` statistics
* added compliance with `R-devel` (i.e. `R-4.0.0`)
* fixed bugs in `ESTIMATE` stats reports

# bimets 1.4.2

Oct. 2019

* Licensed under GPL-3

# bimets 1.4.1

Jun. 2019

* Fixed typos in `*.Rnw`
* Added support to `xts()` ver. 0.12


# bimets 1.4.0

Nov. 2018

* Refined simulation capabilities
* Refined modeling documentation


# bimets 1.3.1

Apr. 2018

* Added weekly support, i.e. frequency 53


# bimets 1.3.0

Mar. 2018

* Multi-Platform Support Code Refactoring

# bimets 1.2.20

Nov. 2017

* Added function `CUMPROD`

# bimets 1.2.19

Nov. 2017

* Added function `TSTRIM`

# bimets 1.2.18

Aug. 2017

* Added function `MOVTOT`

# bimets 1.2.16

Jun. 2017

* Fixed Bugs in `TSMERGE`
* Added option `EXTEND `in `TSPROJECT`

# bimets 1.2.15

Mar. 2017

* Added option `FILLVALUE ` and `VERBOSE `in `as.bimets`

# bimets 1.2.13

Aug. 2016

* Added option `NSTOCK `in `ANNUAL`, `SEMIANNUAL`, `QUARTERLY`, `MONTHLY`

# bimets 1.2.11

Feb. 2016

* Added function `CUMULO`.

# bimets 1.2.10

Feb. 2015

* Added function `INDEXNUM`
* Fixed bug in `as.bimets` with a single observation time series
* Extended time range in 1800-2199

# bimets 1.2.7

Apr. 2014

* Added option `BIMETS_CONF_NOC `in order to globally disable the compliance check

# bimets 1.2.6

Mar. 2014

* Added support to frequencies 24 and 36

# bimets 1.2.5

Mar. 2014

* Speedup in time series conversion from/to `bimets`
* Added `Date`, `yearmon` and `yearqtr` as input class of argument `START` in the `TSERIES` function
* Added array as input class in functions `date2yp`, `yq2yp`, `ym2yp`
* `xtsPeriodicity` and `xts.frequency` now give high priority to attribute `.bimetsFreq`, if any

# bimets 1.2.4

Mar. 2014

* New global option for selecting base class `BIMETS_CONF_CCT`
* Added function `is.bimets`, added class-type check in function `isCompliant`
* Added function `as.bimets`, `fromBIMETStoTS`, `fromBIMETStoXTS`
* Added `coredata` filter in override `[[.xts`
* Bug fix in function `date2yp`

# bimets 1.2.3

Jan. 2014

* Bug fix in `xts()` daily in range 1970-1979 in daylight saving time change

# bimets 1.2.1

Nov. 2013  

* Added daily support in functions `ANNUAL`, `SEMIANNUAL`, `QUARTERLY`, `MONTHLY`
* Added function `DAILY`

# bimets 1.2.0

Nov. 2013

* Added single observation time series support

# bimets 1.1.0

...and earlier versions.

* Draft private releases
