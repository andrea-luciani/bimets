
**bimets Change-Log**

*bimets ver. 1.5.0 - May 2020*

* added LHS functions on `MDL` `EQ>` definition
* added `avoidCompliance` in `ESTIMATE`, `SIMULATE`, `RENORM`
* added `residuals_no_error_correction` in `ESTIMATE`
* added `TSDELTALOG` time series function
* removed `stat:time` from code
* fixed typos on `README.md`


*bimets ver. 1.4.3 - Mar. 2020*

* added structural stability analysis in `ESTIMATE`
* added endogenous selection in `SIMULATE` of type `RESCHECK`
* removed check of existence for all vendog and vexog in `CHECK_MODEL_DATA`
* added `forceTSRANGE` in `ESTIMATE`
* added frequency and more details on model object
* added mode details on behaviorals and `ESTIMATE` statistics
* added compliance with R-devel (R-4.0.0)
* fixed bugs on stats reports

*bimets ver. 1.4.2 - Oct. 2019*

* Licensed under GPL-3


*bimets ver. 1.4.1 - Jun. 2019*

* Fixed typos on *.Rnw
* Added support to xts 0.12


*bimets ver. 1.4.0 - Nov. 2018*

* Refined simulation capabilities.
* Refined modeling documentation.


*bimets ver. 1.3.1 - Apr. 2018*

* Added weekly support, i.e. frequency 53.


*bimets ver. 1.3.0 - Mar. 2018*

* Multi-Platform Support Code Refactoring.


*bimets ver. 1.2.20 - Nov. 2017*

* Added function `CUMPROD`.


*bimets ver. 1.2.19 - Nov. 2017*

* Added function `TSTRIM`.


*bimets ver. 1.2.18 - Aug. 2017*

* Added function `MOVTOT`.


*bimets ver. 1.2.16 - Jun. 2017*

* Fixed Bugs in `TSMERGE`.
* Added option `EXTEND `in `TSPROJECT`.


*bimets ver. 1.2.15 - Mar. 2017*

* Added option `FILLVALUE ` and `VERBOSE `in `as.bimets`.


*bimets ver. 1.2.13 - Aug. 2016*

* Added option `NSTOCK `in `ANNUAL`, `SEMIANNUAL`, `QUARTERLY`, `MONTHLY`.


*bimets ver. 1.2.11 - Feb. 2016*

* Added function `CUMULO`.


*bimets ver. 1.2.10 - Feb. 2015*

* Added function `INDEXNUM`.
* Fixed bug in `as.bimets()` with a single observation time series.
* Extended time range in 1800-2199.


*bimets ver. 1.2.7 - Apr. 2014*

* Added option `BIMETS_CONF_NOC `in order to globally disable the compliance check.


*bimets ver. 1.2.6 - Mar. 2014*

* Added support to frequencies 24 and 36.


*bimets ver. 1.2.5 - Mar. 2014*

* Speedup in timeseries conversion from and to bimets.
* Added `Date()`, `yearmon()` and `yearqtr()` as input class of argument `START` in the `TSERIES` function.
* Added array as input class in functions `date2yp()`, `yq2yp()`, `ym2yp()`.
* `xtsPeriodicity()` and `frequency(xts)` now give high priority to the attribute `.bimetsFreq`, if any.


*bimets ver. 1.2.4 - Mar. 2014*

* New global option for selecting base class `BIMETS_CONF_CCT`.
* Added function `is.bimets`, added class-type check in function `isCompliant`.
* Added function `as.bimets`, `fromBIMETStoTS`, `fromBIMETStoXTS`.
* Added `coredata` filter in override `[[.xts`.
* Bug fix in function `date2yp()`.


*bimets ver. 1.2.3 - Jan. 2014*

* Bug fix in xts() daily in range 1970-1979 on daylight saving time change.


*bimets ver. 1.2.1 - Nov. 2013*

* Added daily support in functions `ANNUAL`, `SEMIANNUAL`, `QUARTERLY`, `MONTHLY`.
* Added function `DAILY`.


*bimets ver. 1.2.0 - Nov. 2013*

* Added single observation timeseries support.


*bimets ver. < 1.2.0*

* Draft private releases
