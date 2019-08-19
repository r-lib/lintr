# biolink

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# BTYDplus

Version: 1.0.1

## In both

*   R CMD check timed out
    

# caretEnsemble

Version: 2.0.0

## In both

*   R CMD check timed out
    

# CoGAPS

Version: 2.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      (.../revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:122-124)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘Samples’
      (.../revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:126-128)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘value’
      (.../revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:126-128)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘BySet’
      (.../revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:126-128)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘Samples’
      (.../revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:130-132)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘value’
      (.../revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:130-132)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘BySet’
      (.../revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:130-132)
    Undefined global functions or variables:
      BySet Samples i value
    ```

# dat

Version: 0.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tests/testthat/test-open_curly_linter.R:30:1: style: lines should not be more than 100 characters.
          rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tests/testthat/test-open_curly_linter.R:34:1: style: lines should not be more than 100 characters.
          rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      
      testthat results ================================================================
      OK: 108 SKIPPED: 0 FAILED: 1
      1. Failure: Package Style (@test-lintr.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# datarobot

Version: 2.7.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘curl’
      All declared Imports should be used.
    ```

# datastructures

Version: 0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        doc    1.2Mb
        libs   3.7Mb
    ```

# DBItest

Version: 1.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘desc’
      All declared Imports should be used.
    ```

# diffusr

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# easyml

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘corrplot’ ‘scorer’
      All declared Imports should be used.
    ```

# edpclient

Version: 0.2.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 55 SKIPPED: 1 FAILED: 16
      1. Failure: source lints (@test_lint.R#7) 
      2. Error: populations can be listed (@test_population.R#3) 
      3. Error: population object utilities work (@test_population.R#12) 
      4. Error: population model object utilities work (@test_population.R#22) 
      5. Error: population models on a population can be listed (@test_population.R#35) 
      6. Error: we can find the latest population model (@test_population.R#43) 
      7. Error: basic edp_session operations work (@test_session.R#3) 
      8. Error: we can work with population models (@test_session.R#11) 
      9. Error: we can select from yaxcatpeople (@test_session.R#24) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# geofacet

Version: 0.1.5

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
         ^
      inst/example/bad.R:9:5: style: Trailing whitespace is superfluous.
        5}  
          ^~
      inst/example/bad.R:10:1: error: unexpected end of input
      {
      ^
      
      
      testthat results ================================================================
      OK: 14 SKIPPED: 0 FAILED: 1
      1. Failure: package Style (@test-zzz-lintr.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 79 marked UTF-8 strings
    ```

# ggfortify

Version: 0.4.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfortify-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gglagplot
    > ### Title: Plot time series against lagged versions of themselves
    > ### Aliases: gglagplot
    > 
    > ### ** Examples
    > 
    > gglagplot(AirPassengers)
    Error: `x` must be a vector, not a ts object, do you want `stats::lag()`?
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      12: lag(ts, k)
      13: bad_args("x", "must be a vector, not a ts object, do you want `stats::lag()`?")
      14: glubort(fmt_args(args), ..., .envir = .envir)
      15: .abort(text)
      
      testthat results ================================================================
      OK: 1617 SKIPPED: 0 FAILED: 5
      1. Failure: autoplot.aareg works for lung (@test-surv.R#220) 
      2. Failure: autoplot.aareg works for lung (@test-surv.R#221) 
      3. Failure: autoplot.aareg works for lung (@test-surv.R#222) 
      4. Failure: autoplot.aareg works for lung (@test-surv.R#223) 
      5. Error: gglagplot (@test-tslib.R#103) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc   5.0Mb
    ```

# ggthemes

Version: 3.4.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘latticeExtra’
    ```

# modules

Version: 0.6.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tests/testthat/test-open_curly_linter.R:30:1: style: lines should not be more than 100 characters.
          rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tests/testthat/test-open_curly_linter.R:34:1: style: lines should not be more than 100 characters.
          rex("Opening curly braces should never go on their own line and should always be followed by a new line."),
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      
      testthat results ================================================================
      OK: 62 SKIPPED: 0 FAILED: 1
      1. Failure: Package Style (@test-lintr.R#3) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# netReg

Version: 1.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 65.5Mb
      sub-directories of 1Mb or more:
        include  64.8Mb
    ```

# opencage

Version: 0.1.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: `_f`(placename = placename, key = key, bounds = bounds, countrycode = countrycode, 
             language = language, limit = limit, min_confidence = min_confidence, no_annotations = no_annotations, 
             no_dedupe = no_dedupe, no_record = no_record, abbrv = abbrv)
      4: opencage_check(temp) at .../revdep/checks/opencage/new/opencage.Rcheck/00_pkg_src/opencage/R/forward_geocoding.R:44
      5: stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE) at .../revdep/checks/opencage/new/opencage.Rcheck/00_pkg_src/opencage/R/utils.R:5
      
      testthat results ================================================================
      OK: 23 SKIPPED: 0 FAILED: 4
      1. Error: opencage_parse returns what it should for both functions (@test-opencage_parse.R#6) 
      2. Error: opencage_parse returns what it should for both functions with several parameters (@test-opencage_parse.R#40) 
      3. Error: opencage_parse deals well with resuls being NULL (@test-opencage_parse.R#72) 
      4. Error: the bounds argument is well taken into account (@test-opencage_parse.R#86) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 41-47 (opencage.Rmd) 
    Error: processing vignette 'opencage.Rmd' failed with diagnostics:
    HTTP failure: 403
    Invalid or missing api key (forbidden)
    Execution halted
    ```

# rbokeh

Version: 0.5.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘shiny’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# rodham

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 59 marked UTF-8 strings
    ```

# ropenaq

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      uh oh, the OpenAQ API seems to be having some issues, try again later
      1: aq_measurements(page = 1, limit = 10, city = "Chennai") at testthat/test-measurements.R:31
      2: getResults(urlAQ, argsList) at .../revdep/checks/ropenaq/new/ropenaq.Rcheck/00_pkg_src/ropenaq/R/measurements.R:104
      3: getResults_bypage(urlAQ, argsList) at .../revdep/checks/ropenaq/new/ropenaq.Rcheck/00_pkg_src/ropenaq/R/utils.R:218
      4: stop("uh oh, the OpenAQ API seems to be having some issues, try again later") at .../revdep/checks/ropenaq/new/ropenaq.Rcheck/00_pkg_src/ropenaq/R/utils.R:237
      
      testthat results ================================================================
      OK: 75 SKIPPED: 0 FAILED: 4
      1. Error: Queries work with spaces and accents (@test-buildQueries.R#54) 
      2. Error: The value_from and value_to arguments work as they should (@test-measurements.R#10) 
      3. Error: measurements returns a data.frame (tbl_df) (@test-measurements.R#16) 
      4. Error: measurements has the right columns (@test-measurements.R#31) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 34-48 (using_openair_package_with_openaq_data.Rmd) 
    Error: processing vignette 'using_openair_package_with_openaq_data.Rmd' failed with diagnostics:
    uh oh, the OpenAQ API seems to be having some issues, try again later
    Execution halted
    ```

# tuber

Version: 0.9.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
         ^
      inst/example/bad.R:9:5: style: Trailing whitespace is superfluous.
        5}  
          ^~
      inst/example/bad.R:10:1: error: unexpected end of input
      {
      ^
      
      
      testthat results ================================================================
      OK: 4 SKIPPED: 0 FAILED: 1
      1. Failure: Package Style (@test-pkg-style.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# virustotal

Version: 0.2.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      inst/example/bad.R:9:5: style: Trailing whitespace is superfluous.
        5}  
          ^~
      inst/example/bad.R:10:1: error: unexpected end of input
      {
      ^
      
      
      testthat results ================================================================
      OK: 6 SKIPPED: 0 FAILED: 2
      1. Error: can decrypt secrets and data structures verified (@test-data-structures.R#40) 
      2. Failure: Package Style (@test-pkg-style.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

