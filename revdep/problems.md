# biolink

Version: 0.1.2

## Newly fixed

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
         })
      9: FUN(X[[i]], ...)
      10: lint(file, ..., parse_settings = FALSE)
      11: get_source_expressions(filename)
      12: extract_r_source(source_file$filename, source_file$lines)
      13: get_knitr_pattern(filename, lines)
      14: detect_pattern(lines, tolower(file_ext(filename)))
      15: grep(pat, text)
      
      testthat results ================================================================
      OK: 116 SKIPPED: 0 FAILED: 1
      1. Error: Package Style (@test-style.r#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# CoGAPS

Version: 2.10.0

## In both

*   checking whether package ‘CoGAPS’ can be installed ... WARNING
    ```
    ...
      GibbsSampler-update.cpp:602:18: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      GibbsSampler-update.cpp:604:14: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      GibbsSampler-update.cpp:721:46: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      GibbsSampler-update.cpp:722:46: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      GibbsSampler-update.cpp:778:18: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      GibbsSampler-update.cpp:780:14: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      GibbsSampler-update.cpp:1009:54: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      GibbsSampler-update.cpp:1010:54: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      GibbsSampler-update.cpp:1103:46: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      GibbsSampler-update.cpp:1104:46: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cogapsmaptestR.cpp:177:27: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cogapsmaptestR.cpp:179:27: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cogapsmaptestR.cpp:181:27: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cogapsmaptestR.cpp:183:27: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cogapsmaptestR.cpp:528:18: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cogapstestR.cpp:159:27: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cogapstestR.cpp:161:27: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cogapstestR.cpp:163:27: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cogapstestR.cpp:165:27: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cogapstestR.cpp:532:18: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
    See ‘/Users/jhester/Dropbox/projects/lintr/revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00install.out’ for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/jhester/Dropbox/projects/lintr/revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:122-124)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘Samples’
      (/Users/jhester/Dropbox/projects/lintr/revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:126-128)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘value’
      (/Users/jhester/Dropbox/projects/lintr/revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:126-128)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘BySet’
      (/Users/jhester/Dropbox/projects/lintr/revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:126-128)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘Samples’
      (/Users/jhester/Dropbox/projects/lintr/revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:130-132)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘value’
      (/Users/jhester/Dropbox/projects/lintr/revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:130-132)
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘BySet’
      (/Users/jhester/Dropbox/projects/lintr/revdep/checks/CoGAPS/new/CoGAPS.Rcheck/00_pkg_src/CoGAPS/R/patternMatcher.R:130-132)
    Undefined global functions or variables:
      BySet Samples i value
    ```

# datastructures

Version: 0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 19.7Mb
      sub-directories of 1Mb or more:
        doc    1.2Mb
        libs  17.4Mb
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

Version: 0.1.1

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

# geofacet

Version: 0.1.5

## Newly fixed

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
         })
      9: FUN(X[[i]], ...)
      10: lint(file, ..., parse_settings = FALSE)
      11: get_source_expressions(filename)
      12: extract_r_source(source_file$filename, source_file$lines)
      13: get_knitr_pattern(filename, lines)
      14: detect_pattern(lines, tolower(file_ext(filename)))
      15: grep(pat, text)
      
      testthat results ================================================================
      OK: 14 SKIPPED: 0 FAILED: 1
      1. Error: package Style (@test-zzz-lintr.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

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
      8: eval_bare(dot$expr, dot$env)
      9: lapply(seq(1:lags), .lag)
      10: FUN(X[[i]], ...)
      11: as.vector(lag(ts, k)) at /Users/jhester/Dropbox/projects/lintr/revdep/checks/ggfortify/new/ggfortify.Rcheck/00_pkg_src/ggfortify/R/tslib.R:336
      12: lag(ts, k)
      13: bad_args("x", "must be a vector, not a ts object, do you want `stats::lag()`?")
      14: glubort(fmt_args(args), ..., .envir = .envir)
      15: .abort(text)
      
      testthat results ================================================================
      OK: 1442 SKIPPED: 9 FAILED: 1
      1. Error: gglagplot (@test-tslib.R#103) 
      
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

# netReg

Version: 1.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 66.0Mb
      sub-directories of 1Mb or more:
        include  64.8Mb
    ```

# Plasmidprofiler

Version: 0.1.6

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### Name: zetner_score
    > ### Title: Adds the Zetner Score column to report
    > ### Aliases: zetner_score
    > 
    > ### ** Examples
    > 
    > ## Not run: 
    > ##D zetner_score(report)
    > ## End(Not run)
    > 
    > 
    > 
    > ### * <FOOTER>
    > ###
    > options(digits = 7L)
    > base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
    Time elapsed:  10.307 0.181 13.31 0.518 0.067 
    > grDevices::dev.off()
    Error in grDevices::dev.off() : 
      cannot shut down device 1 (the null device)
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

