# chimeraviz

<details>

* Version: 
* GitHub: https://github.com/r-lib/lintr
* Source code: NA
* Number of recursive dependencies: 0

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# foundry

<details>

* Version: 
* GitHub: https://github.com/r-lib/lintr
* Source code: NA
* Number of recursive dependencies: 0

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# FSelectorRcpp

<details>

* Version: 0.3.11
* GitHub: https://github.com/mi2-warsaw/FSelectorRcpp
* Source code: https://github.com/cran/FSelectorRcpp
* Date/Publication: 2023-04-28 16:10:02 UTC
* Number of recursive dependencies: 158

Run `revdepcheck::revdep_details(, "FSelectorRcpp")` for more info

</details>

## In both

*   checking whether package ‘FSelectorRcpp’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/michael/github/lintr/revdep/checks/FSelectorRcpp/new/FSelectorRcpp.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘FSelectorRcpp’ ...
** package ‘FSelectorRcpp’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0’
using C++ compiler: ‘g++ (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0’
g++ -std=gnu++17 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/Rcpp/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/BH/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/RcppArmadillo/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/testthat/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2 -flto -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/Rcpp/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/BH/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/RcppArmadillo/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/testthat/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2 -flto -c cutOff.cpp -o cutOff.o
g++ -std=gnu++17 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/Rcpp/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/BH/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/RcppArmadillo/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/testthat/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2 -flto -c discretize.cpp -o discretize.o
g++ -std=gnu++17 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/Rcpp/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/BH/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/RcppArmadillo/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/testthat/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2 -flto -c discretizeControl.cpp -o discretizeControl.o
...
g++ -std=gnu++17 -shared -g -O2 -flto -fpic -L/usr/local/lib/R/lib -L/usr/local/lib -o FSelectorRcpp.so RcppExports.o cutOff.o discretize.o discretizeControl.o information_gain.o init.o support.o test-cpp.o test-runner.o -L/usr/local/lib/R/lib -lR
lto1: fatal error: error writing to /tmp/cckFIEZ9.s: No space left on device
compilation terminated.
lto-wrapper: fatal error: g++ returned 1 exit status
compilation terminated.
/usr/bin/ld: error: lto-wrapper failed
collect2: error: ld returned 1 exit status
make: *** [/usr/local/lib/R/share/make/shlib.mk:10: FSelectorRcpp.so] Error 1
ERROR: compilation failed for package ‘FSelectorRcpp’
* removing ‘/home/michael/github/lintr/revdep/checks/FSelectorRcpp/new/FSelectorRcpp.Rcheck/FSelectorRcpp’


```
### CRAN

```
* installing *source* package ‘FSelectorRcpp’ ...
** package ‘FSelectorRcpp’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0’
using C++ compiler: ‘g++ (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0’
g++ -std=gnu++17 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/Rcpp/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/BH/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/RcppArmadillo/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/testthat/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2 -flto -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/Rcpp/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/BH/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/RcppArmadillo/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/testthat/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2 -flto -c cutOff.cpp -o cutOff.o
g++ -std=gnu++17 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/Rcpp/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/BH/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/RcppArmadillo/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/testthat/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2 -flto -c discretize.cpp -o discretize.o
g++ -std=gnu++17 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/Rcpp/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/BH/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/RcppArmadillo/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/testthat/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2 -flto -c discretizeControl.cpp -o discretizeControl.o
...
g++ -std=gnu++17 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/Rcpp/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/BH/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/RcppArmadillo/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/testthat/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2 -flto -c test-cpp.cpp -o test-cpp.o
g++ -std=gnu++17 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/Rcpp/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/BH/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/RcppArmadillo/include' -I'/home/michael/github/lintr/revdep/library/FSelectorRcpp/testthat/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2 -flto -c test-runner.cpp -o test-runner.o
g++ -std=gnu++17 -shared -g -O2 -flto -fpic -L/usr/local/lib/R/lib -L/usr/local/lib -o FSelectorRcpp.so RcppExports.o cutOff.o discretize.o discretizeControl.o information_gain.o init.o support.o test-cpp.o test-runner.o -L/usr/local/lib/R/lib -lR
lto-wrapper: fatal error: g++ returned 1 exit status
compilation terminated.
/usr/bin/ld: error: lto-wrapper failed
collect2: error: ld returned 1 exit status
make: *** [/usr/local/lib/R/share/make/shlib.mk:10: FSelectorRcpp.so] Error 1
ERROR: compilation failed for package ‘FSelectorRcpp’
* removing ‘/home/michael/github/lintr/revdep/checks/FSelectorRcpp/old/FSelectorRcpp.Rcheck/FSelectorRcpp’


```
# latrend

<details>

* Version: 
* GitHub: https://github.com/r-lib/lintr
* Source code: NA
* Number of recursive dependencies: 0

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# MetaScope

<details>

* Version: 
* GitHub: https://github.com/r-lib/lintr
* Source code: NA
* Number of recursive dependencies: 0

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# mlr

<details>

* Version: 
* GitHub: https://github.com/r-lib/lintr
* Source code: NA
* Number of recursive dependencies: 0

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# RLSeq

<details>

* Version: 
* GitHub: https://github.com/r-lib/lintr
* Source code: NA
* Number of recursive dependencies: 0

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# simaerep

<details>

* Version: 0.4.3
* GitHub: https://github.com/openpharma/simaerep
* Source code: https://github.com/cran/simaerep
* Date/Publication: 2023-03-03 10:30:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::revdep_details(, "simaerep")` for more info

</details>

## Newly broken

*   checking whether package ‘simaerep’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/michael/github/lintr/revdep/checks/simaerep/new/simaerep.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘simaerep’ ...
** package ‘simaerep’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
Warning in FUN(X[[i]], ...) : internal error -3 in R_decompress1
*** copying figures
...
** building package indices
Warning in FUN(X[[i]], ...) : internal error -3 in R_decompress1
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
Error in base::serialize(base::as.list(base::getNamespace("simaerep"),  : 
  error writing to connection
Calls: <Anonymous> -> withCallingHandlers -> <Anonymous>
Execution halted
ERROR: loading failed
* removing ‘/home/michael/github/lintr/revdep/checks/simaerep/new/simaerep.Rcheck/simaerep’


```
### CRAN

```
* installing *source* package ‘simaerep’ ...
** package ‘simaerep’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (simaerep)


```
# singleCellTK

<details>

* Version: 
* GitHub: https://github.com/r-lib/lintr
* Source code: NA
* Number of recursive dependencies: 0

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# TBSignatureProfiler

<details>

* Version: 
* GitHub: https://github.com/r-lib/lintr
* Source code: NA
* Number of recursive dependencies: 0

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
