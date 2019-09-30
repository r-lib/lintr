# DepthProc

<details>

* Version: 2.1.1
* Source code: https://github.com/cran/DepthProc
* URL: https://www.depthproc.zstat.pl/, https://github.com/zzawadz/DepthProc
* BugReports: https://github.com/zzawadz/DepthProc/issues
* Date/Publication: 2019-04-22 12:00:03 UTC
* Number of recursive dependencies: 108

Run `revdep_details(,"DepthProc")` for more info

</details>

## In both

*   checking whether package ‘DepthProc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘.../revdep/checks.noindex/DepthProc/new/DepthProc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DepthProc’ ...
** package ‘DepthProc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c Depth.cpp -o Depth.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c LocationEstimators.cpp -o LocationEstimators.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c LocationScaleDepth.cpp -o LocationScaleDepth.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c LocationScaleDepthCPP.cpp -o LocationScaleDepthCPP.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RobCovLib.cpp -o RobCovLib.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c TukeyDepth.cpp -o TukeyDepth.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c Utils.cpp -o Utils.o
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
make: *** [LocationScaleDepth.o] Error 1
make: *** Waiting for unfinished jobs....
make: *** [LocationScaleDepthCPP.o] Error 1
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
make: *** [Utils.o] Error 1
clang: error: unsupported option '-fopenmp'make: *** [TukeyDepth.o] Error 1

clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
make: *** [LocationEstimators.o] Error 1
make: *** [RobCovLib.o] Error 1
make: *** [Depth.o] Error 1
ERROR: compilation failed for package ‘DepthProc’
* removing ‘.../revdep/checks.noindex/DepthProc/new/DepthProc.Rcheck/DepthProc’

```
### CRAN

```
* installing *source* package ‘DepthProc’ ...
** package ‘DepthProc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/DepthProc/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c Depth.cpp -o Depth.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/DepthProc/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c LocationEstimators.cpp -o LocationEstimators.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/DepthProc/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c LocationScaleDepth.cpp -o LocationScaleDepth.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/DepthProc/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c LocationScaleDepthCPP.cpp -o LocationScaleDepthCPP.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/DepthProc/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/DepthProc/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RobCovLib.cpp -o RobCovLib.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/DepthProc/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c TukeyDepth.cpp -o TukeyDepth.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/DepthProc/Rcpp/include" -I".../revdep/library.noindex/DepthProc/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c Utils.cpp -o Utils.o
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
make: *** Waiting for unfinished jobs....
clang: error: unsupported option '-fopenmp'
make: *** [LocationEstimators.o] Error 1
make: *** [TukeyDepth.o] Error 1
clang: error: unsupported option '-fopenmp'clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'

make: *** [LocationScaleDepthCPP.o] Error 1
make: *** [Depth.o] Error 1
make: *** [LocationScaleDepth.o] Error 1
make: *** [RobCovLib.o] Error 1
clang: error: unsupported option '-fopenmp'
make: *** [Utils.o] Error 1
ERROR: compilation failed for package ‘DepthProc’
* removing ‘.../revdep/checks.noindex/DepthProc/old/DepthProc.Rcheck/DepthProc’

```
# FSelectorRcpp

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/FSelectorRcpp
* URL: http://mi2-warsaw.github.io/FSelectorRcpp/
* BugReports: https://github.com/mi2-warsaw/FSelectorRcpp/issues
* Date/Publication: 2019-04-22 11:40:08 UTC
* Number of recursive dependencies: 122

Run `revdep_details(,"FSelectorRcpp")` for more info

</details>

## In both

*   checking whether package ‘FSelectorRcpp’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘.../revdep/checks.noindex/FSelectorRcpp/new/FSelectorRcpp.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘FSelectorRcpp’ ...
** package ‘FSelectorRcpp’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/new/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/new/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c cutOff.cpp -o cutOff.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/new/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c discretize.cpp -o discretize.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/new/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c discretizeControl.cpp -o discretizeControl.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/new/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c information_gain.cpp -o information_gain.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/new/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/new/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c support.cpp -o support.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/lintr/new/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/new/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c test-cpp.cpp -o test-cpp.o
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
make: *** [test-cpp.o] Error 1
make: *** Waiting for unfinished jobs....
make: *** [cutOff.o] Error 1
make: *** [RcppExports.o] Error 1
clang: error: unsupported option '-fopenmp'
make: *** [discretize.o] Error 1
clang: error: unsupported option '-fopenmp'
make: *** [discretizeControl.o] Error 1
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
make: *** [support.o] Error 1
make: *** [information_gain.o] Error 1
ERROR: compilation failed for package ‘FSelectorRcpp’
* removing ‘.../revdep/checks.noindex/FSelectorRcpp/new/FSelectorRcpp.Rcheck/FSelectorRcpp’

```
### CRAN

```
* installing *source* package ‘FSelectorRcpp’ ...
** package ‘FSelectorRcpp’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/old/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/old/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c cutOff.cpp -o cutOff.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/old/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c discretize.cpp -o discretize.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/old/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c discretizeControl.cpp -o discretizeControl.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/old/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c information_gain.cpp -o information_gain.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/old/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/old/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c support.cpp -o support.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I".../revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I".../revdep/library.noindex/FSelectorRcpp/BH/include" -I".../revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I".../revdep/library.noindex/lintr/old/testthat/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -I../inst/include -fPIC  -Wall -g -O2  -c test-cpp.cpp -o test-cpp.o
clang: error: clangclang: errorclang: error: clang: error: : unsupported option '-fopenmp'unsupported option '-fopenmp'unsupported option '-fopenmp'unsupported option '-fopenmp'

: 
error: unsupported option '-fopenmp'

clang: error: unsupported option '-fopenmp'
make: *** [test-cpp.o] Error 1
make: *** Waiting for unfinished jobs....
make: *** [support.o] Error 1
make: *** [information_gain.o] Error 1
make: *** [discretizeControl.o] Error 1
make: *** [cutOff.o] Error 1
make: *** [RcppExports.o] Error 1
clang: error: unsupported option '-fopenmp'
make: *** [discretize.o] Error 1
ERROR: compilation failed for package ‘FSelectorRcpp’
* removing ‘.../revdep/checks.noindex/FSelectorRcpp/old/FSelectorRcpp.Rcheck/FSelectorRcpp’

```
# fst

<details>

* Version: 0.9.0
* Source code: https://github.com/cran/fst
* URL: https://fstpackage.github.io
* BugReports: https://github.com/fstpackage/fst/issues
* Date/Publication: 2019-04-09 04:43:13 UTC
* Number of recursive dependencies: 43

Run `revdep_details(,"fst")` for more info

</details>

## In both

*   checking whether package ‘fst’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘.../revdep/checks.noindex/fst/new/fst.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fst’ ...
** package ‘fst’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/lintr/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/lintr/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c flex_store.cpp -o flex_store.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/lintr/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c fst_blockrunner_char.cpp -o fst_blockrunner_char.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/lintr/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c fst_compress.cpp -o fst_compress.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/lintr/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c fst_error.cpp -o fst_error.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/lintr/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c fst_table.cpp -o fst_table.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/lintr/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/lintr/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c openmp.cpp -o openmp.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
make: *** Waiting for unfinished jobs....
clang: error: unsupported option '-fopenmp'
make: *** [fst_compress.o] Error 1
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
make: *** [flex_store.o] Error 1
make: *** [fst_error.o] Error 1
make: *** [fst_blockrunner_char.o] Error 1
clang: error: unsupported option '-fopenmp'
make: *** [fst_table.o] Error 1
clang: error: unsupported option '-fopenmp'
make: *** [openmp.o] Error 1
ERROR: compilation failed for package ‘fst’
* removing ‘.../revdep/checks.noindex/fst/new/fst.Rcheck/fst’

```
### CRAN

```
* installing *source* package ‘fst’ ...
** package ‘fst’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/fst/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/fst/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c flex_store.cpp -o flex_store.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/fst/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c fst_blockrunner_char.cpp -o fst_blockrunner_char.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/fst/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c fst_compress.cpp -o fst_compress.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/fst/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c fst_error.cpp -o fst_error.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/fst/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c fst_table.cpp -o fst_table.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/fst/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I. -Ifstcore -Ifstcore_v1 -Ifstcore/LZ4 -Ifstcore/ZSTD -Ifstcore/ZSTD/common -Ifstcore/ZSTD/decompress -Ifstcore/ZSTD/compress -I".../revdep/library.noindex/fst/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c openmp.cpp -o openmp.o
clang: error: unsupported option '-fopenmp'clang: error: unsupported option '-fopenmp'

make: *** [RcppExports.o] Error 1
make: *** Waiting for unfinished jobs....
make: *** [fst_blockrunner_char.o] Error 1
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
make: *** [flex_store.o] Error 1
make: *** [fst_error.o] Error 1
make: *** [fst_compress.o] Error 1
clang: error: unsupported option '-fopenmp'
make: *** [fst_table.o] Error 1
clang: error: unsupported option '-fopenmp'
make: *** [openmp.o] Error 1
ERROR: compilation failed for package ‘fst’
* removing ‘.../revdep/checks.noindex/fst/old/fst.Rcheck/fst’

```
