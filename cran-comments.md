## Comments
This is a resubmission, it fixes a bug in the previous submission that caused
an error in a reverse dependency.

This release fixes R CMD check issues in R-devel, as requested by CRAN.

## Test environments

* local: linux-gnu-3.6.1
* travis: 3.2, 3.3, 3.4, oldrel, release, devel
* r-hub: windows-x86_64-devel, ubuntu-gcc-release, fedora-clang-devel
* win-builder: windows-x86_64-devel

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

* I have run R CMD check on the 81 downstream dependencies.
  Summary at: https://github.com/jimhester/lintr/blob/master/revdep/
  There were no errors found.
