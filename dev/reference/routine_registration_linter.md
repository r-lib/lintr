# Identify unregistered native routines

It is preferable to register routines for efficiency and safety.

## Usage

``` r
routine_registration_linter()
```

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Registering-native-routines>

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = '.Call("cpp_routine", PACKAGE = "mypkg")',
  linters = routine_registration_linter()
)
#> <text>:1:7: warning: [routine_registration_linter] Register your native code routines with useDynLib and R_registerRoutines().
#> .Call("cpp_routine", PACKAGE = "mypkg")
#>       ^~~~~~~~~~~~~

lint(
  text = '.Fortran("f_routine", PACKAGE = "mypkg")',
  linters = routine_registration_linter()
)
#> <text>:1:10: warning: [routine_registration_linter] Register your native code routines with useDynLib and R_registerRoutines().
#> .Fortran("f_routine", PACKAGE = "mypkg")
#>          ^~~~~~~~~~~

# okay
lint(
  text = ".Call(cpp_routine)",
  linters = routine_registration_linter()
)
#> ℹ No lints found.

lint(
  text = ".Fortran(f_routine)",
  linters = routine_registration_linter()
)
#> ℹ No lints found.
```
