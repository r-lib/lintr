# backport_linter produces error when R version misspecified

    Code
      lint(text = "numToBits(2)", linters = backport_linter(420L))
    Condition
      Error in `normalize_r_version()`:
      ! `r_version` must be an R version number, returned by `R_system_version()`, or a string.

