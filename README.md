# lintr
[![R build status](https://github.com/r-lib/lintr/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/lintr/actions)
[![codecov.io](https://codecov.io/github/r-lib/lintr/coverage.svg?branch=master)](https://codecov.io/github/r-lib/lintr?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/lintr)](https://cran.r-project.org/package=lintr)
[![Join the chat at https://gitter.im/jimhester-lintr/Lobby](https://badges.gitter.im/jimhester-lintr/Lobby.svg)](https://gitter.im/jimhester-lintr/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

`lintr` provides [static code analysis for R](https://en.wikipedia.org/wiki/Static_program_analysis). It checks for adherence to a given style, identifying syntax errors and possible semantic issues, then reports them to you so you can take action. Watch lintr in action in the following animation:

![](http://i.imgur.com/acV27NV.gif "")

`lintr` is complementary to [the `styler` package](https://github.com/r-lib/styler) which automatically restyles code, eliminating some of the problems that `lintr` can detect.

## Installation

Install the stable version from CRAN:

```R
install.packages("cli")
```

Or the development version from GitHub:

```R
devtools::install_github("r-lib/lintr")
```

## Usage

```R
# in a project:
lintr::use_lintr(type = "tidyverse")
usethis::use_github_action("lint-project")
lintr::lint_dir()

# in a package:
lintr::use_lintr(type = "tidyverse")
usethis::use_github_action("lint")
lintr::lint_package()
```

You can also run lintr during continuous integration or within your IDE or text editor. See `vignette("continuous-integration")` and `vignette("editor")` for more details.

Without further configuration, this will run the [default linters](https://lintr.r-lib.org/reference/default_linters.html). See `vignette("lintr")` to learn how to modify these defaults.
