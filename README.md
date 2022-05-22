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

## Configuration

Lintr supports per-project configuration of the following fields.
The config file (default file name: `.lintr`) is in [Debian Control Field Format](https://www.debian.org/doc/debian-policy/ch-controlfields.html).

- `linters` - see `?linters_with_defaults` for example of specifying only a few non-default linters and 
  `?linters_with_tags` for more fine-grained control.
- `exclusions` - a list of filenames to exclude from linting.  You can use a
  named item to exclude only certain lines from a file.
- `exclude` - a regex pattern for lines to exclude from linting.  Default is "# nolint"
- `exclude_start` - a regex pattern to start exclusion range. Default is "# nolint start"
- `exclude_end` - a regex pattern to end exclusion range. Default is "# nolint end"
- `encoding` - the encoding used for source files. Default inferred from .Rproj or DESCRIPTION files, fallback to UTF-8

### .lintr File Example

Below is an example .lintr file that uses:

- 120 character line lengths
- Excludes a couple of files
- Disables a specific linter, and; 
- Sets different default exclude regexes
- Specifies the file encoding to be ISO-8859-1 (Latin 1)

```
linters: linters_with_defaults(
  line_length_linter(120), 
  commented_code_linter = NULL
  )
exclusions: list("inst/doc/creating_linters.R" = 1, "inst/example/bad.R", "tests/testthat/exclusions-test")
exclude: "# Exclude Linting"
exclude_start: "# Begin Exclude Linting"
exclude_end: "# End Exclude Linting"
encoding: "ISO-8859-1"
```

With the following command, you can create a configuration file for `lintr` that ignores all linters that show at least one error:

```r
# Create configuration file for lintr
# Source this file in package root directory

# List here files to exclude from lint checking, as a character vector
excluded_files <- c(
    list.files("data",      recursive = TRUE, full.names = TRUE),
    list.files("docs",      recursive = TRUE, full.names = TRUE),
    list.files("inst/doc",  recursive = TRUE, full.names = TRUE),
    list.files("man",       recursive = TRUE, full.names = TRUE),
    list.files("vignettes", recursive = TRUE, full.names = TRUE)
)

### Do not edit after this line ###

library(magrittr)
library(dplyr)

# Make sure we start fresh
if (file.exists(".lintr")) { file.remove(".lintr") }

# List current lints
lintr::lint_package() %>%
    as.data.frame %>%
    group_by(linter) %>%
    tally(sort = TRUE) %$%
    sprintf("linters: linters_with_defaults(\n    %s\n    dummy_linter = NULL\n  )\n",
            paste0(linter, " = NULL, # ", n, collapse = "\n    ")) %>%
    cat(file = ".lintr")

sprintf("exclusions: list(\n    %s\n  )\n",
        paste0('"', excluded_files, '"', collapse = ",\n    ")) %>%
    cat(file = ".lintr", append = TRUE)

# Clean up workspace
remove(excluded_files)
```

The resulting configuration will contain each currently failing linter and the corresponding number of hits as a comment. Proceed by successively enabling linters, starting with those with the least number of hits. Note that this requires `lintr` 0.3.0.9001 or later.

If you are developing a package, you can add `^\.lintr$` to your `.Rbuildignore` file using `usethis::use_build_ignore(".lintr")`.


